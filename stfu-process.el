;;; stfu-process.el --- Truncate long output from processes -*- lexical-binding: t -*-

;; Copyright (C) 2023 Dan Gillis

;; Author: Dan Gillis <dev@dangillis.net>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: processes
;; URL: https://github.com/drgillis/stfu-process

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


;;; Commentary:
;; stfu-process-mode (Stop Text From Unterse Process)

;;; Code:

;; NOTE: This mode is very much a work-in-progress at this stage
;; plan for now: modify `comint-preoutput-filter-functions`
;; WARNING: This mode may not play nice with other output filters!

;; TODO: add ability to use in buffers associated with process!
;; TODO: try using set-process-filter rather than comint-preoutput-filter-functions?
(require 'cl-lib)
(require 'comint)

(defgroup stfu-process nil
  "Truncate long output from processes."
  :group 'processes)

(defcustom stfu-process-suppression-string ".\n"
  "The string to replace long outputs with."
  :type 'string
  :group 'stfu-process)

(defcustom stfu-process-suppression-long-line-string "\n[...STFU-Process continued]\n"
  "The string to separate long lines with."
  :type 'string
  :group 'stfu-process)

(defcustom stfu-process-total-limit 100000
  "Output length limit before output is suppressed."
  :type 'natnum
  :group 'stfu-process)

(defcustom stfu-process-line-limit 5000
  "Output line length limit before output is broken up."
  :type 'natnum
  :group 'stfu-process)


(defcustom stfu-process-add-filter-placement -1
  "Where to insert the STFU filter in `comint-preoutput-filter-functions`.

A value can either be an integer or a function.  A negative valued integer
prepends the filter.  A non-negative value appends the filter.  A function
should take the current filter list and the filter to be placed and return
the new filter list.

In the future, the integer may be used to place it in a specific place."
  :type '(choice (integer :tag "Placement in filter functions")
                 (function :tag "Generate new fliter functions"))
  :group 'stfu-process)

(defvar stfu-process-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-!") 'stfu-now)
    map))

(defvar-local stfu-process--original-preoutput-filters nil)


(defvar-local stfu-process--cur-output-length 0)
(defvar-local stfu-process--cur-line-length 0)

;; Really hacky of way trying to tell if an output string may be the prompt
;; - (flawed) logic here: long outputs will tend to fill the pty/pipe output
;; - buffer and this value is far below that amount (typically 1024)
;; TODO: improve this
;; - a better solution might involve the buffer's comint-prompt-regexp
(defvar-local stfu-process--min-output-len-nonprompt 50)

(defun stfu-process--reset-cur-output-length ()
  "Reset current output length."
  (setq stfu-process--cur-output-length 0))

(defun stfu-process--add-str-len-to-cur-output-length (str-len)
  "Add STR-LEN to current output length."
  (setq stfu-process--cur-output-length (+ stfu-process--cur-output-length str-len)))


(defun stfu-process--update-cur-line-length
    (has-newline len-after-newline true-str-len)
  "Update current line length after processing string.
If incoming string HAS-NEWLINE, then this length is LEN-AFTER-NEWLINE.
Otherwise, add TRUE-STR-LEN to the existing length.  Note that the
TRUE-STR-LEN subtracts any backspace characters from the incoming string
length (so as to accomodate text that updates in-place)."
  (setq stfu-process--cur-line-length
        (if has-newline
            len-after-newline
          (+ stfu-process--cur-line-length true-str-len))))

(defun stfu-process--cur-output-too-long-p ()
  "Check if current output is longer than total limit."
  (> stfu-process--cur-output-length stfu-process-total-limit))


(defun stfu-process--cur-line-too-long-p ()
  "Check if current line is longer than line limit."
  (> stfu-process--cur-line-length stfu-process-line-limit))

(defun stfu-process--get-supression-length-str (str-len)
  "Generate a string showing the number (STR-LEN) of characters suppressed."
  (concat "["
          (number-to-string str-len)
          " characters truncated]"))

(defun stfu-process--handle-too-long-output (str-len)
  "Handle output that has exceeded total limit.
Take STR-LEN to generate information about suppressed text."
  (concat (stfu-process--get-supression-length-str str-len)
          stfu-process-suppression-string))

(defun stfu-process--handle-too-long-line (string true-str-len)
    "Handle STRING when line length exceeds limit.
Take TRUE-STR-LEN to properly update line length."
  (setq stfu-process--cur-line-length
        (+ true-str-len
           (length stfu-process-suppression-long-line-string)))
  (concat stfu-process-suppression-long-line-string string))


(defun stfu-process--should-reset-output-length-p (string)
  "Determine from STRING whether output length should be reset."
  ;; super hacky way of doing this
  ;; TODO: replace with better method in future
  (< (length string) stfu-process--min-output-len-nonprompt))


(defun stfu-process--handle-output-length (string)
  "Handle the action needed to update output length based on STRING."
  (if (stfu-process--should-reset-output-length-p string)
        ;; possibly the prompt: reset-output length
        (stfu-process--reset-cur-output-length)
    (stfu-process--add-str-len-to-cur-output-length (length string))))


(defun stfu-process--handle-line-length (string)
  "Handle the actions needed to update line length based on STRING."
  (let* ((str-len (length string))
         (has-newline (cl-search "\n" string :from-end t))
         (last-newline-loc (or has-newline 0))
         (len-after-newline (- str-len last-newline-loc))
         ;; todo: allow user to choose whether to count backspaces
         (num-backspaces (cl-count ?\b string)))
    (stfu-process--update-cur-line-length has-newline
                                          len-after-newline
                                          (- str-len num-backspaces))
    (cond ((stfu-process--cur-output-too-long-p)
           (stfu-process--handle-too-long-output str-len))
          ;; TODO: let user decide whether to fully suppress or to insert newline
          ((stfu-process--cur-line-too-long-p)
           (stfu-process--handle-too-long-line string (- str-len num-backspaces)))
          (t string))))


(defun stfu-process-preoutput-filter (string)
  "Truncate incoming STRING if line or total output is too long."
    (stfu-process--handle-output-length string)
    (stfu-process--handle-line-length string))


(defun stfu-process--append-preoutput-filter ()
  "Add the preoutput-filter to the end of the list of filter functions."
  (append comint-preoutput-filter-functions '(stfu-process-preoutput-filter)))

(defun stfu-process--prepend-preoutput-filter ()
  "Add the preoutput-filter to the beginning of the list of filter functions."
  (cons 'stfu-process-preoutput-filter
        comint-preoutput-filter-functions))

(defun stfu-process-add-preoutput-filter ()
  "Add the preoutput-filter to the list of filter functions."
  (let ((new-filter-list (if (numberp stfu-process-add-filter-placement)
                             (if (>= stfu-process-add-filter-placement 0)
                                 (stfu-process--append-preoutput-filter)
                               (stfu-process--prepend-preoutput-filter))
                           (funcall stfu-process-add-filter-placement
                                    comint-preoutput-filter-functions
                                    'stfu-process-preoutput-filter))))
    (setq-local comint-preoutput-filter-functions new-filter-list)))


(defun stfu-process--set-cur-output (val)
  "Set current total length to VAL."
  (setq stfu-process--cur-output-length val))

(defun stfu-process--set-cur-line (val)
  "Set current line length to VAL."
  (setq stfu-process--cur-line-length val))

;; TODO: Should filtration instead be toggled with flag?
(defun stfu-process--set-cur-output-to-max ()
  "Set current total length to a value exceeding the maximum allowed.
This will initiate the truncation process."
  (setq stfu-process--cur-output-length (+ 1
                                           stfu-process-total-limit)))

;; TODO: Should line filtration instead be toggled with flag?
(defun stfu-process--set-cur-line-to-max ()
    "Set current line length to a value exceeding the maximum allowed.
This will initiate the truncation process."
  (setq stfu-process--cur-line-length (+ 1
                                         stfu-process-line-limit)))


(defun stfu-process-ignore ()
  "Immediately activate stfu filter but don't kill stream."
  (interactive)
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (stfu-process-mode)
      (stfu-process--set-cur-output-to-max)
      (stfu-process--set-cur-line-to-max))))


(defun stfu-process-now ()
  "Activate STFU filter and kill stream (if possible)."
  (interactive)
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (stfu-process-ignore)
      (when (process-tty-name process)
        (comint-interrupt-subjob)))))


;; optional aliases
; (defalias 'stfu-now 'stfu-process-now)
; (defalias 'stfu-ignore 'stfu-process-ignore)

(define-minor-mode stfu-process-mode
  "Toggle STFU Process mode.

When enabled, STFU Process mode changes the process output
filter to suppress output if prompt."
  :init-value nil
  :lighter " STFU"
  :keymap stfu-process-mode-map
  (if stfu-process-mode
      (progn
        (setq stfu-process--original-preoutput-filters
              comint-preoutput-filter-functions)
        (stfu-process-add-preoutput-filter))
    ;; revert process
    ;; WARNING: This will dismiss any changes made to the process
    ;; variables while STFU is active!
    ;; - in future, may just remove new filter instead!
    (setq comint-preoutput-filter-functions stfu-process--original-preoutput-filters)))


(provide 'stfu-process)
;;; stfu-process.el ends here
