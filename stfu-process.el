;;; stfu-process.el --- Emacs Python Development Environment -*- lexical-binding: t -*-

;; Copyright (C) 2022 Dan Gillis

;; Author: Dan Gillis <daniel.r.gillis@gmail.com>
;; URL: https://github.com/drgillis/stfu-process
;; Version: 0.0.1
;; Keywords: comint, shell, process
;; Package-Requires: TBD

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
;; stfu-process-mode (Stop Text From Un-terminating Process)

;;; Code:

;; NOTE: This mode is very much a work-in-progress at this stage
;; plan for now: modify `comint-preoutput-filter-functions`
;; WARNING: This mode may not play nice with other output filters!

;; TODO: add ability to use in buffers associated with process!
(require 'cl-lib)

(defgroup stfu-process nil
  "")


(defcustom stfu-process-suppression-string ".\n"
  ""
  :group 'stfu-process)

(defcustom stfu-process-suppression-long-line-string "\n[...STFU-Process continued]"
  ""
  :group 'stfu-process)

(defvar-local stfu-process--original-preoutput-filters nil)


(defvar-local stfu-process--cur-output-length 0)
(defvar-local stfu-process--cur-line-length 0)

;; Really hacky of way trying to tell if an output string may be the prompt
;; - (flawed) logic here: long outputs will tend to fill the pty/pipe output 
;; - buffer and this value is far below that amount (typically 1024)
;; TODO: improve this
;; a better solution might involve the buffer's comint-prompt-regexp
(defvar-local stfu-process--min-output-len-nonprompt 50)

;; in-future, nil value will prevent output from being suppressed
(defvar-local stfu-process--max-output-len 100000)

(defvar-local stfu-process--max-output-line-len 5000)


(defun stfu-process-preoutput-filter (string)
  (let* ((str-len (length string))
         (has-newline (cl-search "\n" string :from-end t))
         (last-newline-loc (or has-newline 0))
         (len-after-newline (- str-len last-newline-loc))
         ;; todo: allow user to choose whether to count backspaces
         (num-backspaces (cl-count ?\b string)))
    (if (< str-len stfu-process--min-output-len-nonprompt)
        ;; possibly the prompt: reset-output length
        (setq stfu-process--cur-output-length 0)
      (setq stfu-process--cur-output-length (+ (length string)
                                               stfu-process--cur-output-length)))
    (setq stfu-process--cur-line-length (if has-newline
                                            len-after-newline
                                          (+ stfu-process--cur-line-length
                                             (- str-len
                                                num-backspaces))))
    (cond ((> stfu-process--cur-output-length stfu-process--max-output-len)
           stfu-process-suppression-string)
          ;; TODO: let user decide whether to fully suppress or to insert newline
          ((> stfu-process--cur-line-length stfu-process--max-output-line-len)
           (setq stfu-process--cur-line-length (+ str-len (length stfu-process-suppression-long-line-string)))
           (concat stfu-process-suppression-long-line-string string))
          (t string))))


(defun stfu-process-add-preoutput-filter ()
  (setq-local comint-preoutput-filter-functions
              (cons 'stfu-process-preoutput-filter
                    comint-preoutput-filter-functions)))


(define-minor-mode stfu-process-mode
  "Toggle STFU Process mode.

When enabled, STFU Process mode changes the process output
filter to suppress output if prompt.
"
  :init-value nil
  :lighter " STFU"
  (let ((buffer-process (get-buffer-process (current-buffer))))
    (if stfu-process-mode
        (progn
          (setq stfu-process--original-preoutput-filters comint-preoutput-filter-functions)
          (stfu-process-add-preoutput-filter))
      ;; revert process
      ;; WARNING: This will dismiss any changes made to the process
      ;; variables while STFU is active!
      ;; - in future, may just remove new filter instead!
      (setq comint-preoutput-filter-functions stfu-process--original-preoutput-filters))))
