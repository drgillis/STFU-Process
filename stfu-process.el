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
;; plan for now: modify comint-preoutput-filter-functions
;; WARNING: This mode may not play nice with other output filters!

(defgroup stfu-process nil
  "")


(defcustom stfu-process-supression-string ".\n"
  ""
  :group 'stfu-process)

(defcustom stfu-process-supression-long-line "\n[...STFU-Process continued]"
  ""
  :group 'stfu-process)


(define-minor-mode stfu-process-mode
  "Toggle STFU Process mode if in `comint-mode`.

When enabled, STFU Process mode changes the process output
filter to surpress output if prompt.
"
  :init-value nil
  :lighter " STFU")
