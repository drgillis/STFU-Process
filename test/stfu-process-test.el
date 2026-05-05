;;; stfu-process-test.el --- Tests for stfu-process -*- lexical-binding: t -*-

;;; Commentary:
;; ERT tests for stfu-process.el

;;; Code:

(require 'ert)
(require 'stfu-process)

;;; Core truncation logic tests

(ert-deftest stfu-process-test-output-under-limit ()
  "Output under the limit should pass through unchanged."
  (with-temp-buffer
    (comint-mode)
    (stfu-process-mode 1)
    (let ((stfu-process-total-limit 100)
          (stfu-process-line-limit 50))
      (setq stfu-process--cur-output-length 0)
      (setq stfu-process--cur-line-length 0)
      (should (equal (stfu-process-preoutput-filter "hello")
                     "hello")))))

(ert-deftest stfu-process-test-output-exceeds-total-limit ()
  "Output exceeding total limit should be truncated."
  (with-temp-buffer
    (comint-mode)
    (stfu-process-mode 1)
    (let ((stfu-process-total-limit 100)
          (stfu-process-line-limit 5000)
          (stfu-process--min-output-len-nonprompt 10))
      (setq stfu-process--cur-output-length 0)
      (setq stfu-process--cur-line-length 0)
      ;; First, push output length past the limit
      (stfu-process-preoutput-filter (make-string 101 ?x))
      ;; Now additional output should be truncated (must be longer than min-nonprompt)
      (let ((result (stfu-process-preoutput-filter (make-string 50 ?y))))
        (should (string-match-p "truncated" result))
        (should-not (string-match-p "yyyyy" result))))))

(ert-deftest stfu-process-test-line-exceeds-limit ()
  "Line exceeding line limit should have separator inserted."
  (with-temp-buffer
    (comint-mode)
    (stfu-process-mode 1)
    (let ((stfu-process-total-limit 100000)
          (stfu-process-line-limit 50))
      (setq stfu-process--cur-output-length 0)
      (setq stfu-process--cur-line-length 0)
      ;; Push line length past the limit
      (stfu-process-preoutput-filter (make-string 51 ?x))
      ;; Additional output on the same line should get separator
      (let ((result (stfu-process-preoutput-filter "more")))
        (should (string-match-p "STFU-Process continued" result))))))

(ert-deftest stfu-process-test-newline-resets-line-length ()
  "Newline should reset line length tracking."
  (with-temp-buffer
    (comint-mode)
    (stfu-process-mode 1)
    (let ((stfu-process-total-limit 100000)
          (stfu-process-line-limit 50))
      (setq stfu-process--cur-output-length 0)
      (setq stfu-process--cur-line-length 0)
      ;; Long line followed by newline
      (stfu-process-preoutput-filter (concat (make-string 40 ?x) "\n"))
      ;; Line length should have reset, so this should pass through
      (let ((result (stfu-process-preoutput-filter (make-string 30 ?y))))
        (should (equal result (make-string 30 ?y)))))))

(ert-deftest stfu-process-test-short-output-resets-total ()
  "Short output (likely prompt) should reset total output length."
  (with-temp-buffer
    (comint-mode)
    (stfu-process-mode 1)
    (let ((stfu-process-total-limit 100)
          (stfu-process-line-limit 5000)
          (stfu-process--min-output-len-nonprompt 50))
      (setq stfu-process--cur-output-length 0)
      (setq stfu-process--cur-line-length 0)
      ;; Build up output length close to limit
      (stfu-process-preoutput-filter (make-string 80 ?x))
      ;; Short output (like a prompt) should reset
      (stfu-process-preoutput-filter ">>> ")
      ;; Now we should be able to output more without truncation
      (let ((result (stfu-process-preoutput-filter (make-string 80 ?y))))
        (should (equal result (make-string 80 ?y)))))))

;;; Filter placement tests (from stfu-process--add-preoutput-filter.el)

(defun stfu-process--test-custom-placement (current-filter-list preoutput-filter)
  "Test helper: place filter after first element."
  (cons (car current-filter-list)
        (cons preoutput-filter
              (cdr current-filter-list))))

(ert-deftest stfu-process-test-prepend-preoutput-filter ()
  "Test prepending of output filter to comint filter functions."
  (with-temp-buffer
    (comint-mode)
    (setq comint-preoutput-filter-functions '(identity))
    (setq stfu-process-add-filter-placement -1)
    (stfu-process-mode 1)
    (should (equal comint-preoutput-filter-functions
                   '(stfu-process-preoutput-filter identity)))))

(ert-deftest stfu-process-test-append-preoutput-filter ()
  "Test appending of output filter to comint filter functions."
  (with-temp-buffer
    (comint-mode)
    (setq comint-preoutput-filter-functions '(identity))
    (setq stfu-process-add-filter-placement 1)
    (stfu-process-mode 1)
    (should (equal comint-preoutput-filter-functions
                   '(identity stfu-process-preoutput-filter)))))

(ert-deftest stfu-process-test-custom-placement-preoutput-filter ()
  "Test custom function for placing output filter."
  (with-temp-buffer
    (comint-mode)
    (setq comint-preoutput-filter-functions '(identity identity identity))
    (setq stfu-process-add-filter-placement 'stfu-process--test-custom-placement)
    (stfu-process-mode 1)
    (should (equal comint-preoutput-filter-functions
                   '(identity stfu-process-preoutput-filter identity identity)))))

(ert-deftest stfu-process-test-remove-preoutput-filter ()
  "Test that disabling mode actually removes the filter."
  (with-temp-buffer
    (comint-mode)
    (setq comint-preoutput-filter-functions '(identity))
    (stfu-process-mode 1)
    (should (memq 'stfu-process-preoutput-filter comint-preoutput-filter-functions))
    (stfu-process-mode -1)
    (should-not (memq 'stfu-process-preoutput-filter comint-preoutput-filter-functions))))

(provide 'stfu-process-test)
;;; stfu-process-test.el ends here
