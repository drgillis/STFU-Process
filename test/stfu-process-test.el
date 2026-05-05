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

;;; Backspace handling tests

(ert-deftest stfu-process-test-backspace-reduces-line-length ()
  "Backspaces should reduce tracked line length."
  (with-temp-buffer
    (comint-mode)
    (stfu-process-mode 1)
    (let ((stfu-process-total-limit 100000)
          (stfu-process-line-limit 20))
      (setq stfu-process--cur-output-length 0)
      (setq stfu-process--cur-line-length 0)
      ;; Print "hello" (5 chars), then backspace 3 and print "xyz" (3 chars)
      ;; Net line length should be 5 (he + xyz)
      (stfu-process-preoutput-filter "hello")
      (should (= stfu-process--cur-line-length 5))
      (stfu-process-preoutput-filter "\b\b\bxyz")
      ;; 6 chars - 2*3 backspaces = 0 net change, so still 5
      (should (= stfu-process--cur-line-length 5)))))

(ert-deftest stfu-process-test-progress-bar-style-output ()
  "Progress bar style output with backspaces should not trigger line limit."
  (with-temp-buffer
    (comint-mode)
    (stfu-process-mode 1)
    (let ((stfu-process-total-limit 100000)
          (stfu-process-line-limit 30))
      (setq stfu-process--cur-output-length 0)
      (setq stfu-process--cur-line-length 0)
      ;; Simulate progress: "Progress: 10%" then update to "20%", "30%", etc.
      (stfu-process-preoutput-filter "Progress: 10%")
      (should (= stfu-process--cur-line-length 13))
      ;; Backspace over "10%" and write "20%"
      (let ((result (stfu-process-preoutput-filter "\b\b\b20%")))
        ;; Should pass through unchanged (no truncation)
        (should (equal result "\b\b\b20%"))
        ;; Line length should still be 13 (net change: 6 - 2*3 = 0)
        (should (= stfu-process--cur-line-length 13)))
      ;; Continue updating...
      (stfu-process-preoutput-filter "\b\b\b30%")
      (stfu-process-preoutput-filter "\b\b\b40%")
      (stfu-process-preoutput-filter "\b\b\b50%")
      ;; Line length should remain stable, not triggering the limit
      (should (< stfu-process--cur-line-length stfu-process-line-limit)))))

(ert-deftest stfu-process-test-backspace-line-length-floor-zero ()
  "Line length should not go negative with excessive backspaces."
  (with-temp-buffer
    (comint-mode)
    (stfu-process-mode 1)
    (let ((stfu-process-total-limit 100000)
          (stfu-process-line-limit 100))
      (setq stfu-process--cur-output-length 0)
      (setq stfu-process--cur-line-length 0)
      ;; Print short text then many backspaces
      (stfu-process-preoutput-filter "hi")
      (should (= stfu-process--cur-line-length 2))
      ;; 10 backspaces would try to make length negative
      (stfu-process-preoutput-filter "\b\b\b\b\b\b\b\b\b\b")
      ;; Should floor at 0
      (should (= stfu-process--cur-line-length 0)))))

(ert-deftest stfu-process-test-mixed-text-and-backspaces ()
  "Mixed text and backspaces in same string should be handled."
  (with-temp-buffer
    (comint-mode)
    (stfu-process-mode 1)
    (let ((stfu-process-total-limit 100000)
          (stfu-process-line-limit 100))
      (setq stfu-process--cur-output-length 0)
      (setq stfu-process--cur-line-length 0)
      ;; "abc\b\b\bxyz" = erase abc, write xyz, net 3 chars
      (stfu-process-preoutput-filter "abc\b\b\bxyz")
      ;; Length: 9 chars - 2*3 backspaces = 3
      (should (= stfu-process--cur-line-length 3)))))

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

;;; Integration tests with real subprocess

(defun stfu-process-test--wait-for-output (buf pattern &optional timeout)
  "Wait for PATTERN to appear in BUF, with TIMEOUT seconds (default 5)."
  (let ((timeout (or timeout 5))
        (start-time (float-time))
        (proc (get-buffer-process buf)))
    (while (and proc
                (< (- (float-time) start-time) timeout)
                (not (with-current-buffer buf
                       (string-match-p pattern (buffer-string)))))
      (accept-process-output proc 0.1))))

(ert-deftest stfu-process-test-integration-truncates-long-output ()
  "Integration test: verify truncation with real subprocess."
  (let ((buf (make-comint "stfu-test-proc" "sh" nil)))
    (unwind-protect
        (with-current-buffer buf
          ;; Wait for shell prompt
          (stfu-process-test--wait-for-output buf "\\$ ")
          ;; Now enable stfu-process and configure
          (stfu-process-mode 1)
          (setq-local stfu-process-total-limit 100)
          (setq-local stfu-process-line-limit 5000)
          (setq-local stfu-process--min-output-len-nonprompt 10)
          ;; Generate output exceeding limit (seq 1 200 produces ~600 chars)
          (comint-send-string (get-buffer-process buf) "seq 1 200\n")
          ;; Wait for truncation message
          (stfu-process-test--wait-for-output buf "truncated")
          ;; Buffer should contain truncation message
          (should (string-match-p "truncated" (buffer-string))))
      ;; Cleanup
      (when (get-buffer-process buf)
        (delete-process (get-buffer-process buf)))
      (kill-buffer buf))))

(ert-deftest stfu-process-test-integration-long-line-gets-broken ()
  "Integration test: verify long lines get separator inserted.
Note: This tests the scenario where output arrives in chunks that
build up a long line - the realistic case for slow streaming output."
  (let ((buf (make-comint "stfu-test-proc" "sh" nil)))
    (unwind-protect
        (with-current-buffer buf
          ;; Wait for shell prompt
          (stfu-process-test--wait-for-output buf "\\$ ")
          ;; Now enable stfu-process and configure
          (stfu-process-mode 1)
          (setq-local stfu-process-total-limit 100000)
          (setq-local stfu-process-line-limit 50)
          ;; Use multiple printf calls to simulate chunked output on same line
          ;; Each printf outputs without newline, building up a long line
          (comint-send-string (get-buffer-process buf)
                              "for i in 1 2 3; do printf '%030s' | tr ' ' 'x'; sleep 0.1; done; echo\n")
          ;; Wait for separator message (90 x's across 3 chunks, limit is 50)
          (stfu-process-test--wait-for-output buf "STFU-Process continued")
          ;; Buffer should contain the line-break message
          (should (string-match-p "STFU-Process continued" (buffer-string))))
      ;; Cleanup
      (when (get-buffer-process buf)
        (delete-process (get-buffer-process buf)))
      (kill-buffer buf))))

(ert-deftest stfu-process-test-integration-normal-output-unchanged ()
  "Integration test: normal output passes through unchanged."
  (let ((buf (make-comint "stfu-test-proc" "sh" nil)))
    (unwind-protect
        (with-current-buffer buf
          ;; Wait for shell prompt
          (stfu-process-test--wait-for-output buf "\\$ ")
          ;; Now enable stfu-process and configure
          (stfu-process-mode 1)
          (setq-local stfu-process-total-limit 100000)
          (setq-local stfu-process-line-limit 5000)
          ;; Small output that shouldn't be truncated
          (comint-send-string (get-buffer-process buf) "echo hello\n")
          ;; Wait for hello to appear
          (stfu-process-test--wait-for-output buf "hello")
          ;; Buffer should contain "hello" without truncation
          (should (string-match-p "hello" (buffer-string)))
          (should-not (string-match-p "truncated" (buffer-string))))
      ;; Cleanup
      (when (get-buffer-process buf)
        (delete-process (get-buffer-process buf)))
      (kill-buffer buf))))

(provide 'stfu-process-test)
;;; stfu-process-test.el ends here
