;; Test functions for adding preoutput filter to `comint-preoutput-filter-functions
;; Note: Many or all of these tests are implementation dependent; all rely on
;; setting `stfu-process-add-filter-placement` directly.

(defun stfu-process--test-custom-placement (current-filter-list preoutput-filter)
  (cons (car current-filter-list)
        (cons preoutput-filter
              (cdr current-filter-list))))

(ert-deftest stfu-process--prepend-preoutput-filter ()
  "Tests prepending of output filter to comint filter functions."
  (with-temp-buffer
    (comint-mode)
    (setq comint-preoutput-filter-functions '(identity))
    (setq stfu-process-add-filter-placement -1)
    (stfu-process-mode)
    (should (equal comint-preoutput-filter-functions '(stfu-process-preoutput-filter identity)))))

(ert-deftest stfu-process--append-preoutput-filter ()
  "Tests appending of output filter to comint filter functions."
  (with-temp-buffer
    (comint-mode)
    (setq comint-preoutput-filter-functions '(identity))
    (setq stfu-process-add-filter-placement 1)
    (stfu-process-mode)
    (should (equal comint-preoutput-filter-functions '(identity stfu-process-preoutput-filter)))))

(ert-deftest stfu-process--custom-placement-preoutput-filter ()
  "Tests use of custom function for placing output filter into comint filter functions."
  (with-temp-buffer
    (comint-mode)
    (setq comint-preoutput-filter-functions '(identity identity identity))
    (setq stfu-process-add-filter-placement 'stfu-process--test-custom-placement)
    (stfu-process-mode)
    (should (equal comint-preoutput-filter-functions '(identity stfu-process-preoutput-filter identity identity)))))
