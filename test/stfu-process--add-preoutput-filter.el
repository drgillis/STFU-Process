(ert-deftest stfu-process--prepend-preoutput-filter ()
  (with-temp-buffer
    (comint-mode)
    (setq comint-preoutput-filter-functions '(identity))
    (setq stfu-process-add-filter-placement -1)
    (stfu-process-mode)
    (should (equal comint-preoutput-filter-functions '(stfu-process-preoutput-filter identity)))))

(ert-deftest stfu-process--append-preoutput-filter ()
  (with-temp-buffer
    (comint-mode)
    (setq comint-preoutput-filter-functions '(identity))
    (setq stfu-process-add-filter-placement 1)
    (stfu-process-mode)
    (should (equal comint-preoutput-filter-functions '(identity stfu-process-preoutput-filter)))))



(defun stfu-process--test-custom-placement (current-filter-list preoutput-filter)
  (cons (car current-filter-list)
        (cons preoutput-filter
              (cdr current-filter-list))))


(ert-deftest stfu-process--custom-placement-preoutput-filter ()
  (with-temp-buffer
    (comint-mode)
    (setq comint-preoutput-filter-functions '(identity identity identity))
    (setq stfu-process-add-filter-placement 'stfu-process--test-custom-placement)
    (stfu-process-mode)
    (should (equal comint-preoutput-filter-functions '(identity stfu-process-preoutput-filter identity identity)))))
