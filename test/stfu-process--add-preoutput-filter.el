(ert-deftest stfu-process--prepend-preoutput-filter ()
  (with-temp-buffer
    (comint-mode)
    (setq comint-preoutput-filter-functions '(identity))
    (setq stfu-process-add-filter-placement -1)
    (stfu-process-mode)
    (should (equal comint-preoutput-filter-functions '(stfu-process-preoutput-filter identity)))))

;; (ert-deftest stfu-process--append-preoutput-filter ())
(ert-deftest stfu-process--append-preoutput-filter ()
  (with-temp-buffer
    (comint-mode)
    (setq comint-preoutput-filter-functions '(identity))
    (setq stfu-process-add-filter-placement 1)
    (stfu-process-mode)
    (should (equal comint-preoutput-filter-functions '(identity stfu-process-preoutput-filter)))))


;; (ert-deftest stfu-process--custom-preoutput-filter ())
