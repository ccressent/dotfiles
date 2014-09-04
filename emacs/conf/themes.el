(load-theme 'solarized-dark t)

(defun toggle-solarized-dark-light ()
  "Switch between Solarized dark and light themes."
  (interactive)
  (if (eq (frame-parameter (next-frame) 'background-mode) 'dark)
      (load-theme 'solarized-light t)
    (load-theme 'solarized-dark t)))

(provide 'conf/themes)
