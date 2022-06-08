;;;###autoload
(defun +ccressent/find-in-dotfiles ()
  "Open a file somewhere in ~/.dotfiles via a fuzzy filename search."
  (interactive)
  (doom-project-find-file (expand-file-name "~/.dotfiles")))

(defun +ccressent/org-copy-link (&optional arg)
  "Extract the URL from an org-mode link and add it to the kill ring."
  (interactive "P")
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
         (type (org-element-property :type link))
         (path (org-element-property :path link))
         (url  (concat type ":" path)))
    (if (and type path)
        (progn (kill-new url) (message "Copied: %s" url))
      (message "Couldn't find any org link"))))
