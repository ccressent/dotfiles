;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(setq org-directory "~/notes/")
(setq org-default-notes-file (expand-file-name "refile.org" org-directory))

(setq org-capture-templates
  '(("t" "todo" entry
     (file+headline "" "Tasks")
     "* [ ] %?\n%i" :kill-buffer t)

    ("n" "note" entry
     (file+headline "" "Notes")
     "* %u %?\n%i" :kill-buffer t)))

(setq org-refile-targets '((nil . (:maxlevel . 5))
                           (org-agenda-files . (:maxlevel . 5))))

(setq org-agenda-files (list org-directory))
(setq org-agenda-restore-windows-after-quit t)

;; Use full outline paths and do not try to complete a path in hierarchical
;; order: we use fuzzy matching to find the right target.
(setq org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil)

(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Number of lines of margin to keep at the top and bottom when moving around.
(setq scroll-margin 20)

;; Enable all of magithub features by default
(setq +magit-hub-features t)
