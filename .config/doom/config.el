;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(setq org-directory (expand-file-name "~/notes/"))
(setq org-default-notes-file (expand-file-name "refile.org" org-directory))

(setq org-ellipsis " ⤵")

(setq org-capture-templates
  '(("t" "todo" entry
     (file+headline "" "Tasks")
     "* [ ] %?\n%i" :kill-buffer t)

    ("n" "note" entry
     (file+headline "" "Notes")
     "* %u %?\n%i" :kill-buffer t)))

(setq org-refile-targets '((nil . (:maxlevel . 5))
                           (org-agenda-files . (:maxlevel . 5))))

;; Use full outline paths and do not try to complete a path in hierarchical
;; order: we use fuzzy matching to find the right target.
(setq org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil)

;; Allow creation of new nodes when refiling, after confirmation.
(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-agenda-files (directory-files-recursively org-directory "\.org$"))
(setq org-agenda-restore-windows-after-quit t)

;; Number of lines of margin to keep at the top and bottom when moving around.
(setq scroll-margin 20)

(setq magit-repository-directories '(("~/src" . 2)))

;; Enable all of magithub features by default
(setq +magit-hub-features t)

(map!
  (:leader
    (:prefix "f"
      :desc "Find file in dotfiles" :n "d" #'+ccressent/find-in-dotfiles)))
