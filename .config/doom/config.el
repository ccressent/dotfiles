;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Use Ivy's regex-ignore-order completion style everywhere.
(setq ivy-re-builders-alist
    '((t . ivy--regex-ignore-order)))

;; Variables to figure out
;; org-tags-column
;; org-tag-alist
;; org-persistent-tag-alist and add @home @work?
;; org-agenda-skip-deadline-prewarning-if-scheduled
;; org-todo-keywords

(setq org-directory (expand-file-name "~/notes/"))
(setq org-default-notes-file (expand-file-name "refile.org" org-directory))

(setq org-ellipsis " ⤵"
      org-hide-emphasis-markers t)

(setq org-capture-templates
  '(("t" "todo" entry
     (file+headline "" "Tasks")
     "* [ ] %?\n%i" :kill-buffer t)

    ("n" "note" entry
     (file+headline "" "Notes")
     "* %u %?\n%i" :kill-buffer t)))

(setq org-refile-targets '((nil . (:maxlevel . 5))
                           (org-agenda-files . (:maxlevel . 5))))

;; When refiling, use the full org outline paths, prefixed by the file name, and
;; do not try to complete a path in hierarchical order: we use fuzzy matching to
;; find the right target.
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)

;; Allow creation of new nodes when refiling, after confirmation.
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Add an advice to org-refile so that after a refile, all the org buffers get
;; automatically saved.
;;
;; An alternative approach could have been to use org-after-refile-insert-hook,
;; but while these functions are called after content is added to the refile
;; target, they are called _before_ the content is removed from the old
;; location, leaving the source buffer unsaved.
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(setq org-agenda-files (directory-files-recursively org-directory "\.org$"))
(setq org-agenda-restore-windows-after-quit t)

(setq org-agenda-custom-commands
      '((" " "Agenda"
         ((agenda "" nil)
          (tags "REFILE"
                ((org-agenda-overriding-header "Nodes to refile")
                 (org-tags-match-list-sublevels t)))))))

;; Number of lines of margin to keep at the top and bottom when moving around.
(setq scroll-margin 20)

(setq magit-repository-directories '(("~/src" . 2)))
(setq projectile-project-search-path '("~/src"))

(map!
  (:leader
    (:prefix "f"
      :desc "Find file in dotfiles" :n "d" #'+ccressent/find-in-dotfiles)
    (:prefix "p"
      :desc "Test project" :n "t" #'projectile-test-project)))
