;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(setq doom-theme 'doom-solarized-light)

;; Use relative line numbers
(setq display-line-numbers-type 'relative)

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

;; Display "entities", such as \pi and \alpha, as UTF-8 characters. Similarly,
;; sub and superscript is displayed nicely, as long as the sub or superscripted
;; text is enclosed in {}; e.g.: R_{t}, R^{2}.
;;
;; The actual buffer content remains ASCII, this is purely for display purposes.
(setq org-pretty-entities t
      org-use-sub-superscripts '{})

(setq org-capture-templates
  '(("t" "todo" entry
     (file+headline "" "Tasks")
     "* TODO %?\n%i" :kill-buffer t)

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

;; I'm experimenting with having all my org-agenda related settings in a
;; literate configuration file. Let's tangle it whenever needed and load it.
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "agenda.org" doom-private-dir))

;; Number of lines of margin to keep at the top and bottom when moving around.
(setq scroll-margin 20)

(setq magit-repository-directories '(("~/src" . 2)))
(setq projectile-project-search-path '("~/src"))

;; Always use a commit hash rather than a branch name when generating a link to
;; a file or a region.
(setq browse-at-remote-prefer-symbolic nil)

(map!
  (:leader
    (:prefix "f"
      :desc "Find file in dotfiles" :n "d" #'+ccressent/find-in-dotfiles)
    (:prefix "p"
      :desc "Test project" :n "t" #'projectile-test-project))

  (:after evil-magit
    :when (featurep! :ui workspaces)
    :map magit-status-mode-map
      :nv "g t" #'+workspace:switch-next
      :nv "g T" #'+workspace:switch-previous)

  (:after treemacs-evil
    :when (featurep! :ui workspaces)
    :map evil-treemacs-state-map
      "g t" #'+workspace:switch-next
      "g T" #'+workspace:switch-previous))

;; Make TRAMP respect $PATH on the remote machine
;; See: https://www.gnu.org/software/tramp/#Configuration
;; See: https://www.gnu.org/software/tramp/#Remote-programs
(after! tramp (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; The keyfreq package logs the most frequent key presses and functions called
;; and shows summary statistics when calling keyfreq-show. This data can be
;; analyzed to further improve key bindings.
(keyfreq-mode)
(keyfreq-autosave-mode)
