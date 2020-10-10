;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(setq doom-theme 'doom-solarized-light)

;; Use relative line numbers
(setq display-line-numbers-type 'relative)

;; Use Ivy's regex-ignore-order completion style everywhere.
(setq ivy-re-builders-alist
    '((t . ivy--regex-ignore-order)))

;; I'm experimenting with having all my org-mode and org-agenda related settings
;; in a literate configuration file. Let's tangle it and load it.
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "orgmode.org" doom-private-dir))

;; Number of lines of margin to keep at the top and bottom when moving around.
(setq scroll-margin 20)

(setq magit-repository-directories '(("~/src" . 2) ("~/.dotfiles")))
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
    ;; Ideally I should change magit-mode-map to that my bindings are added to
    ;; all the derived magit modes. I think the issue is that some of these
    ;; magit mode specific maps already define a g prefix and override their
    ;; parent map.
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
