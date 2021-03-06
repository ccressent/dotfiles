(require 'package)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(dolist (p '(;; core
             auto-complete
             evil
             evil-leader
             evil-surround
             exec-path-from-shell
             flx-ido
             flycheck
             helm
             helm-projectile
             key-chord
             powerline
             projectile

             ;; languages
             haskell-mode
             flycheck-haskell

             lua-mode
             markdown-mode
             scss-mode

             ;; themes
             solarized-theme))
  (unless (package-installed-p p)
    (package-install p)))

;; set environment variables from the shell, when running in a GUI on OSX
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(line-number-mode t)
(column-number-mode t)
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; reload buffers automatically when the underlying file changes
(global-auto-revert-mode t)

(setq-default fill-column 78)
(setq-default show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; get rid of those annoying bells
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Automatically delete trailing whitespaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq show-paren-delay 0)
(show-paren-mode 1)
(electric-pair-mode t)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Helm
(require 'helm)
(require 'helm-config)
(define-key helm-map (kbd "C-z")   'helm-select-action)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

(setq helm-quick-update                 t
      helm-split-window-in-side-p       t
      helm-buffers-fuzzy-matching       t
      helm-move-to-line-cycle-in-source t)

(helm-mode 1)
(global-set-key (kbd "M-x")     'helm-M-x)
(global-set-key (kbd "C-h a")   'helm-apropos)
(global-set-key (kbd "C-x b")   'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Powerline
(require 'powerline)
(powerline-center-evil-theme)

;; highlight the current line
(global-hl-line-mode)

(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs empty trailing line-tail))

(require 'auto-complete-config)
(ac-config-default)

(add-to-list 'load-path (concat user-emacs-directory "conf"))

(require 'conf-evil)
(require 'conf-themes)
(require 'conf-languages)
