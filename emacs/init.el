(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(dolist (p '(;; core
             auto-complete
             evil
             evil-surround
             exec-path-from-shell
             flx-ido
             flycheck
             key-chord
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

(electric-pair-mode t)

;; Powerline
(require 'powerline)
(require 'powerline-evil)
(powerline-center-evil-theme)

;; make the buffer names unique if needed
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; highlight the current line
(global-hl-line-mode)

(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs empty trailing line-tail))

(require 'auto-complete-config)
(ac-config-default)

(ido-mode t)
(flx-ido-mode t)

(require 'haskell-mode)
(setq haskell-process-type 'cabal-repl)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook
	  (lambda ()
	    (ghc-init)))

(ac-define-source ghc-mod
  '((depends ghc)
    (candidates . (ghc-select-completion-symbol))
    (symbol . "s")
    (cache)))

(defun my-ac-haskell-mode ()
  (setq ac-sources '(ac-source-words-in-same-mode-buffers
		     ac-source-dictionary
		     ac-source-ghc-mod)))
(add-hook 'haskell-mode-hook 'my-ac-haskell-mode)

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/conf")

(require 'conf/evil)
(require 'conf/themes)
(require 'conf-languages)
