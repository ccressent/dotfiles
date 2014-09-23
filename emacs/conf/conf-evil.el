(setq evil-want-C-u-scroll t)
(setq evil-emacs-state-cursor  '("red"  box))
(setq evil-normal-state-cursor '("gray" box))
(setq evil-visual-state-cursor '("gray" box))
(setq evil-motion-state-cursor '("gray" box))
(setq evil-insert-state-cursor '("gray" bar))

(evil-mode t)
(global-evil-surround-mode t)

(evil-set-initial-state 'haskell-interactive-mode 'emacs)

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

(provide 'conf-evil)
