(setq evil-want-C-u-scroll t)
(setq evil-emacs-state-cursor '("red" box))

(evil-mode t)
(global-evil-surround-mode t)

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

(provide 'conf/evil)
