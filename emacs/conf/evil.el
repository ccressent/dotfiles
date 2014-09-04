(setq evil-want-C-u-scroll t)

(evil-mode t)
(global-evil-surround-mode t)

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
;(define-key evil-insert-state-map (kbd "jk") 'evil-normal-state)

(provide 'conf/evil)
