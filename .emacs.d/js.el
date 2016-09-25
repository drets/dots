;; Javascript mode settings.
(require 'js-mode)

;; Enable flycheck in js mode
(add-hook 'js-mode-hook lambda() (flycheck-mode 1))

(provide 'js)
