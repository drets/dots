;; Javascript mode settings.
(require 'js)

(add-hook 'js-mode-hook #'flycheck-mode)
(add-hook 'js-mode-hook #'electric-indent-mode)

(provide 'javascript)
