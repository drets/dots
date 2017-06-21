;; Javascript mode settings.
(require 'js)
(require 'web-mode)

(defun use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'use-eslint-from-node-modules)
(add-hook 'js-mode-hook
          (lambda()
            (flycheck-mode)))

(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))

(add-hook 'css-mode-hook
      (lambda()
        (setq css-indent-offset 2)))

(add-hook 'web-mode-hook
          (lambda()
            (setq web-mode-markup-indent-offset 2)))

(provide 'javascript-wrap)
