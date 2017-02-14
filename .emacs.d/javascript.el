;; Javascript mode settings.
(require 'js)
(require 'company-tern)

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
            (tern-mode)
            (flycheck-mode)))

(add-to-list 'company-backends 'company-tern)

(provide 'javascript)
