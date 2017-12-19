;; Javascript mode settings.
(require 'web-mode)
(require 'flycheck-flow)

(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

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

(add-hook 'css-mode-hook
          (lambda()
            (setq css-indent-offset 2)))

(defun eslint-fix-file ()
  (interactive)
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (shell-command (concat eslint " --fix " (buffer-file-name))))))

(defun eslint-fix-file-and-revert ()
  (interactive)
  (eslint-fix-file)
  (revert-buffer t t))

 (eval-after-load 'web-mode
                    '(define-key  web-mode-map (kbd "M-s e") #'eslint-fix-file-and-revert))

(add-hook 'web-mode-hook
          (lambda()
            (flow-minor-enable-automatically)
            (company-mode)
            (flycheck-mode)
            (setq web-mode-markup-indent-offset 2
                  web-mode-markup-indent-offset 2
                  web-mode-code-indent-offset 2
                  web-mode-attr-indent-offset 2)))

(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(flycheck-add-mode 'javascript-flow 'web-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-next-checker 'javascript-flow 'javascript-eslint)

(provide 'javascript-wrap)
