;; Shell adjustments.
(require #'shell)

(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "HISTFILE")

(add-hook 'shell-mode-hook
          (lambda ()
            (turn-on-comint-history (getenv "HISTFILE"))))

(add-hook 'kill-buffer-hook #'comint-write-input-ring)
(add-hook 'kill-emacs-hook
          (lambda ()
            (--each (buffer-list)
              (with-current-buffer it (comint-write-input-ring)))))

(provide 'myshell)
