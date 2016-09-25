;; This enables Haskell-mode. Useful tips:
;;
;;   * [C-c C-b] shows the REPL buffer.
;;
;;   * To enable tag generation, you have to install hasktags from Hackage.
;;     After that you will be able to instantly jump to function definitions
;;     with [M-.].
;;
;;   * [C-c C-t] shows type of function at point.
;;
;;   * [M-x imenu] provides an index of functions defined in module.

(require 'haskell-mode)

(defun pretty-lambdas-haskell ()
  (font-lock-add-keywords
   nil `((,(concat "(?\\(" (regexp-quote "\\") "\\)")
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(add-to-list 'exec-path "/home/drets/.cabal/bin/")

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'pretty-lambdas-haskell)

(provide 'haskell)
