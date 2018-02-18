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

(add-to-list 'exec-path "/home/drets/.cabal/bin/")

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(setq haskell-process-args-ghci
      '("-ferror-spans" "-fshow-loaded-modules"))

(setq haskell-process-args-cabal-repl
      '("--ghc-options=-ferror-spans -fshow-loaded-modules"))

(setq haskell-process-args-stack-ghci
      '("--ghci-options=-ferror-spans -fshow-loaded-modules"
        "--no-build" "--no-load"))

(setq haskell-process-args-cabal-new-repl
      '("--ghc-options=-ferror-spans -fshow-loaded-modules"))

(provide 'haskell-wrap)
