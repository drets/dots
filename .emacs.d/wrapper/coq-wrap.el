;; Coq
(require #'font-wrap)

(setq proof-three-window-mode-policy 'hybrid)
(setq proof-script-fly-past-comments t)
(setq proof-splash-seen t)
(setq overlay-arrow-string "")
(setq company-coq-disabled-features '(code-folding))

(add-hook 'coq-mode-hook
          (lambda ()
            (add-fira-code-symbol-keywords)
            (company-coq-mode)))

(add-hook 'coq-goals-mode-hook #'add-fira-code-symbol-keywords)
(add-hook 'coq-response-mode-hook #'add-fira-code-symbol-keywords)

(setq proofgeneral (shell-command-to-string "printf \"$(dirname $(dirname $(readlink $(which proofgeneral))))/share/emacs/site-lisp/ProofGeneral/generic/proof-site\""))
(load proofgeneral)

(define-abbrev-table 'coq-mode-abbrev-table '())
(define-abbrev coq-mode-abbrev-table "r" "rewrite ")
(define-abbrev coq-mode-abbrev-table "i" "inversion ")
(define-abbrev coq-mode-abbrev-table "s" "subst")
(define-abbrev coq-mode-abbrev-table "f" "Search ")
(define-abbrev coq-mode-abbrev-table "u" "unfold ")
(define-abbrev coq-mode-abbrev-table "a" "apply ")
(define-abbrev coq-mode-abbrev-table "c" "clear ")
(advice-add 'proof-assert-next-command-interactive :before #'expand-abbrev)

(provide 'coq-wrap)
