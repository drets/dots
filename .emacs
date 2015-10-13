(add-to-list 'load-path "~/lib/emacs/haskell-mode/")
(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/lib/emacs/haskell-mode/")

(add-to-list 'custom-theme-load-path "~/lib/emacs/emacs-color-theme-solarized")
(load-theme 'solarized t)
(custom-set-variables
  '(frame-background-mode `dark))
(enable-theme 'solarized)
(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(haskell-types
                   (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                   (modes quote (haskell-mode literate-haskell-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(haskell-assignment
                  (regexp . "\\(\\s-+\\)=\\s-+")
                  (modes quote (haskell-mode literate-haskell-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(haskell-arrows
                  (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                  (modes quote (haskell-mode literate-haskell-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(haskell-left-arrows
                  (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                  (modes quote (haskell-mode literate-haskell-mode)))))
