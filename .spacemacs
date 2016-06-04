(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(auto-completion
     better-defaults
     c-c++
     emacs-lisp
     emoji
     elixir
     git
     github
     gtags
     (haskell :variables haskell-enable-hindent-style "cramer")
     html
     java
     javascript
     markdown
     lua
     nixos
     org
     php
     python
     restclient
     search-engine
     semantic
     (shell :variables
            shell-default-shell 'eshell
            shell-enable-smart-eshell t)
     spell-checking
     syntax-checking
     themes-megapack
     unimpaired
     yaml)
   dotspacemacs-additional-packages
   '(key-chord)
   dotspacemacs-excluded-packages
   '(smartparens)
   dotspacemacs-delete-orphan-packages t)
)

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '(bookmarks recents projects)
   dotspacemacs-themes '(zenburn flatui spacemacs-dark spacemacs-light monokai default)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 22
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-command-key ":"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-auto-save-file-location nil
   dotspacemacs-use-ido t
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'top
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'right-then-bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("grep")
   dotspacemacs-default-package-repository nil)
)

(defun dotspacemacs/user-config ()
  (setq-default
   ring-bell-function 'ignore
   indent-tabs-mode nil
   org-agenda-files (file-expand-wildcards "~/org/*.org")
   org-refile-use-outline-path 'file
   org-refile-targets '((org-agenda-files :level . 1))
   org-confirm-babel-evaluate)

  (org-babel-do-load-languages 'org-babel-load-languages '((python . t)))

  (defun sync-wikia-app ()
    (interactive)
    (save-some-buffers t)
    (let ((default-directory "/home/drets/src/wikia/app/")
          (devbox-path "dmytror@dev-dmytror:/usr/wikia/source/wiki"))
      (call-process-shell-command
       (format "rsync -avz --exclude-from=%s --exclude=.git %s %s" (expand-file-name ".gitignore") default-directory devbox-path)
       nil
       0)))

  (key-chord-mode 1)
  (key-chord-define-global "jk" 'evil-normal-state)
  (key-chord-define-global "qp" 'sync-wikia-app)

  (global-company-mode)
  (setq create-lockfiles nil)

  (add-hook 'js-mode-hook
            (lambda ()
              (setq flycheck-checker 'javascript-jshint)))

  (defun copy-nix ()
    (copy-file "/etc/nixos/configuration.nix" "/home/drets/src/dots/nixos" t))

  (setq company-ghc-show-info t)
  (setq flycheck-ghc-stack-use-nix t)
  (setq haskell-process-args-stack-ghci '("--ghc-options=-ferror-spans" "--test" "--nix"))
  (setq haskell-interactive-popup-errors nil)
  (setq haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t)

  (with-eval-after-load 'align
    (add-to-list 'align-rules-list
                 '(haskell-types
                   (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                   (modes quote (haskell-mode literate-haskell-mode))))
    (add-to-list 'align-rules-list
                 '(haskell-assignment
                   (regexp . "\\(\\s-+\\)=\\s-+")
                   (modes quote (haskell-mode literate-haskell-mode))))
    (add-to-list 'align-rules-list
                 '(haskell-arrows
                   (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                   (modes quote (haskell-mode literate-haskell-mode))))
    (add-to-list 'align-rules-list
                 '(haskell-left-arrows
                   (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                   (modes quote (haskell-mode literate-haskell-mode)))))
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
