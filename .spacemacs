(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '((auto-completion :variables
                      auto-completion-enable-sort-by-usage t)
     better-defaults
     c-c++
     eyebrowse
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
   dotspacemacs-themes '(spacemacs-dark spacemacs-light molokai)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 26
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
   dotspacemacs-which-key-position 'bottom
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
   dotspacemacs-search-tools '("ag" "grep")
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

  (defun sync-devbox (src dest)
    (interactive)
    (save-some-buffers t)
    (let ((dest-path (concat "dmytror@dev-dmytror:" dest)))
      (call-process-shell-command
       (format
        "rsync -avz --exclude-from=%s --exclude=.git %s %s"
        (concat src ".gitignore") src dest-path)
       nil
       0)))

  (key-chord-mode 1)
  (key-chord-define-global "jk" 'evil-normal-state)

  (global-set-key (kbd "<f8>")
                  (lambda ()
                    (interactive)
                    (sync-devbox "/home/drets/src/wikia/app/" "/usr/wikia/source/wiki")
                    (sync-devbox "/home/drets/src/wikia/mercury/" "/usr/wikia/mercury")))

  (global-company-mode)

  (setq create-lockfiles nil)
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

  (defun flip-bool-at-point ()
    (interactive)
    (let* ((bools '(("true" . "false")
                    ("True" . "False")
                    ("TRUE" . "FALSE")
                    ("1" . "0")))
           (true  (cdr (assoc  (current-word) bools)))
           (false (car (rassoc (current-word) bools)))
           (wrd (cond (true true)
                      (false false)
                      (t (current-word)))))
      (save-excursion
        (forward-word)
        (backward-kill-word 1)
        (insert wrd))))

  (keyboard-translate ?\C-i ?\H-i)
  (global-set-key [?\H-i] 'evil-jump-forward)
  (global-set-key (kbd "C-s-a") 'spacemacs/evil-numbers-increase)
  (global-set-key (kbd "C-s-x") 'spacemacs/evil-numbers-decrease)
  (global-set-key (kbd "C-s-b") 'flip-bool-at-point)
  (global-set-key (kbd "M-m o r") 'align-regexp)
  (global-set-key (kbd "M-m o s") 'helm-semantic-or-imenu)
  (global-set-key (kbd "C-j") 'spacemacs/insert-line-above-no-indent)
  (global-set-key (kbd "C-k") 'spacemacs/insert-line-below-no-indent)
  )
