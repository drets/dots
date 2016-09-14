(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '((auto-completion :variables
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t)
     better-defaults
     c-c++
     emacs-lisp
     emoji
     elixir
     git
     github
     (haskell :variables
              haskell-completion-backend 'intero)
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
     (shell :variables shell-default-term-shell "/run/current-system/sw/bin/fish")
     spell-checking
     syntax-checking
     themes-megapack
     unimpaired
     yaml)
   dotspacemacs-additional-packages
   '(key-chord)
   dotspacemacs-excluded-packages
   '(smartparens anaconda-mode)
   dotspacemacs-delete-orphan-packages t)
  )

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(sanityinc-solarized-dark sanityinc-solarized-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Consolas"
                               :size 30
                               :weight normal
                               :width normal
                               :powerline-scale 1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab t
   dotspacemacs-command-key ":"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-default-layout-name "Default"
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
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "grep")
   dotspacemacs-default-package-repository nil)
   dotspacemacs-whitespace-cleanup nil
  )

(defun dotspacemacs/user-config ()

  ;; Set default values

  (setq-default
   ring-bell-function 'ignore
   indent-tabs-mode nil
   org-agenda-files (file-expand-wildcards "~/org/*.org")
   org-refile-use-outline-path 'file
   org-refile-targets '((org-agenda-files :level . 1))
   create-lockfiles nil
   org-confirm-babel-evaluate)

  (org-babel-do-load-languages 'org-babel-load-languages '((python . t) (haskell . t)))

  (global-hl-line-mode -1)
  (global-auto-complete-mode t)

  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "google-chrome-stable")

  ;; Dired

  (add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))
  (setq dired-recursive-copies 'top
        dired-recursive-copies 'top
        dired-dwim-target t)

  ;; Tramp

  (setq tramp-default-method "ssh"
        tramp-chunksize 2000)

  ;; Haskell

  (setq flycheck-ghc-stack-use-nix t)

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

  (add-hook 'haskell-mode-hook 'pretty-lambdas-haskell)

  ;; Keys remapping

  (global-set-key (kbd "M-m SPC") 'save-buffer)

  (global-set-key (kbd "s-a") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "s-x") 'evil-numbers/dec-at-pt)
  (global-set-key (kbd "s-b") 'flip-bool-at-point)

  (global-set-key (kbd "<f8>")
                  (lambda ()
                    (interactive)
                    (sync-devbox "/home/drets/src/wikia/app/" "/usr/wikia/source/wiki")
                    (sync-devbox "/home/drets/src/wikia/mercury/" "/usr/wikia/mercury")))

  (key-chord-mode 1)
  (key-chord-define-global "jk" 'evil-normal-state)
  (key-chord-define-global "df" 'evil-avy-goto-subword-1)

  )

(defun pretty-lambdas-haskell ()
  (font-lock-add-keywords
   nil `((,(concat "(?\\(" (regexp-quote "\\") "\\)")
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

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
