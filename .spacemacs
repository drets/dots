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
     eyebrowse
     git
     github
     gtags
     (haskell :variables haskell-enable-hindent-style "cramer")
     html
     java
     javascript
     markdown
     org
     osx
     pdf
     php
     python
     ranger
     restclient
     search-engine
     semantic
     (shell :variables shell-default-shell 'eshell)
     (shell :variables shell-enable-smart-eshell t)
     speed-reading
     spell-checking
     syntax-checking
     themes-megapack
     unimpaired
     yaml)
   dotspacemacs-additional-packages '(key-chord)
   dotspacemacs-excluded-packages '()
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-themes '(monokai default)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 14
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
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'top
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'right-then-bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup t
   dotspacemacs-fullscreen-use-non-native t
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("grep")
   dotspacemacs-default-package-repository nil))

(defun dotspacemacs/user-init ())

(defun dotspacemacs/user-config ()
  (defun sync-wikia-app ()
    (interactive)
    (save-some-buffers t)
    (let ((default-directory "/Users/wikia/wikia/app/")
          (devbox-path "dev-dmytror:/usr/wikia/source/wiki"))
      (call-process-shell-command
       (format "rsync -avz --exclude-from=%s --exclude=.git %s %s" (expand-file-name ".gitignore") default-directory devbox-path)
       nil
       0)))

  (setq-default
   ring-bell-function 'ignore
   ranger-override-dired t

   ;; Org mode
   org-agenda-files (file-expand-wildcards "~/org/*.org")
   org-refile-use-outline-path 'file
   org-refile-targets '((org-agenda-files :level . 1))
   org-confirm-babel-evaluate)

  (setq remember-data-file "~/org/notes.org")

  ;; fix org html exporting
  (org-reload)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (js . t)
     (haskell . t)
     (sh . t)))

  ;; Remember mode
  (define-key global-map (kbd "<f9> r") 'remember)
  (define-key global-map (kbd "<f9> R") 'remember-region)

  (define-key global-map "\C-xb" 'helm-buffers-list)

  ;; Key chord
  (key-chord-mode 1)

  (key-chord-define-global "jk" 'evil-normal-state)

  ;; Rsync wikia app
  (key-chord-define-global "qp" 'sync-wikia-app)

  (add-hook 'prog-mode-hook
            (lambda ()
              (which-function-mode 1)
              (local-set-key (kbd "C-x y") 'sort-lines)
              ()))

  ;; Disable smartparens highlighting
  (with-eval-after-load 'smartparens
    (show-smartparens-global-mode -1))

  (global-company-mode)

  (add-hook 'haskell-mode-hook
            (lambda ()
              (local-set-key (kbd "C-x f") 'hindent-reformat-region)
              (haskell-doc-mode 1)))

  (setq create-lockfiles nil)

  (spacemacs-centered-buffer-mode 1)

  (setq haskell-process-args-stack-ghci '("--ghc-options=-ferror-spans" "--test"))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.