(package-initialize)

(require 'flycheck)
(require 'langtool)
(require 'powerline)
(require 'haskell-mode)
(require 'evil)
(require 'key-chord)
(require 'org-bullets)
(require 'remember)
(require 'magit)
(require 'term)
(require 'expand-region)
(require 'multiple-cursors)
(require 'ace-jump-mode)
(require 'yasnippet)
(require 'auto-complete)
(require 'web-mode)
(require 'restclient)
(require 'helm)
(require 'helm-config)
(require 'projectile)
(require 'helm-projectile)
(require 'js2-mode)
(require 'smartparens)
(require 'golden-ratio)
(require 'writegood-mode)
(require 'google-translate)
(require 'google-translate-smooth-ui)

;; MELPA
(add-to-list 'package-archives
     '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;;(add-to-list 'package-archives
;;             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Set theme
(load-theme 'solarized t)
(custom-set-variables
 ;; Your init file should contain only one such instance
 '(frame-background-mode (\` dark)))
(enable-theme 'solarized)

;; Auto-refresh buffers
(global-auto-revert-mode t)

;; Enable flyspell-mode for org-mode
(defun turn-on-flyspell () (flyspell-mode 1))
(add-hook 'org-mode-hook 'turn-on-flyspell)

;; Set line numbers
(global-linum-mode 1)

;; Disable cursor blinking
(blink-cursor-mode -1)

;; Set cursor color to dark green
(custom-set-faces
 ;; Your init file should contain only one such instance
 '(cursor ((t (:background "#0F7D21" :foreground "#002b36")))))

;; Bind mac keys
(setq mac-right-command-modifier 'meta)
(setq ns-function-modifier 'hyper)
(setq mac-option-modifier 'alt)
(setq mac-right-option-modifier 'super)

;; Disable the extra GUI
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) 
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) 
(setq menu-prompting nil)

;; Set path
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Set parenthesis
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Move minibuffer 
(setq default-minibuffer-frame
      (make-frame
       '((name . "minibuffer")
         (width . 80)
         (height . 1)
         (minibuffer . only)
         (top . 0)
         (left . 0)
         )))
(setq new-frame
      (make-frame
       '((name . "editor")
         (width . 80)
         (height . 30)
         (minibuffer . nil)
         (top . 50)
         (left . 0)
         )))

;; Set keys for control buffers
(global-set-key "\C-xj" 'previous-buffer)
(global-set-key "\C-xk" 'next-buffer)
(global-set-key "\C-xp" 'kill-this-buffer)

; Disable backup
(setq backup-inhibited t)

;; Disable startup message
(setq inhibit-startup-message t)

;; Disable sound
(setq visible-bell t)

;; Clear buffer
(put 'erase-buffer 'disabled nil)

;; Langtool
(setq langtool-language-tool-jar "~/lib/emacs/languagetool/languagetool-commandline.jar")

(global-set-key "\C-x4w" 'langtool-check)
(global-set-key "\C-x4W" 'langtool-check-done)
(global-set-key "\C-x4l" 'langtool-switch-default-language)
(global-set-key "\C-x44" 'langtool-show-message-at-point)
(global-set-key "\C-x4c" 'langtool-correct-buffer)

;; Tramp mode
(setq tramp-default-method "ssh")

;; Powerline
(powerline-default-theme)

;; Set font type and size
(set-frame-font "Monaco 13")

;; y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Kills all buffers, except the current one
(defun kill-other-buffers ()
      "Kill all other buffers."
      (interactive)
      (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
(global-set-key "\C-xO" 'kill-other-buffers)

;; Evil mode
(evil-mode 1)

(key-chord-mode 1)
(key-chord-define-global "ii" 'evil-normal-state)

(define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
(define-key evil-insert-state-map "\C-e" 'end-of-line)
(define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
(define-key evil-motion-state-map "\C-e" 'evil-end-of-line)
(define-key evil-normal-state-map "\C-f" 'evil-forward-char)
(define-key evil-insert-state-map "\C-f" 'evil-forward-char)
(define-key evil-insert-state-map "\C-f" 'evil-forward-char)
(define-key evil-normal-state-map "\C-b" 'evil-backward-char)
(define-key evil-insert-state-map "\C-b" 'evil-backward-char)
(define-key evil-visual-state-map "\C-b" 'evil-backward-char)
(define-key evil-normal-state-map "\C-d" 'evil-delete-char)
(define-key evil-insert-state-map "\C-d" 'evil-delete-char)
(define-key evil-visual-state-map "\C-d" 'evil-delete-char)
(define-key evil-normal-state-map "\C-n" 'evil-next-line)
(define-key evil-insert-state-map "\C-n" 'evil-next-line)
(define-key evil-visual-state-map "\C-n" 'evil-next-line)
(define-key evil-normal-state-map "\C-p" 'evil-previous-line)
(define-key evil-insert-state-map "\C-p" 'evil-previous-line)
(define-key evil-visual-state-map "\C-p" 'evil-previous-line)
(define-key evil-normal-state-map "\C-w" 'evil-delete)
(define-key evil-insert-state-map "\C-w" 'evil-delete)
(define-key evil-visual-state-map "\C-w" 'evil-delete)
(define-key evil-normal-state-map "\C-y" 'yank)
(define-key evil-insert-state-map "\C-y" 'yank)
(define-key evil-visual-state-map "\C-y" 'yank)
(define-key evil-normal-state-map "\C-k" 'kill-line)
(define-key evil-insert-state-map "\C-k" 'kill-line)
(define-key evil-visual-state-map "\C-k" 'kill-line)
(define-key evil-normal-state-map "Q" 'call-last-kbd-macro)
(define-key evil-visual-state-map "Q" 'call-last-kbd-macro)
(define-key evil-normal-state-map (kbd "TAB") 'evil-undefine)

(defun evil-undefine ()
 (interactive)
 (let (evil-mode-map-alist)
   (call-interactively (key-binding (this-command-keys)))))

(define-key evil-normal-state-map (kbd "DEL") (lambda ()
                    (interactive)
                    (previous-line 10)
                    (evil-scroll-line-up 10)
                    ))

(define-key evil-normal-state-map (kbd "=") (lambda ()
                      (interactive)
                      (next-line 10)
                      (evil-scroll-line-down 10)
                      ))

;; Org mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))

(setq org-refile-use-outline-path 'file)
(setq org-refile-targets '((org-agenda-files :level . 1)))

(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (js . t)
   (haskell . t)
   (sh . t)
   (emacs-lisp . nil)
   ))

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(find-file "~/org/today.org")

;; Remember mode
(define-key global-map (kbd "<f9> r") 'remember)
(define-key global-map (kbd "<f9> R") 'remember-region)

(setq remember-data-file "~/org/notes.org")

;; Magit mode
(global-set-key "\C-xg" 'magit-status)

;; Expand-region mode
(global-set-key (kbd "C-=") 'er/expand-region)

;; Multiple-cursors mode
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Ace Jump Mode
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
   t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

;; Yasnippet mode
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<C-return>") 'yas-expand)

;; Auto complete mode
(ac-config-default)

;; Web mode
(add-to-list 'auto-mode-alist '("\\.react.js\\'" . web-mode))

;; Flycheck mode
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Ansi-term customisation
(defun visit-ansi-term ()
  "If the current buffer is:
     1) a running ansi-term named *ansi-term*, rename it.
     2) a stopped ansi-term, kill it and create a new one.
     3) a non ansi-term, go to an already running ansi-term
        or start a new one while killing a defunt one"
  (interactive)
  (let ((is-term (string= "term-mode" major-mode))
        (is-running (term-check-proc (buffer-name)))
        (term-cmd "/bin/bash")
        (anon-term (get-buffer "*ansi-term*")))
    (if is-term
        (if is-running
            (if (string= "*ansi-term*" (buffer-name))
                (call-interactively 'rename-buffer)
              (if anon-term
                  (switch-to-buffer "*ansi-term*")
                (ansi-term term-cmd)))
          (kill-buffer (buffer-name))
          (ansi-term term-cmd))
      (if anon-term
          (if (term-check-proc "*ansi-term*")
              (switch-to-buffer "*ansi-term*")
            (kill-buffer "*ansi-term*")
            (ansi-term term-cmd))
        (ansi-term term-cmd)))))
(global-set-key "\C-xt" 'visit-ansi-term)

;; Helm
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)

(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(global-set-key (kbd "C-x b") 'helm-mini)

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)

(global-set-key "\C-xf" 'helm-find-files)

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(setq helm-locate-fuzzy-match t)

(global-set-key (kbd "C-c h o") 'helm-occur)

(setq helm-apropos-fuzzy-match t)

(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

(global-set-key (kbd "C-c h x") 'helm-register)

(global-set-key (kbd "C-c h g") 'helm-google-suggest)

(set-face-attribute 'helm-selection nil 
                    :background "#0F7D21"
                    :foreground "#002b36"
		    :underline  "#0F7D21")

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(setq projectile-switch-project-action 'helm-projectile)

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Smartparens mode
(smartparens-global-mode 1)

;; Golden ratio
(golden-ratio-mode 1)

;; Writegood mode
(global-set-key "\C-cg" 'writegood-mode)

;; Google translate
(global-set-key "\C-ct" 'google-translate-smooth-translate)
(setq google-translate-translation-directions-alist
      '(("en" . "uk") ("uk" . "en")))
