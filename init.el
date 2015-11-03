
;; MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Set theme
(add-to-list 'custom-theme-load-path "~/lib/emacs/emacs-color-theme-solarized")
(load-theme 'solarized t)
(custom-set-variables
 ;; Your init file should contain only one such instance
 '(frame-background-mode (\` dark)))
(enable-theme 'solarized)

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
(setq mac-option-modifier 'alt)
(setq mac-right-option-modifier 'super)

;; Disable the extra GUI
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) 
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) 
(setq menu-prompting nil)

;; Set path
(package-initialize)
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

;; Disable backup files
(setq make-backup-files 'nil)

;; Disable startup message
(setq inhibit-startup-message t)

;; Disable sound
(setq visible-bell t)

;; Clear buffer
(put 'erase-buffer 'disabled nil)

;; Langtool
(add-to-list 'load-path "~/lib/emacs/Emacs-langtool/")
(require 'langtool)
(setq langtool-language-tool-jar "~/lib/emacs/Emacs-langtool/JAR/languagetool-commandline.jar")

(global-set-key "\C-x4w" 'langtool-check)
(global-set-key "\C-x4W" 'langtool-check-done)
(global-set-key "\C-x4l" 'langtool-switch-default-language)
(global-set-key "\C-x44" 'langtool-show-message-at-point)
(global-set-key "\C-x4c" 'langtool-correct-buffer)

;; Tramp mode
(setq tramp-default-method "ssh")

;; Helm
(require 'helm)
(helm-mode 1)

;; Powerline
(add-to-list 'load-path "~/lib/emacs/powerline")
(require 'powerline)
(powerline-default-theme)

;; Set font type and size
(set-frame-font "Monaco 13")

;; y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Define close all buffers
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; Haskell mode
(add-to-list 'load-path "~/lib/emacs/haskell-mode/")
(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/lib/emacs/haskell-mode/")

;; Evil mode
(add-to-list 'load-path "~/lib/emacs/evil")
(require 'evil)
(evil-mode 1)

(add-to-list 'load-path "~/lib/emacs/key-chord")
(require 'key-chord)
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

(add-to-list 'load-path "~/lib/emacs/org-bullets")
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Remember mode
(require 'remember)

;; Magit mode
(require 'magit)
(global-set-key "\C-xg" 'magit-status)
