;; Add .emacs.d to the list of folders to load Lisp files from. “t” means
;; that .emacs.d would be added to the end, which is done so that there won't
;; be conflicts between my files and files of various libraries.
(add-to-list #'load-path user-emacs-directory t)

(setq package-list
      '(
        avy
        counsel
        counsel-dash
        company
        exec-path-from-shell
        flx
        flycheck
        expand-region
        haskell-mode
        ivy-hydra
        lua-mode
        magit
        markdown-mode
        move-text
        multiple-cursors
        nix-mode
        noflet
        org-pomodoro
        origami
        restclient
        smex
        solarized-theme
        wrap-region
        wgrep
        zygospore
        )
      )

;; Initialise package system.
(require #'package)
(add-to-list #'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Install packages if needed.
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Prevent the Magit upgrade warning from showing every time.
(setq magit-last-seen-setup-instructions "1.4.0")

;; The file name and the major mode as window name
(setq-default frame-title-format '("%f [%m]"))

;; Load haskell-mode.
(require #'haskell)

;; Load custom keybindings.
(require #'keys)

;; Enable kill-as-delete.
(require #'kill-as-delete)

;; Load js-mode.
(require #'javascript)

;; Adjust shell mode
(require #'shell-wrapper)

;; Get rid of annoying “yes or no” questions.
(defalias #'yes-or-no-p #'y-or-n-p)

;; Code folding.
(require #'origami)
(global-origami-mode)

;; Ivy.
(require #'ivy)
(setq ivy-initial-inputs-alist nil)
(setq ivy-re-builders-alist '((t   . ivy--regex-ignore-order)))

;; Dired.
(require #'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook #'dired-mode-hook
  (lambda ()
    (dired-hide-details-mode)
    (dired-omit-mode)))

;; Adjust org mode.
(require #'org-wrapper)

;; Enable autocompletion
(global-company-mode)

;; Wrap mode
(wrap-region-global-mode)

;; Offline docs (sqlite3 dependency)
(require 'counsel-dash)
(setq counsel-dash-common-docsets
      '("CSS" "Emacs Lisp" "Flask" "HTML" "JavaScript" "Less" "Python 3" "React" "Sass" "jQuery" "MomentJS"))

;; Prompt for directory creation automatically when saving a file
;; and delete trailing whitespaces
(add-hook #'before-save-hook
  (lambda ()
     (lambda ()
       (when buffer-file-name
         (let ((dir (file-name-directory buffer-file-name)))
           (when (and (not (file-exists-p dir))
                      (y-or-n-p (format "Create directory %s does not exist. Create it?" dir)))
             (make-directory dir t)))))
      (delete-trailing-whitespace)))

;; Load “customize”d variables.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
(put #'upcase-region #'disabled nil)
