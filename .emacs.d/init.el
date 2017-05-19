;; Add .emacs.d to the list of folders to load Lisp files from. “t” means
;; that .emacs.d would be added to the end, which is done so that there won't
;; be conflicts between my files and files of various libraries.
(add-to-list #'load-path user-emacs-directory t)

;; Initialise package system.
(require #'package)
(add-to-list #'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list #'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

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

;; TeX
(require #'tex-wrapper)

;; Wrap mode
(wrap-region-global-mode)

;; Edit in browser from emacs
(edit-server-start)

;; Offline docs (sqlite3 dependency)
(require 'counsel-dash)
(setq counsel-dash-common-docsets
      '("CSS" "Emacs Lisp" "Flask" "HTML" "JavaScript" "Less" "Python 3"
        "React" "Sass" "jQuery" "MomentJS" "Bash" "Sinon" "PostgreSQL" "Haskell"))

;; Specify which keys count as “nice” keys for avy.
(setq avy-keys
      (string-to-list "eklioswadfxcrvn,hm./"))

;; Prompt for directory creation automatically when saving a file
(add-hook #'before-save-hook
  (lambda ()
     (lambda ()
       (when buffer-file-name
         (let ((dir (file-name-directory buffer-file-name)))
           (when (and (not (file-exists-p dir))
                      (y-or-n-p (format "Create directory %s does not exist. Create it?" dir)))
             (make-directory dir t)))))))

;; Agda
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

;; Load “customize”d variables.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
(put #'upcase-region #'disabled nil)
