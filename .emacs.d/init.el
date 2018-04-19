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

;; Autoinstall packages from package-selected-packages variable.
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

;; The file name and the major mode as window name.
(setq-default frame-title-format '("%b"))

;; Load wrappers.
(add-to-list #'load-path (concat user-emacs-directory "wrapper/") t)
(require #'haskell-wrap)
(require #'javascript-wrap)
(require #'shell-wrap)
(require #'tex-wrap)
(require #'org-wrap)
(require #'font-wrap)
(require #'coq-wrap)

;; Load custom keybindings.
(require #'keys)

(require #'github-urls)
(require #'audit)

;; Load Yasnippet.
(require 'yasnippet)
(yas-global-mode 1)

;; Enable kill-as-delete.
(require #'kill-as-delete)

;; Get rid of annoying “yes or no” questions.
(defalias #'yes-or-no-p #'y-or-n-p)

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

;; Wrap string mode.
(wrap-region-global-mode)

;; Specify which keys count as “nice” keys for avy.
(setq avy-keys
      (string-to-list "eklioswadfxcrvn,hm./"))

;; Prompt for directory creation automatically when saving a file.
(add-hook #'before-save-hook
  (lambda ()
     (lambda ()
       (when buffer-file-name
         (let ((dir (file-name-directory buffer-file-name)))
           (when (and (not (file-exists-p dir))
                      (y-or-n-p (format "Create directory %s does not exist. Create it?" dir)))
             (make-directory dir t)))))))

;; Switch between projects quicker using projectile.
(projectile-mode)
(counsel-projectile-mode)

;; Load “customize”d variables.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
(put #'upcase-region #'disabled nil)
