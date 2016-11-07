(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.saves"))))
 '(browse-url-browser-display nil)
 '(browse-url-browser-function (quote browse-url-chrome))
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(delete-old-versions t)
 '(dired-auto-revert-buffer t)
 '(dired-dwim-target t)
 '(dired-recursive-copies (quote top))
 '(dired-recursive-deletes (quote top))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fill-column 77)
 '(fiplr-ignored-globs
   (quote
    ((directories
      (".git" ".svn" ".hg" ".bzr" ".cabal-sandbox" ".stack-work" "dist"))
     (files
      (".#*" "*~" "*.so" "*.jpg" "*.png" "*.gif" "*.pdf" "*.gz" "*.zip")))))
 '(fiplr-root-markers (quote (".git" ".svn" ".hg" ".bzr" "stack.yaml")))
 '(flx-ido-mode t)
 '(global-undo-tree-mode t)
 '(haskell-indentation-show-indentations nil)
 '(haskell-indentation-starter-offset 2)
 '(haskell-interactive-mode-eval-pretty t)
 '(haskell-interactive-popup-errors nil)
 '(haskell-interactive-prompt "> ")
 '(haskell-interactive-prompt2 "| ")
 '(haskell-mode-hook
   (quote
    (turn-on-haskell-indentation interactive-haskell-mode)))
 '(haskell-process-log t)
 '(haskell-process-show-debug-tips nil)
 '(haskell-process-suggest-language-pragmas t)
 '(haskell-process-suggest-no-warn-orphans nil)
 '(haskell-process-suggest-overloaded-strings t)
 '(haskell-process-suggest-remove-import-lines nil)
 '(haskell-process-type (quote auto))
 '(haskell-tags-on-save t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-use-faces nil)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(initial-buffer-choice "~/org/today.org")
 '(kept-new-versions 6)
 '(kill-whole-line t)
 '(magit-push-always-verify nil)
 '(magit-use-overlays nil)
 '(menu-bar-mode nil)
 '(my/kill-as-delete t)
 '(neo-smart-open t)
 '(org-agenda-files (quote ("~/org/birthday.org" "~/org/today.org")))
 '(org-babel-load-languages (quote ((python . t))))
 '(org-confirm-babel-evaluate nil)
 '(package-selected-packages
   (quote
    (findr flx-ido fiplr move-text markdown-mode flx smex php-mode undo-tree zygospore org-pomodoro yasnippet solarized-theme noflet neotree multiple-cursors magit haskell-mode flycheck expand-region ace-jump-mode)))
 '(python-shell-interpreter "python3")
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(smex-flex-matching nil)
 '(split-width-threshold 100)
 '(swoop-font-size-change: nil)
 '(text-scale-mode-step 1.1)
 '(tool-bar-mode nil)
 '(tramp-chunksize 2000)
 '(tramp-default-method "ssh")
 '(undo-limit 8000000)
 '(undo-strong-limit 12000000)
 '(undo-tree-visualizer-diff t)
 '(use-dialog-box nil)
 '(word-wrap t)
 '(yas-global-mode t)
 '(yas-indent-line (quote fixed))
 '(yas-snippet-dirs (quote ("~/.emacs.d/snippets"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 102 :width normal))))
 '(font-lock-comment-face ((t (:inherit highlight))))
 '(font-lock-doc-face ((t (:foreground "#6c71c4" :slant normal))))
 '(variable-pitch ((t (:height 1.225 :family "Sans")))))
