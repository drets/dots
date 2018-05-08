(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["white" "#303030" "#b3b3b3" "#606060"])
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.saves"))))
 '(browse-url-browser-display nil)
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "google-chrome-stable")
 '(column-number-mode t)
 '(comint-input-ring-size 10000)
 '(comint-scroll-show-maximum-output nil)
 '(company-dabbrev-downcase nil)
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (monochrome-bright)))
 '(custom-safe-themes
   (quote
    ("edd7f7399da71596e6b15465d79bd770579d6f3170e5ffb1e1c97fae04b71185" "d306cc44632e662b61a483ccb92c49a8aedfbdefa528ef933f09a33258fed45e" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "cab317d0125d7aab145bc7ee03a1e16804d5abdfa2aa8738198ac30dc5f7b569" "31992d4488dba5b28ddb0c16914bf5726dc41588c2b1c1a2fd16516ea92c1d8e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(dabbrev-case-fold-search nil)
 '(delete-old-versions t)
 '(dired-auto-revert-buffer t)
 '(dired-dwim-target t)
 '(dired-listing-switches "-al --group-directories-first")
 '(dired-recursive-copies (quote top))
 '(dired-recursive-deletes (quote top))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(electric-indent-mode t)
 '(elfeed-feeds
   (quote
    ("https://www.reddit.com/r/haskell/top.rss?t=week" "https://www.reddit.com/r/LessWrong/top.rss?t=week" "https://www.reddit.com/r/InteractiveThmProving/top.rss?t=week" "https://www.reddit.com/r/Coq/top.rss?t=week")))
 '(fill-column 77)
 '(fiplr-ignored-globs
  (quote
   ((directories
     (".git" ".svn" ".hg" ".bzr" ".cabal-sandbox" ".stack-work" "dist" "bower_components" "node_modules" "output"))
    (files
     (".#*" "*~" "*.so" "*.jpg" "*.png" "*.gif" "*.pdf" "*.gz" "*.zip")))))
 '(fiplr-root-markers (quote (".git" ".svn" ".hg" ".bzr" "stack.yaml")))
 '(flycheck-javascript-flow-args nil)
 '(global-whitespace-cleanup-mode t)
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
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(initial-buffer-choice (quote remember-notes))
 '(ivy-mode t)
 '(ivy-use-virtual-buffers t)
 '(kept-new-versions 6)
 '(kill-whole-line t)
 '(magit-dispatch-arguments nil)
 '(magit-push-always-verify nil)
 '(magit-submodule-arguments nil)
 '(magit-use-overlays nil)
 '(menu-bar-mode nil)
 '(my/kill-as-delete t)
 '(nyan-mode nil)
 '(org-agenda-files (quote ("~/org")))
 '(org-babel-load-languages (quote ((python . t) (js . t) (sh . t))))
 '(org-confirm-babel-evaluate nil)
 '(org-default-notes-file "~/org/tasks.org")
 '(org-support-shift-select (quote always))
 '(package-selected-packages
   (quote
    (auto-save-buffers-enhanced magit org-plus-contrib centered-window elfeed undo-tree ghc string-inflection company-coq monochrome-theme htmlize phi-search yasnippet flycheck-flow flow-minor-mode company counsel-projectile web-mode auctex fiplr whitespace-cleanup-mode wrap-region exec-path-from-shell nix-mode lua-mode restclient avy wgrep counsel move-text markdown-mode flx smex noflet multiple-cursors haskell-mode flycheck expand-region)))
 '(preview-scale-function 2.5)
 '(psc-ide-add-import-on-completion t)
 '(python-shell-interpreter "python3")
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(smex-flex-matching nil)
 '(split-width-threshold 180)
 '(swoop-font-size-change: nil)
 '(text-scale-mode-step 1.1)
 '(tool-bar-mode nil)
 '(tramp-chunksize 2000)
 '(tramp-default-method "ssh")
 '(use-dialog-box nil)
 '(whitespace-cleanup-mode-preserve-point t)
 '(winner-mode t)
 '(yas-indent-line (quote fixed)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka" :foundry "unknown" :slant normal :weight normal :height 100 :width normal))))
 '(audit-comment-face ((t (:foreground "tomato"))))
 '(audit-heading-face ((t (:foreground "firebrick" :weight bold))))
 '(coq-cheat-face ((t (:background "gray40"))))
 '(coq-solve-tactics-face ((t (:foreground "#93937070DBDB"))))
 '(font-lock-comment-face ((t (:foreground "dark gray" :slant italic))))
 '(font-lock-doc-face ((t (:foreground "#6c71c4" :slant normal))))
 '(fringe ((t (:background "black"))))
 '(proof-locked-face ((t nil)))
 '(variable-pitch ((t (:height 1.225 :family "Sans")))))
