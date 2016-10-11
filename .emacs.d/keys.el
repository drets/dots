(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

;; Required libraries

(require 'multiple-cursors)
(require 'expand-region)
(require 'magit)
(require 'findr)
(require 'utils)
(require 'ace-jump-mode)
(require 'fiplr)
(require 'neotree)
(require 'swoop)

;; Movement and navigation

(define-key my-keys-minor-mode-map (kbd "C-1") 'beginning-of-buffer)
(define-key my-keys-minor-mode-map (kbd "C-2") 'end-of-buffer)

(define-key my-keys-minor-mode-map (kbd "C-9") 'my/move-beginning-of-line)
(define-key my-keys-minor-mode-map (kbd "C-0") 'move-end-of-line)

(define-key my-keys-minor-mode-map (kbd "C-k") 'my/scroll-down-some)
(define-key my-keys-minor-mode-map (kbd "C-l") 'my/scroll-up-some)

(define-key my-keys-minor-mode-map (kbd "M-l") 'recenter-top-bottom)

(define-key my-keys-minor-mode-map (kbd "C-/") 'next-error)
(define-key my-keys-minor-mode-map (kbd "M-/") 'previous-error)

(define-key my-keys-minor-mode-map (kbd "M-y") 'imenu)

(define-key my-keys-minor-mode-map (kbd "<tab>") 'ace-jump-word-mode)

(define-key my-keys-minor-mode-map (kbd "C-b") 'my/push-mark)
(define-key my-keys-minor-mode-map (kbd "M-b") 'my/pop-mark)

(define-key my-keys-minor-mode-map (kbd "M-9") 'backward-sexp)
(define-key my-keys-minor-mode-map (kbd "M-0") 'forward-sexp)

;; Regions

(define-key my-keys-minor-mode-map (kbd "C-a") 'mark-paragraph)
(define-key my-keys-minor-mode-map (kbd "M-a") 'mark-whole-buffer)

(define-key my-keys-minor-mode-map (kbd "C-,") 'mc/mark-next-like-this)

(define-key my-keys-minor-mode-map (kbd "C-n") 'er/expand-region)
(define-key my-keys-minor-mode-map (kbd "M-s =") 'indent-region)

(define-key my-keys-minor-mode-map (kbd "M-s l") 'sort-lines)

;; Editing

(define-key my-keys-minor-mode-map (kbd "C-S-<backspace>") 'backward-kill-sexp)

(define-key my-keys-minor-mode-map (kbd "C-d") 'kill-line)
(define-key my-keys-minor-mode-map (kbd "M-c") 'kill-whole-line)

(define-key my-keys-minor-mode-map (kbd "M-=") 'align-regexp)

(define-key my-keys-minor-mode-map (kbd "C-<") 'my/indent-left)
(define-key my-keys-minor-mode-map (kbd "C->") 'my/indent-right)

(define-key my-keys-minor-mode-map (kbd "C-'") 'my/duplicate)

(define-key my-keys-minor-mode-map (kbd "M-d") 'just-one-space)

;; Macros

(define-key my-keys-minor-mode-map (kbd "C-(") 'kmacro-start-macro)
(define-key my-keys-minor-mode-map (kbd "C-)") 'kmacro-end-macro)
(define-key my-keys-minor-mode-map (kbd "C-.") 'kmacro-end-and-call-macro)

;; Information

(define-key my-keys-minor-mode-map (kbd "M-s c") 'count-words)

(define-key my-keys-minor-mode-map (kbd "M-s i") 'describe-char)

;; Search and replacement

(define-key my-keys-minor-mode-map (kbd "C-s") 'isearch-forward-regexp)
(define-key my-keys-minor-mode-map (kbd "C-r") 'query-replace-regexp)

(define-key my-keys-minor-mode-map (kbd "M-s r") 'rgrep)
(define-key my-keys-minor-mode-map (kbd "M-s a") 'ag-project)
(define-key my-keys-minor-mode-map (kbd "M-s f") 'findr-query-replace)

(define-key my-keys-minor-mode-map (kbd "M-s s") 'swoop-pcre-regexp)
(define-key swoop-map (kbd "C-/") 'swoop-action-goto-line-next)
(define-key swoop-map (kbd "M-/") 'swoop-action-goto-line-prev)

;; Files

(define-key my-keys-minor-mode-map (kbd "C-f") (kbd "C-x C-s")) ; generic

(define-key my-keys-minor-mode-map (kbd "C-o") 'find-file)
(define-key my-keys-minor-mode-map (kbd "C-p") 'fiplr-find-file)

(define-key my-keys-minor-mode-map (kbd "M-r") 'revert-buffer)

(define-key my-keys-minor-mode-map (kbd "C-x C-r") 'my/rename-current-buffer-file)

(define-key my-keys-minor-mode-map (kbd "C-4") 'neotree-toggle)

;; Windows and buffers

(define-key my-keys-minor-mode-map (kbd "C-t") 'switch-to-buffer)
(define-key my-keys-minor-mode-map (kbd "C-c k") 'my/switch-to-previous-buffer)
(define-key my-keys-minor-mode-map (kbd "M-t") 'ibuffer)

(define-key my-keys-minor-mode-map (kbd "C-w") 'delete-window)
(define-key my-keys-minor-mode-map (kbd "M-w") 'kill-buffer)

(define-key my-keys-minor-mode-map (kbd "M-e") 'split-window-right)
(define-key my-keys-minor-mode-map (kbd "C-e") 'my/switch-to-next-window)
(define-key my-keys-minor-mode-map (kbd "C-c o") 'clone-indirect-buffer)

;; Display

(define-key my-keys-minor-mode-map (kbd "C--") 'text-scale-decrease)
(define-key my-keys-minor-mode-map (kbd "C-=") 'text-scale-increase)

(define-key my-keys-minor-mode-map (kbd "M-s v") 'variable-pitch-mode)

(define-key my-keys-minor-mode-map (kbd "M-s t") 'my/switch-theme)

;; Magit

(define-key my-keys-minor-mode-map (kbd "C-8") 'magit-status)

;; Haskell

(define-key haskell-mode-map (kbd "C-y") 'haskell-mode-jump-to-def-or-tag)

(define-key haskell-mode-map (kbd "C-c C-p") 'haskell-process-restart)

(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-session-change-target)

;; Snippets

(define-key my-keys-minor-mode-map (kbd "M-s M-s") 'yas-new-snippet)

;; Auto-completion
(define-key my-keys-minor-mode-map (kbd "C-<tab>") 'dabbrev-expand)

;; Ido in M-x
(define-key my-keys-minor-mode-map (kbd "M-x") 'smex)

;; Visualize undo tree
(define-key my-keys-minor-mode-map (kbd "M-4") 'undo-tree-visualize)

;; Wikia
(define-key my-keys-minor-mode-map (kbd "<f8>")
   (lambda ()
     (interactive)
     (my/sync "/home/drets/src/wikia/app/"
        "dmytror@dev-dmytror:/usr/wikia/source/wiki")
     (my/sync "/home/drets/src/wikia/mercury/"
        "dmytror@dev-dmytror:/usr/wikia/mercury")))

;; Reversible version of delete-other-windows
(define-key my-keys-minor-mode-map (kbd "M-8") 'zygospore-toggle-delete-other-windows)

;; End of key definitions.

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

(defun disable-my-keys ()
  (my-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'disable-my-keys)

(provide 'keys)
