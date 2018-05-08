(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

;; Required libraries

(require 'counsel)
(require 'counsel-projectile)
(require 'expand-region)
(require 'fiplr)
(require 'ivy)
(require 'magit)
(require 'move-text)
(require 'multiple-cursors)
(require 'org)
(require 'phi-search)
(require 'swiper)
(require 'utils)
(require 'yasnippet)
(require 'string-inflection)

;; Movement and navigation

(define-key my-keys-minor-mode-map (kbd "C-/")     #'next-error)
(define-key my-keys-minor-mode-map (kbd "C-0")     #'move-end-of-line)
(define-key my-keys-minor-mode-map (kbd "C-1")     #'beginning-of-buffer)
(define-key my-keys-minor-mode-map (kbd "C-2")     #'end-of-buffer)
(define-key my-keys-minor-mode-map (kbd "C-9")     #'my/move-beginning-of-line)
(define-key my-keys-minor-mode-map (kbd "C-b")     #'my/push-mark)
(define-key my-keys-minor-mode-map (kbd "C-k")     #'my/scroll-down-some)
(define-key my-keys-minor-mode-map (kbd "C-l")     #'my/scroll-up-some)
(define-key my-keys-minor-mode-map (kbd "M-/")     #'previous-error)
(define-key my-keys-minor-mode-map (kbd "M-0")     #'forward-sexp)
(define-key my-keys-minor-mode-map (kbd "M-9")     #'backward-sexp)
(define-key my-keys-minor-mode-map (kbd "M-b")     #'my/pop-mark)
(define-key my-keys-minor-mode-map (kbd "M-l")     #'recenter-top-bottom)
(define-key my-keys-minor-mode-map (kbd "M-i")     #'avy-goto-char-in-line)
(define-key my-keys-minor-mode-map (kbd "C-i")     #'my/goto-char-or-expand)
(define-key my-keys-minor-mode-map (kbd "M-s y")   #'counsel-semantic-or-imenu)
(define-key my-keys-minor-mode-map (kbd "M-s b")   #'browse-url-at-point)
(define-key my-keys-minor-mode-map (kbd "M-g")     #'goto-line)

(move-text-default-bindings)

;; Regions

(define-key my-keys-minor-mode-map (kbd "C-,")     #'mc/mark-next-like-this)
(define-key my-keys-minor-mode-map (kbd "M-s C-,") #'mc/cycle-forward)
(define-key my-keys-minor-mode-map (kbd "M-a")     #'mark-paragraph)
(define-key my-keys-minor-mode-map (kbd "C-a")     #'my/select-current-line)
(define-key my-keys-minor-mode-map (kbd "C-n")     #'er/expand-region)
(define-key my-keys-minor-mode-map (kbd "C-M-n")   #'er/mark-inside-quotes)
(define-key my-keys-minor-mode-map (kbd "M-n")     #'er/mark-inside-pairs)
(define-key my-keys-minor-mode-map (kbd "C-S-n")   #'my/collapse-region)
(define-key my-keys-minor-mode-map (kbd "M-s i")   #'indent-region)

;; Editing

(define-key my-keys-minor-mode-map (kbd "C-'")             #'my/duplicate)
(define-key my-keys-minor-mode-map (kbd "C-<")             #'my/indent-left)
(define-key my-keys-minor-mode-map (kbd "C->")             #'my/indent-right)
(define-key my-keys-minor-mode-map (kbd "C-<backspace>")   #'my/delete-word-at-point)
(define-key my-keys-minor-mode-map (kbd "M-<backspace>")   #'backward-kill-word)
(define-key my-keys-minor-mode-map (kbd "C-d")             #'kill-line)
(define-key my-keys-minor-mode-map (kbd "M-s 1")           #'just-one-space)
(define-key my-keys-minor-mode-map (kbd "M-d")             #'my/top-join-line)
(define-key my-keys-minor-mode-map (kbd "M-s /")           #'my/toggle-comment)
(define-key my-keys-minor-mode-map (kbd "M-s a")           #'align-regexp)
(define-key my-keys-minor-mode-map (kbd "M-s o")           #'sort-lines)
(define-key my-keys-minor-mode-map (kbd "M-=")             #'my/increment-integer-at-point)
(define-key my-keys-minor-mode-map (kbd "M--")             #'my/decrement-integer-at-point)
(define-key my-keys-minor-mode-map (kbd "M-t")             #'my/flip-bool-at-point)
(define-key my-keys-minor-mode-map (kbd "C-y")             #'my/smart-new-line)
(define-key my-keys-minor-mode-map (kbd "M-s x")           #'overwrite-mode)
(define-key my-keys-minor-mode-map (kbd "°")               #'string-inflection-all-cycle)

;; Macros

(define-key my-keys-minor-mode-map (kbd "C-(") #'kmacro-start-macro)
(define-key my-keys-minor-mode-map (kbd "C-)") #'kmacro-end-macro)
(define-key my-keys-minor-mode-map (kbd "C-.") #'kmacro-end-and-call-macro)
(define-key my-keys-minor-mode-map (kbd "M-s C-c") #'cua-copy-region)
(define-key my-keys-minor-mode-map (kbd "M-s C-x") #'cua-cut-region)

;; Information

(define-key my-keys-minor-mode-map (kbd "M-s d") #'count-words)

;; Search and replacement

(define-key my-keys-minor-mode-map (kbd "C-s")   #'phi-search)
(define-key my-keys-minor-mode-map (kbd "M-s w") #'swiper)
(define-key my-keys-minor-mode-map (kbd "M-s p") #'counsel-projectile-switch-project)
(define-key my-keys-minor-mode-map (kbd "M-o")   #'counsel-find-file)
(define-key my-keys-minor-mode-map (kbd "M-s q") #'query-replace-regexp)
(define-key my-keys-minor-mode-map (kbd "M-s f") #'find-file-at-point)

;; Files

(define-key my-keys-minor-mode-map (kbd "C-4")     #'dired-jump)
(define-key my-keys-minor-mode-map (kbd "C-f")     (kbd "C-x C-s")) ; generic
(define-key my-keys-minor-mode-map (kbd "C-o")     #'counsel-rg)
(define-key my-keys-minor-mode-map (kbd "M-s M-o") #'my/occur-region)
(define-key my-keys-minor-mode-map (kbd "C-p")     #'counsel-git)
(define-key my-keys-minor-mode-map (kbd "M-s r")   #'my/rgrep)
(define-key my-keys-minor-mode-map (kbd "M-s l")   #'counsel-locate)
(define-key my-keys-minor-mode-map (kbd "M-p")     #'counsel-yank-pop)

;; Windows and buffers

(define-key my-keys-minor-mode-map (kbd "C-c k") #'my/switch-to-previous-buffer)
(define-key my-keys-minor-mode-map (kbd "C-e")   #'my/switch-to-next-window)
(define-key my-keys-minor-mode-map (kbd "C-r")   #'ivy-switch-buffer)
(define-key my-keys-minor-mode-map (kbd "C-w")   #'delete-window)
(define-key my-keys-minor-mode-map (kbd "M-e")   #'split-window-right)
(define-key my-keys-minor-mode-map (kbd "C-M-e") #'split-window-below)
(define-key my-keys-minor-mode-map (kbd "M-r")   #'ibuffer)
(define-key my-keys-minor-mode-map (kbd "M-w")   #'kill-buffer)
(define-key my-keys-minor-mode-map (kbd "C-t")   #'winner-undo)
(define-key my-keys-minor-mode-map (kbd "C-M-t") #'winner-redo)
(define-key my-keys-minor-mode-map (kbd "M-s m") #'centered-window-mode)
(define-key my-keys-minor-mode-map (kbd "M-s c") #'clone-indirect-buffer)


;; Display

(define-key my-keys-minor-mode-map (kbd "C--")   #'text-scale-decrease)
(define-key my-keys-minor-mode-map (kbd "C-=")   #'text-scale-increase)
(define-key my-keys-minor-mode-map (kbd "M-s t") #'my/switch-theme)
(define-key my-keys-minor-mode-map (kbd "M-s v") #'variable-pitch-mode)

;; Magit

(define-key my-keys-minor-mode-map (kbd "M-8") #'magit-status)
(define-key my-keys-minor-mode-map (kbd "M-4") #'magit-section-toggle)

;; Haskell

(define-key haskell-mode-map (kbd "C-c C-o") #'haskell-session-change-target)
(define-key haskell-mode-map (kbd "C-c C-p") #'haskell-process-restart)
(define-key haskell-mode-map (kbd "C-c C-m") #'ghc-insert-module)
(define-key haskell-mode-map (kbd "M-s g")   #'my/haskell-goothub)

;; Coq

(eval-after-load "proof-script" '(progn
  (define-key proof-mode-map (kbd "ö")   #'proof-assert-next-command-interactive)
  (define-key proof-mode-map (kbd "M-ö") #'proof-undo-last-successful-command)
  (define-key proof-mode-map (kbd "ß")   #'proof-goto-point)
  (define-key proof-mode-map (kbd "ü")   #'expand-abbrev)))

;; Completion

(define-key my-keys-minor-mode-map (kbd "C-<tab>") #'dabbrev-expand)
(define-key my-keys-minor-mode-map (kbd "M-x")     #'counsel-M-x)

;; Shell

(define-key my-keys-minor-mode-map (kbd "M-s s") #'my/create-shell)
(define-key my-keys-minor-mode-map (kbd "M-s S") #'my/create-shell-with-name)
(define-key comint-mode-map        (kbd "M-y")   #'counsel-shell-history)

;; Dired
(define-key dired-mode-map "H" #'dired-omit-mode)
(define-key dired-mode-map "O" #'my/dired-open-in-chrome)

;; Paste word to minibuffer

(define-key ivy-minibuffer-map (kbd "C-w") #'ivy-yank-word)

;; Org

(define-key my-keys-minor-mode-map (kbd "C-c a") #'org-agenda)
(define-key my-keys-minor-mode-map (kbd "C-c b") #'org-iswitchb)
(define-key my-keys-minor-mode-map (kbd "C-c c") #'org-capture)
(define-key my-keys-minor-mode-map (kbd "C-c l") #'org-store-link)

;; Calc
(define-key my-keys-minor-mode-map (kbd "M-s =") 'quick-calc)

;; Fun
(define-key my-keys-minor-mode-map (kbd "C-x w") 'elfeed)

;; GetSafe

(defun my/sync-getsafe ()
  (interactive)
  (my/sync "/home/drets/src/getsafe/react-native-app/"
           "drets@10.0.11.89:/Users/drets/getsafe/react-native-app"))

(define-key my-keys-minor-mode-map (kbd "M-s n") #'my/sync-getsafe)

;; End of key definitions.

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

(defun disable-my-keys ()
  (my-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'disable-my-keys)

(provide 'keys)
