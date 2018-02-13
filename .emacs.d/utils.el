(require 'thingatpt)

(defun my/indent-left ()
  "Shift code block one column to the left."
  (interactive)
  (indent-code-rigidly
   (region-beginning) (region-end)
   -1))

(defun my/indent-right ()
  "Shift code block one column to the right."
  (interactive)
  (indent-code-rigidly
   (region-beginning) (region-end)
   1))

(defun my/duplicate ()
  (interactive)
  (if mark-active
    (my/duplicate-region)
    (my/duplicate-line)))

(defun my/duplicate-line ()
  (let ((text (buffer-substring (point)
                                (beginning-of-thing 'line))))
    (forward-line)
    (push-mark)
    (insert text)
    (open-line 1)))

(defun my/duplicate-region ()
  (let* ((end (region-end))
         (text (buffer-substring (region-beginning)
                                 end)))
    (goto-char end)
    (insert text)
    (push-mark end)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))

(defun my/push-mark ()
  (interactive)
  (cua-set-mark)
  (cua-set-mark))

(defun my/pop-mark ()
  (interactive)
  (cua-set-mark 0))

(defun my/switch-to-next-window ()
  "Switch to the next window"
  (interactive)
  (other-window 1))

(defun my/move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun my/scroll-down-some ()
  "Move buffer several lines down (i.e. point goes up)."
  (interactive)
  (cua-scroll-down 18))

(defun my/scroll-up-some ()
  "Move buffer several lines up (i.e. point goes down)."
  (interactive)
  (cua-scroll-up 18))

(defun my/switch-theme ()
  "Switch between light and dark themes."
  (interactive)
  (let ((is-light (member 'monochrome-bright custom-enabled-themes)))
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (load-theme (if is-light 'monochrome 'monochrome-bright))))

(defun my/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun my/sync (from to)
  "Synchronize 2 source folders using rsync. Save buffers before sync."
  (interactive)
  (save-some-buffers t)
  (call-process-shell-command
   (format "rsync -avz --exclude-from=%s --exclude=.git %s %s"
           (concat from ".gitignore") from to) nil 0))

(defun my/toggle-comment ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))

(provide 'utils)

(defun my/google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string "Google: "))))))

(defun my/top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

(defun my/create-shell ()
  "Create a shell with a default directory name."
  (interactive)
  (shell (concat "*Shell " (read-string "Shell: ") "*")))

(defun my/collapse-region ()
  "Collapse selected region."
  (interactive)
  (er/expand-region -1))

(defun thing-at-point-goto-end-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip over optional sign
    (when (looking-at "[+-]")
      (forward-char 1))
    ;; Skip over digits
    (skip-chars-forward "[[:digit:]]")
    ;; Check for at least one digit
    (unless (looking-back "[[:digit:]]")
      (error "No integer here"))))
(put 'integer 'beginning-op 'thing-at-point-goto-end-of-integer)

(defun thing-at-point-goto-beginning-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip backward over digits
    (skip-chars-backward "[[:digit:]]")
    ;; Check for digits and optional sign
    (unless (looking-at "[+-]?[[:digit:]]")
      (error "No integer here"))
    ;; Skip backward over optional sign
    (when (looking-back "[+-]")
        (backward-char 1))))
(put 'integer 'beginning-op 'thing-at-point-goto-beginning-of-integer)

(defun thing-at-point-bounds-of-integer-at-point ()
  "Get boundaries of integer at point."
  (save-excursion
    (let (beg end)
      (thing-at-point-goto-beginning-of-integer)
      (setq beg (point))
      (thing-at-point-goto-end-of-integer)
      (setq end (point))
      (cons beg end))))
(put 'integer 'bounds-of-thing-at-point 'thing-at-point-bounds-of-integer-at-point)

(defun thing-at-point-integer-at-point ()
  "Get integer at point."
  (let ((bounds (bounds-of-thing-at-point 'integer)))
    (string-to-number (buffer-substring (car bounds) (cdr bounds)))))
(put 'integer 'thing-at-point 'thing-at-point-integer-at-point)

(defun my/increment-integer-at-point (&optional inc)
  "Increment integer at point by one.

With numeric prefix arg INC, increment the integer by INC amount."
  (interactive "p")
  (let ((inc (or inc 1))
        (n (thing-at-point 'integer))
        (bounds (bounds-of-thing-at-point 'integer)))
    (delete-region (car bounds) (cdr bounds))
    (insert (int-to-string (+ n inc)))))

(defun my/decrement-integer-at-point (&optional dec)
  "Decrement integer at point by one.

With numeric prefix arg DEC, decrement the integer by DEC amount."
  (interactive "p")
  (my/increment-integer-at-point (- (or dec 1))))

(defun my/flip-bool-at-point ()
  (interactive)
  (let* ((bools '(("true" . "false")
                  ("True" . "False")
                  ("TRUE" . "FALSE")
                  ("1" . "0")
                  ("T" . "F")))
         (true  (cdr (assoc  (current-word) bools)))
         (false (car (rassoc (current-word) bools)))
         (wrd (cond (true true)
                    (false false)
                    (t (current-word)))))
    (save-excursion
      (forward-word)
      (backward-kill-word 1)
      (insert wrd))))

(defun my/select-current-line ()
  "Select the current line"
  (interactive)
  (move-beginning-of-line 1)
  (push-mark nil nil t)
  (forward-line 1))

(defun my/goto-char-or-expand ()
  "Expand a snippet; if there's no expandable snippet, run avy."
  (interactive)
  (unless (yas-expand)
    (call-interactively 'avy-goto-word-or-subword-1)))

(defun my/rgrep ()
  "rgrep without ivy"
  (interactive)
  (let ((completing-read-function 'completing-read-default)
        (completion-in-region-function 'completion--in-region))
    (call-interactively 'rgrep)))

(defun my/kill-the-word ()
  "kill word under the cursor"
  (interactive)
  (progn (er/mark-symbol-with-prefix) (call-interactively 'delete-region)))

(defun my/smart-new-line ()
  (interactive)
  (end-of-line)
  (if (current-line-empty-p)
      (indent-for-tab-command)
    (if (next-line-empty-p)
        (progn (next-line) (indent-for-tab-command))
      (newline-and-indent))))

(defun next-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (next-line)
    (looking-at "[[:space:]]*$")))

(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))
