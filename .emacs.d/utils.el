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
  (let ((is-light (find 'solarized-light custom-enabled-themes)))
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (load-theme (if is-light 'solarized-dark 'solarized-light))))

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
  (shell (concat "shell " (expand-file-name default-directory))))

(defun my/collapse-region ()
  "Collapse selected region."
  (interactive)
  (er/expand-region -1))
