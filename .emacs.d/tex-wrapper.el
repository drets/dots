(defun latex-compile ()
  (interactive)
  (save-buffer)
  (TeX-command "LaTeX" 'TeX-master-file))

(add-hook #'LaTeX-mode-hook
	  (lambda ()
	     (setq TeX-auto-save t)
	     (setq TeX-parse-self t)
	     (setq TeX-save-query nil)
             (define-key LaTeX-mode-map (kbd "C-c C-l") #'latex-compile)))

(provide 'tex-wrapper)
