(add-hook #'LaTeX-mode-hook
	  (lambda ()
	     (setq TeX-auto-save t)
	     (setq TeX-parse-self t)
	     (setq TeX-save-query nil)
             (define-key LaTeX-mode-map (kbd "C-c C-l") #'preview-region)))

(provide 'tex-wrap)
