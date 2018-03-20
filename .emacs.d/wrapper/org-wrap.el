;; Org mode adjustments.
(require #'org)

(setq org-capture-templates
      '(("t" "Task" entry (file "~/org/tasks.org")
         "* TODO %?\n%U\n")
        ("d" "Journal" entry (file "~/org/journal.org") "* %?\n%U\n")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((haskell . t)
   (python . t)))

(provide 'org-wrap)
