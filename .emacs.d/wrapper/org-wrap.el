;; Shell adjustments.
(require #'org)

(setq org-capture-templates
      '(("e" "English" entry (file+headline "~/org/pamparam/doc/sets/english/english.org" "Words") "** %?")
        ("d" "German" entry (file+headline "~/org/pamparam/doc/sets/german/german.org" "Words") "** %?")
        ("r" "Remember any" entry (file+headline "~/org/pamparam/doc/sets/any/any.org" "Any") "** %?")
        ("a" "Appointment" entry (file+headline "~/org/tasks.org" "Calendar")
         "* APPT %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n\n%U")
        ("t" "Task" entry (file+datetree "~/org/tasks.org")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n\n%U")
        ("j" "Journal" entry (file+datetree "~/org/journal.org") "** %?")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((js . t)
   (haskell . t)))

(provide 'org-wrap)
