;; Shell adjustments.
(require #'org)
(require 'org-drill)

(setq org-capture-templates
      '(("a" "Appointment" entry (file+headline "~/org/tasks.org" "Calendar")
         "* APPT %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n\n%U")
        ("n" "Note" entry (file+datetree "~/org/tasks.org") "* %?\n\n%U")
        ("t" "Task" entry (file+datetree "~/org/tasks.org")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n\n%U")
        ("j" "Journal" entry (file+datetree "~/org/journal.org") "** %?")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((js . t)
   (haskell . t)))

(provide 'org-wrap)
