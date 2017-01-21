;; Shell adjustments.
(require #'org)

(setq org-capture-templates
      '(("a" "Appointment" entry (file+headline "~/org/tasks.org" "Calendar")
         "* APPT %^{Description} %^g\n%?\nAdded: %U")
         ("n" "Note" entry (file+datetree "~/org/tasks.org")
         "* %^{Description} %^g\n%?\nAdded: %U")
         ("t" "Task" entry (file+datetree "~/org/tasks.org")
         "* TODO %^{Description}  %^g\n%?\nAdded: %U")
         ("j" "Journal" entry (file+datetree
         "~/org/journal.org")
         "** %^{Description}\n%?")))

(provide 'org-wrapper)
