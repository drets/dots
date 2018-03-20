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

(setq org-agenda-custom-commands
      '(("h" "Daily habits"
         ((agenda ""))
         ((org-agenda-show-log t)
          (org-agenda-ndays 7)
          (org-agenda-log-mode-items '(state))
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":HABIT:"))))
        ))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((haskell . t)
   (python . t)))

(provide 'org-wrap)
