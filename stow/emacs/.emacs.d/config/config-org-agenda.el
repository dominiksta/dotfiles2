;; --- setup ---
(setq org-agenda-default-appointment-duration 60
      org-agenda-window-setup 'current-window)

;; --- files ---
(setq org-agenda-files (list (concat sync-directory "general/org/meinleben/privat.org")
                             (concat sync-directory "general/org/meinleben/studium.org")
                             (concat sync-directory "general/org/meinleben/arbeit.org")
                             (concat sync-directory "general/org/meinleben/capture.org"))
      org-icalendar-combined-agenda-file (concat sync-directory "org/ics/combine.ics"))


(setq org-modules '(org-info org-habit))
;; --------------------------------------------------------------------------------
;; bindings
;; --------------------------------------------------------------------------------
(evil-set-initial-state 'org-agenda-mode 'normal)
(evil-define-key 'normal org-agenda-mode-map
  "j"         'org-agenda-next-line
  (kbd "g j") 'org-agenda-next-date-line
  (kbd "M-j") 'org-agenda-next-date-line
  "k"         'org-agenda-previous-line
  (kbd "g k") 'org-agenda-previous-date-line
  (kbd "M-k") 'org-agenda-previous-date-line
  "f"         'org-agenda-later
  "b"         'org-agenda-earlier
  (kbd "RET") 'org-agenda-switch-to
  "q"         'quit-window
  "r"         'org-agenda-redo
  "a"         'org-agenda

  "t"         'org-agenda-todo
  "s"         'org-agenda-schedule
  "d"         'org-agenda-deadline
  "e"         'org-agenda-set-effort
  "p"         'org-agenda-set-property
  ":"         'org-agenda-set-tags
  "."         'org-agenda-goto-today
  "C" 'org-agenda-clockreport-mode
  "c" 'org-agenda-log-mode
  "i" 'org-agenda-show-clocking-issues
  [mouse-2] 'org-agenda-goto-mouse)


;; --------------------------------------------------------------------------------
;; custom commands
;; --------------------------------------------------------------------------------
(setq org-agenda-custom-commands
      '(("a" "Agenda for current Week"
         ((agenda ""))
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":DAILY:"))))

        ("t" "All TODO and NEXT entries"
         ((alltodo ""))
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "FAIL" "TASK")))))

        ("n" "Agenda and all TODOs"
         ((agenda "") (todo "TODO"))
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":DAILY:"))))

        ("s" "Stundenplan"
         ((agenda ""))
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":DAILY:"))
          (org-agenda-files '("~/Dropbox/general/org/meinleben/stundenplan.org"))))

        ("b" "All"
         ((agenda "") (alltodo ""))
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "FAIL" "TASK")))
          (org-agenda-files (append org-agenda-files '("~/Dropbox/general/org/meinleben/stundenplan.org")))))))

;; --------------------------------------------------------------------------------
;; timetable
;; --------------------------------------------------------------------------------
(config-add-external-dependency 'wget 'config-org-agenda "download timetable"
                                (lambda () (executable-find "wget"))
                                "apt install wget" "cyg-get wget")

(config-add-external-dependency 'sh 'config-org-agenda "timetable script"
                                (lambda () (executable-find "sh")) "default" "cygwin or something")

(config-add-external-dependency 'gawk 'config-org-agenda "convert timetable ics to org"
                                (lambda () (executable-find "gawk"))
                                "apt install gawk" "cyg-get awk")

(config-add-external-dependency 'python 'config-org-agenda
                                "insert 'scheduled' into timetable for orgzly"
                                (lambda () (executable-find "python"))
                                "apt install python" "cyg-get python3")

(when (config-external-check-list '(wget sh gawk python))
  (defun fp/get-timetable ()
    (interactive)
    (with-current-buffer (get-buffer-create "*timetable*")
      (cd (concat sync-directory "general/org/mystundenplan")) (erase-buffer)
      (start-process-shell-command "timetable" "*timetable*"
                                   (concat "sh " default-directory "getmystundenplan.sh"))))
  (run-with-timer (* 20 60) (* 60 60) 'fp/get-timetable))



(provide 'config-org-agenda)
