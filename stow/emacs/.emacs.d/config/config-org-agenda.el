;; --- random ---
(setq org-agenda-default-appointment-duration 60)

;; -- window setup
(setq org-agenda-window-setup 'current-window)

;; --- Always start org-agenda on eyebrowse workspace 0 ---
(defun fp/org-agenda-wm-advice (orig-fun &rest args)
  (eyebrowse-switch-to-window-config-0)
  (apply orig-fun args))

(advice-add 'org-agenda :around 'fp/org-agenda-wm-advice)

;; --- files ---
(setq org-agenda-files (list (concat sync-directory "general/org/meinleben/privat.org")
                             (concat sync-directory "general/org/meinleben/studium.org")
                             (concat sync-directory "general/org/meinleben/arbeit.org")
                             (concat sync-directory "general/org/meinleben/capture.org")
                             (concat sync-directory "general/org/meinleben/webcal.org"))
      org-icalendar-combined-agenda-file (concat sync-directory "org/ics/combine.ics"))


(setq org-modules '(org-info org-habit))

;; --------------------------------------------------------------------------------
;; custom commands
;; --------------------------------------------------------------------------------
(setq org-agenda-custom-commands
      '(("a" "Default - Today"
         ((agenda "" ((org-agenda-span 1))) (alltodo ""))
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "FAIL" "TASK")))))

        ("w" "Week all"
         ((agenda "") (alltodo ""))
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "FAIL" "TASK")))))

        ("t" "All entries of type TODO"
         ((alltodo ""))
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "FAIL")))))

        ("s" "stundenplan"
         ((agenda ""))
         ((org-agenda-files '("~/sync/general/org/meinleben/stundenplan.org"))))))

;; --------------------------------------------------------------------------------
;; webcal
;; --------------------------------------------------------------------------------
(config-add-external-dependency 'gawk 'config-org-agenda "convert ics to org"
                                (lambda () (executable-find "gawk"))
                                "apt-get install -y gawk" "cyg-get awk")

(when (config-external-check-list '(gawk))
  (defun fp/webcal-to-org ()
    (interactive)
    (with-current-buffer (get-buffer-create "*webcal-to-org*")
      (cd (concat sync-directory "general/org/scripts")) (erase-buffer)
      (start-process-shell-command "webcal-to-org" "*webcal-to-org*"
                                   (concat "sh " default-directory "allwebcal.sh"))))
  (run-with-timer (* 20 60) (* 60 60) 'fp/webcal-to-org))

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
  "I" 'org-agenda-clock-in
  "O" 'org-agenda-clock-out
  "C" 'org-agenda-clockreport-mode
  "c" 'org-agenda-log-mode
  "i" 'org-agenda-show-clocking-issues)


(provide 'config-org-agenda)
