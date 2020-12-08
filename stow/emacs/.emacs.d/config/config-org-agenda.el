;; --- random ---
(setq org-agenda-default-appointment-duration 60)
(setq org-modules '(org-info org-habit))


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


;; --------------------------------------------------------------------------------
;; custom commands
;; --------------------------------------------------------------------------------
(setq org-agenda-custom-commands
      '(("a" "Default - Today"
         ((agenda "" ((org-agenda-span 1))) (todo "NEXT") (todo "TODO") (todo "WAIT") (todo "TASK"))
         ((org-agenda-start-with-log-mode t)))

        ("w" "Week all"
         ((agenda ""))
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))

        ("s" "stundenplan"
         ((agenda ""))
         ((org-agenda-files '("~/sync/general/org/meinleben/stundenplan.org"))))))

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
  "F"         'org-agenda-follow-mode
  "f"         'org-agenda-later
  "b"         'org-agenda-earlier
  "gd"        'org-agenda-goto-date

  (kbd "RET") 'org-agenda-goto
  "o"         'org-agenda-switch-to
  "s"         'org-agenda-show

  "q"         'quit-window
  "r"         'org-agenda-redo
  "a"         'org-agenda

  (kbd "C-s") 'org-agenda-filter

  "t"         'org-agenda-todo
  "S"         'org-agenda-schedule
  "D"         'org-agenda-deadline
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
