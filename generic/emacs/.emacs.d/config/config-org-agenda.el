;; --- random ---
(setq org-agenda-default-appointment-duration 60)
(setq org-modules '(org-info org-habit))


;; -- window setup
(setq org-agenda-window-setup 'current-window)

;; --- Always start org-agenda on eyebrowse workspace 8 ---
(defun fp/org-agenda-wm-advice (orig-fun &rest args)
  (eyebrowse-switch-to-window-config-8)
  (apply orig-fun args))

(advice-add 'org-agenda :around 'fp/org-agenda-wm-advice)

;; --- files ---
(custom-set-variables '(org-agenda-files '()))
(setq org-agenda-files '()
      org-icalendar-combined-agenda-file (concat sync-directory "org/ics/combine.ics"))


;; --------------------------------------------------------------------------------
;; custom commands
;; --------------------------------------------------------------------------------
(setq org-agenda-custom-commands
      '(("a" "All"
         ((agenda "" ((org-agenda-span 1)))
          (todo "NEXT") (todo "TODO") (todo "WAIT"))
         ((org-agenda-start-with-log-mode t)
          (org-agenda-tag-filter-preset '("-prv"))))
        ("p" "Without Private"
         ((agenda "" ((org-agenda-span 1)))
          (todo "NEXT") (todo "TODO") (todo "WAIT"))
         ((org-agenda-start-with-log-mode t)))
        ("w" "Work Only"
         ((agenda "" ((org-agenda-span 1)))
          (todo "NEXT") (todo "TODO") (todo "WAIT"))
         ((org-agenda-start-with-log-mode t)
          (org-agenda-tag-filter-preset '("+wrk"))))))

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
  "a"         'org-agenda-show

  "q"         'quit-window
  "u"         'org-agenda-undo
  "r"         'org-agenda-redo
  "R"         'org-agenda

  "A"         'org-agenda-archive
  "gw"        'org-agenda-week-view
  "gd"        'org-agenda-day-view

  "s"         'org-agenda-filter

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
