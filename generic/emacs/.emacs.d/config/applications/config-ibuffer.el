;; --------------------------------------------------------------------------------
;; bindings
;; --------------------------------------------------------------------------------
(evil-set-initial-state 'ibuffer-mode 'normal)

(evil-define-key 'normal ibuffer-mode-map
  "0" 'evil-beginning-of-line
  "r" 'ibuffer-update
  "gg" 'evil-goto-first-line
  "t" 'ibuffer-toggle-marks
  "U" 'ibuffer-unmark-all-marks
  "j" 'ibuffer-forward-line
  "k" 'ibuffer-backward-line

  (kbd "C-k") 'evil-scroll-up

  (kbd "M-j") 'ibuffer-forward-filter-group
  "gj" 'ibuffer-forward-filter-group
  (kbd "M-k") 'ibuffer-backward-filter-group
  "gk" 'ibuffer-backward-filter-group)

;; --------------------------------------------------------------------------------
;; filter groups
;; --------------------------------------------------------------------------------
(setq ibuffer-saved-filter-groups
      '(("home"
         ("prog" (derived-mode . prog-mode))
         ("org" (mode . org-mode))
         ("dir" (mode . dired-mode))
         ("doc" (or
                 (mode . pdf-view-mode)
                 (mode . pdf-occur-buffer-mode)
                 (mode . image-mode)
                 (mode . nov-mode)))
         ("mail" (or
                  (mode . wl-folder-mode)
                  (mode . wl-summary-mode)
                  (mode . mime-view-mode)
                  (mode . wl-draft-mode)
                  (mode . message-mode)
                  (mode . mail-mode)
                  ))
         (">_" (or
                (mode . cider-repl-mode)
                (derived-mode . comint-mode)
                (mode . shell-mode)
                (mode . term-mode)
                (mode . eshell-mode)
                ))
         ("man" (mode . Man-mode))
         ("help" (or (name . "\*Help\*")
                     (name . "\*Apropos\*")
                     (name . "\*info\*")))
         ("helm" (mode . helm-major-mode)))))

(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-switch-to-saved-filter-groups "home")))

(setq ibuffer-never-show-predicates nil)
(setq ibuffer-never-show-predicates '("^\\*helm.*\\*"))



(provide 'config-ibuffer)
