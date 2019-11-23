;; --- disable default ui ---
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(toggle-frame-maximized)
;; (blink-cursor-mode -1)
(tooltip-mode 0)

(setq ring-bell-function
      (lambda () (let ((orig-fg (face-foreground 'mode-line)))
              (set-face-foreground 'mode-line "#F2804F")
              (run-with-idle-timer
               0.1 nil (lambda (fg) (set-face-foreground 'mode-line fg))
               orig-fg))))

;; --------------------------------------------------------------------------------
;; themes and fonts
;; --------------------------------------------------------------------------------
(setq fp/theme-font-family "Dejavu Sans Mono"
      fp/theme-font-family-size "11"
      fp/theme-font-family-fallback "Lucida Console"
      fp/theme-font-family-fallback-size "12"
      fp/theme-font-family-variable-pitch "DejaVu Serif"
      fp/theme-font-family-variable-pitch-size 120
      ;; these are used to set `fp/theme-font-family' in `fp/theme-switch'
      fp/theme-light-font-bold nil)

(defun font-exists-p (font) "check if font exists"
       (if (null (x-list-fonts font)) nil t))

(defun fp/theme-font-setup ()
  "Check if `fp/theme-font-family' exists. If not, fall back to
`fp/theme-font-family-fallback'. If that does not exist either,
dont do anything. Ohterwise setup default, variable-pitch and
fixed-pitch faces."
  (when (not (font-exists-p fp/theme-font-family))
    (setq fp/theme-font-family fp/theme-font-family-fallback))
  (when (font-exists-p fp/theme-font-family)
    (set-face-attribute 'default nil :font
                        (concat fp/theme-font-family
                                " " fp/theme-font-family-size))
    (set-face-attribute 'variable-pitch nil
                        :family fp/theme-font-family-variable-pitch
                        :height 1.0
                        :weight 'normal)
    (set-face-attribute 'fixed-pitch nil
                        :height 1.0
                        :font fp/theme-font-family
                        :weight 'normal)))

(fp/theme-font-setup)

(setq fp/theme-light-theme 'tsdh-light
      fp/theme-dark-theme 'misterioso
      custom-safe-themes t)

;; (use-package flucui-themes
;;   :ensure t
;;   :config
;;   (setq fp/theme-light-theme 'flucui-light
;;         fp/theme-dark-theme 'flucui-dark))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (use-package grandshell-theme :ensure t)
  (setq fp/theme-light-theme 'sanityinc-tomorrow-day
        fp/theme-dark-theme 'sanityinc-tomorrow-night))

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (doom-themes-org-config)
;;   (setq fp/theme-light-theme 'doom-one-light
;;         fp/theme-dark-theme 'doom-city-lights))

;; (use-package "spacemacs-theme"
;;   :ensure t
;;   :no-require t
;;   :config (setq fp/theme-light-theme 'spacemacs-light
;;                 fp/theme-dark-theme 'spacemacs-dark
;;                 spacemacs-theme-comment-bg nil
;;                 spacemacs-theme-org-height t))

;; (use-package ujelly-theme
;;   :ensure t
;;   :config (setq fp/theme-light-theme 'tsdh-light
;;                 fp/theme-dark-theme 'ujelly))

;; (use-package grandshell-theme
;;   :ensure t
;;   :config (setq fp/theme-light-theme 'tsdh-light
;;                 fp/theme-dark-theme 'grandshell))

;; (use-package alect-themes
;;   :ensure t
;;   :config
;;   (setq fp/theme-light-theme 'alect-light
;;         fp/theme-dark-theme 'alect-black)
;;   (alect-set-color 'light 'bg-1 "#fafafa")
;;   (alect-set-color 'light 'bg-0\.5 "#e7e7e7"))

;; (use-package material-theme
;;   :ensure t
;;   :config (setq fp/theme-light-theme 'material-light
;;                 fp/theme-dark-theme 'material))

;; (use-package zenburn-theme
;;   :ensure t
;;   :config (setq fp/theme-light-theme 'leuven
;;                 fp/theme-dark-theme 'zenburn))

;; (use-package solarized-theme
;;   :ensure t
;;   :config (setq fp/theme-light-theme 'solarized-light
;;                 fp/theme-dark-theme 'solarized-dark
;;                 solarized-use-variable-pitch nil
;;                 solarized-scale-org-headlines nil))

;; load a theme and disable all others
(defun load-reset-theme ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (call-interactively 'load-theme))

(setq fp/toggle-large-font-current nil
      fp/toggle-large-font-size 200)

(defun fp/toggle-large-font ()
  "Intended for presentations. TODO: make this a minor-mode"
  (interactive)
  (if fp/toggle-large-font-current
      (progn
        (set-face-attribute 'default (selected-frame)
                            :height 130)
        (setq fp/toggle-large-font-current nil))
    (progn
      (set-face-attribute 'default (selected-frame)
                          :height fp/toggle-large-font-size)
      (setq fp/toggle-large-font-current t))))

(defun fp/theme-adjust-global-font-size (inc)
  (set-face-attribute 'default nil
                      :height (+ (face-attribute 'default :height) inc)))

(global-set-key (kbd "C-c C-+") (lambda () (interactive) (fp/theme-adjust-global-font-size 20)))
(global-set-key (kbd "C-c C--") (lambda () (interactive) (fp/theme-adjust-global-font-size -20)))

(defun window-show-cursor (&optional show)
  (interactive)
  (if (and (not show) (internal-show-cursor-p))
      (internal-show-cursor nil nil)
    (internal-show-cursor nil t)))


(defvar fp/theme-switch-hour-end 20 "the hour in a 24 hour day where a dark theme should be loaded")
(defvar fp/theme-switch-hour-start 6 "the hour in a 24 hour day where a light theme should be loaded")
(defvar fp/current-theme 'light "either `dark' or `light', used to determine what to toggle to")

(defun fp/theme-init ()
  "Sets the current theme based on the time (and `fp/theme-switch-hour')
    and sets a time to switch to the dark theme when necessary"
  (let ((time (string-to-number (format-time-string "%H"))))
    (if (and (< time fp/theme-switch-hour-end) (> time fp/theme-switch-hour-start))
        (progn (fp/theme-switch 'light)
               (run-at-time (concat (number-to-string fp/theme-switch-hour-end)
                                    ":00")
                            nil (lambda () (fp/theme-switch 'dark))))
      (fp/theme-switch 'dark))))

(defun fp/theme-switch (type)
  (if (eq type 'light)
      (progn
        (setq fp/current-theme 'light)
        (disable-theme fp/theme-dark-theme)
        (when fp/theme-light-font-bold (set-face-attribute 'default nil :bold t))
        ;; without this if you could not select the default theme
        (if fp/theme-light-theme (load-theme fp/theme-light-theme)))
    (progn
      (setq fp/current-theme 'dark)
      (disable-theme fp/theme-light-theme)
      (set-face-attribute 'default nil :bold nil)
      (if fp/theme-dark-theme (load-theme fp/theme-dark-theme)))))

(defun fp/theme-toggle ()
  (interactive)
  (if (eq fp/current-theme 'light)
      (fp/theme-switch 'dark)
    (fp/theme-switch 'light)))

(defvar after-load-theme-hook nil "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(add-hook 'after-load-theme-hook
          (lambda ()
            ;; Terminus is too thin to be used with a light theme imo, so i have to
            ;; use it in bold. However, i do want to see bold highlighting in some
            ;; way (for instance in org-mode), so i just give it another color.
            (set-face-attribute 'bold nil :inherit 'font-lock-string-face)

            ;; got to hate this thing tbqh famalam
            (set-face-attribute 'fringe nil :background nil)))

(fp/theme-init)

;; --------------------------------------------------------------------------------
;; other
;; --------------------------------------------------------------------------------

(setq frame-title-format "fp@emacs")
(use-package rainbow-mode :ensure t :defer t)

(use-package olivetti
  :ensure t
  :defer t
  :config
  (setq-default olivetti-body-width 103)
  (evil-define-key 'normal olivetti-mode-map
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line)
  (evil-define-key 'visual olivetti-mode-map
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line)

  ;; wtf
  (add-hook 'olivetti-mode-hook
            (lambda () (interactive)
              (evil-emacs-state)
              (evil-normal-state))))

(provide 'config-ui)
