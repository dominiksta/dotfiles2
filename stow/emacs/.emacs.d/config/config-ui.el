;; --- disable default ui ---
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(toggle-frame-maximized)
(blink-cursor-mode -1)
(tooltip-mode 0)
(setq use-dialog-box nil)

;; Do not send size hints to the window manager. The default value makes it
;; impossible to properly tile emacs.
(setq frame-resize-pixelwise t)

;; A visual bell
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
      fp/theme-font-family-size "13"
      fp/theme-font-family-variable-pitch "DejaVu Serif 13"
      ;; these are used to set `fp/theme-font-family' in `fp/theme-switch'
      fp/theme-light-font-bold nil)

(setq default-frame-alist
      (list (cons 'font (concat fp/theme-font-family " " fp/theme-font-family-size))
            '(vertical-scroll-bars . nil)
            '(horizontal-scroll-bars . nil)
            '(fullscreen . maximized)))
(set-fontset-font "fontset-default" 'unicode-bmp
                  (font-spec :family fp/theme-font-family))

(set-face-font 'fixed-pitch fp/theme-font-family)
(set-face-font 'variable-pitch fp/theme-font-family-variable-pitch)

;; --- defaults, to be changed further down ---
(setq fp/theme-light-theme 'tsdh-light
      fp/theme-dark-theme 'wombat
      custom-safe-themes t)

;; (straight-use-package 'flucui-themes)
;; (setq fp/theme-light-theme 'flucui-light
;;       fp/theme-dark-theme 'flucui-dark)

;; (straight-use-package 'kaolin-themes)
;; (setq fp/theme-light-theme 'kaolin-light
;;       fp/theme-dark-theme 'kaolin-dark)

(straight-use-package 'gruvbox-theme)
(setq fp/theme-light-theme 'gruvbox-light-soft
      fp/theme-dark-theme 'gruvbox-dark-soft)

;; (straight-use-package 'tangotango-theme)
;; (setq fp/theme-light-theme 'tango
;;       fp/theme-dark-theme 'tangotango)

;; (straight-use-package 'color-theme-sanityinc-tomorrow)
;; (setq fp/theme-light-theme 'sanityinc-tomorrow-day
;;       fp/theme-dark-theme 'sanityinc-tomorrow-bright)

;; (straight-use-package 'modus-operandi-theme)
;; (straight-use-package 'modus-vivendi-theme)
;; (setq fp/theme-light-theme 'modus-operandi
;;       fp/theme-dark-theme 'modus-vivendi)

;; (straight-use-package 'doom-themes)
;; (doom-themes-org-config)
;; (custom-set-faces
;;  '(highlight ((t (:underline t :background nil :foreground nil :distant-foreground nil)))))
;; (setq fp/theme-light-theme 'doom-one-light
;;       fp/theme-dark-theme 'doom-one)

;; (straight-use-package 'spacemacs-theme)
;; (setq fp/theme-light-theme 'spacemacs-light
;;       fp/theme-dark-theme 'spacemacs-dark
;;       spacemacs-theme-comment-bg nil
;;       spacemacs-theme-org-height t)

;; (straight-use-package 'ujelly-theme)
;; (setq fp/theme-light-theme 'tsdh-light
;;       fp/theme-dark-theme 'ujelly)

;; (straight-use-package 'grandshell-theme)
;; (setq fp/theme-light-theme 'tsdh-light
;;       fp/theme-dark-theme 'grandshell)

;; (straight-use-package 'alect-themes)
;; (setq fp/theme-light-theme 'alect-light
;;       fp/theme-dark-theme 'alect-black)
;; (alect-set-color 'light 'bg-1 "#fafafa")
;; (alect-set-color 'light 'bg-0\.5 "#e7e7e7")

;; (straight-use-package 'material-theme)
;; (setq fp/theme-light-theme 'material-light
;;       fp/theme-dark-theme 'material)

;; (straight-use-package 'zenburn-theme)
;; (setq fp/theme-light-theme 'tango
;;       fp/theme-dark-theme 'zenburn)

;; (straight-use-package 'solarized-theme)
;; (setq fp/theme-light-theme 'solarized-light
;;       fp/theme-dark-theme 'solarized-dark
;;       solarized-use-variable-pitch nil
;;       solarized-scale-org-headlines nil)

;; load a theme and disable all others
(defun load-reset-theme ()
  (interactive)
  (let ((enabled-themes custom-enabled-themes))
    (call-interactively 'load-theme)
    (mapc #'disable-theme enabled-themes)))

(setq fp/toggle-large-font-current nil
      fp/toggle-large-font-size 200)

;; --- adjusting font size ---

(defun fp/toggle-large-font ()
  "Intended for presentations. TODO: make this a minor-mode"
  (interactive)
  (if fp/toggle-large-font-current
      (progn
        (set-face-attribute 'default nil :font
                            (concat fp/theme-font-family " "
                                    fp/theme-font-family-size))
        (setq fp/toggle-large-font-current nil))
    (progn
      (set-face-attribute 'default (selected-frame)
                          :height fp/toggle-large-font-size)
      (setq fp/toggle-large-font-current t))))

(defun fp/theme-adjust-global-font-size (inc)
  (if (eq inc 0)
      (set-face-attribute 'default nil :font (concat fp/theme-font-family " "
                                                     fp/theme-font-family-size))
    (set-face-attribute 'default nil
                        :height (+ (face-attribute 'default :height) inc))))

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
            ;; ;; Terminus is too thin to be used with a light theme imo, so i have to
            ;; ;; use it in bold. However, i do want to see bold highlighting in some
            ;; ;; way (for instance in org-mode), so i just give it another color.
            ;; (set-face-attribute 'bold nil :inherit 'font-lock-string-face)

            ;; got to hate this thing tbqh famalam
            (set-face-attribute 'fringe nil :background nil)))

(fp/theme-init)

;; --------------------------------------------------------------------------------
;; image display
;; --------------------------------------------------------------------------------

(defface image-background
  '((((background  dark)) :background "#DDDDDD")
    (((background light)) :background nil))
  "The background of any inline images."
  :group 'basic-faces)

(defun create-image-with-background-color (args)
  "Specify background color of Org-mode inline image through modify `ARGS'."
  (let* ((file (car args))
         (type (cadr args))
         (data-p (caddr args))
         (props (cdddr args)))
    ;; get this return result style from `create-image'
    (append (list file type data-p)
            (list :background (face-background 'default))
            props)))

(advice-add 'create-image :filter-args
            #'create-image-with-background-color)

;; --------------------------------------------------------------------------------
;; other
;; --------------------------------------------------------------------------------

(setq frame-title-format "f1p@emacs")

(straight-use-package 'rainbow-mode)

(straight-use-package 'olivetti)

(with-eval-after-load "olivetti"
  (setq-default olivetti-body-width 103)
  (evil-define-key 'normal olivetti-mode-map
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line)
  (evil-define-key 'visual olivetti-mode-map
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line)

  ;; evil bug
  (add-hook 'olivetti-mode-hook
            (lambda () (interactive) (evil-emacs-state) (evil-normal-state))))


(provide 'config-ui)
