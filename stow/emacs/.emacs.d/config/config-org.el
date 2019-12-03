(require 'org)
(require-and-log 'config-programming-general)
(require-and-log 'config-org-agenda)

;; --------------------------------------------------------------------------------
;; babel
;; --------------------------------------------------------------------------------

;; allow execution of code without confirmation in certain directories
(setq org-confirm-babel-evaluate-directories (list sync-directory)
      org-confirm-babel-evaluate
      (lambda (lang body) (not (seq-filter
                           'identity
                           (mapcar (lambda (dir) (string-match-p dir default-directory))
                                   org-confirm-babel-evaluate-directories)))))

(with-eval-after-load "ob"
  (setq org-src-fontify-natively t
        org-src-window-setup 'current-window)
  (require 'ob-python)
  (require 'ob-ditaa)
  (require 'ob-shell)
  (require 'ob-octave)
  (setq org-babel-load-languages
        '((python . t)
          (perl . t)
          (emacs-lisp . t)
          (ditaa . t)
          (shell . t)
          (C . t)
          (java . t)
          (plantuml . t)
          (octave . t)
          (js . t)))

  (setq org-plantuml-jar-path
        (expand-file-name (concat sync-directory "emacs/bin/plantuml.jar")))
  ;; bindings
  (define-key org-src-mode-map (kbd "C-c C-c") 'org-edit-src-exit)

  ;; TODO what
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;; snippets
(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp :results output none\n?\n#+END_SRC"))
(add-to-list
 'org-structure-template-alist
 (list "of" (concat "#+BEGIN_SRC octave "
                    ":results file :exports both :session \"*octave-org-session*\""
                    "\n?\n#+END_SRC")))
(add-to-list
 'org-structure-template-alist
 (list "o" (concat "#+BEGIN_SRC octave "
                   ":results output :exports both :session \"*octave-org-session*\""
                   "\n?\n#+END_SRC")))

;; redisplay images after code-block evaluation
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
;; --------------------------------------------------------------------------------
;; appearance
;; --------------------------------------------------------------------------------
;; --- variable pitch ---
(defun fp/org-variable-pitch ()
  (interactive)
  (variable-pitch-mode 1)
  (fp/theme-font-setup)
  (mapcar
   (lambda (face)
     (set-face-attribute face nil
                         :family (face-attribute 'fixed-pitch :family)
                         :weight (face-attribute 'default :weight)))
   '(org-code org-link org-block org-table org-block-begin-line
              org-indent org-block-end-line org-meta-line org-document-info-keyword))
  (mapcar
   (lambda (face)
     (set-face-attribute face nil :family fp/theme-font-family-variable-pitch))
   '(org-quote)))

;; --- fontify bullet-points ---
(font-lock-add-keywords
 'org-mode
 '(("^ *- " . 'fixed-pitch) ; these two fix bullet lists indentation
   ("^ * " . 'fixed-pitch)
   ("^ *\\([-+]\\) " ; change '-' into a unicode bullet
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


;; --- other ---
(add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))
(setq org-image-actual-width nil  ; respect ATTR_* attributes
      org-pretty-entities-include-sub-superscripts t)

(setq org-fontify-whole-heading-line nil
      org-fontify-done-headline nil
      org-fontify-quote-and-verse-blocks t
      org-hide-emphasis-markers t
      org-tags-column -75
      org-ellipsis " »")

;; --- modeline ---
(with-eval-after-load "org-clock"
  (defun fp/org-clock-format-clock-string (oldfun)
    (propertize (concat " " (car (split-string (substring-no-properties (funcall oldfun)))))
                'face 'org-mode-line-clock))
  (advice-add 'org-clock-get-clock-string :around 'fp/org-clock-format-clock-string)

  (custom-set-faces
   '(org-mode-line-clock ((((background light)) (:foreground "dark blue"))
                          (t (:foreground "medium spring green"))))))

(setq org-clock-mode-line-total 'today)

;; --- latex ---
(config-add-external-dependency 'latex 'config-org "org latex snippets"
                                (lambda () (executable-find "latex"))
                                "sudo apt-get install -y texlive-latex-*" "choco install miktex")
(config-add-external-dependency 'dvipng 'config-org "org latex snippets"
                                (lambda () (executable-find "dvipng"))
                                "sudo apt-get install -y dvipng" "None")

(setq org-highlight-latex-and-related '(latex))
(plist-put org-format-latex-options :scale 1.5)

(defun fp/org-rebuild-latex-previews ()
  "Delete ltximg folder of the current org file and rebuild all images."
  (interactive)
  (let ((ltxdir (concat default-directory "ltximg/"))
        (current-prefix-arg '(16)))
    (when (file-exists-p ltxdir) (delete-directory ltxdir t nil))
    (org-remove-latex-fragment-image-overlays)
    (call-interactively 'org-toggle-latex-fragment)))

(evil-leader/set-key-for-mode 'org-mode
  "mL" 'fp/org-rebuild-latex-previews
  "ml" 'org-toggle-latex-fragment)

;; --- bullets ---
(use-package org-bullets
  :ensure t
  :after org
  :init (add-hook 'org-mode-hook 'org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("●" "○" "¬")))

;; --------------------------------------------------------------------------------
;; bindings
;; --------------------------------------------------------------------------------

(use-package evil-org
  :ensure t
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar)))

(evil-define-key 'insert org-mode-map (kbd "<backspace>") 'org-delete-backward-char)
(autoload 'org-tree-slide-enter "config-org-tree-slide.el")
(define-key org-mode-map (kbd "<f12>") 'org-tree-slide-enter)
(evil-define-key 'normal org-mode-map
  (kbd "RET") 'org-return
  "gx" 'org-open-at-point)

(evil-leader/set-key-for-mode 'org-mode
  "mh" 'helm-org-in-buffer-headings
  "mo" 'org-edit-special
  "mO" (lambda () (interactive)
         (let ((org-src-window-setup 'other-window)) (org-edit-special)))
  "mi" 'org-toggle-inline-images
  "mI" 'org-redisplay-inline-images
  "mt" 'org-todo
  "ma" 'org-archive-subtree-default)

(define-key org-mode-map [mouse-3]
  (lambda (event) (interactive "e") (fp/point-to-mouse event) (org-cycle)))

(define-key org-mode-map fp/mouse-back
  (lambda (event) (interactive "e") (fp/point-to-mouse event) (org-ctrl-c-ctrl-c)))


;; --------------------------------------------------------------------------------
;; agenda and todos
;; --------------------------------------------------------------------------------
;; --- todo states ---
(setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)"
                                    "TASK(a)" "NEXT(n)"
                                    "ASK(?)"
                                    "|" "DONE(d)" "FAIL(f)"))
      org-log-done t
      org-log-into-drawer t)


;; --------------------------------------------------------------------------------
;; exporting
;; --------------------------------------------------------------------------------
(with-eval-after-load "ox"
  (use-package htmlize :ensure t)
  (setq org-export-default-language "de")

  (setq org-style-css "~/git/dotfiles/other/org-style.css")

  (defun my-org-inline-css-hook (exporter)
    "Insert custom inline css"
    (when (eq exporter 'html)
      (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
             (path (concat dir "style.css"))
             (homestyle (or (null dir) (null (file-exists-p path))))
             (final (if homestyle org-style-css path))) ;; <- set your own style file path
        (setq org-html-head-include-default-style nil)
        (setq org-html-head (concat
                             ;; "<link rel=\"stylesheet\" media=\"screen\" href=\"https://fontlibrary.org/face/dejavu-serif\" type=\"text/css\">"
                             "<style type=\"text/css\">\n"
                             "<!--/*--><![CDATA[/*><!--*/\n"
                             (with-temp-buffer
                               (insert-file-contents final)
                               (buffer-string))
                             "/*]]>*/-->\n"
                             "</style>\n")))))

  (setq org-html-htmlize-output-type 'css)   ; default: 'inline-css
  (setq org-html-htmlize-font-prefix "org-") ; default: "org-"
  (add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook))

(setq org-ditaa-jar-path (concat sync-directory "emacs/bin/ditaa.jar"))

;; --------------------------------------------------------------------------------
;; files
;; --------------------------------------------------------------------------------
;; --- associations ---
(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . default)
        ("\\.png\\'" . default)
        ("\\.xoj\\'" . "xournal %s")))

;; manually set applications platform independent
;; these have to be set in org-file-apps to a value before
(setcdr (assoc "\\.png\\'" org-file-apps) (if (eq system-type 'windows-nt)
                                              "mspaint.exe %s" "pinta %s"))


;; --- archiving ---
(setq org-archive-location (concat sync-directory "general/org/archive.org::datetree/"))

;; --- download ---
(use-package org-download
  :ensure t
  :defer t
  :commands (org-download-screenshot org-download-yank org-download-rename-at-point)
  :init
  (evil-leader/set-key-for-mode 'org-mode
    "mds" 'org-download-screenshot
    "mdy" 'org-download-yank
    "mdr" 'org-download-rename-at-point)
  :config
  (setq org-download-annotate-function (lambda (link) "")
        org-download-screenshot-file (expand-file-name "~/.emacs.d/tempscreenshot.png"))
  (if (eq system-type 'windows-nt)
      (progn (config-add-external-dependency 'irfanview 'config-org "org-download"
                                             (lambda () (executable-find "i_view64"))
                                             "None" "cinst -y irfanview")
             (setq org-download-screenshot-method "i_view64 /capture=4 /convert=\"%s\""
                   org-download-backend "wget \"%s\" -O \"%s\""))
    (progn (config-add-external-dependency 'scrot 'config-org "org-download"
                                           (lambda () (executable-find "scrot"))
                                           "sudo apt-get install -y scrot" "None")
           (setq org-download-screenshot-method "scrot -s %s"))))

;; --------------------------------------------------------------------------------
;; capture
;; --------------------------------------------------------------------------------

(global-set-key (kbd "<S-f10>") 'org-store-link)
(setq org-capture-templates
      '(("t" "TODO" entry (file+headline (concat sync-directory "org/meinleben/capture.org") "Capture")
         "* TODO %?\n%i\n")
        ("T" "TASK" entry (file+headline (concat sync-directory "org/meinleben/capture.org") "Capture")
         "* TASK %?\n%i\n")

        ("l" "Link TODO" entry (file+headline (concat sync-directory "org/meinleben/capture.org") "Capture")
         "* TODO %?\n%i\n%a")
        ("L" "Link TASK" entry (file+headline (concat sync-directory "org/meinleben/capture.org") "Capture")
         "* TASK %?\n%i\n%a")))


;; --------------------------------------------------------------------------------
;; calendar
;; --------------------------------------------------------------------------------

(with-eval-after-load "calendar"
  (setq calendar-week-start-day 1)
  (setq calendar-intermonth-text
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'font-lock-warning-face))

  (setq calendar-intermonth-header
        (propertize "Wk"
                    'font-lock-face 'font-lock-keyword-face)))

;; --------------------------------------------------------------------------------
;; notifications
;; --------------------------------------------------------------------------------

(require 'notifications)
(require 'appt)
(appt-activate t)

(setq appt-message-warning-time 30) ; Show notification 30 minutes before event
(setq appt-display-interval 5)      ; Show notification every 5 minutes
(setq appt-display-mode-line t)     ; Show notification in mode-line

(defun fp/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

;; Update alarms every hour
(setq fp/org-appt-timer
      (run-with-timer
       2 (* 30 60) (lambda ()
                     (let ((inhibit-message t))
                       (fp/org-agenda-to-appt)))))

;; Display appointments as a dbus-message
(setq appt-disp-window-function 'fp/appt-display)

(defun fp/appt-display (min-to-app new-time msg)
  (let ((min-to-app (format "%s minutes left" min-to-app)))
    (if (executable-find "espeak")
        (start-process-shell-command
         "" nil (format "espeak -a 200 \"%s\"" min-to-app)))
    (generic-notification-notify min-to-app msg)))

;; ================================================================================
(provide 'config-org)
;; ================================================================================
