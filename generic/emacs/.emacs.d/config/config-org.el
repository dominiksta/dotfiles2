(require-and-log 'config-language-natural)
(require-and-log 'config-programming-general)
(require-and-log 'config-language-latex)
(require-and-log 'config-org-agenda)
(require-and-log 'config-session)

;; (straight-use-package
;;  '(org-plus-contrib
;;    :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git/"
;;    :branch "main"
;;    :local-repo "org"
;;    :files (:defaults "contrib/lisp/*.el")
;;    :includes (org)))
(straight-use-package 'org)
(straight-use-package 'org-contrib)

(require 'org)

;; --------------------------------------------------------------------------------
;; wournal
;; --------------------------------------------------------------------------------

(defvar wournal-executable
  "~/AppData/Local/Programs/Wournal/Wournal.exe")

(defvar wournal-file-format "./img/%Y-%m-%d_%H-%M-%S.svg")

(defun fp/org-wournal-new (size)
  (interactive ((lambda nil (list (completing-read
                              "Size:" '("800x500" "600x300"))))))
  (let* ((split (split-string size "x"))
         (width (car split))
         (height (cadr split))
         (file (format-time-string wournal-file-format)))
    (mkdir (file-name-directory wournal-file-format) t)
    (start-process "wournal" nil wournal-executable
                   (format "--page-height=%s" height)
                   (format "--page-width=%s" width)
                   (format "--page-color=%s" "white")
                   (format "--page-style=%s" "graph")
                   (expand-file-name file))
    file))

(defun fp/org-wournal-new-at-point ()
  (interactive)
  (let ((file (call-interactively 'fp/org-wournal-new)))
    (insert (format "[[file:%s]]" file))))

(evil-leader/set-key-for-mode 'org-mode "mw" 'fp/org-wournal-new-at-point)

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
        org-src-window-setup 'current-window
        org-edit-src-content-indentation 0)
  (require 'ob-python)
  (setq org-babel-python-command "python3")

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
        (expand-file-name (concat sync-directory "documents/code/emacs/bin/plantuml.jar")))
  ;; bindings
  (define-key org-src-mode-map (kbd "C-c C-c") 'org-edit-src-exit)

  ;; TODO what
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;; snippets
;; (add-to-list 'org-structure-template-alist
;;              '("el" "#+BEGIN_SRC emacs-lisp :results output none\n?\n#+END_SRC"))
;; (add-to-list
;;  'org-structure-template-alist
;;  (list "of" (concat "#+BEGIN_SRC octave "
;;                     ":results file :exports both :session \"*octave-org-session*\""
;;                     "\n?\n#+END_SRC")))
;; (add-to-list
;;  'org-structure-template-alist
;;  (list "o" (concat "#+BEGIN_SRC octave "
;;                    ":results output :exports both :session \"*octave-org-session*\""
;;                    "\n?\n#+END_SRC")))

;; redisplay images after code-block evaluation
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
;; --------------------------------------------------------------------------------
;; appearance
;; --------------------------------------------------------------------------------

(straight-use-package 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)
;; (setq org-bullets-bullet-list '("◉" "○" "✸" "✿"))
(setq org-bullets-bullet-list '("▸"))

(custom-set-faces
 ;; fix for emacs27's new :extend keyword and org babel
 '(org-block ((t (:extend t))))
 '(org-block-begin-line ((t (:extend t))))
 '(org-block-end-line ((t (:extend t))))

 ;; heading sizes
 '(org-level-1 ((t (:overline t :height 1.2))))
 '(org-level-2 ((t (:overline t :height 1.0))))
 '(org-level-3 ((t (:overline t :height 1.0))))

 ;; title size
 '(org-document-title ((t (:height 1.4))))

 ;; drawers
 '(org-drawer ((t (:inherit 'font-lock-comment-face)))))

;; --- other ---
(add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))
(add-hook 'org-mode-hook (lambda () (eldoc-mode 0)))
(setq org-image-actual-width nil  ; respect ATTR_* attributes
      org-pretty-entities-include-sub-superscripts t)

(setq org-fontify-whole-heading-line nil
      org-fontify-done-headline nil
      org-fontify-quote-and-verse-blocks t
      org-hide-emphasis-markers nil
      org-ellipsis " ▾"
      org-tags-column -75)

;; --------------------------------------------------------------------------------
;; clocking
;; --------------------------------------------------------------------------------
(straight-use-package 'org-pomodoro)

(with-eval-after-load "org-pomodoro"
  (setq org-pomodoro-format "P:%s"
        org-pomodoro-time-format "%.2m"
        org-pomodoro-play-sounds nil)
  (add-hook 'org-pomodoro-finished-hook
            (lambda () (generic-notification-notify "Pomodoro finished"
                                               "Take a break!" 10)))
  (add-hook 'org-pomodoro-break-finished-hook
            (lambda () (generic-notification-notify "Break is over"
                                               "Start a new pomodoro?" 10))))


(with-eval-after-load "org-clock"
  (defun fp/org-clock-format-clock-string (oldfun)
    (propertize (concat " " (car (split-string (substring-no-properties (funcall oldfun)))))
                'face 'org-mode-line-clock))
  (advice-add 'org-clock-get-clock-string :around 'fp/org-clock-format-clock-string)
  (set-face-attribute 'org-mode-line-clock nil
                      :foreground nil
                      :background nil
                      :inherit 'font-lock-variable-name-face)
  (defun fp/org-clock-kill-emacs-query-function ()
    (if (org-clock-is-active)
        (yes-or-no-p "An Org-mode Clock is currently active. Kill anyway? ")
      t))
  (add-to-list 'kill-emacs-query-functions 'fp/org-clock-kill-emacs-query-function))

(setq org-clock-mode-line-total 'today)

;; ----------------------------------------------------------------------
;; latex
;; ----------------------------------------------------------------------

;; ---------------------------------- references ---------------------------------
(straight-use-package 'org-ref)
(require 'org-ref)

(with-eval-after-load "org-ref"
  (setq
   ;; Set the default bibliography file. Can be overwritten with
   ;; 'bibliograph:/path/to/somefile' in any org-file
   org-ref-default-bibliography (list reftex-default-bibliography)
   ;; Open up pdfs from the file-attribute in the bib-file
   org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex)

  (evil-leader/set-key-for-mode 'org-mode
    "mr" 'org-ref-helm-insert-ref-link))

;; ---------------------------------- exporting ----------------------------------

(with-eval-after-load "ox"
  ;; include the ability to ignore headlines while still including their body
  ;; with an :ignore: tag
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))

  ;; Add komascript's scrreprt to the available classes with my preffered
  ;; settings. With this, you can use "#+LATEX_CLASS: scrreprt" in your
  ;; org-file.
  (add-to-list 'org-latex-classes
               '("scrreprt"
                 "\\documentclass[11pt, DIV=13, parskip=half, headings]{scrreprt}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; Includes the `minted` package in `org-latex-default-packages-alist' and
  ;; sets up some other stuff for minted. We need this for syntax highlighting
  ;; in code.
  (config-add-external-dependency
   'minted 'config-org "syntax highlighting in latex export"
   (lambda () (executable-find "pygmentize"))
   "sudo pip3 install Pygments" "pip3 install Pygments")
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-packages-alist nil)

  ;; --- beamer ---
  (evil-leader/set-key-for-mode 'org-mode "mb" 'org-beamer-select-environment)

  (add-to-list 'org-latex-classes
               '("beamer_handout" "\\documentclass[handout]{beamer}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

  ;; Sets up the build-pipeline. We call pdflatex multiple times here to prevent
  ;; weirdness like toc not being generated. This is a normal latex problem. The
  ;; '-shell-escape' is necessary for latex to call out to external processes
  ;; such as pygmentize from minted.
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

;; ----------------------------------- previews -----------------------------------

(config-add-external-dependency 'latex 'config-org "org latex snippets"
                                (lambda () (executable-find "latex"))
                                "sudo apt-get install -y texlive-latex-*" "choco install miktex")
(config-add-external-dependency 'dvipng 'config-org "org latex snippets"
                                (lambda () (executable-find "dvipng"))
                                "sudo apt-get install -y dvipng" "None")

(setq org-highlight-latex-and-related '(latex))

;; --- size ---

(defun org--get-display-dpi ()
  "Overwrite the original `org--get-display-dpi'.
This will simply return the number 82, as that is what the
original `org--get-display-dpi' returned for my main
monitor.  Returning a constant number makes the display of latex
previews consistent for laptop and docked usage."
  82)

(plist-put org-format-latex-options :scale 2)

;; --- color ---

(defun fp/switch-org-latex-dir-for-theme ()
  "Set `org-preview-latex-image-directory' for the background
color of the current theme and regenerate all latex previews in
all open org-mode buffers. When called after a theme change, this
will adapt the latex previews to the theme. This makes me really
happy."
  (setq org-preview-latex-image-directory
        (format "ltximg/%s/"
                (substring (alist-get 'background-color (frame-parameters)) 1)))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'org-mode)
        (org-toggle-latex-fragment '(64))
        (org-toggle-latex-fragment '(16))))))

(fp/switch-org-latex-dir-for-theme) ;; set on startup
(add-hook 'after-load-theme-hook 'fp/switch-org-latex-dir-for-theme)

(evil-leader/set-key-for-mode 'org-mode "ml" 'org-toggle-latex-fragment)

;; --------------------------------------------------------------------------------
;; bindings
;; --------------------------------------------------------------------------------
(straight-use-package 'evil-org)

(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))


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

;; I find the other functions of org-meta-return get in my way more often than
;; not.
(define-key org-mode-map (kbd "<M-return>") 'org-insert-heading)

(define-key org-mode-map [mouse-3]
  (lambda (event) (interactive "e") (fp/point-to-mouse event) (org-cycle)))

(setq org-imenu-depth 10)

(add-hook 'org-mode-hook 'visual-line-mode)

;; --------------------------------------------------------------------------------
;; agenda and todos
;; --------------------------------------------------------------------------------
;; --- todo states ---
(setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "TASK(a)" "NEXT(n)"
                                    "|" "DONE(d)" "FAIL(f)"))
      org-log-done t
      org-log-into-drawer t)


;; --------------------------------------------------------------------------------
;; exporting
;; --------------------------------------------------------------------------------

(with-eval-after-load "ox"
  (straight-use-package 'ox-gfm) ;; github flavoured markdown
  (straight-use-package 'htmlize)
  (setq org-export-default-language "de"
        org-html-validation-link nil)

  (setq org-style-css "~/Source/git/dotfiles/other/org-style.css")

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

(setq org-ditaa-jar-path (concat sync-directory "documents/code/emacs/bin/ditaa.jar"))

;; --------------------------------------------------------------------------------
;; files
;; --------------------------------------------------------------------------------
;; --- associations ---
(setq org-file-apps
      (list '(auto-mode . emacs)
            '("\\.mm\\'" . default)
            '("\\.x?html?\\'" . default)
            '("\\.png\\'" . default)
            (cons "\\.woj\\'" "wslview %s")
            (cons "\\.svg\\'" "wslview %s")
            '("\\.xoj\\'" . "xournal %s")))

;; manually set applications platform independent
;; these have to be set in org-file-apps to a value before
(setcdr (assoc "\\.png\\'" org-file-apps)
        (cond ((eq system-type 'windows-nt) "mspaint.exe %s")
              (fp/running-on-wsl-p "wslview %s")
              ((eq system-type 'windows-nt) "mspaint.exe %s")))

;; archiving: also see `config-mvtn'
(setq org-archive-location "::* Erledigt")

;; --- download/screenshots ---
(straight-use-package 'org-download)

(autoload 'org-download-screenshot "org-download")
(autoload 'org-download-yank "org-download")
(autoload 'org-download-rename-at-point "org-download")

(evil-leader/set-key-for-mode 'org-mode
  "mds" 'org-download-screenshot
  "mdy" 'org-download-yank
  "mdr" 'org-download-rename-at-point)

(with-eval-after-load "org-download"
  (setq org-download-annotate-function (lambda (link) "")
        org-download-screenshot-file (expand-file-name "~/.emacs.d/screenshot.png"))
  (setq-default org-download-heading-lvl nil
                org-download-image-dir "orgimg")
  (setq org-download-screenshot-method
        (cond
         ((or fp/running-on-wsl-p (eq system-type 'windows-nt))
          (lambda (filename)
            (let ((default-directory "~/.emacs.d/"))
              (fp/powershell-command
               "(Get-Clipboard -Format image).save(\"screenshot.png\")"))))
         (t "import %s"))))

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
;; attachments
;; --------------------------------------------------------------------------------

(with-eval-after-load "org-attach"
  (setq org-attach-use-inheritance t)

  (defun fp/org-attach-insert-link ()
    "Insert a link to an attachment at point."
    (interactive)
    (insert
     (format "[[attachment:%s]]"
             (completing-read "Insert link to attachment: "
                              (org-attach-file-list (org-attach-dir)))))))

(autoload 'fp/org-attach-insert-link "org-attach.el")

(evil-leader/set-key-for-mode 'org-mode
  "mda" 'org-attach
  "mdi" 'fp/org-attach-insert-link)

;; --------------------------------------------------------------------------------
;; spelling
;; --------------------------------------------------------------------------------

(defun fp/ispell-setup-org ()
  (make-local-variable 'ispell-skip-region-alist)
  (push '(org-property-drawer-re) ispell-skip-region-alist)
  (push '(org-link-any-re) ispell-skip-region-alist)
  (push '("^#\\+ATTR_.+$") ispell-skip-region-alist)
  (push '("~" "~") ispell-skip-region-alist)
  (push '("=" "=") ispell-skip-region-alist)
  (push '("cite:[a-zA-Z_-]+") ispell-skip-region-alist)
  (push '("^#\\+BEGIN_SRC" . "^#\\+END_SRC") ispell-skip-region-alist))

(add-hook 'org-mode-hook 'fp/ispell-setup-org)

;; ----------------------------------------------------------------------
;; ticket/bug links
;; ----------------------------------------------------------------------

(defun fp/insert-redmine-org-url (ticketnr)
  (interactive "nTicket Nr: ")
  (insert (format "[[https://redmine.recom.eu/issues/%s][#%s]]" ticketnr ticketnr)))

;; ================================================================================
(provide 'config-org)
;; ================================================================================
