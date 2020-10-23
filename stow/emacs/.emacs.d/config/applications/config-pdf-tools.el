(use-package pdf-tools
  :ensure t
  :demand t
  :config

  (config-add-external-dependency 'epdfinfo 'config-pdf-tools "view and annotate pdfs"
                                  (lambda () (file-exists-p pdf-info-epdfinfo-program))
                                  "apt-get install -y elpa-pdf-tools-server" "None / sync-directory")
  (config-add-external-dependency 'pdftotext 'config-pdf-tools "show text of a pdf"
                                  (lambda () (executable-find "pdftotext"))
                                  "apt-get install -y poppler-utils" "choco install -y xpdf-utils")

  ;; --------------------------------------------------------------------------------
  ;; hook
  ;; --------------------------------------------------------------------------------
  (defun fp/pdf-view-mode-hook ()
    (interactive)
    ;; (face-remap-add-relative 'cursor '(:background "#002b36"))
    ;; (setq-local cursor-type nil)
    (pdf-tools-enable-minor-modes)
    (pdf-view-fit-page-to-window))
  (add-hook 'pdf-view-mode-hook 'fp/pdf-view-mode-hook)


  (defun fp/pdf-view-update-on-theme-change ()
    (interactive)
    (setq pdf-view-midnight-colors (cons (face-attribute 'default :foreground)
                                         (face-attribute 'default :background)))
    (run-function-in-mode-buffers
     'pdf-view-mode (lambda ()
                      (when (or (bound-and-true-p pdf-view-midnight-minor-mode)
                               (eq fp/current-theme 'dark))
                        (pdf-view-midnight-minor-mode 0)
                        (pdf-view-midnight-minor-mode 1)
                        (remove-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode))
                      (when (eq fp/current-theme 'light)
                        (pdf-view-midnight-minor-mode 0))))
    (if (eq fp/current-theme 'dark)
        (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)
      (remove-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)))

  (fp/pdf-view-update-on-theme-change)
  (add-hook 'after-load-theme-hook 'fp/pdf-view-update-on-theme-change)

  ;; --------------------------------------------------------------------------------
  ;; org-mode links
  ;; --------------------------------------------------------------------------------
  ;; Was: org-pdfview. Links can be 'updated' to org-pdftools by changing the
  ;; link from "pdfview:..." to "pdftools:..."
  (use-package org-pdftools :ensure t
    :hook (org-mode . org-pdftools-setup-link))
  ;; --------------------------------------------------------------------------------
  ;; open text in new window
  ;; --------------------------------------------------------------------------------
  (defun fp/pdf-view-open-text (keep-layout)
    "Open the output of the `pdftotext' shell-command in a new buffer.
  If keep-layout is t, then pass the -layout flag to `pdftotext'"
    (interactive)
    (let ((file (buffer-file-name)))
      (switch-to-buffer
       (get-buffer-create (concat file "<text>")))
      (shell-command (concat "pdftotext " (when keep-layout "-layout ") "\"" file "\" -") (current-buffer))))

  (evil-leader/set-key-for-mode 'pdf-view-mode
    "mt" (lambda () (interactive) (fp/pdf-view-open-text t))
    "mT" (lambda () (interactive) (fp/pdf-view-open-text nil)))

  (defun fp/pdf-view-open-text-other-window ())


  ;; --------------------------------------------------------------------------------
  ;; evil collection
  ;; --------------------------------------------------------------------------------


  (require 'pdf-tools nil t)
  (require 'pdf-view nil t)

  (declare-function pdf-view-last-page "pdf-view")
  (declare-function pdf-view-first-page "pdf-view")
  (declare-function pdf-view-goto-page "pdf-view")
  (declare-function pdf-view-previous-line-or-previous-page "pdf-view")
  (declare-function pdf-view-next-line-or-next-page "pdf-view")

  (defvar pdf-view-mode-map)
  (defvar pdf-outline-buffer-mode-map)
  (defvar pdf-occur-buffer-mode-map)

  (defvar pdf-view-mode-map)
  (defvar pdf-outline-buffer-mode-map)
  (defvar pdf-occur-buffer-mode-map)

  ;; TODO: The following 2 functions are workarounds for
  ;; 'pdf-view-next-line-or-next-page' and
  ;; 'pdf-view-previous-line-or-previous-page' not playing well with
  ;; EVIL. The root cause should be found and fixed instead.
  ;; See https://github.com/emacs-evil/evil-collection/pull/137 for
  ;; details.
  (defun evil-collection-pdf-view-next-line-or-next-page (&optional count)
    "'evil' wrapper include a count argument to `pdf-view-next-line-or-next-page'"
    (interactive "P")
    (if count
        (dotimes (_ count nil)
          (pdf-view-next-line-or-next-page 1))
      (pdf-view-next-line-or-next-page 1)))

  (defun evil-collection-pdf-view-previous-line-or-previous-page (&optional count)
    "'evil' wrapper include a count argument to `pdf-view-previous-line-or-previous-page'"
    (interactive "P")
    (if count
        (dotimes (_ count nil)
          (pdf-view-previous-line-or-previous-page 1))
      (pdf-view-previous-line-or-previous-page 1)))

  (defun evil-collection-pdf-view-goto-page (&optional page)
    "`evil' wrapper around `pdf-view-last-page'."
    (interactive "P")
    (if page
        (pdf-view-goto-page page)
      (pdf-view-last-page)
      (image-eob)))

  (defun evil-collection-pdf-view-goto-first-page (&optional page)
    "`evil' wrapper around `pdf-view-first-page'."
    (interactive "P")
    (if page
        (pdf-view-goto-page page)
      (pdf-view-first-page)
      (image-bob)))

  (defun evil-collection-pdf-setup ()
    "Set up `evil' bindings for `pdf-view'."
    (evil-set-initial-state 'pdf-view-mode 'normal)
    (evil-define-key 'normal pdf-view-mode-map
      ;; motion
      (kbd "RET") 'image-next-line
      "j" 'evil-collection-pdf-view-next-line-or-next-page
      "k" 'evil-collection-pdf-view-previous-line-or-previous-page
      (kbd "SPC") 'pdf-view-scroll-up-or-next-page
      (kbd "S-SPC") 'pdf-view-scroll-down-or-previous-page
      (kbd "<delete>") 'pdf-view-scroll-down-or-previous-page
      (kbd "C-j") 'pdf-view-scroll-up-or-next-page
      (kbd "C-k") 'pdf-view-scroll-down-or-previous-page
      "d" 'pdf-view-scroll-up-or-next-page
      "u" 'pdf-view-scroll-down-or-previous-page
      "]" 'pdf-view-next-page-command
      "[" 'pdf-view-previous-page-command
      (kbd "C-f") 'pdf-view-next-page-command
      (kbd "C-b") 'pdf-view-previous-page-command
      "gj" 'pdf-view-next-page-command
      "gk" 'pdf-view-previous-page-command
      (kbd "<next>") 'forward-page
      (kbd "<prior>") 'backward-page
      (kbd "<down>") 'evil-collection-pdf-view-next-line-or-next-page
      (kbd "<up>") 'evil-collection-pdf-view-previous-line-or-previous-page
      "gg" 'evil-collection-pdf-view-goto-first-page
      "G" 'evil-collection-pdf-view-goto-page

      ;; mark
      "'" 'pdf-view-jump-to-register
      "m" 'pdf-view-position-to-register

      ;; zoom
      "+" 'pdf-view-enlarge
      "zi" 'pdf-view-enlarge
      "=" 'pdf-view-enlarge
      "-" 'pdf-view-shrink
      "zo" 'pdf-view-shrink
      "0" 'pdf-view-scale-reset
      "z0" 'pdf-view-scale-reset

      ;; TODO: Why are those image-* bindings in pdf-tools?
      "a+" 'image-increase-speed
      "a-" 'image-decrease-speed
      "a0" 'image-reset-speed
      "ar" 'image-reverse-speed
      "F" 'image-goto-frame
      "b" 'image-previous-frame
      "f" 'image-next-frame
      "h" 'image-backward-hscroll
      "^" 'image-bol
      "$" 'image-eol
      "l" 'image-forward-hscroll

      "H" 'pdf-view-fit-height-to-window ; evil-image has "H"
      "P" 'pdf-view-fit-page-to-window
      "W" 'pdf-view-fit-width-to-window ; evil-image has "W"

      ;; refresh
      "r" 'revert-buffer

      (kbd "<C-down-mouse-1>") 'pdf-view-mouse-extend-region
      (kbd "<M-down-mouse-1>") 'pdf-view-mouse-set-region-rectangle
      (kbd "<down-mouse-1>")  'pdf-view-mouse-set-region

      (kbd "C-c C-c") 'docview-mode
      (kbd "C-c <tab>") 'pdf-view-extract-region-image

      "sb" 'pdf-view-set-slice-from-bounding-box
      "sa" 'pdf-view-auto-slice-minor-mode
      "sm" 'pdf-view-set-slice-using-mouse
      "sr" 'pdf-view-reset-slice

      ;; goto
      "gl" 'pdf-view-goto-label

      ;; search
      (kbd "C-s") 'pdf-occur
      (kbd "M-s o") 'pdf-occur
      (kbd "M-s n") 'pdf-occur-next-error
      (kbd "M-s p") (lambda () (interactive) (pdf-occur-next-error -1))

      "/" 'isearch-forward
      "?" 'isearch-backward
      "n" 'isearch-repeat-forward
      "N" 'isearch-repeat-backward

      "zd" 'pdf-view-dark-minor-mode
      "zm" 'pdf-view-midnight-minor-mode
      "i"  'pdf-view-midnight-minor-mode
      "zp" 'pdf-view-printer-minor-mode

      "o" 'pdf-outline

      ;; quit
      "q" 'quit-window)

    (evil-define-key 'visual pdf-view-mode-map
      "y" 'pdf-view-kill-ring-save)

    (evil-set-initial-state 'pdf-outline-buffer-mode 'normal)
    (evil-define-key 'normal pdf-outline-buffer-mode-map
      ;; open
      (kbd "RET") 'pdf-outline-follow-link-and-quit
      (kbd "S-RET") 'pdf-outline-follow-link
      (kbd "M-RET") 'pdf-outline-display-link
      "go" 'pdf-outline-follow-link
      "." 'pdf-outline-move-to-current-page
      (kbd "SPC") 'pdf-outline-select-pdf-window

      "G" 'pdf-outline-end-of-buffer
      "^" 'pdf-outline-up-heading
      "<" 'pdf-outline-up-heading ; TODO: Don't set this by default?

      "zf" 'pdf-outline-follow-mode ; Helm has "C-c C-f" in Emacs state.

      ;; quit
      (kbd "C-w q") 'pdf-outline-quit-and-kill ; TODO: Do we need to set this? I think not.
      "q" 'quit-window
      "ZQ" 'quit-window
      "ZZ" 'pdf-outline-quit-and-kill)

    (evil-set-initial-state 'pdf-occur-buffer-mode 'normal)
    (evil-define-key 'normal pdf-occur-buffer-mode-map
      ;; open
      (kbd "RET") 'pdf-occur-goto-occurrence
      (kbd "S-RET") 'pdf-occur-view-occurrence
      (kbd "SPC") 'pdf-occur-view-occurrence
      "gd" 'pdf-occur-goto-occurrence
      "gD" 'pdf-occur-view-occurrence

      "A" 'pdf-occur-tablist-gather-documents
      "D" 'pdf-occur-tablist-do-delete

      ;; sort
      "o" 'tabulated-list-sort
      "O" 'tablist-sort ; TODO: Do we need this?

      ;; refresh
      "gR" 'tablist-revert
      "G" 'end-of-buffer

      "K" 'pdf-occur-abort-search

      ;; mark
      "*m" 'tablist-mark-forward
      "m" 'tablist-mark-forward
      "~" 'tablist-toggle-marks
      "u" 'tablist-unmark-forward
      "U" 'tablist-unmark-all-marks
      "*!" 'tablist-unmark-all-marks
      "*c" 'tablist-change-marks
      "*n" 'tablist-mark-items-numeric
      "*r" 'tablist-mark-items-regexp
      "%"  'tablist-mark-items-regexp

      "a" 'tablist-flag-forward

      ;; "f" 'tablist-find-entry ; TODO: Equivalent to 'pdf-occur-goto-occurrence?
      "r" 'pdf-occur-revert-buffer-with-args
      "d" 'tablist-do-kill-lines
      "x" 'pdf-occur-tablist-do-flagged-delete
      (kbd "<delete>") 'tablist-unmark-backward
      (kbd "S-SPC") 'scroll-down-command
      (kbd "<backtab>") 'tablist-backward-column
      (kbd "C-c C-e") 'tablist-export-csv

      [remap evil-first-non-blank] 'tablist-move-to-major-columnj
      [remap evil-next-line] 'tablist-next-line
      [remap evil-previous-line] 'tablist-previous-line

      ;; filter
      ;; TODO: See if overriding "/" is a good idea.
      "/!" 'tablist-negate-filter
      "//" 'tablist-display-filter
      "/=" 'tablist-push-equal-filter
      "/C" 'tablist-clear-filter
      "/D" 'tablist-delete-named-filter
      "/a" 'tablist-push-named-filter
      "/d" 'tablist-deconstruct-named-filter
      "/e" 'tablist-edit-filter
      "/n" 'tablist-push-numeric-filter
      "/p" 'tablist-pop-filter
      "/r" 'tablist-push-regexp-filter
      "/s" 'tablist-name-current-filter
      "/t" 'tablist-toggle-first-filter-logic
      "/z" 'tablist-suspend-filter

      ;; quit
      "q" 'tablist-quit
      "ZQ" 'tablist-quit
      "ZZ" 'tablist-quit))

  (evil-collection-pdf-setup))

(provide 'config-pdf-tools)
