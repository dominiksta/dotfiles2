(require 'gnus-group)

;; ----------------------------------------------------------------------
;; accounts
;; ----------------------------------------------------------------------

(if (file-exists-p "~/sync/emacs/mail/gnus-accounts.el")
    (load "~/sync/emacs/mail/gnus-accounts.el")
  (warn (concat "No gnus-accounts file found in ~/sync/emacs/mail/gnus-accounts.el. "
                "Accounts will not be available")))

;; ----------------------------------------------------------------------
;; general behaviour
;; ----------------------------------------------------------------------

(setq gnus-gcc-mark-as-read t
      gnus-asynchronous t
      gnus-agent t)

;; Don't save newsrc; I don't care about compatibility with other news readers
(setq gnus-read-newsrc-file nil
      gnus-save-newsrc-file nil)

;; Move gnus startup file out of $HOME
(setq gnus-startup-file "~/.emacs.d/newsrc")

;; ----------------------------------------------------------------------
;; caching
;; ----------------------------------------------------------------------

;; Enable caching (reading offline). Use `gnus-summary-tick-article-forward' in
;; a summary buffer to save specific articles. You can set
;; `gnus-cacheable-groups' to a regexp to limit what groups can be cached.
(setq gnus-use-cache t
      gnus-cache-enter-articles '(read unread ticked dormant)
      gnus-cache-remove-articles nil)

;; ----------------------------------------------------------------------
;; group buffer
;; ----------------------------------------------------------------------

(add-hook 'gnus-group-mode-hook 'hl-line-mode)
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Hit C-c C-s to apply this sorting
(setq gnus-group-sort-function
      '(gnus-group-sort-by-unread
        gnus-group-sort-by-alphabet
        gnus-group-sort-by-rank))

(define-key gnus-group-mode-map (kbd "q")
  (lambda () (interactive) (gnus-save-newsrc-file) (quit-window)))

;; ----------------------------------------------------------------------
;; summary buffer
;; ----------------------------------------------------------------------

(setq gnus-auto-select-first nil)
(setq gnus-summary-goto-unread nil)
;; (setq gnus-thread-sort-functions
;;       '((not gnus-thread-sort-by-date)
;;         (not gnus-thread-sort-by-number)))
;; (setq gnus-subthread-sort-functions
;;       'gnus-thread-sort-by-date)
;; (setq gnus-thread-hide-subtree nil)
;; (setq gnus-thread-ignore-subject nil)

(setq gnus-summary-mode-line-format "%p [current: %A, unread: %Z]")

;; --- formatting of entries ---
(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "Today, %H:%M")
        ((+ 86400 (gnus-seconds-today)) . "Yesterday, %H:%M")
        (t . "%Y-%m-%d %H:%M")))

(setq gnus-summary-line-format
      (concat
       "%U%R%z"            ; read, replied to, score
       "%-16,16"           ; next column
       "&user-date;"       ; date in the format of `gnus-user-date-format-alist'
       "  "                ; next column
       "%4L:%-30,30f"      ; "length in lines":"'from' or 'to' header"
       "  "                ; next column
       "%B"                ; tree style threads. see `gnus-sum-thread-tree-*'
       "%S"                ; subject
       "\n"                ; end
       ))

(setq gnus-sum-thread-tree-false-root "─┬➤ ")
(setq gnus-sum-thread-tree-indent " ")
(setq gnus-sum-thread-tree-leaf-with-other "├─➤ ")
(setq gnus-sum-thread-tree-root "")
(setq gnus-sum-thread-tree-single-leaf "└─➤ ")
(setq gnus-sum-thread-tree-vertical "│")

(add-hook 'gnus-summary-mode-hook 'hl-line-mode)

(add-hook 'gnus-summary-prepared-hook (lambda () (end-of-buffer) (previous-line)))

;; ----------------------------------------------------------------------
;; message buffer
;; ----------------------------------------------------------------------

;; --- use html mail only if necessary
(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

;; Disables display of images by default.  Call `gnus-article-show-images' when
;; needed
(setq gnus-inhibit-images t)

;; ----------------------------------------------------------------------
;; dired interaction
;; ----------------------------------------------------------------------

(turn-on-gnus-dired-mode)

;; ----------------------------------------------------------------------
;; sending mail
;; ----------------------------------------------------------------------

(add-to-list 'auto-mode-alist (cons "\\.message\\'" 'message-mode))


(provide 'config-gnus)
