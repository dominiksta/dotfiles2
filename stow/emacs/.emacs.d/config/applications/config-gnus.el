(require 'gnus-group)

;; ----------------------------------------------------------------------
;; accounts
;; ----------------------------------------------------------------------

;; For obvious privacy reasons, I do not want to have my e-mail accounts
;; specified in my public dotfiles.
(if (file-exists-p "~/sync/emacs/mail/gnus-accounts.el")
    (load "~/sync/emacs/mail/gnus-accounts.el")
  (warn (concat "No gnus-accounts file found in ~/sync/emacs/mail/gnus-accounts.el. "
                "Accounts will not be available")))

;; ----------------------------------------------------------------------
;; general behaviour
;; ----------------------------------------------------------------------

(setq
 ;; I don't like my $HOME getting messed up more than it needs to be.
 gnus-directory "~/.emacs.d/News"
 gnus-startup-file "~/.emacs.d/newsrc"
 ;; The gcc header can be used to put e-mail into other folders upon being
 ;; sent. Obviously I don't want e-mails in "Sent" to be unread by default.
 gnus-gcc-mark-as-read t
 ;; I'm not 100% sure what this does. But apparently, it enables gnus to
 ;; fetch articles in advance some of the time.
 gnus-asynchronous t)

;; Don't save newsrc; I don't care about compatibility with other news readers
(setq gnus-read-newsrc-file nil
      gnus-save-newsrc-file nil)

;; ----------------------------------------------------------------------
;; caching
;; ----------------------------------------------------------------------

;; Gnus has two different ways of doing caching. The "regular" cache, enabled by
;; `gnus-use-cache' is mostly intended for manual use. So if you want to
;; explicitly save an article to disk, you would use that. This cache stores
;; things based on marks. What marks enter the cache is determined by
;; `gnus-cache-enter-articles' and what exits the cache by
;; `gnus-cache-remove-articles'. So, by default you can use
;; `gnus-summary-tick-article-forward' to manually cache something.
(setq gnus-use-cache t)

;; The gnus "agent" is the other, more modern and automated way of doing
;; caching. It is supposed to run mostly invisibly without user interaction.
(setq gnus-agent t)

;; To actually make it work as expected and enter all articles you fetch
;; automatically to it's cache, you use this incantation:
(add-hook 'gnus-select-article-hook 'gnus-agent-fetch-selected-article)

;; ----------------------------------------------------------------------
;; group buffer
;; ----------------------------------------------------------------------

;; To categorize groups, we can use "topics". All topic actions in the group
;; buffer start with the prefix "T". Personally, I use topics create a
;; traditional e-mail client interface where I have the folders of my different
;; e-mail accounts grouped together by account. The alignment of these groups
;; gets saved to your newsrc.
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; I like to sort by rank first, alphabet second. You can hit C-c C-s to apply
;; this sorting.
(setq gnus-group-sort-function
      '(gnus-group-sort-by-unread
        gnus-group-sort-by-alphabet
        gnus-group-sort-by-rank))

;; I prefer gnus to keep running in the background, so i just tell it so save my
;; newsrc and hide the buffer instead of exiting.
(define-key gnus-group-mode-map (kbd "q")
  (lambda () (interactive) (gnus-save-newsrc-file) (quit-window)))

;; Visuals
(add-hook 'gnus-group-mode-hook 'hl-line-mode)

;; ----------------------------------------------------------------------
;; summary buffer
;; ----------------------------------------------------------------------

;; --- formatting of entries ---
(setq gnus-summary-line-format
      (concat
       "%R%U%uR%z"         ; cached, read, `gnus-user-format-function-R' , score
       "%-16,16"           ; next column
       "&user-date;"       ; date in the format of `gnus-user-date-format-alist'
       "  "                ; next column
       "%4L:%-30,30f"      ; "length in lines":"'from' or 'to' header"
       "  "                ; next column
       "%B"                ; tree style threads. see `gnus-sum-thread-tree-*'
       "%S"                ; subject
       "\n"                ; end
       ))

(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "Today, %H:%M")
        ((+ 86400 (gnus-seconds-today)) . "Yesterday, %H:%M")
        (t . "%Y-%m-%d %H:%M")))

;; 32 the char for a space
(setq gnus-replied-mark 32
      gnus-forwarded-mark 32)

(setq gnus-sum-thread-tree-false-root "─┬➤ "
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-leaf-with-other "├─➤ "
      gnus-sum-thread-tree-root ""
      gnus-sum-thread-tree-single-leaf "└─➤ "
      gnus-sum-thread-tree-vertical "│")

;; --- starting position ---
(setq gnus-summary-goto-unread nil)
(add-hook 'gnus-summary-prepared-hook (lambda () (end-of-buffer) (previous-line)))

;; --- other ---
(setq gnus-auto-select-first nil)
(setq gnus-summary-mode-line-format "%p [current: %A, unread: %Z]")
(add-hook 'gnus-summary-mode-hook 'hl-line-mode)

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

;; This mostly just allows you to attach files from dired to a message by
(turn-on-gnus-dired-mode)

;; ----------------------------------------------------------------------
;; sending mail
;; ----------------------------------------------------------------------

(setq message-cite-style
      '((message-cite-function 'message-cite-original)
        (message-citation-line-function 'message-insert-formatted-citation-line)
        (message-cite-reply-position 'above)
        (message-yank-prefix "> ")
        (message-yank-cited-prefix ">")
        (message-yank-empty-prefix ">")
        (message-citation-line-format "On %D %R %p, %N wrote:")))

(add-to-list 'auto-mode-alist (cons "\\.message\\'" 'message-mode))


;; ----------------------------------------------------------------------
;; evil binds
;; ----------------------------------------------------------------------


;; ----------------------------------------------------------------------
;; summary
;; ----------------------------------------------------------------------
(evil-set-initial-state 'gnus-summary-mode 'normal)
(evil-define-key 'normal gnus-summary-mode-map
  ;; session
  "q" 'gnus-summary-exit
  "Q" 'gnus-summary-exit-no-update
  "r" 'gnus-summary-rescan-group

  ;; writing mail
  (kbd "M-r") 'gnus-summary-reply
  (kbd "M-R") 'gnus-summary-reply-with-original
  (kbd "M-f") 'gnus-summary-mail-forward
  (kbd "M-m") 'gnus-summary-mail-other-window

  ;; movement
  "J" 'gnus-summary-next-article
  "K" 'gnus-summary-prev-article
  (kbd "RET") 'gnus-summary-scroll-up
  "ga" 'gnus-summary-goto-article
  "zz" 'gnus-recenter

  ;; marking and executing
  "m" 'gnus-summary-mark-as-processable
  "u" 'gnus-summary-unmark-as-processable
  "U" 'gnus-summary-unmark-all-processable
  "x" 'gnus-summary-universal-argument

  "o" 'gnus-summary-put-mark-as-read-next
  "O" 'gnus-summary-put-mark-as-unread-next

  "b" 'gnus-summary-move-article

  "zt" 'gnus-summary-toggle-header

  ;; threads
  "tt" 'gnus-summary-toggle-threads
  "tf" 'gnus-summary-refer-thread

  ;; sorting
  "sa" 'gnus-summary-sort-by-author
  "sc" 'gnus-summary-sort-by-chars
  "sd" 'gnus-summary-sort-by-date
  "si" 'gnus-summary-sort-by-score
  "sl" 'gnus-summary-sort-by-lines
  "smd" 'gnus-summary-sort-by-most-recent-date
  "smm" 'gnus-summary-sort-by-marks
  "smn" 'gnus-summary-sort-by-most-recent-number
  "sn" 'gnus-summary-sort-by-number
  "so" 'gnus-summary-sort-by-original
  "sr" 'gnus-summary-sort-by-random
  "ss" 'gnus-summary-sort-by-subject
  "st" 'gnus-summary-sort-by-recipient)

;; ----------------------------------------------------------------------
;; article
;; ----------------------------------------------------------------------
(evil-set-initial-state 'gnus-article-mode 'normal)
(evil-define-key 'normal gnus-article-mode-map
  (kbd "M-r") 'gnus-summary-reply
  (kbd "M-R") 'gnus-summary-reply-with-original
  "q" 'evil-window-delete)

;; ----------------------------------------------------------------------
;; group
;; ----------------------------------------------------------------------
(evil-set-initial-state 'gnus-group-mode 'normal)
(evil-define-key 'normal gnus-group-mode-map
  "q" (lambda () (interactive) (gnus-save-newsrc-file) (quit-window))

  "x" 'gnus-group-kill-group
  "p" 'gnus-group-yank-group

  "r" 'gnus-group-get-new-news
  "R" (lambda () (interactive) (gnus-group-get-new-news '(4)))

  "T" 'gnus-group-topic-map

  "gs" 'gnus-group-enter-server-mode

  (kbd "RET") 'gnus-group-select-group

  "M-m" 'gnus-group-mail)

;; ----------------------------------------------------------------------
;; server
;; ----------------------------------------------------------------------
(evil-set-initial-state 'gnus-server-mode 'normal)
(evil-define-key 'normal gnus-server-mode-map
  "q"         'gnus-server-exit
  (kbd "RET") 'gnus-server-read-server

  ;; online status
  "C"         'gnus-server-close-server
  "D"         'gnus-server-deny-server
  "L"         'gnus-server-offline-server
  "O"         'gnus-server-open-server

  "r"         'gnus-server-regenerate-server
  "x"         'gnus-server-kill-server
  "s"         'gnus-server-scan-server
  "p"         'gnus-server-yank-server
  )

;; ----------------------------------------------------------------------
;; browse servers
;; ----------------------------------------------------------------------
(evil-set-initial-state 'gnus-browse-mode 'normal)
(evil-define-key 'normal gnus-browse-mode-map
  "q"         'gnus-browse-exit
  "u"         'gnus-browse-unsubscribe-current-group
  (kbd "RET") 'gnus-browse-read-group)


(provide 'config-gnus)