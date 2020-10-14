(require-and-log 'config-window-management)
(require 'eyebrowse)
(require 'gnus-group)
(require 'ol-gnus) ;; required for `org-store-link`

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

;; --- Always start gnus on eyebrowse workspace 9 ---
(setcdr (assq 'gnus org-link-frame-setup) 'gnus)

(defun fp/gnus-wm-advice (orig-fun &rest args)
  (eyebrowse-switch-to-window-config-9)
  (apply orig-fun args))

(advice-add 'gnus :around 'fp/gnus-wm-advice)

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
;; archiving
;; ----------------------------------------------------------------------

;; Gnus archives messages in several ways: First, it automatically archives
;; every e-mail you sent into `gnus-message-archive-group'. Second, it will
;; archive everything marked as "expired" to `nnmail-expiry-target' if it is
;; older than `nnmail-expiry-wait' days. In order to have every mail you read
;; marked as "expired" by default, I set (auto-expire . t) for the relevant
;; accounts in `gnus-parameters'.

;; I use nnmaildir as a backend for archiving. This is because I keep that
;; maildir on my nextcloud instance as a "backup" solution. In order to not get
;; sync-conflicts between computers though, it helps to have every mail as a
;; separate file. If you are concerned about inode usage (which you probably
;; aren't, check with \"df -h\"), you may want to the default nnfolder
;; instead. Additionally, you will need to create the folder to the maildir
;; yourself and add an entry like this to your `gnus-secondary-select-methods':
;;
;; (nnmaildir "archive"
;;            (directory "~/Documents/Mail/archive")
;;            (get-new-mail nil))

(setq gnus-message-archive-group (format-time-string "nnmaildir+archive:unsorted.sent.%Y")

      ;; wait for N days before expiring articles
      nnmail-expiry-wait         365

      ;; This is just a (imo) sensible default. I change this variable on a
      ;; per-account basis using `gnus-parameters'.
      nnmail-expiry-target (lambda (groupname) (concat "nnmaildir+archive:unsorted.expired."
                                                  (fp/expiry-mail-date))))

(defun fp/expiry-mail-date ()
  "If run in buffer containing valid email-headers, this will
return the year in the date header of that mail as a string. If
that fails, it will return the current year. Useful to use for a
`nnmail-expiry-target'"
  (condition-case nil
      (save-excursion
        (goto-char (point-min))
        (format-time-string "%Y" (mail-header-parse-date
                                  (mail-header 'date (mail-header-extract)))))
    (error (format-time-string "%Y" (current-time)))))


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

(setq gnus-summary-next-group-on-exit nil) ; Don't switch to the next group on
                                        ; summary exit

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
       "%R%U%z"            ; cached, read, score
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

(setq gnus-sum-thread-tree-false-root "─┬> "
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-leaf-with-other "├─> "
      gnus-sum-thread-tree-root ""
      gnus-sum-thread-tree-single-leaf "└─> "
      gnus-sum-thread-tree-vertical "│")

;; Disable threads by default (I don't have a lot of long conversations, so it
;; is more useful to just have a stream of the most recent messages.)
(setq gnus-show-threads nil)

;; --- starting position ---
(setq gnus-summary-goto-unread nil)
(add-hook 'gnus-summary-prepared-hook (lambda () (end-of-buffer) (previous-line)))

;; --- viewing threads/referring articles ---
(setq gnus-refer-thread-use-nnir t  ; search in all groups
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
      )
;; NOTE: I configure `gnus-refer-article-method' in `gnus-group-parameters' to
;; be something like '(current (nnir "nnimap:<the_group>")).

;; --- scoring ---
(defun fp/gnus-score-file (group)
  (concat "~/sync/emacs/mail/" group ".SCORE"))
(setq gnus-home-score-file 'fp/gnus-score-file)

;; --- open feed2imap url ---
(defun fp/gnus-open-feed2imap-url ()
  "Open the URL of a feed2imap article from the summary buffer."
  (interactive)
  (save-selected-window
    (save-excursion
      (gnus-summary-goto-article (gnus-summary-article-number))
      (gnus-summary-select-article-buffer)
      (goto-char (point-max))
      (search-backward-regexp "^<https://.+>$")
      (forward-char)
      (browse-url-at-point))))

(defun fp/gnus-open-feed2imap-url-in-mpv ()
  "Open the URL of a feed2imap article from the summary buffer."
  (interactive)
  (save-selected-window
    (save-excursion
      (gnus-summary-goto-article (gnus-summary-article-number))
      (gnus-summary-select-article-buffer)
      (goto-char (point-max))
      (search-backward-regexp "^<https://.+>$")
      (forward-char)
      (let ((command (format "mpv %s" (thing-at-point-url-at-point))))
        (message "starting %s" command)
        (start-process-shell-command "" nil command)))))

;; --- other ---
(setq gnus-auto-select-first nil)
(setq gnus-summary-mode-line-format "%p [current: %A, unread: %Z]")
(add-hook 'gnus-summary-mode-hook 'hl-line-mode)
(add-hook 'gnus-summary-prepared-hook 'gnus-summary-sort-by-date)

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
;; attachments
;; ----------------------------------------------------------------------

;; This mostly just allows you to attach files from dired to a message by
(turn-on-gnus-dired-mode)

;; --- this is from "https://github.com/redguardtoo/mastering-emacs-in-one-
;; year-guide/blob/master/gnus-guide-en.org#double-check-content-of-mail-before-
;; sending-it" ---

(defun fp/message-says-attachment-p ()
  "Return t if the message suggests there can be an attachment."
  (string-match "\\(attach\\|anhang\\|pdf\\|file\\|screen ?shot\\)"
                (buffer-substring-no-properties (point-min) (point-max))))

(defun fp/message-has-attachment-p ()
  "Return t if an attachment is already attached to the message."
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (re-search-forward "<#part" nil t))))

(defun fp/message-pre-send-check-attachment ()
  "Check attachment before send mail."
  (when (and (fp/message-says-attachment-p)
           (not (fp/message-has-attachment-p)))
    (unless
        (y-or-n-p "Attachment suggested, but not found. Send anyway?")
      (error "It seems that an attachment is needed, but none was found. Aborting sending."))))

(add-hook 'message-send-hook 'fp/message-pre-send-check-attachment)

;; ----------------------------------------------------------------------
;; sending mail
;; ----------------------------------------------------------------------

;; This determines how gnus formats the original message with
;; `gnus-summary-reply-with-original' and friends.
(setq message-cite-style
      '((message-cite-function 'message-cite-original)
        (message-citation-line-function 'message-insert-formatted-citation-line)
        (message-cite-reply-position 'above)
        (message-yank-prefix "> ")
        (message-yank-cited-prefix ">")
        (message-yank-empty-prefix ">")
        (message-citation-line-format "Am %d.%m.%Y, %R Uhr schrieb %N:")))

;; The simplest (imo) way to do message templating is to just compose something
;; in message mode, save that as a file and then as a bookmark. So i just invent
;; the ".message" extension to open up in message mode for this purpose.
(add-to-list 'auto-mode-alist (cons "\\.message\\'" 'message-mode))

;; ----------------------------------------------------------------------
;; viewing mail
;; ----------------------------------------------------------------------

;; Colors in rendered html look ugly and can be distracting; So i turn them
;; off. If you want to see the content as it was intended, use
;; `gnus-article-browse-html-article' to open it in your default browser.
(setq shr-use-colors nil)
(add-hook 'gnus-article-mode-hook 'visual-line-mode)
(add-hook 'gnus-article-mode-hook 'olivetti-mode)

;; ----------------------------------------------------------------------
;; searching
;; ----------------------------------------------------------------------

;; TODO set up search for local maildirs
;; (setq nnir-method-default-engines
;;       '((nnimap . imap)
;;         (nndraft . find-grep)
;;         (nnfolder . find-grep)
;;         (nnmaildir . find-grep)))

;; ----------------------------------------------------------------------
;; encryption
;; ----------------------------------------------------------------------

;; You can define aliases in your gpg.conf (normally in ~/.gnupg/gpg.conf) with
;; a line like this:
;; group newalias@domain.com = emailwithkey@domain.com

;; ----------------------------------------------------------------------
;; contacts
;; ----------------------------------------------------------------------

;; You have multiple options to handle insertion of contacts when composing
;; mail. One of them is `eudc' - which is a unified interface to bbdb and
;; ldap. If your workplace/uni/whatever does not have a public ldap server
;; though (like my workplace/uni) then this is not of much use to you. Instead,
;; the package `vdirel' can be combined with the external program 'vdirsyncer'
;; to synchronize with a carddav server. TODO

;; ----------------------------------------------------------------------
;; modeline
;; ----------------------------------------------------------------------
;; TODO https://github.com/seagle0128/doom-modeline/blob/15c859dc4b4d6e6b7bafe4bdacf447de6a6253dd/doom-modeline-segments.el#L2213-L2287

;; ======================================================================
;; evil binds
;; ======================================================================

;; These binds are often not a direct translation of the originals.

;; ----------------------------------------------------------------------
;; summary
;; ----------------------------------------------------------------------
(evil-set-initial-state 'gnus-summary-mode 'normal)
(evil-define-key '(visual normal) gnus-summary-mode-map
  ;; session
  "q" 'gnus-summary-exit
  "Q" 'gnus-summary-exit-no-update
  "r" 'gnus-summary-insert-new-articles
  "R" 'gnus-summary-rescan-group

  ;; writing mail
  (kbd "M-r") 'gnus-summary-wide-reply
  (kbd "M-R") 'gnus-summary-wide-reply-with-original
  (kbd "M-f") 'gnus-summary-mail-forward
  (kbd "M-m") 'gnus-summary-mail-other-window

  ;; movement
  "J" 'gnus-summary-next-article
  "K" 'gnus-summary-prev-article
  (kbd "RET") 'gnus-summary-scroll-up
  "ga" 'gnus-summary-goto-article
  "gx" 'fp/gnus-open-feed2imap-url
  "gm" 'fp/gnus-open-feed2imap-url-in-mpv
  "zz" 'gnus-recenter

  "Se" 'gnus-score-edit-current-scores
  "Sl" 'gnus-summary-lower-score
  "Si" 'gnus-summary-increase-score

  ;; marking and executing
  "m" 'gnus-summary-mark-as-processable
  "u" 'gnus-summary-unmark-as-processable
  "U" 'gnus-summary-unmark-all-processable
  "x" 'gnus-summary-universal-argument

  "o" 'gnus-summary-put-mark-as-read-next
  "O" 'gnus-summary-put-mark-as-unread-next
  "e" 'gnus-summary-put-mark-as-expirable
  (kbd "<delete>") 'gnus-summary-delete-article

  "p" nil
  "ph" 'gnus-article-browse-html-article
  "ps" 'gnus-article-save-part
  "pv" 'gnus-article-view-part
  "pe" 'gnus-article-view-part-externally
  "pR" 'gnus-summary-show-raw-article

  "b" 'gnus-summary-move-article
  "c" 'gnus-summary-copy-article

  "zt" 'gnus-summary-toggle-header

  ;; threads
  "tt" 'gnus-summary-toggle-threads
  "tf" 'gnus-summary-refer-thread
  "tp" 'gnus-summary-refer-parent-article

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
(evil-define-key '(visual normal) gnus-article-mode-map
  (kbd "M-r") 'gnus-summary-reply
  (kbd "M-R") 'gnus-summary-reply-with-original

  "K" 'gnus-summary-prev-article
  "J" 'gnus-summary-next-article

  "p" nil
  "ph" 'gnus-article-browse-html-article
  "ps" 'gnus-article-save-part
  ;; Copy in this case means to "copy" into a new buffer. This opens pdfs in
  ;; pdf-tools.
  "pe" 'gnus-article-view-part
  "pc" 'gnus-article-copy-part
  "pR" 'gnus-summary-show-raw-article

  "i" 'gnus-article-show-images
  "I" 'gnus-article-remove-images

  "q" 'evil-window-delete)

(evil-define-key '(visual normal) gnus-mime-button-map
  "e" 'gnus-article-view-part
  "c" 'gnus-article-copy-part)

;; ----------------------------------------------------------------------
;; group
;; ----------------------------------------------------------------------
(evil-set-initial-state 'gnus-group-mode 'normal)
(evil-define-key '(visual normal) gnus-group-mode-map
  "q" (lambda () (interactive) (gnus-save-newsrc-file) (quit-window))

  "x" 'gnus-group-kill-group
  "p" 'gnus-group-yank-group

  "r" 'gnus-group-get-new-news
  "R" (lambda () (interactive) (gnus-group-get-new-news '(4)))

  "s" 'gnus-group-make-nnir-group
  "S" 'gnus-group-save-newsrc

  "f" 'gnus-group-jump-to-group

  "I" 'gnus-group-list-all-groups
  "i" 'gnus-group-list-groups

  "L" 'gnus-group-set-current-level

  "T" 'gnus-group-topic-map

  "c" 'gnus-topic-catchup-articles

  "gs" 'gnus-group-enter-server-mode
  "gj" 'gnus-topic-goto-next-topic
  "gk" 'gnus-topic-goto-previous-topic

  "o" 'gnus-group-select-group
  (kbd "RET") (lambda () (interactive) (gnus-group-select-group 100))

  "M-m" 'gnus-group-mail)

;; ----------------------------------------------------------------------
;; server
;; ----------------------------------------------------------------------
(evil-set-initial-state 'gnus-server-mode 'normal)
(evil-define-key '(visual normal) gnus-server-mode-map
  "q"         'gnus-server-exit
  (kbd "RET") 'gnus-server-read-server

  ;; online status
  "C"         'gnus-server-close-server
  "D"         'gnus-server-deny-server
  "L"         'gnus-server-offline-server
  "O"         'gnus-server-open-server

  "Aa"        'gnus-agent-add-server
  "Ar"        'gnus-agent-remove-server

  "r"         'gnus-server-regenerate-server
  "x"         'gnus-server-kill-server
  "s"         'gnus-server-scan-server
  "p"         'gnus-server-yank-server)

;; ----------------------------------------------------------------------
;; browse servers
;; ----------------------------------------------------------------------
(evil-set-initial-state 'gnus-browse-mode 'normal)
(evil-define-key '(visual normal) gnus-browse-mode-map
  "q"         'gnus-browse-exit
  "u"         'gnus-browse-unsubscribe-current-group
  (kbd "RET") 'gnus-browse-read-group)


(provide 'config-gnus)
