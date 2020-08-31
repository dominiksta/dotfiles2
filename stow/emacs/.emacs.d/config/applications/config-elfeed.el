(require 'seq)

(use-package elfeed :ensure t :demand t)
(use-package elfeed-protocol :ensure t :demand t)

;; (setq elfeed-feeds '(("ttrss+https://admin@rss.icyou.icu" :use-authinfo t)))
;; (setq elfeed-feeds '(("fever+https://admin@rss.icyou.icu/api/fever.php" :use-authinfo t)))

(setq elfeed-protocol-newsblur-maxpages 20)
(setq elfeed-curl-extra-arguments '("-c" "/tmp/newsblur-cookie"
                                    "-b" "/tmp/newsblur-cookie"))
(setq elfeed-feeds '(("newsblur+https://f1p@newsblur.com" :use-authinfo t)))

;; (setq elfeed-feeds '(("owncloud+https://dstahmer@icyou.icu" :use-authinfo t)))

(elfeed-protocol-enable)

;; ----------------------------------------------------------------------
;; toggle display of read articles
;; ----------------------------------------------------------------------

(defun fp/elfeed-toggle-display-read ()
  "Toggle display of read entries."
  (interactive)
  (if (string-match-p "+unread" elfeed-search-filter)
      (let* ((split (split-string elfeed-search-filter "+unread"))
             (new (s-trim-right (apply 'concat split))))
        (elfeed-search-set-filter new))
    (elfeed-search-set-filter (concat elfeed-search-filter " +unread"))))

;; ----------------------------------------------------------------------
;; checking logs for debugging
;; ----------------------------------------------------------------------

(defun fp/elfeed-open-logs ()
  (interactive)
  (delete-other-windows)
  (elfeed)
  (split-window-vertically)
  (other-window 1)
  (switch-to-buffer "*elfeed-log*")
  (split-window-horizontally)
  (switch-to-buffer "*elfeed-mpv*"))

;; ----------------------------------------------------------------------
;; opening links and enclosures with mpv
;; ----------------------------------------------------------------------

(defun fp/elfeed-mpv (url)
  "Play URL with mpv"
  (start-process "elfeed-mpv" (get-buffer-create "*elfeed-mpv*") "mpv"
                 "--quiet" "--no-input-terminal" url))

(defun fp/elfeed-current-entry ()
  "Return current entry in search or show mode"
  (if (eq major-mode 'elfeed-show-mode)
      elfeed-show-entry
    (elfeed-search-selected :single)))

(defun fp/elfeed-play-link-with-mpv (link)
  "Play entry link with mpv."
  (interactive)
  (let* ((quality-arg "")
         (quality-val (completing-read "Max height resolution (0 for unlimited): "
                                       '("0" "480" "720" "1080") nil nil)))
    (setq quality-val (string-to-number quality-val))
    (message "Opening %s with max height %s with mpv..." link quality-val)
    (when (< 0 quality-val)
      (setq quality-arg (format "--ytdl-format=[height<=?%s]" quality-val)))
    (start-process "elfeed-mpv" (get-buffer-create "*elfeed-mpv*") "mpv"
                   quality-arg "--quiet" "--no-input-terminal" link)))

(defun fp/elfeed-play-enclosure-or-link-with-mpv ()
  "Play enclosure or link with mpv. Prefers enclosures, except for youtube-links."
  (interactive)
  (let* ((entry (fp/elfeed-current-entry))
         (enclosure (caar (elfeed-entry-enclosures entry)))
         (link (elfeed-entry-link entry)))
    (if (and enclosure (not (string-match "\\`https://www.youtube.com" link)))
        (progn (message "Opening enclosure %s with mpv..." link)
               (fp/elfeed-mpv enclosure))
      (fp/elfeed-play-link-with-mpv link))))


;; ----------------------------------------------------------------------
;; binds
;; ----------------------------------------------------------------------

(evil-define-key 'normal elfeed-search-mode-map
  (kbd "RET") 'elfeed-search-show-entry
  "r" 'elfeed-update
  "O" 'elfeed-search-tag-all-unread
  "o" 'elfeed-search-untag-all-unread
  "s" 'elfeed-search-live-filter
  "y" 'elfeed-search-yank
  "p" 'fp/elfeed-play-enclosure-or-link-with-mpv
  "b" 'elfeed-search-browse-url
  "d" 'fp/elfeed-toggle-display-read
  "L" 'fp/elfeed-open-logs
  "q" 'elfeed-search-quit-window)

(evil-define-key 'normal elfeed-show-mode-map
  "y"  'elfeed-show-yank
  "u"  'elfeed-show-tag--unread
  "J"  'elfeed-show-next
  "K"  'elfeed-show-prev
  "gh" 'elfeed-show-visit
  "p" 'fp/elfeed-play-enclosure-or-link-with-mpv
  "b" 'elfeed-search-browse-url
  "q"  'elfeed-kill-buffer)


(provide 'config-elfeed)