(require-and-log 'config-helpers)
(require-and-log 'config-ui) ; olivetti for the reader mode
(require-and-log 'config-editor)

(use-package elfeed
  :ensure t
  :demand t
  :commands elfeed elfeed-load-db-and-open
  :config

  (setq elfeed-db-directory (concat sync-directory "emacs/elfeed-db"))

  (use-package elfeed-org :ensure t :config
    (elfeed-org)
    (setq rmh-elfeed-org-files (list (concat sync-directory "emacs/random/myfeed.org"))))

  ;; --------------------------------------------------------------------------------
  ;; syncing the database
  ;; --------------------------------------------------------------------------------

  (defun elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))

  (defun elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer"
    (interactive)
    (elfeed-db-save)
    (kill-buffer (current-buffer)))

  ;; (add-hook 'elfeed-show-mode-hook 'olivetti-mode)
  (evil-leader/set-key-for-mode 'elfeed-search-mode "x" 'elfeed-save-db-and-bury)


  ;; --------------------------------------------------------------------------------
  ;; watching videos
  ;; --------------------------------------------------------------------------------
  (defvar fp/default-video-player "mpv"
    "If this is set to mpv, then the user will be prompted with a quality selection.")

  (defun fp/elfeed-play-with-mpv ()
    "Play either entry link under point or link of currently shown entry 
with `fp-default-video-player'."
    (interactive)
    (let ((entry (if (eq major-mode 'elfeed-show-mode)
                     elfeed-show-entry (elfeed-search-selected :single)))
          (quality-arg "")
          (quality-val (if (string= fp/default-video-player "mpv")
                           (string-to-number (completing-read
                                              "Max height resolution (0 for unlimited): "
                                              '("0" "480" "720") nil nil))
                         nil)))

      (if (string= fp/default-video-player "mpv")
          (progn
            (message "Opening %s with quality %s with %s..."
                     (elfeed-entry-link entry) quality-val fp/default-video-player)
            (when (< 0 quality-val)
              (setq quality-arg (format "--ytdl-format=[height<=?%s]" quality-val))))
        (message "Opening %s with %s..." (elfeed-entry-link entry) fp/default-video-player))

      (start-process "elfeed-mpv" nil fp/default-video-player
                     (if (string= fp/default-video-player "mpv") quality-arg "")
                     (elfeed-entry-link entry))))


  ;; --------------------------------------------------------------------------------
  ;; bindings
  ;; --------------------------------------------------------------------------------
  (evil-define-key 'normal elfeed-search-mode-map
    "ov" 'fp/elfeed-play-with-mpv
    "ob" 'elfeed-search-browse-url
    "q" 'elfeed-save-db-and-bury
    (kbd "RET") 'elfeed-search-show-entry
    "a" 'elfeed-search-show-entry
    "R" 'elfeed-search-fetch
    "r" 'elfeed-search-update--force
    "s" (defhydra elfeed-quick-search ()
          "searching"
          ("e" (elfeed-search-set-filter "@6-months-ago +emacs") "emacs")
          ("n" (elfeed-search-set-filter "@6-months-ago +news") "news")
          ("u" (elfeed-search-set-filter "@6-months-ago +unread") "unread")
          ("a" (elfeed-search-set-filter "@6-months-ago") "all")
          ("s" elfeed-search-set-filter "manual")
          ("q" nil "quit" :color blue)))

  (evil-define-key 'normal elfeed-show-mode-map
    "ov" 'fp/elfeed-play-with-mpv
    "ob" 'elfeed-search-browse-url
    "q" 'kill-this-buffer
    "J" 'elfeed-show-next
    "K" 'elfeed-show-prev))

(provide 'config-elfeed)
