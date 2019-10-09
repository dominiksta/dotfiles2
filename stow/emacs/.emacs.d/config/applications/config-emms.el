(require-and-log 'config-editor)
(require 'json)

(config-add-external-dependency 'mpv 'config-emms "playing music"
                                (lambda () (executable-find "mpv"))
                                "apt install mpv" "choco install mpv")
(config-add-external-dependency 'mediainfo 'config-emms "reading music-file tags"
                                (lambda () (executable-find "mediainfo"))
                                "apt install mediainfo" "choco install mediainfo-cli")
(config-add-external-dependency 'youtube-dl 'config-emms "emms-integration with youtube"
                                (lambda () (executable-find "youtube-dl"))
                                "pip3 install youtube-dl" "pip3 install youtube-dl")
(config-add-external-dependency 'socat 'config-emms "emms-player-simple-ipc-mpv needs this"
                                (lambda () (and (not (eq system-type 'windows-nt))
                                         (executable-find "socat")))
                                "apt install socat" "None")

(use-package emms
  :ensure t
  :demand t
  :commands (emms emms-browser)
  :init
  :config

  ;; --------------------------------------------------------------------------------
  ;; basics
  ;; --------------------------------------------------------------------------------
  (require 'emms-setup)
  (require 'emms-cache)
  ;; (require 'emms-player-mpv)
  (require 'emms-player-simple-ipc-mpv)
  (require 'emms-mark)
  (use-package emms-info-mediainfo :ensure t)
  (emms-all)
  (emms-cache-disable)

  (emms-player-set 'emms-player-simple-ipc-mpv 'regex
                   (emms-player-simple-regexp
                    "ogg" "opus" "mp3" "wav" "mpg" "mpeg" "wmv" "wma"
                    "mov" "avi" "divx" "ogm" "ogv" "asf" "mkv" "rm" "rmvb"
                    "mp4" "flac" "vob" "m4a" "ape" "flv" "webm" "aif"))

  (setq emms-seek-seconds 5
        emms-player-list '(emms-player-simple-ipc-mpv)
        emms-source-file-default-directory (expand-file-name "~/Music/")
        emms-info-functions '(emms-info-mediainfo)
        emms-history-file-coding-system 'utf-8
        emms-playing-time-display-p nil)

  (emms-history-load)

  (put 'emms-browser-delete-files 'disabled nil)
  ;; --------------------------------------------------------------------------------
  ;; youtube-integration
  ;; --------------------------------------------------------------------------------
  ;; --- configure simple-ipc-mpv for youtube ---
  (emms-player-set 'emms-player-simple-ipc-mpv 'regex
                   (concat (emms-player-get 'emms-player-simple-ipc-mpv 'regex)
                           "\\|www.youtube.com\\|youtu.be"))


  ;; --- get youtube-playlists as lists ---
  ;; (defun fp/youtube-playlist-from-url (url)
  ;;   (let ((result '())
  ;;         (ytpl (shell-command-to-string
  ;;                (concat "youtube-dl -j --flat-playlist \"" url "\""))))
  ;;     (dolist (rawstring (split-string (replace-regexp-in-string "}" "}\t\n" ytpl) "\t\n"))
  ;;       (when (not (string= rawstring "\n"))
  ;;         (add-to-list 'result (cons
  ;;                               (concat
  ;;                                "https://youtu.be/"
  ;;                                (cdr (assoc 'id (json-read-from-string rawstring))))
  ;;                               (cdr (assoc 'title (json-read-from-string rawstring)))
  ;;                               ))))
  ;;     (reverse result)))

  (defun fp/youtube-playlist-from-url (url)
    (let* ((comma-seperated
            (mapconcat 'identity (split-string
                                  (shell-command-to-string
                                   (concat "youtube-dl -j --flat-playlist \""
                                           url "\"")) "\n") ", "))
           (without-comma (concat "[" (substring comma-seperated 0
                                                 (- (length comma-seperated) 2)
                                                 ) "]"))
           (json-list (append (json-read-from-string without-comma) nil))
           (result nil))
      (dolist (vid json-list)
        (add-to-list 'result (cons (concat "https://youtu.be/"
                                           (cdr (assoc 'id vid)))
                                   (cdr (assoc 'title vid)))))
      (reverse result)))


  ;; --- adding the tracks to the playlist ---
  (defun emms-add-youtube-url (url)
    (interactive "sPlay Youtube-URL: ")
    (when (not (string-match "www.youtube.com" url))
      (error "Not a youtube Link!"))
    (if (string-match "https://www.youtube.com/playlist\\?list=" url)
        (progn
          (message "Adding youtube playlist...")
          (dolist (url-and-title (fp/youtube-playlist-from-url url))
            (let ((track (emms-track 'url (car url-and-title))))
              (message (cdr url-and-title))
              (emms-track-set track 'info-title (cdr url-and-title))
              (emms-track-set track 'info-url (car url-and-title))
              (with-current-emms-playlist
                (goto-char (point-max))
                (emms-playlist-insert-track track)))))
      (progn
        (message "Adding youtube video...")
        (let ((track (emms-track 'url url)))
          (emms-track-set track 'info-title
                          (substring  (shell-command-to-string
                                       (concat "youtube-dl -e " url))
                                      0 -1))
          (with-current-emms-playlist
            (goto-char (point-max))
            (emms-playlist-insert-track track))))))


  ;; --- display the tracks properly ---
  (defun fg-emms-track-description (track)
    "Return a somewhat nice track description."
    (let ((artist (emms-track-get track 'info-artist))
          (year (emms-track-get track 'info-year))
          (album (emms-track-get track 'info-album))
          (tracknumber (emms-track-get track 'info-tracknumber))
          (title (emms-track-get track 'info-title))
          (name (emms-track-get track 'name)))
      (cond
       ((string-match "http?s://" name)
        (concat name " - "
                (if (> (length title) 0) title "Unknown title")))
       (t
        (emms-info-track-description track)))))


  ;; --------------------------------------------------------------------------------
  ;; modeline
  ;; --------------------------------------------------------------------------------
  (emms-mode-line-disable)
  (emms-playing-time 0)

  (defun fp/emms-playlist-mode-hook ()
    (interactive)
    (setq emms-mode-line-active-p t)
    (add-hook 'emms-track-updated-functions 'emms-mode-line-alter)
    (add-hook 'emms-player-finished-hook 'emms-mode-line-blank)
    (add-hook 'emms-player-stopped-hook 'emms-mode-line-blank)
    (add-hook 'emms-player-started-hook 'emms-mode-line-alter)
    (add-hook 'emms-player-started-hook       'emms-playing-time-start)
    (add-hook 'emms-player-stopped-hook       'emms-playing-time-stop)
    (add-hook 'emms-player-finished-hook      'emms-playing-time-stop)
    (add-hook 'emms-player-paused-hook        'emms-playing-time-pause)
    (add-hook 'emms-player-seeked-functions   'emms-playing-time-seek)
    (add-hook 'emms-player-time-set-functions 'emms-playing-time-set)

    (setq-local header-line-format
                '(:eval (concat "[ "
                                (format "%02d" (/ emms-playing-time 60)) ":"
                                (format "%02d" (mod emms-playing-time 60))" ] "
                                (let ((artist (cdr (assoc 'info-artist
                                                          (emms-playlist-current-selected-track)))))
                                  (when artist (concat artist " - ")))
                                (cdr (assoc 'info-title
                                            (emms-playlist-current-selected-track)))))))

  (add-hook 'emms-playlist-mode-hook 'fp/emms-playlist-mode-hook)

  ;; --------------------------------------------------------------------------------
  ;; volume
  ;; --------------------------------------------------------------------------------
  (if (eq system-type 'windows-nt)
      (progn (defun emms-volume-nircmd-change (amount)
               (let ((inhibit-message t))
                 (shell-command
                  (concat "nircmd changesysvolume " (format "%s" (* amount 1000))) nil)))
             (setq emms-volume-change-function 'emms-volume-nircmd-change))
    (setq emms-volume-change-function 'emms-volume-pulse-change))

  ;; --------------------------------------------------------------------------------
  ;; my bindings
  ;; --------------------------------------------------------------------------------
  (evil-leader/set-key-for-mode 'emms-playlist-mode "fs" 'emms-playlist-save)
  (evil-leader/set-key-for-mode 'dired-mode "me" 'emms-add-dired)

  ;; --- mouse bindings ---
  (define-key emms-browser-mode-map [mouse-3]
    (lambda (event) (interactive "e") (fp/point-to-mouse event)
      (emms-browser-toggle-subitems-recursively)))

  (define-key emms-browser-mode-map fp/mouse-back
    (lambda (event) (interactive "e") (fp/point-to-mouse event)
      (emms-browser-add-tracks)))


  (define-key emms-playlist-mode-map [mouse-3]
    (lambda (event) (interactive "e") (fp/point-to-mouse event)
      (emms-playlist-mode-play-smart)))

  (define-key emms-playlist-mode-map fp/mouse-back
    (lambda (event) (interactive "e") (fp/point-to-mouse event)
      (emms-playlist-mode-kill-track)))

  ;; --------------------------------------------------------------------------------
  ;; evil-collection
  ;; --------------------------------------------------------------------------------
  ;; i had to copy and modify this code, since evil-collection really does a lot of things
  ;; i dont want


  ;; ;;; evil-collection-emms.el --- Evil bindings for EMMS -*- lexical-binding: t -*-

  ;; Copyright (C) 2017 Pierre Neidhardt

  ;; Author: Pierre Neidhardt <ambrevar@gmail.com>
  ;; Maintainer: James Nguyen <james@jojojames.com>
  ;; Pierre Neidhardt <ambrevar@gmail.com>
  ;; URL: https://github.com/emacs-evil/evil-collection
  ;; Version: 0.0.1
  ;; Package-Requires: ((emacs "25.1"))
  ;; Keywords: evil, emms, tools

  ;; This file is free software; you can redistribute it and/or modify
  ;; it under the terms of the GNU General Public License as published
  ;; by the Free Software Foundation; either version 3, or (at your
  ;; option) any later version.
  ;;
  ;; This file is distributed in the hope that it will be useful,
  ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
  ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  ;; GNU General Public License for more details.
  ;;
  ;; For a full copy of the GNU General Public License
  ;; see <http://www.gnu.org/licenses/>.

  ;;; Commentary:
  ;; Evil bindings for EMMS.

  ;;; Code:
  (declare-function emms-with-inhibit-read-only-t "emms")
  (declare-function emms-playlist-mode-correct-previous-yank "emms-playlist-mode")

  (defun evil-collection-emms-playlist-mode-insert-newline-above ()
    "Insert a newline above point."
    (interactive)
    (emms-with-inhibit-read-only-t
     (evil-insert-newline-above)))

  (defun evil-collection-emms-playlist-mode-insert-newline-below ()
    "Insert a newline below point."
    (interactive)
    (emms-with-inhibit-read-only-t
     (evil-insert-newline-below)))

  (defun evil-collection-emms-playlist-mode-paste-before ()
    "Pastes the latest yanked playlist items before the cursor position.
  The return value is the yanked text."
    (interactive)
    (emms-with-inhibit-read-only-t
     (goto-char (point-at-bol))
     (yank)
     (emms-playlist-mode-correct-previous-yank)
     (evil-previous-line)
     (evil-beginning-of-line)))

  (defun evil-collection-emms-playlist-mode-paste-after ()
    "Pastes the latest yanked playlist items behind point.
  The return value is the yanked text."
    (interactive)
    (unless (eobp) (evil-next-line))
    (evil-collection-emms-playlist-mode-paste-before))

  (defun evil-collection-emms-browser-setup ()
    "Set up `evil' bindings for `emms-browser'."

    (add-hook 'emms-browser-mode-hook 'evil-normal-state)
    (evil-define-key 'normal emms-browser-mode-map
      ;; playback controls
      "r" 'emms-browse-by-artist
      "x" 'emms-pause
      "X" 'emms-stop
      "=" 'emms-seek
      "<" 'emms-seek-backward
      ">" 'emms-seek-forward
      "a" 'emms-browser-add-tracks
      (kbd "RET") 'emms-browser-add-tracks
      (kbd "C-RET") 'emms-browser-add-tracks-and-play

      ;; volume controls
      "+" 'emms-volume-raise
      "-" 'emms-volume-lower

      "u" 'emms-playlist-mode-undo

      ;; motion
      "gk" 'emms-browser-prev-non-track
      "gj" 'emms-browser-next-non-track
      (kbd "M-k") 'emms-browser-prev-non-track
      (kbd "M-j") 'emms-browser-next-non-track

      (kbd "<tab>") 'emms-browser-toggle-subitems-recursively
      (kbd "<backtab>") 'emms-browser-collapse-all
      "^" 'emms-browser-move-up-level
      (kbd "SPC") 'emms-browser-toggle-subitems
      "g1" 'emms-browser-collapse-all
      "g2" 'emms-browser-expand-to-level-2
      "g3" 'emms-browser-expand-to-level-3
      "g4" 'emms-browser-expand-to-level-4
      "g0" 'emms-browser-expand-all
      "ga" 'emms-browse-by-artist
      "gA" 'emms-browse-by-album
      "gb" 'emms-browse-by-genre
      "gy" 'emms-browse-by-year
      "gc" 'emms-browse-by-composer
      "gp" 'emms-browse-by-performer
      "zm" 'emms-browser-collapse-all
      "zr" 'emms-browser-expand-all
      "zo" 'emms-browser-expand-one-level
      "zc" 'emms-browser-collapse-all

      "/" 'emms-isearch-buffer ; This shows hidden items during search.
      "n" 'isearch-repeat-forward
      "N" 'isearch-repeat-backward

      "s" (lookup-key emms-browser-mode-map (kbd "s"))
      "g" (lookup-key emms-browser-mode-map (kbd "W"))

      "C" 'emms-browser-clear-playlist
      "d" 'emms-browser-delete-files
      "gd" 'emms-browser-view-in-dired ; "d" does the same, keep "gd" for consistency.
      "q" (lambda () (interactive) (emms-browser-hide-linked-window) (quit-window))))

  (defun evil-collection-emms-setup ()
    "Set up `evil' bindings for `emms'."
    (with-eval-after-load 'emms-browser
      (evil-collection-emms-browser-setup))

    (evil-set-initial-state 'emms-playlist-mode 'normal)
    (evil-define-key 'normal emms-playlist-mode-map
      ;; playback controls
      "x" 'emms-pause
      "X" 'emms-stop
      "r" 'emms-random
      "=" 'emms-seek
      "<" 'emms-seek-backward
      ">" 'emms-seek-forward
      "gj" 'emms-next
      "gk" 'emms-previous
      (kbd "M-j") 'emms-next
      (kbd "M-k") 'emms-previous
      (kbd "RET") 'emms-playlist-mode-play-smart

      ;; volume controls
      "+" 'emms-volume-raise
      "-" 'emms-volume-lower

      "u" 'emms-playlist-mode-undo

      ;; motion
      "gg" 'emms-playlist-mode-first
      "G" 'emms-playlist-mode-last
      "]" 'emms-playlist-mode-next
      "[" 'emms-playlist-mode-previous

      "d" 'emms-playlist-mode-kill-track  ; emms-browser uses "D"
      "C" 'emms-playlist-clear
      "O" 'evil-collection-emms-playlist-mode-insert-newline-above
      "o" 'evil-collection-emms-playlist-mode-insert-newline-below
      "P" 'evil-collection-emms-playlist-mode-paste-before
      "p" 'evil-collection-emms-playlist-mode-paste-after

      "u" 'emms-playlist-mode-undo

      "ze" 'emms-tag-editor-edit
      "R" 'emms-toggle-random-playlist

      "." 'emms-playlist-mode-center-current
      "D" 'emms-playlist-mode-goto-dired-at-point
      "gd" 'emms-playlist-mode-goto-dired-at-point ; "d" does the same, keep "gd" for consistency.

      "zs" 'emms-show
      "a" 'emms-playlist-mode-add-contents
      "zp" 'emms-playlist-set-playlist-buffer

      ;; filter
      "S" (lookup-key emms-playlist-mode-map (kbd "S"))
      "s" (lookup-key emms-playlist-mode-map (kbd "/"))

      (kbd "M-y") 'emms-playlist-mode-yank-pop
      "q" 'previous-buffer)

    (evil-define-key 'visual emms-playlist-mode-map
      ;; "d" 'emms-playlist-mode-kill
      "d" 'emms-playlist-mode-kill)

    (evil-define-key 'normal emms-browser-search-mode-map
      "q" 'emms-browser-kill-search)

    (evil-set-initial-state 'emms-metaplaylist-mode 'normal)
    (evil-define-key 'normal emms-metaplaylist-mode-map
      (kbd "RET") 'emms-metaplaylist-mode-goto-current
      (kbd "<space>") 'emms-metaplaylist-mode-set-active
      "gr" 'emms-metaplaylist-mode-update
      "C" 'emms-metaplaylist-mode-new-buffer
      "." 'emms-metaplaylist-mode-center-current
      "D" 'emms-metaplaylist-mode-kill-buffer
      "q" 'kill-this-buffer)

    (evil-set-initial-state 'emms-stream-mode 'normal)
    (evil-define-key 'normal emms-stream-mode-map
      (kbd "RET") 'emms-stream-play
      "j" 'emms-stream-next-line
      "k" 'emms-stream-previous-line
      "y" 'emms-stream-yank-bookmark
      "d" 'emms-stream-kill-bookmark
      "c" 'emms-stream-edit-bookmark
      "r" 'emms-stream-edit-bookmark
      "i" 'emms-stream-info-bookmark
      "s" 'emms-stream-save-bookmarks-file
      "x" 'emms-stream-toggle-default-action
      "q" 'emms-stream-quit)

    (evil-define-key 'normal emms-tag-editor-mode-map
      "j" 'evil-next-line
      "k" 'evil-previous-line
      "gj" 'emms-tag-editor-next-track
      "gk" 'emms-tag-editor-prev-track
      (kbd "M-j") 'emms-tag-editor-next-track
      (kbd "M-k") 'emms-tag-editor-prev-track
      (kbd "C-c C-k") (lambda () (interactive) (kill-buffer (current-buffer)))))

  ;;; evil-collection-emms.el ends here

  (evil-collection-emms-browser-setup)
  (evil-collection-emms-setup))


(provide 'config-emms)
