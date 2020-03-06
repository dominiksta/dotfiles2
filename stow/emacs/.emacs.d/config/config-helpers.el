(require 'battery)

(setq sync-directory (expand-file-name "~/sync/")
      download-directory (expand-file-name "~/Downloads")
      video-directory (expand-file-name "~/Video")
      picture-directory (expand-file-name "~/bilder")
      trash-directory (expand-file-name "~/.trash")
      delete-by-moving-to-trash t
      bookmark-default-file (concat sync-directory "emacs/random/bookmarks"))

(when (file-exists-p (concat sync-directory "emacs/lisp/"))
  (let ((default-directory (concat sync-directory "emacs/lisp/")))
    (normal-top-level-add-subdirs-to-load-path)))


(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "waterfox")

;; (setq-default buffer-file-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")

(defun return-nil (&rest rest) (interactive) nil)

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defmacro set-local-variable-in-all-buffers (var val)
  `(dolist (b (buffer-list))
     (with-current-buffer b
       (setq ,var ,val))))

(defmacro run-function-in-mode-buffers (mode function)
  `(dolist (b (buffer-list))
     (with-current-buffer b
       (if (eq major-mode ,mode)
           (funcall ,function)))))

(defun volatile-kill-buffer (&optional buffer-name)
  "Kill current buffer unconditionally."
  (interactive)
  (set-buffer-modified-p nil)
  (let ((buf (if buffer-name (get-buffer buffer-name) (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq-local kill-buffer-query-functions (lambda () t))
        (kill-buffer buf)))))

(defun kill-this-buffer ()
  (interactive) (kill-buffer (current-buffer)))

(defun fp/running-on-laptop-p ()
  (and battery-status-function
     (not (string-match-p
           "N/A"
           (battery-format "%B"
                           (funcall battery-status-function))))))

(defun fp/quarter-window-vertically ()
  "create a new window a quarter size of the current window"
  (split-window-vertically)
  (other-window 1)
  (let ((window (split-window-vertically)))
    ;; (other-window -1)
    (delete-window)
    window))

;; --- system notifications ---
(require 'notifications)
(defvar temp-notification nil "current notification")
(defun generic-notification-notify (title body)
  (message "SYSTEM NOTIFICATION: %s || %s" title body)
  (if (eq system-type 'windows-nt)
      (progn
        (setq temp-notification (w32-notification-notify :tip "test" :body body :title title))
        (run-with-timer 5 nil (lambda () (w32-notification-close temp-notification))))
    (notifications-notify :title title :body body))
  ;; play a sound asynchronously or (when possible) read the notification with tts
  (if (executable-find "espeak")
      (start-process-shell-command "" nil (format "espeak -a 200 \"%s\"" (concat title "." body)))
    (start-process-shell-command "" nil (concat "mpv " sync-directory "/emacs/random/notify.wav"))))


;; --- mouse ---
(setq mouse-1-click-follows-link 450
      org-mouse-1-follows-link mouse-1-click-follows-link)

(defun fp/point-to-mouse (event)
  "Move the point to the clicked position"
  (interactive "e")
  (let ((es (event-start event)))
    (select-window (posn-window es))
    (goto-char (posn-point es))))

(setq fp/mouse-back (if (eq system-type 'windows-nt) [mouse-4] [mouse-8])
      fp/double-mouse-back (if (eq system-type 'windows-nt) [(double-mouse-4)] [(double-mouse-8)])
      fp/triple-mouse-back (if (eq system-type 'windows-nt) [(triple-mouse-4)] [(triple-mouse-8)]))

(setq fp/mouse-forward (if (eq system-type 'windows-nt) [mouse-5] [mouse-9])
      fp/double-mouse-forward (if (eq system-type 'windows-nt) [(double-mouse-5)] [(double-mouse-9)])
      fp/triple-mouse-forward (if (eq system-type 'windows-nt) [(triple-mouse-5)] [(triple-mouse-9)]))

(provide 'config-helpers)
