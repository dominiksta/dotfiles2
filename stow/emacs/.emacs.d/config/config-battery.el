(require 'battery)
(require 'color)

(defvar battery-unicode-update-interval 30)
(defvar battery-unicode-update-timer nil)
(defvar battery-unicode-mode-line-string "")
(put 'battery-unicode-mode-line-string 'risky-local-variable t)

(defun battery-unicode--mode-line-string ()
  (let* ((status-result (funcall battery-status-function))
         (bat (read (cdr (assoc ?p status-result))))
         (remaining (cdr (assoc ?t status-result)))
         (charging (cdr (assoc ?B status-result)))
         (index (cl-position-if (lambda (e) (> bat e)) '(87 75 62 50 37 25 12 7 -1)))
         (symbol (nth index '("█" "▇" "▆" "▅" "▄" "▃" "▂" "▁" "!")))
         (color (nth index (mapcar (lambda (c) (apply 'color-rgb-to-hex c))
                                   (color-gradient '(.3 1 .2) '(1 .2 .1) 9)))))
    (propertize symbol 'face (list :foreground color :box (if (<= bat 7) color nil))
                'help-echo (concat (number-to-string bat) "%, " charging ", "
                                   "remaining: " remaining ))))

(defun battery-unicode-update ()
  (setq battery-unicode-mode-line-string (concat " " (battery-unicode--mode-line-string) " "))
  (force-mode-line-update))

(setq battery-unicode-update-timer (run-with-timer
                                    nil battery-unicode-update-interval
                                    'battery-unicode-update))
(define-minor-mode battery-unicode-mode
  "Displays the current battery level in a fancy unicode format"
  nil "" nil
  :global t
  (if battery-unicode-mode
      (progn
        (if (not (member battery-unicode-update-timer timer-list))
            (timer-activate battery-unicode-update-timer))
        (if (not (member 'battery-unicode-mode-line-string global-mode-string))
            (setq global-mode-string
                  (append global-mode-string '(battery-unicode-mode-line-string)))))
    (progn
      (timerp battery-unicode-update-timer)
      (cancel-timer battery-unicode-update-timer)
      (setq global-mode-string (remove 'battery-unicode-mode-line-string global-mode-string))
      (force-mode-line-update))))

(provide 'config-battery)
