(require 'ansi-color)

(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(define-derived-mode pager-mode fundamental-mode "Pager"
  "Pager Mode"
  (display-ansi-colors))