;; --------------------------------------------------------------------------------
;; format
;; --------------------------------------------------------------------------------

;; (defun simple-mode-line-render (left right)
;;   "Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively."
;;   (let* ((available-width (- (window-total-width)
;;                              (+ (length (format-mode-line left))
;;                                 (length (format-mode-line right))))))
;;     (append left (list (format (format "%%%ds" available-width) "")) right)))


;; (setq mode-line-position ""
;;       fp/mode-line-left '((:eval (if (featurep 'eyebrowse)
;;                                      (concat " [" (number-to-string
;;                                                    (eyebrowse--get 'current-slot))
;;                                              "] ") " "))
;;                           ("%e%l:%c %* "
;;                            (:eval (propertize "%b" 'face 'mode-line-buffer-id))
;;                            mode-line-position
;;                            evil-mode-line-tag))
;;       fp/mode-line-right '((:eval global-mode-string " "))
;;       fp/mode-line-format '((:eval (simple-mode-line-render
;;                                     fp/mode-line-left fp/mode-line-right))))

;; (setq-default mode-line-format fp/mode-line-format)
;; (set-local-variable-in-all-buffers mode-line-format fp/mode-line-format)

(defun fp/toggle-modeline ()
  (interactive)
  (if (eq mode-line-format nil)
      (progn
        (setq-default mode-line-format fp/mode-line-format)
        (set-local-variable-in-all-buffers mode-line-format fp/mode-line-format))
    (progn
      (setq-default mode-line-format nil)
      (set-local-variable-in-all-buffers mode-line-format nil))))

(when (fp/running-on-laptop-p) (require-and-log 'config-battery) (battery-unicode-mode 1))

;; (setq display-time-load-average-threshold 0.8
;;       display-time-format " %H:%M")
;; (display-time-mode 1)

;; (use-package mini-modeline :ensure t
;;   :config
;;   (defun mini-modeline--set-buffer-background () nil)
;;   (setq mode-line-position ""
;;         mini-modeline-r-format
;;         '(((:eval (propertize "%b" 'face 'mode-line-buffer-id))
;;            " %e%l:%c %*"
;;            mode-line-position
;;            evil-mode-line-tag
;;            global-mode-string)
;;           (:eval (if (featurep 'eyebrowse)
;;                      (concat " [" (number-to-string
;;                                    (eyebrowse--get 'current-slot))
;;                              "]") " "))))

;;   (mini-modeline-mode 1))


(use-package telephone-line :ensure t :demand t :config
  (telephone-line-defsegment fp/telephone-line-vcs-segment ()
    "Show current VCS branch and status indicator. Taken from
Protesilaos Stavrou's config."
    (when (and vc-mode buffer-file-name)
      (let* ((backend (vc-backend buffer-file-name))
             (state (vc-state buffer-file-name backend)))
        (concat
         (telephone-line-raw
          (format "%s" (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))))
         (cond ((memq state '(edited added))
                (telephone-line-raw " *"))
               ((eq state 'needs-merge)
                (telephone-line-raw " ?"))
               ((eq state 'needs-update)
                (telephone-line-raw " !"))
               ((memq state '(removed conflict unregistered))
                (telephone-line-raw " ×"))
               (t
                (telephone-line-raw "")))))))

  (setq telephone-line-evil-use-short-tag t
        telephone-line-lhs
        '((evil telephone-line-evil-tag-segment)
          (accent fp/telephone-line-vcs-segment
                  telephone-line-process-segment)
          (nil telephone-line-buffer-segment))
        telephone-line-rhs
        '((nil telephone-line-flycheck-segment)
          (accent telephone-line-airline-position-segment)
          (nil telephone-line-misc-info-segment))))

(telephone-line-mode 1)

(provide 'config-modeline)
