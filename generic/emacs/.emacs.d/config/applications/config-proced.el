(require 'proced)

(defun fp/proced-startup ()
  (interactive)
  (proced)
  (proced-format-interactive 'short))

(setq fp/proced-kill-revert-delay 1)
(defun fp/kill-marked-processes-and-revert()
  (interactive)
  (proced-send-signal "HUP" (proced-marked-processes))
  (run-with-timer fp/proced-kill-revert-delay nil (lambda ()(revert-buffer))))

(setq proced-auto-update-interval 2)

;; --------------------------------------------------------------------------------
;; bindings
;; --------------------------------------------------------------------------------
(add-to-list 'evil-normal-state-modes 'proced-mode)
(evil-define-key 'normal proced-mode-map
  "R"    'proced-toggle-auto-update
  "T"    'proced-toggle-tree
  "sv"   (lambda () (interactive) (proced-sort-interactive 'vsize))
  "sc"   (lambda () (interactive) (proced-sort-interactive 'pcpu))
  "sm"   (lambda () (interactive) (proced-sort-interactive 'pmem))
  "sn"   (lambda () (interactive) (proced-sort-interactive 'comm))
  "sp"   (lambda () (interactive) (proced-sort-interactive 'pid))
  "su"   (lambda () (interactive) (proced-sort-interactive 'user))
  "ss"   (lambda () (interactive) (proced-sort-interactive 'start))
  "st"   (lambda () (interactive) (proced-sort-interactive 'time))
  "sa"   (lambda () (interactive) (proced-sort-interactive 'args))
  "sS"   'proced-sort-interactive

  "F"    nil
  "FF"   'proced-format-interactive
  "Fs"   (lambda () (interactive) (proced-format-interactive 'short))
  "Fm"   (lambda () (interactive) (proced-format-interactive 'medium))
  "Fl"   (lambda () (interactive) (proced-format-interactive 'long))
  "Fv"   (lambda () (interactive) (proced-format-interactive 'verbose))

  "C" '  proced-mark-children
  "P" '  proced-mark-parents

  "f"    nil
  "fF"   'proced-filter-interactive
  "ff"   'proced-filter-interactive
  "fu"   (lambda () (interactive) (proced-filter-interactive 'user))
  "fU"   (lambda () (interactive) (proced-filter-interactive 'user-running))
  "fa"   (lambda () (interactive) (proced-filter-interactive 'all))
  "fA"   (lambda () (interactive) (proced-filter-interactive 'all-running))
  "fe"   (lambda () (interactive) (proced-filter-interactive 'emacs))

  "K"   'fp/kill-marked-processes-and-revert
  "D"   'fp/kill-marked-processes-and-revert

  "S"   'proced-send-signal
  "j"   'next-line
  "k"   'previous-line
  (kbd "C-j") 'evil-scroll-down
  (kbd "C-k") 'evil-scroll-up

  "m"   'proced-mark
  "u"   'proced-unmark
  "t"   'proced-toggle-marks

  "r"   'revert-buffer
  "z"   'proced-renice
  "q"   'quit-window)


(provide 'config-proced)
