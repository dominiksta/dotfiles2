(require-and-log 'config-programming-general)

(evil-set-initial-state 'vc-dir-mode 'motion)
(evil-set-initial-state 'vc-annotate-mode 'motion)
(evil-set-initial-state 'log-view-mode 'motion)
(evil-set-initial-state 'vc-svn-log-view-mode 'motion)
(evil-set-initial-state 'vc-git-log-view-mode 'motion)

(defun fp/vc-marked-or-current-file ()
  (let ((marked (vc-dir-marked-files))
        (curr (vc-dir-current-file)))
    (cond
     (marked marked)
     (curr (list curr))
     (t (error "No selected vc-dir file")))))

(defun fp/vc-dir-beginning-of-buffer ()
  (interactive) (beginning-of-buffer) (vc-dir-next-line 0))

(defun fp/vc-dir-end-of-buffer ()
  (interactive) (end-of-buffer) (vc-dir-previous-line 0))

(evil-define-key 'motion vc-dir-mode-map
  "j" 'evil-next-line
  "k" 'evil-previous-line
  "gj" 'vc-dir-next-directory
  "gk" 'vc-dir-previous-directory
  "gg" 'fp/vc-dir-beginning-of-buffer
  "G" 'fp/vc-dir-end-of-buffer
  "L" 'vc-print-log
  "r" 'fp/vc-revert-buffer
  "m" 'vc-dir-mark
  "M" 'vc-dir-mark-all-files
  "s" (lambda () (interactive)
        (vc-git-command nil 0 (fp/vc-marked-or-current-file) "add")
        (revert-buffer))
  "S" (lambda () (interactive)
        (vc-git-command nil 0 (fp/vc-marked-or-current-file) "rm" "--cached")
        (revert-buffer))
  "u" 'vc-dir-unmark
  "U" 'vc-dir-unmark-all-files
  "x" 'vc-revert
  "D" 'vc-dir-delete-file
  "d" 'vc-diff
  "=" 'vc-diff
  "e" 'vc-ediff
  (kbd "RET") 'vc-dir-find-file
  "o" 'vc-dir-find-file-other-window
  "n" 'vc-next-action
  "c" 'vc-next-action)

(evil-define-key 'motion vc-annotate-mode-map
  "q" 'quit-window
  (kbd "RET") 'vc-annotate-goto-line
  "r" 'vc-annotate-working-revision
  "o" 'vc-annotate-find-revision-at-line
  "P" 'vc-annotate-prev-revision
  "N" 'vc-annotate-next-revision
  "L" 'vc-annotate-show-log-revision-at-line)

(with-eval-after-load "log-view"
  (evil-define-key 'motion log-view-mode-map
    "q" 'quit-window
    "gj" 'log-view-msg-next
    "gk" 'log-view-msg-prev
    "d" 'log-view-diff
    "=" 'log-view-diff-changeset
    (kbd "RET") 'log-view-toggle-entry-display)

  ;; (define-key log-edit-mode-map (kbd "C-c C-a") 'vc-git-log-edit-toggle-amend)
  )


(defun fp/vc-revert-buffer ()
  (interactive)
  (if (not (eq major-mode 'vc-dir-mode))
      (error "Not in vc-dir-mode"))
  (let ((win (get-buffer-window (current-buffer)))
        (dir default-directory)
        (vc-allow-async-revert nil)
        (display-buffer-overriding-action '(display-buffer-same-window . ())))
    (kill-buffer (current-buffer))
    (vc-dir dir)))


(provide 'config-vc)