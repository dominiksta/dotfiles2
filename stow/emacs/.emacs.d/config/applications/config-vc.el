(require-and-log 'config-programming-general)

(evil-set-initial-state 'vc-dir-mode 'motion)
(evil-set-initial-state 'vc-annotate-mode 'motion)
(evil-set-initial-state 'log-view-mode 'motion)
(evil-set-initial-state 'vc-svn-log-view-mode 'motion)
(evil-set-initial-state 'vc-git-log-view-mode 'motion)

(evil-define-key 'motion vc-dir-mode-map
  "j" 'vc-dir-next-line
  "k" 'vc-dir-previous-line
  "gj" 'vc-dir-next-directory
  "gk" 'vc-dir-previous-directory
  "L" 'vc-print-log
  "r" 'revert-buffer
  "m" 'vc-dir-mark
  "M" 'vc-dir-mark-all-files
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

(evil-define-key 'motion log-view-mode-map
  "q" 'quit-window
  "gj" 'log-view-msg-next
  "gk" 'log-view-msg-prev
  "d" 'log-view-diff
  "=" 'log-view-diff-changeset)


(provide 'config-vc)