(require-and-log 'config-programming-general)
(require 'octave)

(evil-leader/set-key-for-mode 'octave-mode
  "eb" 'octave-send-buffer
  "er" 'octave-send-region
  "ef" 'octave-send-defun
  "md" 'octave-help)

(define-key inferior-octave-mode-map (kbd "C-l") 'comint-clear-buffer)

(add-hook 'octave-mode-hook 'company-mode)
(setq inferior-octave-startup-args
      '("-i" "--no-line-editing")
      inferior-octave-startup-file (if (eq system-type 'windows-nt)
                                       (concat sync-directory "emacs/random/init-octave-windows.m")
                                     (concat sync-directory "emacs/random/init-octave.m"))
      octave-comment-start "%")

(provide 'config-language-octave)
