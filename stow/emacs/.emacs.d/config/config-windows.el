(when (eq system-type 'windows-nt)

  ;; open with default application
  (defun w32-browser (doc) (w32-shell-execute 1 doc))
  ;; fix slow icon display
  (setq inhibit-compacting-font-caches t)
  (setq proced-custom-attributes nil)
  (setq w32-pipe-read-delay 0)

  (setq find-program "c:/tools/cygwin/bin/find.exe")

  (add-to-list 'exec-path (file-name-directory (expand-file-name (car command-line-args))))
  (add-to-list 'exec-path (expand-file-name (concat sync-directory "documents/code/emacs/bin")) t)

  (with-eval-after-load "config-eshell"
    (setq eshell-aliases-file
          (concat sync-directory
                  "emacs/random/eshell-aliases-windows"))))



(defun fp/remove-dos-eol ()
  "Do not show  (^M) in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(provide 'config-windows)
