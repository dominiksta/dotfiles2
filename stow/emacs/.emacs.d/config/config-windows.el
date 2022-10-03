(when (eq system-type 'windows-nt)

  ;; open with default application
  (defun w32-browser (doc) (w32-shell-execute 1 doc))
  ;; fix slow icon display
  (setq inhibit-compacting-font-caches t)
  (setq proced-custom-attributes nil)
  (setq w32-pipe-read-delay 0)

  (setq find-program "\"c:/Program Files/Git/usr/bin/find.exe\"")

  (setq browse-url-browser-function 'browse-url-default-windows-browser)

  (add-to-list 'exec-path (file-name-directory (expand-file-name (car command-line-args))))
  (add-to-list 'exec-path (expand-file-name (concat sync-directory "documents/code/emacs/bin")) t)

  (fmakunbound 'system-move-file-to-trash)

  (with-eval-after-load "config-eshell"
    (setq eshell-aliases-file
          (concat sync-directory
                  "emacs/random/eshell-aliases-windows")))

  (defun fp/ignore-wsl-acls (orig-fun &rest args)
    "Ignore ACLs on WSL. WSL does not provide an ACL, but emacs
expects there to be one before saving any file. Without this
advice, files on WSL can not be saved."
    (if (string-match-p "^//wsl\$/" (car args))
        (progn (message "ignoring wsl acls") "")
      (apply orig-fun args)))

  (advice-add 'file-acl :around 'fp/ignore-wsl-acls))


(defun fp/hide-dos-eol ()
  "Do not show  (^M) in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(provide 'config-windows)
