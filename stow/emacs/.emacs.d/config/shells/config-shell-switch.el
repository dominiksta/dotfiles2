(require 'seq)

(defun ss/dispatch-bash-here (&optional arg)
  (interactive "P")
  (ss/dispatch 'ss/bash-here 'ss/bash-predicate arg))

(defun ss/dispatch-bash (&optional arg)
  (interactive "P")
  (ss/dispatch 'ss/bash 'ss/bash-predicate arg))

(defun ss/dispatch (create-shell-fun shell-buffer-predicate &optional arg)
  (interactive "P")
  (let ((open-buffers (seq-filter 'ss/bash-predicate (buffer-list))))
    (cond
     ;; If a prefix arg is given, just pass the arg to create-shell-fun
     ((or (numberp arg) (and (listp arg) (eq (length arg) 1)))
      (funcall create-shell-fun arg))
     ;; When there are no buffers matching the predicate, create a new shell
     ((eq 0 (length open-buffers))
      (funcall create-shell-fun arg))
     ;; If we are currently in a buffer matching the predicate, prompt to
     ;; switch to a different shell
     ((funcall shell-buffer-predicate (current-buffer))
      (switch-to-buffer
       (completing-read "Switch to buffer: "
                        (mapcar (lambda (buf) (buffer-name buf)) open-buffers))))
     ;; If we are not in a buffer matching the predicate but there are some
     ;; buffers matching it, switch to the last used one of those.
     (t
      (switch-to-buffer (car open-buffers))))))

(defun ss/bash-predicate (buf)
  "Returns non-nil if the given buffer BUF is considered to
belong to the list of open bash shells."
  (and (eq (buffer-local-value 'major-mode buf) 'shell-mode)
     (string-match-p "\\*bash\\*" (buffer-name buf))))

(defun ss/bash-here (&optional arg)
  "Same as `ss/bash', but change the current directory of the
given shell (through arg) to `default-directory'."
  (let ((dir default-directory)
        (buf (ss/bash arg)))
    (with-current-buffer buf
      (insert (concat "cd " (shell-quote-argument (expand-file-name dir))))
      (comint-send-input))))

(defun ss/bash (&optional arg)
  "Open a bash shell identified by arg as a number. Creates
buffers with names similar to `eshell' (though it prepends the
hostname if the shell is not local). Return the created/accessed
buffer."
  (interactive "P")
  (let* ((explicit-shell-file-name "/bin/bash")
         (process-environment (cons "TERM=dumb" process-environment))
         (remote-host (file-remote-p default-directory 'host))
         (buffer-name-prefix (format
                              "%s%s"
                              (if remote-host (concat remote-host ": ") "")
                              "*bash*"))
         (buf (cond
               ((numberp arg) ;; numberic arg
                (get-buffer-create (format "%s<%d>" buffer-name-prefix arg)))
               (arg ;; C-u
                (generate-new-buffer buffer-name-prefix))
               (t (get-buffer-create buffer-name-prefix)))))
    (message buffer-name-prefix)
    (pop-to-buffer-same-window buf)
    (unless (derived-mode-p 'shell-mode) (shell buf))
    buf))


(provide 'config-shell-switch)
