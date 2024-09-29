(require-and-log 'config-programming-general)

(defmacro pickle-preview--with-temp-file (content &rest body)
  `(let (file (make-temp-file "emacs-pickle-preview"))
     (with-temp-buffer file (insert ,content))
     (progn ,@body)))

(defun pickle-preview--run-python (code &optional python-interpreter)
  (let* ((file (make-temp-file "emacs-pickle-preview-python-"))
         (python-interpreter (or python-interpreter "python3"))
         (cmd (format "%s %s" python-interpreter file))
         (res (with-temp-buffer
                (insert code)
                (write-file file)
                (shell-command-to-string cmd))))
    (delete-file file)
    res))

(defun pickle-preview--current-buffer ()
  (let ((file (expand-file-name (buffer-file-name))))
    (pickle-preview--run-python
     (format
      "
import pickle
from pprint import pprint

with open('%s', 'rb') as f:
  loaded = pickle.load(f)
  pprint(loaded)
"
      file))))

(define-derived-mode pickle-preview-mode fundamental-mode "pickle-previer"
  (when (yes-or-no-p "Attempt to deserialize python pickle file?")
    (print (current-buffer))
    (let ((buf (generate-new-buffer
                (format "*pickle preview: %s*"
                        (file-name-nondirectory (buffer-file-name)))))
          (preview (pickle-preview--current-buffer))
          (undo-tree-incompatible-major-modes '(pickle-preview-mode python-mode)))
      (kill-buffer (current-buffer))
      (switch-to-buffer buf)
      (insert preview)
      (beginning-of-buffer)
      ;; (python-mode)
      (read-only-mode 1))))

(provide 'config-language-pickle)