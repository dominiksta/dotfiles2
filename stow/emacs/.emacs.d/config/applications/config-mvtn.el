(require-and-log 'config-search)

(straight-use-package '(el-patch :type git :host github :repo "f1rstperson/mvtn.el"))

(with-eval-after-load "mvtn"
  (setq mvtn-note-directory "~/sync/Documents/mvtn"
        mvtn-default-file-extension "org"
        mvtn-search-function 'mvtn-search-full-text-ag)

  (defun fp/mvtn-enable-olivetti ()
    (condition-case nil
        (when (string-match-p (expand-file-name mvtn-note-directory)
                              (buffer-file-name (current-buffer)))
          (olivetti-mode 1))
      (error nil)))

  (add-hook 'text-mode-hook 'fp/mvtn-enable-olivetti))

(evil-leader/set-key
  "nj" 'mvtn-jump-current-year-directory
  "nn" 'mvtn-open-note
  "nN" 'mvtn-new-note
  "nr" 'mvtn-rename-current-file
  "ns" 'mvtn-search-full-text
  "nl" 'mvtn-insert-link
  "no" 'mvtn-follow-link-at-point)


(provide 'config-mvtn)