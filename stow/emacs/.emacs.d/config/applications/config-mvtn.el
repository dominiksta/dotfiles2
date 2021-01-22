
(straight-use-package '(el-patch :type git :host github :repo "f1rstperson/mvtn.el"))
(require 'mvtn)

(setq mvtn-note-directory "~/sync/Documents/mvtn"
      mvtn-default-file-extension "org")

(evil-leader/set-key
  "nj" 'mvtn-jump-current-year-directory
  "nn" 'mvtn-open-note
  "nN" 'mvtn-new-note
  "nr" 'mvtn-rename-current-file
  "ns" 'mvtn-search-full-text
  "nl" 'mvtn-insert-link
  "no" 'mvtn-follow-link-at-point)


(provide 'config-mvtn)