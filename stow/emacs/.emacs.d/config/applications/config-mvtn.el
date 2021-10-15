(require-and-log 'config-search)

(straight-use-package '(mvtn :type git :host github :repo "dominiksta/mvtn.el"))

(with-eval-after-load "mvtn"
  (setq mvtn-note-directories
        '((:dir "~/sync/documents/notes/mvtn" :name "prv" :structure
                ((:dir "flt" :datetree t) ;; fleeting
                 (:dir "lit" :datetree t) ;; literature
                 (:dir "tec" :datetree t) ;; tech (devlog, etc.)
                 (:dir "stc" :datetree nil))) ;; static
          (:dir "~/sync/work/notes" :name "wrk" :structure
                ((:dir "rec" :datetree t)
                 (:dir "tri" :datetree t)
                 (:dir "stc" :datetree nil)))) ;; static
        mvtn-template-locations '("~/sync/documents/notes/mvtn/templates")
        mvtn-default-file-extension "org"
        mvtn-excluded-directories '(".git" ".svn" "ltximg" "orgimg" "data")
        mvtn-search-function 'mvtn-search-full-text-rg
        mvtn-cv-enable t
        mvtn-org-agenda-tag "projekt"
        mvtn-journal-dir "prv/flt"
        mvtn-journal-new-daily-title "Logbuch am %Y-%m-%d")

  (require 'mvtn-link-buttons)
  (add-hook 'mvtn-minor-mode-hook 'olivetti-mode)

  (mvtn-journal-autojournal-set-feature 'git-commit t)
  (mvtn-journal-autojournal-set-feature 'note-changed t)
  (mvtn-journal-autojournal-set-feature 'org-clock t))

(evil-define-key 'normal mvtn-tag-file-list-mode-map
  "o" 'mvtn-tag-file-list-open
  "a" 'mvtn-tag-file-list-open-keep-focus
  "r" 'revert-buffer
  "q" 'quit-window)

;; if `evil-org-mode' is not disabled, it will overshadow the binds in the
;; following section
(add-hook 'mvtn-backlink-buffer-mode-hook
          (lambda () (interactive) (evil-org-mode 0)))
(evil-define-key 'normal mvtn-backlink-buffer-mode-map
  "gj" 'mvtn-backlink-buffer-next-backlink
  (kbd "M-j") 'mvtn-backlink-buffer-next-backlink
  "gk" 'mvtn-backlink-buffer-previous-backlink
  (kbd "M-k") 'mvtn-backlink-buffer-previous-backlink)

(evil-leader/set-key
  "nd" 'mvtn-jump-current-year-directory
  "nj" 'mvtn-journal-new-entry
  "nJ" 'mvtn-journal-new-quick-entry
  "nn" 'mvtn-open-or-create-note
  "nT" 'mvtn-new-note-from-template
  "nr" 'mvtn-rename-current-file
  "ns" 'mvtn-search-full-text
  "nB" 'mvtn-search-backlinks
  "nb" 'mvtn-backlink-buffer-toggle-side-window
  "nl" 'mvtn-insert-link
  "no" 'mvtn-follow-link-at-point
  "nt" 'mvtn-tag-file-list)


(provide 'config-mvtn)