(require-and-log 'config-search)

(straight-use-package '(mvtn :type git :host github :repo "dominiksta/mvtn.el"))

(with-eval-after-load "mvtn"
  ;; ----------------------------------------------------------------------
  ;; general
  ;; ----------------------------------------------------------------------

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
  (mvtn-journal-autojournal-set-feature 'org-clock t)

  ;; ----------------------------------------------------------------------
  ;; citing webpages
  ;; ----------------------------------------------------------------------

  (defun fp/mvtn-store-webpage-from-clipboard-markdown (encrypt)
    (interactive "P")
    "This uses the 'Copy Selection as Markdown'
Addon (https://github.com/0x6b/copy-selection-as-markdown).
Integration of the kill-ring with the system clipboard has to be
enabled (it is by default)."
    (let* ((clip (current-kill 0))
           (first-line (car (split-string clip "\n")))
           (the-rest (mapconcat 'identity (cddr (split-string clip "\n")) "\n")))
      (save-match-data
        (string-match "^\\[\\(.+\\)\\](\\(.+\\))" first-line)
        (let ((title (match-string-no-properties 1 first-line))
              (url (match-string-no-properties 2 first-line)))
          (mvtn-new-note-from-template-string
           "prv/flt" title "org" '("web")
           (format "#+TITLE: {title}
#+DATE: {date}
# mvtn_original_title :: {title}
# mvtn_original_id :: {timestamp}

{point}

Quelle: %s

#+BEGIN_SRC markdown
%s
#+END_SRC\n" url the-rest))))))

  ;; ----------------------------------------------------------------------
  ;; binds
  ;; ----------------------------------------------------------------------

  (evil-define-key 'normal mvtn-tag-file-list-mode-map
    "o" 'mvtn-tag-file-list-open
    "a" 'mvtn-tag-file-list-open-keep-focus
    "r" 'revert-buffer
    "q" 'quit-window)

  ;; If `evil-org-mode' is not disabled, it will overshadow the binds in the
  ;; following section
  (add-hook 'mvtn-backlink-buffer-mode-hook
            (lambda () (interactive) (evil-org-mode 0)))
  (evil-define-key 'normal mvtn-backlink-buffer-mode-map
    "gj" 'mvtn-backlink-buffer-next-backlink
    (kbd "M-j") 'mvtn-backlink-buffer-next-backlink
    "gk" 'mvtn-backlink-buffer-previous-backlink
    (kbd "M-k") 'mvtn-backlink-buffer-previous-backlink))


;; This of course does not not /really/ come from mvtn.el but autoloading like
;; this will trigger the `with-eval-after-load' block above which will define
;; the function.
(autoload 'fp/mvtn-store-webpage-from-clipboard-markdown "mvtn.el")

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
  "nt" 'mvtn-tag-file-list
  "nw" 'fp/mvtn-store-webpage-from-clipboard-markdown)


(provide 'config-mvtn)