(require-and-log 'config-search)

(straight-use-package '(mvtn :type git :host github :repo "dominiksta/mvtn.el"))

(with-eval-after-load "mvtn"
  ;; ----------------------------------------------------------------------
  ;; general
  ;; ----------------------------------------------------------------------
  (setq epg-pinentry-mode 'loopback) ;; use minibuffer for passphrase input

  (setq mvtn-note-directories
        '((:dir "~/sync/documents/notes/mvtn/prv" :name "prv" :structure
                ((:dir "flt" :datetree t) ;; fleeting
                 (:dir "lit" :datetree t) ;; literature
                 (:dir "tec" :datetree t) ;; tech (devlog, etc.)
                 (:dir "stc" :datetree nil))) ;; static
          (:dir "~/sync/documents/notes/mvtn/wrk" :name "wrk" :structure
                ((:dir "rec" :datetree t)
                 (:dir "ltw" :datetree t)
                 (:dir "stc" :datetree nil)))
          ) ;; static
        mvtn-template-locations '("~/sync/documents/notes/mvtn/templates")
        mvtn-default-file-extension "org"
        mvtn-excluded-directories '(".git" ".svn" "ltximg" "orgimg" "wournal" "data")
        mvtn-search-function 'mvtn-search-full-text-rg
        mvtn-cv-enable t
        mvtn-org-agenda-tag "agenda"
        mvtn-journal-dir "prv/flt"
        mvtn-journal-new-daily-title "Logbuch am %Y-%m-%d")

  (require 'mvtn-link-buttons)

  (defun fp/mvtn-minor-mode-hook ()
    (olivetti-mode 1)
    (let ((mvtn-timestamp (substring (file-name-base (buffer-file-name)) 0 15)))
      (setq-local org-download-image-dir (concat "orgimg/" mvtn-timestamp))
      (setq-local wournal-file-format (concat "wournal/" mvtn-timestamp
                                              "/%Y-%m-%d_%H-%M-%S.svg"))))

  (add-hook 'mvtn-minor-mode-hook 'fp/mvtn-minor-mode-hook)

  (mvtn-journal-autojournal-set-feature 'git-commit t)
  (mvtn-journal-autojournal-set-feature 'note-changed t)
  (mvtn-journal-autojournal-set-feature 'org-clock t)

  ;; ----------------------------------------------------------------------
  ;; org-archive
  ;; ----------------------------------------------------------------------

  ;; This /could/ make it into mvtn as a package if I end up finding it useful
  ;; enough.

  (defmacro mvtn-journal-with-org-archive-location-as-daily (&rest body)
    "Let `org-archive-location' be the current daily note."
    `(let ((org-archive-location
            (format-time-string
             (format "%s::%sOrg Archive"
                     (with-current-buffer
                         (mvtn-journal-daily-for-time (current-time))
                       (buffer-file-name))
                     (mvtn-template-for-extension
                      mvtn-journal-default-file-extension
                      mvtn-journal-entry-file-extension-templates)))))
       ,@body))

  (defun mvtn-journal-org-archive-subtree-default ()
    "Archive the current org subtree to the current daily note."
    (interactive)
    (mvtn-journal-with-org-archive-location-as-daily
     (org-archive-subtree-default)))

  (defun mvtn-journal-org-archive-all-done ()
    "Archive the current org subtree to the current daily note."
    (interactive)
    (mvtn-journal-with-org-archive-location-as-daily
     (org-archive-all-done)))

  ;; ----------------------------------------------------------------------
  ;; citing webpages
  ;; ----------------------------------------------------------------------

  (defun fp/mvtn-store-webpage-from-clipboard-markdown (encrypt)
    (interactive "P")
    "Create a note for a section of a webpage in the clipboard.
This uses the 'Copy Selection as Markdown'
addon (https://github.com/0x6b/copy-selection-as-markdown).  In
the addon settings, prepending quotes has to be disabled and
including the link enabled for this to work.  Integration of the
kill-ring with the system clipboard has to be enabled (it is by
default)."
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

  (evil-define-key 'normal mvtn-link-map "gx" 'mvtn-follow-link-at-point)

  ;; If `evil-org-mode' is not disabled, it will overshadow the binds in the
  ;; following section
  (add-hook 'mvtn-backlink-buffer-mode-hook
            (lambda () (interactive) (evil-org-mode 0)))
  (evil-define-key 'normal mvtn-backlink-buffer-mode-map
    "gj" 'mvtn-backlink-buffer-next-backlink
    (kbd "M-j") 'mvtn-backlink-buffer-next-backlink
    "gk" 'mvtn-backlink-buffer-previous-backlink
    (kbd "M-k") 'mvtn-backlink-buffer-previous-backlink
    "q" 'mvtn-backlink-buffer-toggle-side-window))


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
  "na" 'mvtn-org-agenda
  "nw" 'fp/mvtn-store-webpage-from-clipboard-markdown)


(provide 'config-mvtn)