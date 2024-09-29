(require-and-log 'config-editor)
(require 'term)
(require 'seq)
(require 'tramp)

(setq
 ;; dir %
 term-prompt-regexp "^.*\\% "  ;; this is specific to my bashrc
 term-scroll-snap-to-bottom t)

(straight-use-package 'multi-term)
(straight-use-package 'tramp-term)
(setq multi-term-program "/bin/bash")

;; --------------------------------------------------------------------------------
;; bindings
;; --------------------------------------------------------------------------------

(evil-set-initial-state 'term-mode 'emacs)

(defun fp/term-enter-emacs-state ()
  (interactive)
  ;; (message "enter emacs state")
  (term-char-mode)
  (evil-emacs-state)
  (end-of-buffer))

(defun fp/term-enter-normal-state ()
  (interactive)
  ;; (message "enter normal state")
  (term-line-mode)
  (evil-normal-state))

(defun fp/term-mode-hook ()
  (setq-local evil-emacs-state-cursor 'hbar)
  (setq-local evil-move-cursor-back nil)
  (add-hook 'evil-insert-state-exit-hook 'term-line-mode nil t)
  (add-hook 'evil-insert-state-entry-hook 'term-char-mode nil t))

(add-hook 'term-mode-hook 'fp/term-mode-hook)

(defun fp/term-maybe-rename-buffer (_dir)
  (interactive)
  (unless (not (eq major-mode 'term-mode))
    (let* ((split (string-split (file-name-directory default-directory) "/"))
           (pwd (nth (- (length split) 2) split))
           (buffers-same-name
            (seq-filter (lambda (buf)
                          (and (eq (with-current-buffer buf major-mode) 'term-mode)
                               (string-prefix-p (format "*term:%s" pwd)
                                                (buffer-name buf))))
                        (buffer-list))))
      (rename-buffer (format "*term:%s*" pwd) t))))

(advice-add 'cd :after 'fp/term-maybe-rename-buffer)
;; (advice-remove 'cd 'fp/term-maybe-rename-buffer)

(evil-define-key 'normal term-mode-map
  "i" 'fp/term-enter-emacs-state
  "I" 'fp/term-enter-emacs-state
  "a" 'fp/term-enter-emacs-state
  "A" 'fp/term-enter-emacs-state
  "o" 'fp/term-enter-emacs-state
  "O" 'fp/term-enter-emacs-state
  "gj" 'term-next-prompt
  "gk" 'term-previous-prompt
  (kbd "M-j") 'term-next-prompt
  (kbd "M-k") 'term-previous-prompt
  ;; (kbd "C-l") (lambda () (interactive) (recenter 0))
  (kbd "C-l") 'term-send-raw
  ;; (kbd "C-l") (lambda () (interactive) (term-send-raw-string "clear"))
  )

;; HACK: This really should not be global but i cant seem to get it 
;; (evil-define-key '(emacs insert motion) global-map
;;   (kbd "<escape>") 'fp/term-enter-normal-state
;;   )

(evil-set-initial-state 'term-mode 'emacs)

(evil-define-key 'emacs term-raw-map
  (kbd "C-n") 'term-send-raw
  (kbd "C-p") 'term-send-raw
  (kbd "C-r") 'term-send-reverse-search-history
  ;; (kbd "C-l") (lambda () (interactive) (recenter 0))
  (kbd "C-l") 'term-send-raw
  (kbd "<C-left>") 'term-send-backward-word
  (kbd "<C-right>") 'term-send-forward-word
  (kbd "<M-DEL>") 'term-send-backward-kill-word
  (kbd "<C-DEL>") 'term-send-backward-kill-word
  (kbd "C-S-v") 'term-paste
  (kbd "C-S-c") 'copy-region-as-kill
  (kbd "C-c C-j") 'term-send-esc
  (kbd "C-c C-c") 'term-interrupt-subjob
  (kbd "<escape>") 'fp/term-enter-normal-state

  ;; (kbd "<f1>") 'term-send-raw
  ;; (kbd "<f2>") 'term-send-raw
  ;; (kbd "<f3>") 'term-send-raw
  ;; (kbd "<f4>") 'term-send-raw
  ;; (kbd "<f5>") 'term-send-raw
  ;; (kbd "<f6>") 'term-send-raw
  ;; (kbd "<f7>") 'term-send-raw
  ;; (kbd "<f8>") 'term-send-raw
  ;; (kbd "<f9>") 'term-send-raw
  )

(evil-define-key 'normal term-mode-map
  "p" 'term-paste
  )

(defun fp/term-new ()
  (interactive)
  (if (tramp-tramp-file-p default-directory)
      (tramp-term) (multi-term)))

(defun fp/term-switch ()
  (interactive)
  (let* ((buffers
          (seq-filter (lambda (buf) (and (eq (with-current-buffer buf major-mode)
                                        'term-mode)))
                      (buffer-list))))
    (if (eq (length buffers) 0)
        (multi-term)
      (switch-to-buffer (completing-read "Switch to: "
                                         (mapcar 'buffer-name buffers))))))

;; (define-key term-raw-map (kbd "<f1>") 'term-send-raw)
;; (define-key term-raw-map (kbd "<f2>") 'term-send-raw)
;; (define-key term-raw-map (kbd "<f3>") 'term-send-raw)
;; (define-key term-raw-map (kbd "<f4>") 'term-send-raw)
;; (define-key term-raw-map (kbd "<f5>") 'term-send-raw)
;; (define-key term-raw-map (kbd "<f6>") 'term-send-raw)
;; (define-key term-raw-map (kbd "<f7>") 'term-send-raw)
;; (define-key term-raw-map (kbd "<f8>") 'term-send-raw)
;; (define-key term-raw-map (kbd "<f9>") 'term-send-raw)

(provide 'config-term)
