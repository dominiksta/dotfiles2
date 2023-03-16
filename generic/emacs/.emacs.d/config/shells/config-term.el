(require-and-log 'config-editor)

(setq term-prompt-regexp "^\\[.*\\]\\$ ") ;; this is specific to my bashrc

(straight-use-package 'multi-term)
(setq multi-term-program "/bin/bash")

;; --------------------------------------------------------------------------------
;; bindings
;; --------------------------------------------------------------------------------

(evil-set-initial-state 'term-mode 'emacs)

(defun fp/term-enter-emacs-state ()
  (interactive)
  (evil-emacs-state)
  (end-of-buffer))

(defun fp/term-mode-hook ()
  (setq-local evil-emacs-state-cursor 'hbar)

  (add-hook 'evil-insert-state-entry-hook 'fp/term-enter-emacs-state nil t))

(add-hook 'term-mode-hook 'fp/term-mode-hook)

(evil-define-key 'normal term-mode-map
  "i" 'fp/term-enter-emacs-state)

(define-key term-mode-map (kbd "<escape>")
            (lambda nil (interactive)
              (evil-exit-emacs-state) (evil-normal-state)))

(evil-set-initial-state 'term-mode 'emacs)

;; (defun fp/term-char-mode-and-emacs-state ()
;;   (interactive)
;;   (term-char-mode)
;;   (evil-emacs-state))

;; (defun fp/term-line-mode-and-normal-state ()
;;   (interactive)
;;   (copy-region-as-kill (term-bol nil) (line-end-position))
;;   (term-send-raw-string (kbd "C-a"))
;;   (term-send-raw-string (kbd "C-k"))
;;   (term-line-mode)
;;   (evil-normal-state)
;;   (run-at-time "0.1" nil 'yank))

;; (defun fp/term-bindings ()
;;   (setq-local evil-move-cursor-back nil)

;;   (define-key term-mode-map (kbd "C-S-z") 'fp/term-char-mode-and-emacs-state)
;;   (define-key term-raw-map (kbd "C-S-z")  'fp/term-line-mode-and-normal-state)

;;   (define-key term-mode-map (kbd "C-c C-k") 'term-char-mode)
;;   ;; (define-key term-raw-map (kbd "C-c C-j") 'term-line-mode)

;;   (define-key term-mode-map (kbd "C-c C-c") 'term-send-raw)
;;   ;; (define-key term-raw-map (kbd "C-c C-c") 'term-send-raw)

;;   (define-key term-raw-map (kbd "M-o") nil)
;;   (define-key term-mode-map (kbd "M-o") nil)

;;   (define-key term-raw-map (kbd "<C-left>") 'term-send-backward-word)
;;   (define-key term-raw-map (kbd "<C-right>") 'term-send-forward-word)
;;   (define-key term-raw-map (kbd "<M-DEL>") 'term-send-backward-kill-word)
;;   (define-key term-raw-map (kbd "<C-DEL>") 'term-send-backward-kill-word)
;;   (define-key term-raw-map (kbd "<C-a>") 'term-send-home)
;;   (define-key term-raw-map (kbd "<C-e>") 'term-send-end)
;;   (define-key term-raw-map (kbd "C-r") 'term-send-reverse-search-history)
;;   (define-key term-raw-map (kbd "<C-delete>") 'term-send-forward-kill-word)
;;   (define-key term-raw-map (kbd "<M-delete>") 'term-send-forward-kill-word)
;;   (define-key term-raw-map (kbd "C-n") 'term-send-raw)
;;   (define-key term-raw-map (kbd "C-p") 'term-send-raw)
;;   (evil-define-key 'emacs term-raw-map (kbd "<escape>") 'term-send-esc)
;;   ;; (define-key term-raw-map (kbd "<f1>") 'term-send-raw)
;;   ;; (define-key term-raw-map (kbd "<f2>") 'term-send-raw)
;;   ;; (define-key term-raw-map (kbd "<f3>") 'term-send-raw)
;;   ;; (define-key term-raw-map (kbd "<f4>") 'term-send-raw)
;;   ;; (define-key term-raw-map (kbd "<f5>") 'term-send-raw)
;;   ;; (define-key term-raw-map (kbd "<f6>") 'term-send-raw)
;;   ;; (define-key term-raw-map (kbd "<f7>") 'term-send-raw)
;;   ;; (define-key term-raw-map (kbd "<f8>") 'term-send-raw)
;;   ;; (define-key term-raw-map (kbd "<f9>") 'term-send-raw)
;;   (evil-define-key 'insert term-raw-map (kbd "<delete>") 'term-send-del)
;;   (evil-define-key 'normal term-raw-map (kbd "p") 'term-paste)

;;   (evil-define-key 'normal term-mode-map
;;     "0" 'term-bol
;;     (kbd "C-l") (lambda () (interactive) (term-send-raw-string "clear"))
;;     (kbd "M-n") 'term-next-input
;;     (kbd "M-p") 'term-previous-input
;;     (kbd "RET") 'term-send-input))


;; (add-hook 'term-mode-hook 'fp/term-bindings)


(provide 'config-term)
