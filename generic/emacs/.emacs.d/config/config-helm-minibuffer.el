(require-and-log 'config-editor)
(require-and-log 'config-window-management)

(straight-use-package 'helm)
(require 'helm)

(defun evil-minibuffer-setup ()
  "Initialize minibuffer for `evil'."
  (evil-insert-state)

  (evil-define-key 'normal minibuffer-local-completion-map (kbd "RET") 'exit-minibuffer)
  (evil-define-key 'normal minibuffer-local-map (kbd "RET") 'exit-minibuffer)
  (evil-define-key 'insert evil-ex-completion-map (kbd "C-p") 'previous-complete-history-element)
  (evil-define-key 'insert evil-ex-completion-map (kbd "C-n") 'next-complete-history-element)
  (evil-define-key 'normal evil-ex-completion-map (kbd "C-p") 'previous-history-element)
  (evil-define-key 'normal evil-ex-completion-map (kbd "C-n") 'next-history-element))
(add-hook 'minibuffer-setup-hook 'evil-minibuffer-setup)

(setq helm-completing-read-handlers-alist
      '((tmm-menubar . nil)
        (find-file . nil)
        (org-set-tags-command . nil)
        (execute-extended-command . nil)
        (dired-do-rename . nil)
        (dired-do-copy . nil)
        (dired-do-symlink . nil)
        (dired-do-relsymlink . nil)
        (dired-do-hardlink . nil)
        (dired-create-directory . nil)
        (dired-rsync . nil))
      helm-mode-handle-completion-in-region nil)

(helm-mode 1)

(setq helm-split-window-inside-p nil)

;; --------------------------------------------------------------------------------
;; bindings
;; --------------------------------------------------------------------------------
(global-set-key (kbd "M-x") 'helm-M-x)
(define-key helm-map (kbd "RET") 'helm-maybe-exit-minibuffer)
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
(define-key helm-map (kbd "M-j") 'helm-next-line)
(define-key helm-map (kbd "M-k") 'helm-previous-line)

(defun fp/helm-enlarge-window (n)
  (interactive)
  (with-helm-window
    (enlarge-window n)))

(evil-define-key 'normal helm-map
  "j" 'helm-next-line
  "k" 'helm-previous-line
  (kbd "C-j") 'helm-next-page
  (kbd "C-k") 'helm-previous-page
  (kbd "<tab>") 'helm-select-action
  (kbd "RET") 'helm-maybe-exit-minibuffer
  "l" (lambda () (interactive) (evil-forward-char 1 nil t))
  "h" (lambda () (interactive) (evil-backward-char 1 nil t))
  "o" 'helm-execute-persistent-action
  "gg" 'helm-beginning-of-buffer
  "G" 'helm-end-of-buffer

  "(" (lambda () (interactive) (fp/helm-enlarge-window 5))
  ")" (lambda () (interactive) (fp/helm-enlarge-window -5))

  "t" 'helm-toggle-all-marks
  "m" 'helm-toggle-visible-mark
  "U" 'helm-unmark-all)

;; mainly for previewing buffers (no idea what it does besides)
(define-key helm-map (kbd "C-l") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "C-l") 'helm-execute-persistent-action)
(define-key helm-generic-files-map (kbd "C-l") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "C-l") 'helm-execute-persistent-action)

(define-key helm-map (kbd "M-l") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "M-l") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "M-h") 'helm-find-files-up-one-level)
(define-key helm-generic-files-map (kbd "M-l") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "M-l") 'helm-execute-persistent-action)

;; --------------------------------------------------------------------------------
;; occur
;; --------------------------------------------------------------------------------
(defun fp/maybe-buffer-substring-no-properties (start end)
  (if (> (count-lines (point-min) (point-max)) 10000)
      (buffer-substring-no-properties start end)
    (buffer-substring start end)))

(setq helm-occur-buffer-substring-default-mode 'fp/maybe-buffer-substring-no-properties)

(defvar fp/occur-from-helm-occur-last "")
(defun fp/occur-from-helm-occur ()
  (interactive)
  (setq fp/occur-from-helm-occur-last (minibuffer-contents-no-properties))
  (helm-exit-and-execute-action
   (lambda (_candidate) (occur fp/occur-from-helm-occur-last))))

(define-key helm-occur-map (kbd "C-s") 'fp/occur-from-helm-occur)

;; --------------------------------------------------------------------------------
;; appearance
;; --------------------------------------------------------------------------------

(setq helm-display-header-line nil)
(setq helm-mode-line-string "")
(setq helm-buffer-max-length 100)

(setq helm-display-function 'pop-to-buffer) ; make helm play nice with shackle
(push '("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.3) shackle-rules)

(custom-set-faces
 '(helm-source-header ((t (:foreground nil :background nil
                                       :family nil :height 1.0
                                       :inherit font-lock-keyword-face)))))


(provide 'config-helm-minibuffer)
