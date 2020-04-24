(require-and-log 'config-editor)

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

(use-package helm
  :ensure t
  :config
  (helm-mode 1)

  (setq helm-completing-read-handlers-alist
        '((tmm-menubar . nil)
          (find-file . nil)
          (execute-extended-command . nil)
          (dired-do-rename . nil)
          (dired-do-copy . nil)
          (dired-do-symlink . nil)
          (dired-do-relsymlink . nil)
          (dired-do-hardlink . nil)
          (dired-create-directory . nil)))

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

  (evil-leader/set-key
    "SPC" 'helm-M-x
    "ff"  'helm-find-files
    "bb"  'helm-multi-files
    "ht"  'helm-timers
    "hp"  'helm-list-emacs-process
    "hc"  'helm-colors
    "he"  'helm-run-external-command
    "hk"  'helm-show-kill-ring
    "hr"  'helm-resume)

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
  ;; swoop
  ;; --------------------------------------------------------------------------------
  (use-package helm-swoop
    :ensure t
    :config
    (setq helm-swoop-speed-or-color t)
    (defun helm-swoop-no-prefix ()
      (interactive)
      (let ((helm-swoop-pre-input-function 'return-nil))
        (helm-swoop)))

    (global-set-key (kbd "C-s") 'helm-swoop-no-prefix)
    (evil-leader/set-key
      "ss"  'helm-swoop
      "so"  'helm-multi-swoop-org)
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop))

  ;; --------------------------------------------------------------------------------
  ;; appearance
  ;; --------------------------------------------------------------------------------

  (custom-set-faces
   '(helm-source-header ((t (:foreground nil :background nil
                                         :family nil :height 1.3
                                         :inherit font-lock-keyword-face))))))


(provide 'config-helm-minibuffer)
