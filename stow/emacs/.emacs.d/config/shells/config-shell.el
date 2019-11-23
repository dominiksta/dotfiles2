(require-and-log 'config-language-shell-script)
;; --------------------------------------------------------------------------------
;; M-x shell, comint and stuff for all shells
;; --------------------------------------------------------------------------------
;; window setup
(push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)

(add-hook 'shell-mode-hook 'goto-address-mode)
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

(defun fp/new-bash-here (&optional arg)
  (interactive "P")
  (let ((explicit-shell-file-name "/bin/bash")
        (buf (cond ((numberp arg)
		    (get-buffer-create (format "%s<%d>"
					       "*bash*"
					       arg)))
		   (t
		    (get-buffer-create "*bash*")))))
    (pop-to-buffer-same-window buf)
    (unless (derived-mode-p 'shell-mode)
      (shell buf))
    buf))

(defun fp/new-bash-here-xterm (&optional arg)
  "Calls fp/new-bash-here with TERM=xterm-256color added to the
environment. This may or may not be a good idea, since some
default bashrc files set the PS1 to something that emacs cannot
display if TERM contains xterm. But it does allow colors for many
programs such as ls."
  (interactive "P")
  (let ((process-environment
         (cons "TERM=xterm-256color" process-environment)))
    (fp/new-bash-here arg)))

;; --- bindings ---
(define-key shell-mode-map (kbd "C-l") 'comint-clear-buffer)
(evil-leader/set-key-for-mode 'shell-mode "d" 'volatile-kill-buffer)
(define-key shell-mode-map (kbd "M-d") 'volatile-kill-buffer)

(evil-define-key 'normal comint-mode-map
  "gj" 'comint-next-prompt
  "gk" 'comint-previous-prompt)

(use-package shell-pop
  :ensure t
  :defer t
  :config
  ;; (setq shell-pop-shell-type (quote ("terminal" "*terminal*<pop>" 'multi-term)))
  (setq shell-pop-shell-type (quote ("eshell" "*eshell*<pop>" 'eshell)))
  (setq shell-pop-restore-window-configuration nil)
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(defun fp/terminal-here (arg)
  (interactive "P")
  "If running on windows, then with a prefix arg, start terminal as admin"
  (if (eq system-type 'windows-nt)
      (if arg (start-process-shell-command "external-terminal" nil "nircmd elevate cmd")
        (start-process-shell-command "external-terminal" nil "start cmd"))
    (start-process-shell-command "external-terminal" nil "x-terminal-emulator")))

;; Note: send passwords with M-x `send-invisible'

(defun fp/shell-add-colors ()
  (make-local-variable 'process-environment) (setenv "TERM" "dumb-emacs-ansi")
  (setenv "COLORTERM" "1"))
;; (add-hook 'shell-mode-hook 'fp/shell-add-colors)

;; --------------------------------------------------------------------------------
;; completion
;; --------------------------------------------------------------------------------
(when (not (eq system-type 'windows-nt))
  (config-add-external-dependency 'fish 'config-eshell "eshell completion"
                                  (lambda () (executable-find "fish"))
                                  "apt install fish" "None")
  ;; only works in eshell for me
  (when (executable-find "fish")
    (use-package fish-completion
      :ensure t
      :demand t
      :config (global-fish-completion-mode)))

  ;; only works in shell for me
  (use-package bash-completion
    :ensure t
    :init (bash-completion-setup)))

(provide 'config-shell)
