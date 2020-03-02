(require-and-log 'config-language-shell-script)
(require-and-log 'config-shell-switch)
;; --------------------------------------------------------------------------------
;; M-x shell, comint and stuff for all shells
;; --------------------------------------------------------------------------------

;; leave prompt highlighting to the shell
(custom-set-faces '(comint-highlight-prompt ((t nil))))

;; window setup
(push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)

(add-hook 'shell-mode-hook 'goto-address-mode)
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

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
