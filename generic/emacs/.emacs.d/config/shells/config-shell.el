(require 'shell)

(require-and-log 'config-language-shell-script)
(require-and-log 'config-shell-switch)

;; --------------------------------------------------------------------------------
;; M-x shell, comint and stuff for all shells
;; --------------------------------------------------------------------------------

;; leave prompt highlighting to the shell
(set-face-foreground 'comint-highlight-prompt nil)
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

(defun fp/terminal-here (arg)
  (interactive "P")
  (if (eq system-type 'windows-nt)
      (start-process-shell-command
       "external-terminal" nil
       (format "wt -w 0 powershell -NoExit -Command \"cd %s\"" default-directory))
    (start-process-shell-command "external-terminal" nil "xterm-tmux-new-window.sh")))

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
    (straight-use-package 'fish-completion)
    (require 'fish-completion)
    (global-fish-completion-mode))

  ;; only works in shell for me
  (straight-use-package 'bash-completion)
  (bash-completion-setup))

(provide 'config-shell)
