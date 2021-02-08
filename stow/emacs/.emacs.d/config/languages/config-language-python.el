(require-and-log 'config-programming-general)
(require 'python)

;; ----------------------------------------------------------------------
;; debugging
;; ----------------------------------------------------------------------
(setq realgud:pdb-command-name "python -m pdb")
(evil-leader/set-key-for-mode 'python-mode "mD" 'realgud:pdb)

;; ----------------------------------------------------------------------
;; flycheck
;; ----------------------------------------------------------------------
(config-add-external-dependency 'flake8 'config-python "flycheck backend"
                                (lambda () (executable-find "flake8"))
                                "pip install flake8" "pip install flake8")
(add-hook 'python-mode-hook 'flycheck-mode)

;; ----------------------------------------------------------------------
;; completion
;; ----------------------------------------------------------------------
(straight-use-package 'company-jedi)
(with-eval-after-load "company-jedi"
  (add-to-list 'company-backends 'company-jedi)

  (defun fp/python-jedi-hook ()
    (jedi:setup)
    (company-mode))
  (config-add-external-dependency 'virtualenv 'config-python "virtual envs"
                                  (lambda () (executable-find "virtualenv"))
                                  "pip3 install virtualenv" "pip3 install virtualenv")
  (config-add-external-dependency 'jedi 'config-python "autocomplete"
                                  (lambda () (file-exists-p (car jedi:server-command)))
                                  "M-x jedi:install-server" "M-x jedi:install-server")
  (when (config-external-check-list '(jedi))
    (add-hook 'python-mode-hook 'fp/python-jedi-hook)))



;; --------------------------------------------------------------------------------
;; REPL
;; --------------------------------------------------------------------------------
(add-hook 'inferior-python-mode-hook 'company-mode)

(setq python-shell-interpreter "python"
      python-shell-completion-native-enable nil)

(ignore-errors (mkdir "~/.python_venvs/"))
(setenv "WORKON_HOME" (concat (expand-file-name "~") "/.python_venvs/"))
(straight-use-package 'pyvenv)

;; --------------------------------------------------------------------------------
;; appearance
;; --------------------------------------------------------------------------------
(defun config--python-pretty-symbols ()
  (mapc (lambda (pair) (push pair prettify-symbols-alist))
        '(("in" .       #x2208)
          ("not in" .   #x2209)
          ("return" .   #x21d2)))
  (prettify-symbols-mode 1))

(add-hook 'python-mode-hook 'config--python-pretty-symbols)

;; --------------------------------------------------------------------------------
;; calculator
;; --------------------------------------------------------------------------------

(defun fp/run-python-calculator ()
  (interactive)
  (let ((process-environment
         (cons (concat "PYTHONSTARTUP="
                       sync-directory "documents/code/emacs/python-calculator/pythonstartup.py")
               process-environment))
        (buf (find-file-noselect (concat sync-directory
                                         "documents/code/emacs/python-calculator/python-calculator-worksheet.py"))))
    (eyebrowse-switch-to-window-config-8)
    (delete-other-windows)
    (switch-to-buffer buf)
    (with-current-buffer buf
      (when (not (eq major-mode 'python-mode)) (python-mode))
      (run-python (python-shell-calculate-command) t))))

;; --------------------------------------------------------------------------------
;; bindings
;; --------------------------------------------------------------------------------

(evil-leader/set-key-for-mode 'python-mode
  "eb" 'python-shell-send-buffer
  "ef" 'python-shell-send-defun
  "er" 'python-shell-send-region
  "sd" 'lsp-ui-doc-show
  "sD" 'lsp-ui-doc-hide)

(define-key inferior-python-mode-map (kbd "C-l") 'comint-clear-buffer)


(provide 'config-language-python)
