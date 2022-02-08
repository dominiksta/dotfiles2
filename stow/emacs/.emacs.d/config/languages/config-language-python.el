(require-and-log 'config-programming-general)
(require 'python)

;; ----------------------------------------------------------------------
;; debugging
;; ----------------------------------------------------------------------
(setq realgud:pdb-command-name "python -m pdb")
(evil-leader/set-key-for-mode 'python-mode "mD" 'realgud:pdb)

;; ----------------------------------------------------------------------
;; completion/lsp
;; ----------------------------------------------------------------------
;; (straight-use-package 'company-jedi)
(config-add-external-dependency
 'pylsp 'config-python "lsp" (lambda () (executable-find "pylsp"))
 "pip3 install python-lsp-server[all]" "pip3 install python-lsp-server[all]")

;; npm install -g pyright
(when (config-external-check-list '(pylsp))
  (setq lsp-pylsp-plugins-flake8-enabled nil
        lsp-pylsp-plugins-pylint-enabled nil
        lsp-pylsp-plugins-pyflakes-enabled t
        lsp-pylsp-plugins-autopep8-enabled nil
        lsp-pylsp-plugins-pydocstyle-enabled nil)
  (add-hook 'python-mode-hook 'lsp))

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
;; calculator
;; --------------------------------------------------------------------------------

(defun fp/run-python-calculator ()
  (interactive)
  (let ((process-environment
         (cons (concat "PYTHONSTARTUP="
                       sync-directory "documents/code/emacs/python-calculator/pythonstartup.py")
               process-environment))
        (buf (find-file-noselect (concat sync-directory
                                         "documents/code/emacs/python-calculator/worksheet.py"))))
    (eyebrowse-switch-to-window-config-0)
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
