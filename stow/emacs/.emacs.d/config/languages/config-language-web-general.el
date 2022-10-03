(require-and-log 'config-editor)
(require-and-log 'config-programming-general)

;; --------------------------------------------------------------------------------
;; documentation
;; --------------------------------------------------------------------------------
(defun fp/search-caniuse (start end)
  (interactive "r")
  (fp/documentation-with-browser
   start end "https://caniuse.com/#search="))
(evil-leader/set-key-for-mode 'js2-mode "msc" 'fp/search-caniuse)

(defun fp/search-angular (start end)
  (interactive "r")
  (fp/documentation-with-browser
   start end "https://angular.io/api?query="))
(evil-leader/set-key-for-mode 'typescript-mode "msa" 'fp/search-angular)

(defun fp/search-mdn (start end)
  (interactive "r")
  (fp/documentation-with-browser
   start end "https://developer.mozilla.org/en-US/search?q="))
(evil-leader/set-key-for-mode 'js2-mode "msm" 'fp/search-mdn)

;; --------------------------------------------------------------------------------
;; refresh browser
;; --------------------------------------------------------------------------------
(defvar fp/refresh-browser-name     "chrome" "Set this to `chrome' or `firefox' respectively")
(defvar fp/refresh-browser-activate t        "Wether to switch to the browser window or stay in emacs")
(defvar fp/refresh-browser-save     t        "Wether to save before refreshing the browser or not")


(defun fp/refresh-browser ()
  (interactive)
  (when fp/refresh-browser-save (save-buffer) (save-some-buffers))
  (if (eq system-type 'windows-nt)
      (call-process-shell-command "cmd /c start C:\\Users\\fp\\git\\dotfiles\\ahk\\browser-refresh.ahk")
    (progn
      ;; you cannot send keys to chrome with xdotool without activating the window
      ;; thats why i have to speperate them like this
      (if (string= fp/refresh-browser-name "firefox")
          (call-process-shell-command
           (concat "xdotool search --classname \"Navigator\" key ctrl+r"
                   (when fp/refresh-browser-activate " windowactivate")))
        (progn
          (call-process-shell-command
           "sleep 0.1; xdotool search --onlyvisible --classname \"Chromium\" windowactivate key ctrl+shift+r")
          (when (not fp/refresh-browser-activate)
            (call-process-shell-command "xdotool search \"fp@emacs\" windowactivate")))))))

;; --------------------------------------------------------------------------------
;; dap (debugging)
;; --------------------------------------------------------------------------------
(with-eval-after-load "dap-mode"
  (require 'dap-firefox))


;; --------------------------------------------------------------------------------
;; web-mode
;; --------------------------------------------------------------------------------
(straight-use-package 'web-mode) (require 'web-mode)

(setq web-mode-code-indent-offset 4
      web-mode-enable-auto-pairing nil
      web-mode-enable-auto-quoting nil
      web-mode-enable-current-element-highlight t
      web-mode-smart-quotes '("&bdquo;" . "&ldquo;"))

(straight-use-package 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(evil-define-key 'insert emmet-mode-keymap (kbd "C-,") 'emmet-expand-yas)

(setq-default web-mode-comment-formats
              '(("java" . "//")
                ("javascript" . "//")
                ("typescript" . "//")
                ("php" . "//")
                ("css" . "//")))

;; (setq-default web-mode-comment-style 2)
;; (setq-default web-mode-comment-prefixing nil)

(custom-set-faces
 '(web-mode-current-element-highlight-face ((t (:inherit highlight))))
 '(web-mode-html-tag-face ((t nil))))
(evil-leader/set-key-for-mode 'web-mode
  "ed" 'fp/refresh-browser
  "mf" 'web-mode-fold-or-unfold)
(evil-define-key 'normal web-mode-map "gt" 'web-mode-tag-match)
(define-key web-mode-map (kbd "M-R") 'web-mode-element-rename)


(straight-use-package 'apache-mode)
(straight-use-package 'nginx-mode)

(provide 'config-language-web-general)
