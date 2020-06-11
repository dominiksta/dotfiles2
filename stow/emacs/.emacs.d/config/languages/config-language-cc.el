(require 'cc-mode)
(require-and-log 'config-programming-general)
(require-and-log 'config-search)

(setq-default c-basic-offset 4)

;; --- documentation ---
(defun fp/search-msdn (start end)
  (interactive "r")
  (fp/documentation-with-browser
   start end "https://social.msdn.microsoft.com/Search/en-US?query="))

(defun fp/search-cppreference (start end)
  (interactive "r")
  (fp/documentation-with-browser
   start end "http://en.cppreference.com/mwiki/index.php?&search="))


;; --- bindings ---
(evil-leader/set-key-for-mode 'c++-mode
  "mt" (lambda () (interactive) (ff-find-other-file nil 1))
  "msm" 'fp/search-msdn
  "msl" 'fp/search-cppreference)

;; --- comments ---
(add-hook 'c-mode-common-hook
          (lambda () (setq comment-start "// "
                      comment-end "")))

;; --- lsp ---
(use-package cquery :ensure t
  :if (not (eq system-type 'window-nt))
  :demand t
  :config
  (config-add-external-dependency 'cmake 'config-language-cc "required for cquery"
                                  (lambda () (executable-find "cmake"))
                                  "apt install cmake" "None")
  (config-add-external-dependency 'gcc 'config-language-cc "required for cquery"
                                  (lambda () (executable-find "gcc"))
                                  "apt install gcc" "None")
  (config-add-external-dependency 'cquery 'config-language-cc "lsp"
                                  (lambda () (file-exists-p "~/git/cquery/build/release/bin/cquery"))
                                  "~/git/dotfiles/install/cquery.sh" "None")

  (setq cquery-executable "~/git/cquery/build/release/bin/cquery")
  (add-hook 'c-mode-common-hook 'lsp))


;; --- cmake ---
(use-package cmake-mode :ensure t
  :config (add-hook 'cmake-mode-hook 'company-mode))

;; -- other ---
(add-hook 'c-mode-common-hook 'flycheck-mode)
(add-hook 'c-mode-common-hook 'company-mode)

(provide 'config-language-cc)
