;; --------------------------------------------------------------------------------
;; load this all the time
;; --------------------------------------------------------------------------------
(require-and-log 'config-helpers)
(if (eq system-type 'windows-nt) (require 'config-windows))
(require-and-log 'config-ui)
(require-and-log 'config-modeline)
(require-and-log 'config-editor)
(require-and-log 'config-search)
(require-and-log 'config-global-binds)
(require-and-log 'config-helm-minibuffer)
(require-and-log 'config-org)
(require-and-log 'config-org-agenda)
(require-and-log 'config-random)
(require-and-log 'config-window-management)
(require-and-log 'config-dired)
(require-and-log 'config-git)
(require-and-log 'config-programming-general)
(require-and-log 'config-mvtn)
(require-and-log 'config-term)


;; --------------------------------------------------------------------------------
;; programming
;; --------------------------------------------------------------------------------
(config-require '(config-language-elisp)  :feature elisp-mode)
(autoload 'fp/run-python-calculator "config-language-python.el")
(config-require '(config-language-python) :feature python)
(config-require '(config-language-python) :feature pyvenv)

(config-require '(config-language-yaml)         :regexp '("yml" "yaml" "prefab" "meta")   :auto-mode yaml-mode)
(config-require '(config-language-lua)          :regexp "\\.lua\\'"       :auto-mode lua-mode)
(config-require '(config-language-nasm)         :regexp "\\.n?asm\\'"     :auto-mode nasm-mode)
(config-require '(config-language-rust)           :regexp '("rs")         :auto-mode rust-mode)
(config-require '(config-language-cc)           :regexp '("[ch]pp" "ino") :auto-mode c++-mode)
(config-require '(config-language-cc)           :regexp "\\.pro\\'"       :auto-mode makefile-mode)
(config-require '(config-language-cc)           :regexp "\\.[ch]\\'"      :auto-mode c-mode)
(config-require '(config-language-csharp)       :regexp "\\.cs\\'"        :auto-mode csharp-mode)
(config-require '(config-language-go)           :regexp "\\.go\\'"        :auto-mode go-mode)
(config-require '(config-language-java)         :regexp "\\.java\\'"      :auto-mode java-mode)
(config-require '(config-language-scala)        :regexp "\\.scala\\'"    :auto-mode scala-mode)
(config-require '(config-language-clojure)      :regexp "\\.clj\\'"       :auto-mode clojure-mode)
(config-require '(config-language-ahk)          :regexp "\\.ahk\\'"       :auto-mode ahk-mode)
(config-require '(config-language-markdown)     :regexp "\\.md\\'"        :auto-mode markdown-mode)
(config-require '(config-language-csv)          :regexp "\\.csv\\'"       :auto-mode csv-mode)
(config-require '(config-language-shell-script) :regexp "\\.sh\\'"        :auto-mode sh-mode)
(config-require '(config-language-powershell)   :regexp "\\.ps1\\'"       :auto-mode powershell-mode)
(config-require '(config-language-web-json)     :regexp '("json?" "cplan") :auto-mode json-mode)
(config-require '(config-language-web-html)     :regexp '("html?" "xml")  :auto-mode web-mode)
(config-require '(config-language-web-php
                  config-language-sql)          :regexp "\\.php\\'"       :auto-mode web-mode)
(config-require '(config-language-web-php
                  config-language-sql)          :regexp "\\.sql\\'"       :auto-mode sql-mode)
(config-require '(config-web-rest)              :regexp "\\.http\\'"      :auto-mode restclient-mode)
(config-require '(config-language-web-tide)     :regexp '("tsx")          :auto-mode web-tide-mode)
(config-require '(config-language-web-tide)     :regexp '("ts")           :auto-mode typescript-mode)
(config-require '(config-language-web-tide)     :regexp '("jsx?" "mjs")   :auto-mode js-jsx-mode)
(config-require '(config-language-web-svelte)   :regexp '("svelte")       :auto-mode web-svelte-mode)
(config-require '(config-language-web-css)      :regexp "\\.css\\'"       :auto-mode css-mode)
(config-require '(config-language-web-css)      :regexp "\\.scss\\'"      :auto-mode scss-mode)
(config-require '(config-language-web-stylus)   :regexp "\\.styl\\'"      :auto-mode stylus-mode)
(config-require '(config-language-octave) :regexp "\\.m\\'" :auto-mode octave-mode
                :feature octave)
(config-require '(config-language-latex)        :feature tex)
(config-require '(config-language-docker)
                :regexp "Dockerfile\\(?:\\..*\\)?\\'"
                :auto-mode dockerfile-mode)

;; systemd
(add-to-list 'auto-mode-alist '("\\.target\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-mode))
;; mysql
(add-to-list 'auto-mode-alist '("\\.cnf\\'" . conf-mode))
;; gettext
(add-to-list 'auto-mode-alist '("\\.pot?\\'" . conf-mode))


;; --------------------------------------------------------------------------------
;; applications
;; --------------------------------------------------------------------------------
(config-require '(config-pdf-tools) :regexp "\\.pdf?\\'"  :auto-mode pdf-view-mode)
(config-require '(config-nov)       :regexp "\\.epub?\\'" :auto-mode nov-mode)
(autoload 'image-dired-my-window-config "config-images.el")
(autoload 'image-dired-no-window-config "config-images.el")
(config-require '(config-images)  :feature image-mode)
(config-require '(config-ibuffer) :feature ibuffer)
(config-require '(config-proced)  :feature proced)
(config-require '(config-calc)    :feature calc)
(config-require '(config-vc)      :feature vc)
(config-require '(config-org)     :feature calendar)
(config-require '(config-tramp)   :feature tramp)
(config-require '(config-org-tree-slide) :feature org-tree-slide)
;; (config-require '(config-keepassxc-cli)  :feature keepassxc-cli) (require 'keepassxc-cli)

(setq gnus-directory "~/.emacs.d/News") ;; needs to be set before gnus is loaded
(config-require '(config-gnus) :feature gnus)



;; ispell gets loaded with evil no matter what
(config-require '(config-language-natural) :feature ispell)


;; --- shells ---
(config-require '(config-term)   :feature term)
(config-require '(config-eshell) :feature eshell)
(config-require '(config-shell)  :feature shell)


;; --------------------------------------------------------------------------------
;; finishing
;; --------------------------------------------------------------------------------
(require-and-log 'config-session)
(message (concat "Startup in: " (emacs-init-time)))
(provide 'init-default)
