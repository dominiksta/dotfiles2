;; --------------------------------------------------------------------------------
;; load this all the time
;; --------------------------------------------------------------------------------
(require-and-log 'config-helpers)
(require-and-log 'config-ui)
(require-and-log 'config-modeline)
(require-and-log 'config-editor)
(require-and-log 'config-global-binds)
(require-and-log 'config-helm-minibuffer)
(require-and-log 'config-org)
(require-and-log 'config-org-agenda)
(require-and-log 'config-random)
(require-and-log 'config-session)
(require-and-log 'config-window-management)
(require-and-log 'config-dired)
(require-and-log 'config-git)
(require-and-log 'config-search)
(require-and-log 'config-programming-general)


;; --------------------------------------------------------------------------------
;; programming
;; --------------------------------------------------------------------------------
(config-require '(config-language-elisp)  :feature elisp-mode)
(config-require '(config-language-python) :feature python)
(config-require '(config-language-python) :feature pyvenv)

(config-require '(config-language-cc)           :regexp '("[ch]pp" "ino") :auto-mode c++-mode)
(config-require '(config-language-cc)           :regexp "\\.pro\\'"       :auto-mode makefile-mode)
(config-require '(config-language-cc)           :regexp "\\.[ch]\\'"      :auto-mode c-mode)
(config-require '(config-language-go)           :regexp "\\.go\\'"        :auto-mode go-mode)
(config-require '(config-language-java)         :regexp "\\.java\\'"      :auto-mode java-mode)
(config-require '(config-language-clojure)      :regexp "\\.clj\\'"       :auto-mode clojure-mode)
(config-require '(config-language-ahk)          :regexp "\\.ahk\\'"       :auto-mode ahk-mode)
(config-require '(config-language-markdown)     :regexp "\\.md\\'"        :auto-mode markdown-mode)
(config-require '(config-language-shell-script) :regexp "\\.sh\\'"        :auto-mode sh-mode)
(config-require '(config-language-powershell)   :regexp "\\.ps1\\'"       :auto-mode powershell-mode)
(config-require '(config-language-web-js)       :regexp "\\.jsm?\\'"      :auto-mode js2-mode)
(config-require '(config-language-web-json)     :regexp "\\.json?\\'"     :auto-mode json-mode)
(config-require '(config-language-web-html)     :regexp '("html?" "xml")  :auto-mode web-mode)
(config-require '(config-language-web-php
                  config-language-sql)          :regexp "\\.php\\'"       :auto-mode php-mode)
(config-require '(config-language-web-php
                  config-language-sql)          :regexp "\\.sql\\'"       :auto-mode sql-mode)
(config-require '(config-web-rest)              :regexp "\\.http\\'"      :auto-mode restclient-mode)
(config-require '(config-language-web-ts)       :regexp "\\.ts\\'"        :auto-mode typescript-mode)
(config-require '(config-language-web-css)      :regexp "\\.css\\'"       :auto-mode css-mode)
(config-require '(config-language-web-css)      :regexp "\\.scss\\'"      :auto-mode scss-mode)
(config-require '(config-language-octave) :regexp "\\.m\\'" :auto-mode octave-mode
                :feature octave)
(config-require '(config-language-latex)        :feature tex)
(config-require '(config-language-docker)
                :regexp "Dockerfile\\(?:\\..*\\)?\\'"
                :auto-mode dockerfile-mode)

(setq auto-mode-alist (append auto-mode-alist '(("\\.target\\'" . conf-mode)
                                                ("\\.timer\\'" . conf-mode)
                                                ("\\.service\\'" . conf-mode))))


;; --------------------------------------------------------------------------------
;; applications
;; --------------------------------------------------------------------------------
(config-require '(config-pdf-tools) :regexp "\\.pdf?\\'"  :auto-mode pdf-view-mode)
(autoload 'image-dired-my-window-config "config-images.el")
(autoload 'image-dired-no-window-config "config-images.el")
(config-require '(config-images)  :feature image-mode)
(config-require '(config-ibuffer) :feature ibuffer)
(config-require '(config-proced)  :feature proced)
(config-require '(config-calc)    :feature calc)
(autoload 'emms-add-youtube-url "config-emms.el")
(config-require '(config-emms)    :feature emms)
(config-require '(config-org-tree-slide)    :feature org-tree-slide)
(config-require '(config-org)     :feature calendar)
(config-require '(config-elfeed)  :feature elfeed)
(config-require '(config-tramp)   :feature tramp)
;; (config-require '(config-epass)   :feature epass) (require 'epass nil t)
(config-require '(config-keepassxc-cli)   :feature keepassxc-cli) (require 'keepassxc-cli)
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
(if (eq system-type 'windows-nt) (require 'config-windows))
(message (concat "Startup in: " (emacs-init-time)))
(provide 'init-default)
