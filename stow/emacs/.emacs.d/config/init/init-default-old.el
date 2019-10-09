;; --------------------------------------------------------------------------------
;; load this all the time
;; --------------------------------------------------------------------------------
(require-after-feature-and-log nil 'config-helpers)
(require-after-feature-and-log nil 'config-ui)
(require-after-feature-and-log nil 'config-modeline)
(require-after-feature-and-log nil 'config-editor)
(require-after-feature-and-log nil 'config-global-binds)
(require-after-feature-and-log nil 'config-helm-minibuffer)
(require-after-feature-and-log nil 'config-org)
(require-after-feature-and-log nil 'config-org-agenda)
(require-after-feature-and-log nil 'config-random)
(require-after-feature-and-log nil 'config-session)
(require-after-feature-and-log nil 'config-window-management)
(require-after-feature-and-log nil 'config-dired)
(require-after-feature-and-log nil 'config-git)
(require-after-feature-and-log nil 'config-search)
(require-after-feature-and-log nil 'config-language-natural)
(require-after-feature-and-log nil 'config-programming-general)


;; --------------------------------------------------------------------------------
;; defer loading of this until something happens
;; --------------------------------------------------------------------------------
;; --- programming ---
(require-after-feature-and-log "elisp-mode" 'config-language-elisp)
(require-after-feature-and-log "python"     'config-language-python)
(require-after-feature-and-log "pyvenv"     'config-language-python)
(require-after-feature-and-log "tramp"      'config-tramp)

(add-to-list 'auto-mode-alist '("\\.pro\\'" . makefile-mode))
(require-after-extension-and-log '("[ch]pp" "ino") '(config-language-cc)        c++-mode)
(require-after-extension-and-log "\\.[ch]\\'"   '(config-language-cc)           c-mode)
(require-after-extension-and-log "\\.java\\'"   '(config-language-java)         java-mode)
(require-after-extension-and-log "\\.clj\\'"    '(config-language-clojure)      clojure-mode)
(require-after-extension-and-log "\\.ahk\\'"    '(config-language-ahk)          ahk-mode)
(require-after-extension-and-log "\\.md\\'"     '(config-language-markdown)     markdown-mode)
(require-after-extension-and-log "\\.sh\\'"     '(config-language-shell-script) sh-mode)
(require-after-extension-and-log "\\.m\\'"      '(config-language-octave)       octave-mode)

(require-after-extension-and-log "\\.jsm?\\'"      '(config-language-web-js)   js2-mode)
(require-after-extension-and-log "\\.json?\\'"     '(config-language-web-json) json-mode)
(require-after-extension-and-log "\\.html?\\'"     '(config-language-web-html) web-mode)
(require-after-extension-and-log "\\.php\\'"       '(config-language-web-php config-language-sql) php-mode)
(require-after-extension-and-log "\\.sql\\'"       '(config-language-web-php config-language-sql) sql-mode)
(require-after-extension-and-log "\\.http\\'"      '(config-web-rest)         restclient-mode)
(require-after-extension-and-log "\\.ts\\'"        '(config-language-web-ts)  typescript-mode)

;; --- applications ---
(require-after-extension-and-log "\\.pdf?\\'"   '(config-pdf-tools)         pdf-view-mode)
(autoload 'image-dired-my-window-config "config-images.el")
(autoload 'image-dired-no-window-config "config-images.el")
(require-after-feature-and-log "image-mode" 'config-images) ; apparently this is loaded by default
(require-after-feature-and-log "ibuffer"    'config-ibuffer)
(require-after-feature-and-log "proced"     'config-proced)
(require-after-feature-and-log "calc"       'config-calc)
(autoload 'emms-add-youtube-url "config-emms.el")
(require-after-feature-and-log "emms"       'config-emms)
(require-after-feature-and-log "elfeed"     'config-elfeed)

;; --- shells ---
(require-after-feature-and-log "term"       'config-term)
(require-after-feature-and-log "eshell"     'config-eshell)
(require-after-feature-and-log "shell"      'config-shell)




;; --------------------------------------------------------------------------------
;; finishing
;; --------------------------------------------------------------------------------
(if (eq system-type 'windows-nt) (require 'config-windows))
(message (concat "Startup in: " (emacs-init-time)))
(provide 'init-default)
