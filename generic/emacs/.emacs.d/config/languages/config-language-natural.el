;; ----------------------------------------------------------------------
;; hunspell backend for multiple dictionaries
;; ----------------------------------------------------------------------

(config-add-external-dependency 'hunspell 'config-natural-language
                                "spellchecking base"
                                (lambda () (executable-find "hunspell"))
                                "apt install hunspell"
                                "cinst -y hunspell.portable --version=1.3.2.300"
                                ;; the newer versions on windows do not
                                ;; correctly report installed dictionaries
                                )

(config-add-external-dependency 'hunspell-de-de 'config-natural-language
                                "spellchecking base"
                                (lambda () (string-match-p
                                       "de_DE" (shell-command-to-string
                                                            "hunspell -D")))
                                "apt install hunspell-de-de"
                                "wget -O de_DE.aff https://cgit.freedesktop.org/libreoffice/dictionaries/plain/de/de_DE_frami.aff;
wget -O de_DE.dic https://cgit.freedesktop.org/libreoffice/dictionaries/plain/de/de_DE_frami.dic;" ;; install from libreoffice extensions
                                )

(config-add-external-dependency 'hunspell-en-us 'config-natural-language
                                "spellchecking base"
                                (lambda () (string-match-p
                                       "en_US" (shell-command-to-string
                                                "hunspell -D")))
                                "apt install hunspell-en-us"
                                "wget -O en_US.aff https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.aff;
wget -O en_US.dic https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.dic;" ;; install from libreoffice extensions
                                )

(when (config-external-check-list '(hunspell hunspell-de-de hunspell-en-us))


  ;; Sets ispell up to ignore html markup.
  (setq-default ispell-skip-html t)

  (setq ispell-program-name "hunspell"
        ispell-dictionary (concat "en_US," "de_DE"))
  ;; ispell-set-spellchecker-params has to be called before
  ;; ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic (concat "en_US," "de_DE"))


  ;; ----------------------------------------------------------------------
  ;; flyspell
  ;; ----------------------------------------------------------------------

  (defun fp/toggle-flyspell-check-buffer ()
    (interactive)
    (if (bound-and-true-p flyspell-mode)
        (flyspell-mode 0)
      (progn
        (if (derived-mode-p 'prog-mode)
            (flyspell-prog-mode 1))
        (flyspell-mode 1)
        (flyspell-buffer))))

  (straight-use-package 'helm-flyspell)
  (evil-define-key 'normal global-map "zg" 'helm-flyspell-correct))

;; ----------------------------------------------------------------------
;; translations with dict.cc
;; ----------------------------------------------------------------------

(straight-use-package 'dictcc)
(setq dictcc-completion-backend 'helm)

(provide 'config-language-natural)


;; ----------------------------------------------------------------------
;; old aspell
;; ----------------------------------------------------------------------
;; (config-add-external-dependency 'aspell 'config-natural-language "spellchecking base"
;;                                 (lambda () (executable-find "aspell"))
;;                                 "apt install aspell" "cyg-get aspell")

;; (config-add-external-dependency 'aspell-de 'config-natural-language "spellchecking german"
;;                                 (lambda () (string-match-p
;;                                        "de" (shell-command-to-string "aspell dicts")))
;;                                 "apt install aspell-de" "cyg-get aspell-de")

;; (config-add-external-dependency 'aspell-en 'config-natural-language "spellchecking english"
;;                                 (lambda () (string-match-p
;;                                        "en" (shell-command-to-string "aspell dicts")))
;;                                 "apt install aspell-en" "cyg-get aspell-en")
;; (defun ispell-toggle-dictionary()
;;   (interactive)
;;   (let* ((dic ispell-current-dictionary)
;;          (change (if (string= dic "german") "english" "german")))
;;     (ispell-change-dictionary change)
;;     (message "Dictionary switched from %s to %s" dic change)))
