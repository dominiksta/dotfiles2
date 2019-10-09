;; --------------------------------------------------------------------------------
;; linux only
;; --------------------------------------------------------------------------------
(when (not (eq system-type 'windows-nt))

  (config-add-external-dependency 'xcape 'config-external "keybinds"
                                  (lambda () (executable-find "xcape"))
                                  "apt install xcape" "None")

  (config-add-external-dependency 'redshift 'config-external "reduce eyestrain"
                                  (lambda () (executable-find "redshift"))
                                  "apt install redshift" "None")

  (config-add-external-dependency 'dropbox 'config-external "synchronisation"
                                  (lambda () (executable-find "dropbox"))
                                  "installscript" "None")

  (config-add-external-dependency 'thinkpad-scripts 'config-external "rotating the screen"
                                  (lambda () (executable-find "thinkpad-rotate"))
                                  "https://github.com/martin-ueding/thinkpad-scripts" "None"))
;; --------------------------------------------------------------------------------
;; windows only
;; --------------------------------------------------------------------------------
(when (eq system-type 'windows-nt)
  (config-add-external-dependency 'nircmd 'config-external "various shell commands"
                                  (lambda () (executable-find "nircmd"))
                                  "None" "choco install nircmd"))

;; --------------------------------------------------------------------------------
;; everywhere
;; --------------------------------------------------------------------------------
(config-add-external-dependency 'firefox 'config-external "web browser"
                                (lambda () (executable-find "firefox"))
                                "apt install firefox" "choco install Firefox")

(provide 'config-external)
