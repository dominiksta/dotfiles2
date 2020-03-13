
(when (< emacs-major-version 27) (package-initialize))

;; Set all changes made by customize to be saved to this file.
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file) (load custom-file))

;; this line is for running emacs from usb
;; CONFIG_DIRECTORY should be set to something like 'dotfiles/emacs'
(let ((confdir (getenv "CONFIG_DIRECTORY")))
      (if confdir
          (setq config-directory (expand-file-name (concat confdir "/config"))
                user-init-file (expand-file-name (concat confdir "/init.el")))
        (setq config-directory "~/.emacs.d/config")))

;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; load personal config
(add-to-list 'load-path (concat config-directory "/init/"))
(require 'init-config)
