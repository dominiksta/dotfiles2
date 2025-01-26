(require-and-log 'config-programming-general)

;; (straight-use-package 'csharp-mode)

(defun fp/csharp-mode-hook ()
  ;; (setq-local company-idle-delay 0.5)
  )

(add-hook 'csharp-mode-hook 'fp/csharp-mode-hook)

;; ;; The standalone omnisharp package seems to be more reliable than lsp,
;; ;; although it is officially deprecated (but still gets updates so idc).
;; (straight-use-package 'omnisharp)
;; ;; 1. To install the omnisharp server, simply run
;; ;;    `omnisharp-install-server'. On windows, you may need to install a
;; ;;    dotnet sdk (or even msvc idk) and on linux probably mono. At the time
;; ;;    of writing, everything works and I have installed msvc 16 2019 and
;; ;;    dotnet 3.5 and 4.
;; ;; 2. Omnisharp only works if projectile can find the project root. In a
;; ;;    unity project for example, this means creating a '.projectile' file in
;; ;;    the directory where the '.csproj' file is located.

;; ;; ----------------------------------------------------------------------
;; ;; Omnisharp setup
;; ;; ----------------------------------------------------------------------
;; (defun fp/csharp-setup-omnisharp ()
;;   (omnisharp-mode)
;;   (company-mode)
;;   (flycheck-mode)
;;   (setq-local company-backends '(company-omnisharp)))

;; (remove-hook 'csharp-mode-hook 'fp/csharp-setup-omnisharp)

;; ;; ----------------------------------------------------------------------
;; ;; Bindings
;; ;; ----------------------------------------------------------------------
;; (define-key omnisharp-mode-map (kbd "M-R") 'omnisharp-rename)

;; (defun fp/evil-omnisharp-go-to-definition ()
;;   (interactive)
;;   (evil-set-jump)
;;   (omnisharp-go-to-definition))

;; (evil-define-key 'normal omnisharp-mode-map "gd" 'fp/evil-omnisharp-go-to-definition)

(provide 'config-language-csharp)