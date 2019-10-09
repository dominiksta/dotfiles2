;; custom

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(when (< emacs-major-version 27) (package-initialize))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(auth-source-save-behavior nil)
 '(beacon-color "#c82829")
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   (quote
    ("e396098fd5bef4f0dd6cedd01ea48df1ecb0554d8be0d8a924fb1d926f02f90f" "acfac6b14461a344f97fad30e2362c26a3fe56a9f095653832d8fc029cb9d05c" "6bc387a588201caf31151205e4e468f382ecc0b888bac98b2b525006f7cb3307" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "36c86cb6c648b9a15d849026c90bd6a4ae76e4d482f7bcd138dedd4707ff26a5" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "3860a842e0bf585df9e5785e06d600a86e8b605e5cc0b74320dfe667bcbe816c" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "4138944fbed88c047c9973f68908b36b4153646a045648a22083bd622d1e636d" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "54f2d1fcc9bcadedd50398697618f7c34aceb9966a6cbaa99829eb64c0c1f3ca" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "d057f0430ba54f813a5d60c1d18f28cf97d271fd35a36be478e20924ea9451bd" "73c69e346ec1cb3d1508c2447f6518a6e582851792a8c0e57a22d6b9948071b4" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" default)))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(dired-async-mode t)
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };")))
 '(fci-rule-color "#37474f")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote light))
 '(gnus-logo-colors (quote ("#0d7b72" "#adadad")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };")) t)
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(hl-sexp-background-color "#1c1f26")
 '(jdee-db-active-breakpoint-face-colors (cons "#10151C" "#5EC4FF"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#10151C" "#8BD49C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#10151C" "#384551"))
 '(objed-cursor-color "#D95468")
 '(org-agenda-files
   (quote
    ("/home/user/sync/general/org/meinleben/studium.org" "/home/user/sync/general/org/meinleben/capture.org")))
 '(package-selected-packages
   (quote
    (epass flucui-themes pdf-tools org-tree-slide shackle cyberpunk-theme challenger-deep-theme color-theme-sanityinc-tomorrow grandshell-theme highlight-indent-guides yafolding alect-themes dictcc editorconfig ujelly-theme pdfgrep demo-it org-pdfview olivetti avy org-variable-pitch sqlup-mode spacemacs-theme realgud company-jedi yasnippet-snippets markdown-preview-mode lsp-ui restclient sql-indent php-mode ng2-mode json-mode emmet-mode tide cider clojure-mode ahk-mode powershell pcre2el yasnippet-classic-snippets yasnippet helm-flyspell unidecode ag typescript-mode web-mode js2-mode cmake-mode cquery anti-zenburn-theme zenburn-theme hydra fish-completion multi-term elfeed-org elfeed emms-info-mediainfo emms diredfl dired-rainbow dired-k rainbow-mode shell-pop dired-subtree helm-ag helm-rg pyvenv company-lsp lsp-mode lsp hl-todo flycheck helm-projectile projectile evil-magit magit peep-dired dired-du which-key eyebrowse htmlize org-download evil-org org-bullets helm-swoop helm company evil-nerd-commenter evil-surround doom-themes restart-emacs evil-leader evil use-package)))
 '(pdf-view-midnight-colors (quote ("#232333" . "#c7c7c7")))
 '(safe-local-variable-values
   (quote
    ((eval progn
           (olivetti-mode)
           (fp/org-variable-pitch))
     (eval progn
           (olivetti-mode))
     (eval progn
           (fp/org-variable-pitch)
           (olivetti-mode))
     (eval fp/org-variable-pitch)
     (fp/org-tree-slide-variable-pitch . t)
     (projectile-project-run-cmd . "mkdir -p build; cd build; cmake ..; make run")
     (projectile-project-compilation-cmd . "mkdir -p build; cd build; cmake ..; make")
     (epa-file-select-keys . "dominik.stahmer@gmx.de")
     (org-confirm-babel-evaluate)
     (projectile-project-compilation-dir . ".")
     (projectile-project-run-cmd . "build/bin/writerino")
     (projectile-project-compilation-cmd . "cd build && cmake .. && cmake --build ."))))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:foreground "#232333" :background "#d4d4d4")))))


;; this line is for running emacs from usb
;; CONFIG_DIRECTORY should be set to something like 'dotfiles/emacs'
(let ((confdir (getenv "CONFIG_DIRECTORY")))
      (if confdir
          (setq config-directory (expand-file-name (concat confdir "/config"))
                user-init-file (expand-file-name (concat confdir "/init.el")))
        (setq config-directory "~/.emacs.d/config")))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; load personal config
(add-to-list 'load-path (concat config-directory "/init/"))
(require 'init-config)
