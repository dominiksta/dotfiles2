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
 '(blink-cursor-mode nil)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   '("7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" default))
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(dired-async-mode nil)
 '(emms-mode-line-icon-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
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
\"#######..#\" };"))
 '(epg-gpg-program "gpg")
 '(fci-rule-color "#37474f")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'light)
 '(gnus-logo-colors '("#0d7b72" "#adadad") t)
 '(gnus-mode-line-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
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
\"###########.######\" };") t)
 '(hl-paren-background-colors '("#2492db" "#95a5a6" nil))
 '(hl-paren-colors '("#ecf0f1" "#ecf0f1" "#c0392b"))
 '(hl-sexp-background-color "#1c1f26")
 '(jdee-db-active-breakpoint-face-colors (cons "#10151C" "#5EC4FF"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#10151C" "#8BD49C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#10151C" "#384551"))
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(objed-cursor-color "#D95468")
 '(org-agenda-files nil)
 '(package-selected-packages
   '(imenu-anywhere mini-modeline bash-completion modus-vivendi-theme modus-operandi-theme shell-here auctex wgrep-ag buffer-move epass pdf-tools org-tree-slide shackle dumb-jump flucui-themes cyberpunk-theme challenger-deep-theme color-theme-sanityinc-tomorrow grandshell-theme highlight-indent-guides yafolding alect-themes dictcc editorconfig ujelly-theme pdfgrep demo-it org-pdfview olivetti avy org-variable-pitch sqlup-mode spacemacs-theme realgud company-jedi yasnippet-snippets markdown-preview-mode lsp-ui restclient sql-indent php-mode ng2-mode json-mode emmet-mode tide cider clojure-mode ahk-mode powershell pcre2el yasnippet-classic-snippets yasnippet helm-flyspell unidecode ag typescript-mode web-mode js2-mode cmake-mode cquery anti-zenburn-theme zenburn-theme hydra fish-completion multi-term elfeed-org elfeed emms-info-mediainfo emms diredfl dired-rainbow dired-k rainbow-mode shell-pop dired-subtree helm-ag helm-rg pyvenv company-lsp lsp-mode lsp hl-todo flycheck helm-projectile projectile evil-magit magit peep-dired dired-du which-key eyebrowse htmlize org-download evil-org org-bullets helm-swoop helm company evil-nerd-commenter evil-surround doom-themes restart-emacs evil-leader evil use-package))
 '(pdf-view-midnight-colors '("#232333" . "#c7c7c7"))
 '(safe-local-variable-values
   '((TeX-master . main\.tex)
     (eval progn
           (org-babel-goto-named-src-block "startup")
           (org-babel-execute-src-block)
           (outline-hide-sublevels 1))
     (ag-arguments "--smart-case" "--stats" "--hidden")
     (ag-arguments list "--smart-case" "--stats" "--hidden")
     (ag-arguments quote
                   ("--smart-case" "--stats" "--hidden"))
     (helm-ag-command-option . "--hidden")
     (TeX-open-quote . "\\textquote{")
     (TeX-command-extra-options . "-shell-escape")
     (TeX-close-quote . "}")
     (TeX-master . "../main.tex")
     (TeX-master . "main.tex")
     (projectile-project-run-cmd . "cd C:\\Users\\dstahmer\\git\\enp-browser\\deployment-scripts\\ && powershell -File deploy.ps1 -SkipAngularBuild")
     (projectile-project-compilation-cmd . "cd C:\\Users\\dstahmer\\git\\enp-browser\\deployment-scripts\\ && sh push-and-build.sh")
     (projectile-project-run-cmd . "pdflatex -synctex=1 BA_main.tex")
     (projectile-project-run-cmd . "pdflatex BA_main.tex")
     (projectile-project-run-cmd . "cd C:\\Users\\dstahmer\\git\\recom-enp-deploy\\ && powershell -File deploy.ps1 -SkipAngularBuild")
     (projectile-project-compilation-cmd . "cd C:\\Users\\dstahmer\\git\\recom-enp-deploy\\ && powershell -File deploy.ps1")
     (projectile-project-compilation-cmd . "cd C:\\Users\\dstahmer\\git\\recom-enp-deploy\\ && powershell -File deploy.ps1 -SkipAngularBuild")
     (projectile-project-compilation-cmd . "cd C:\\Users\\dstahmer\\git\\recom-enp-deploy\\ &&  && powershell -File deploy.ps1 -SkipAngularBuild")
     (eval progn
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
     (projectile-project-compilation-cmd . "cd build && cmake .. && cmake --build .")))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#f36c60")
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
     (360 . "#8bc34a")))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))


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
