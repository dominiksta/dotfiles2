(require-and-log 'config-org)

(use-package ox-reveal :ensure t :demand t)

(setq org-reveal-title-slide
      "<h1 class=\"title\">%t</h1> <div class=\"author\">%a</div>"
      org-reveal-root (concat "file://" (expand-file-name "~/git/reveal/")))

(provide 'config-org-reveal)
