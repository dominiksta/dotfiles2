(require-and-log 'config-language-cc)

(defun fp/java-indentation-setup ()
  (setq-local c-default-style "java")
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-cont-nonempty '+)
  (c-set-offset 'arglist-close '0)
  (c-set-offset 'case-label '+))

;; (add-hook 'java-mode-hook 'fp/java-indentation-setup)

(provide 'config-language-java)


