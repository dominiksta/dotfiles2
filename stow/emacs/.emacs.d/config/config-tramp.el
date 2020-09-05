
(if (eq system-type 'windows-nt)
    (progn
      (config-add-external-dependency 'plink 'config-tramp "tramp"
                                      (lambda () (executable-find "plink"))
                                      "None" "choco install putty.install")
      (setq tramp-default-method "plink"))
  (setq tramp-default-method "ssh"))


(provide 'config-tramp)
