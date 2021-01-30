(straight-use-package 'clojure-mode)
(straight-use-package 'cider)

(add-hook 'clojure-mode-hook 'company-mode)
(add-hook 'cider-repl-mode-hook 'company-mode)

;; --------------------------------------------------------------------------------
;; bindings
;; --------------------------------------------------------------------------------
(let ((clojure-binding-modes
       '(clojure-mode
         clojurescript-mode
         cider-repl-mode
         cider-clojure-interaction-mode)))
  (dolist (mode clojure-binding-modes)
    (evil-leader/set-key-for-mode mode
      "ee" 'cider-eval-last-sexp
      "er" 'cider-eval-region
      "eb" 'cider-eval-buffer
      "mdd" 'cider-doc
      "mdj" 'cider-javadoc
      "pr" 'cider-run
      "pt" 'cider-test-run-project-tests)))

(evil-define-key 'normal cider-stacktrace-mode-map "q" 'cider-popup-buffer-quit-function)
(evil-define-key 'normal cider-repl-mode-map (kbd "<RET>") 'cider-repl-return)

;; fix eval binds for evil
(defun evil-collection-cider-last-sexp (command &rest args)
  "In normal-state or motion-state, last sexp ends at point."
  (if (and (not evil-move-beyond-eol)
         (or (evil-normal-state-p) (evil-motion-state-p)))
      (save-excursion
        (unless (or (eobp) (eolp)) (forward-char))
        (apply command args))
    (apply command args)))

(unless evil-move-beyond-eol
  (advice-add 'cider-eval-last-sexp :around 'evil-collection-cider-last-sexp)
  (advice-add 'cider-eval-last-sexp-and-replace :around 'evil-collection-cider-last-sexp)
  (advice-add 'cider-eval-last-sexp-to-repl :around 'evil-collection-cider-last-sexp)
  (with-eval-after-load 'cider-eval-sexp-fu
    (advice-add 'cider-esf--bounds-of-last-sexp :around 'evil-collection-cider-last-sexp)))


(provide 'config-language-clojure)
