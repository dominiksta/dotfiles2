;; --------------------------------------------------------------------------------
;; disable garbage-collection during startup
;; --------------------------------------------------------------------------------
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

;; --------------------------------------------------------------------------------
;; set up package-management and use-package
;; --------------------------------------------------------------------------------
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package) (package-refresh-contents) (package-install 'use-package))
(eval-when-compile (require 'use-package))
(use-package try :ensure t :defer t)

(add-to-list 'load-path config-directory)
(add-to-list 'load-path (concat config-directory "/init/"))
(add-to-list 'load-path (concat config-directory "/languages/"))
(add-to-list 'load-path (concat config-directory "/shells/"))
(add-to-list 'load-path (concat config-directory "/applications/"))

(require 'seq)
(require 'bind-key)
(require 'cl-macs)

;; --------------------------------------------------------------------------------
;; the basic functionality of my config system
;; --------------------------------------------------------------------------------
(defvar config-feature-list '()
  "This gets populated by `config-require'. One
can then call `config-load-all' to load all of them at once.")

(defun config--try-require (fet log &optional reason)
  "Try to require FET, but don't stop on error. Instead display the error as a warning.
If LOG is not nil, log to *Messages* when FET is loaded. If
REASON is non-nil, append it to the log output."
  (interactive)
  (let ((fp (featurep fet)))
    (condition-case err
        (progn (when (not fp)
                 (let ((time (current-time)))
                   (require fet)
                   (when log (message
                              (concat "CONFIG [" (format-time-string "%3N" (float-time (time-since time)))
                                      " ms]: Loaded " (symbol-name fet) reason))))))
      (error (warn (concat "CONFIG-ERROR: " (symbol-name fet) ": " (error-message-string err)))))
    fp))


(defun config-create-extension-regexp (exts)
  "Returns a regular expression matching any filename with one of
EXTs elements as its extension."
  (concat "\\." (mapconcat (lambda (ext) (concat "\\(" ext "\\)")) exts "\\|") "\\'"))

(cl-defmacro config-require (config-units
                             &key feature regexp auto-mode)
  ;; MAYB allow multiple features
  ;; MAYB allow specifying autoloads
  `(progn
     (dolist (config-unit ,config-units)
       (when (not (member config-unit config-feature-list))
         (push config-unit config-feature-list)))
     (when ',feature
       (with-eval-after-load
           ',feature
         (dolist (config-unit ,config-units)
           (when (not (featurep 'config-unit))
             (config--try-require config-unit t
                                  (concat " because " (symbol-name ',feature)
                                          " was loaded"))))))
     (when ,regexp
       (if ',auto-mode
           (let ((regex (if (listp ,regexp) (config-create-extension-regexp ,regexp))))
             (if regex
                 (add-to-list
                  'auto-mode-alist
                  (cons regex '(lambda () (dolist (config-unit ,config-units)
                                       (config--try-require config-unit nil))
                                 (,auto-mode))))
               (add-to-list
                'auto-mode-alist
                '(,regexp . (lambda () (dolist (config-unit ,config-units)
                                    (config--try-require config-unit nil))
                              (,auto-mode))))))
         (error "regexp specified but auto-mode not set")))))

(defmacro require-and-log (fet)
  `(when (not (featurep ,fet))
     (config--try-require ,fet t " because it was required in another config-file")))

(defun autoload-list (functions file)
  (dolist (fun functions)
    (autoload fun file)))

(defun config-load-all ()
  "`Requires' all features in `config-feature-list'."
  (interactive)
  (dolist (feature config-feature-list) (config--try-require feature t)))

(defun config-load-one ()
  "`Require' one feature in `config-feature-list' interactively."
  (interactive)
  (let ((feature (completing-read "Feature: " config-feature-list)))
    (config--try-require (intern feature) t " interactively")))

(defun config-show-loaded ()
  ""
  (interactive)
  (switch-to-buffer (get-buffer-create "*loaded configurations*"))
  (font-lock-mode)
  (erase-buffer)
  (dolist (feature config-feature-list)
    (insert (concat (symbol-name feature) ": "
                    (if (featurep feature)
                        (propertize "loaded" 'font-lock-face '(:foreground "orange"))
                      (propertize "not loaded" 'font-lock-face '(:foreground "gray"))) "\n"))))


;; --------------------------------------------------------------------------------
;; dealing with external dependencies
;; --------------------------------------------------------------------------------

(defvar config-external-dependencies "" '())
(setq config-external-dependencies nil)


(defun config--external-dependency (N CU RE PR IL IW)
  `((name . ,N) (config-unit . ,CU) (reason . ,RE) (predicate . ,PR)
    (installation-linux . ,IL)
    (installation-windows . ,IW)))

(defun config-add-external-dependency (name config-unit reason predicate
                                            installation-linux installation-windows)
  "Adds an external dependency. The system checks if its installed by calling PREDICATE
once when the `config-add-external-dependency' is run and whenever `config-external-check-all'
is called"
  (let ((dep (config--external-dependency name config-unit reason predicate
                                          installation-linux installation-windows)))
    (when (not (member dep config-external-dependencies)) (push dep config-external-dependencies)))
  (when (not (funcall predicate))
    (message "----------------------------------------------------------------------")
    (message (concat "WARNING (CONFIG): MISSING EXTERNAL DEPENDENCY " (symbol-name name)))
    (message (concat "Reason: " reason))
    (message (concat "Install it with: " (if (eq system-type 'windows-nt)
                                             installation-windows installation-linux)))
    (message "----------------------------------------------------------------------")))


(defun config-external-check-list (deps)
  "return nil only if one in DEPS is not installed"

  ;; check if elements are in `config-external-dependencies' in the first place

  (when (not (member nil (let ((depnames (mapcar (lambda (dep) (cdr (car dep)))
                                                 config-external-dependencies)))
                           (mapcar (lambda (dep) (member dep depnames)) deps))))
    ;; check whether things are installed
    (let ((deps (seq-filter
                 (lambda (dep) (member (cdr (assoc 'name dep)) deps)) config-external-dependencies))
          (installed nil))
      (dolist (dep deps) (when (funcall (cdr (assoc 'predicate dep))) (push dep installed)))
      (eq (length deps) (length installed)))))


(defun config-external-check-all ()
  "Calls all PREDICATE functions of all dependencies and lists the results in a new buffer"
  (interactive)
  (let ((not-installed nil) (installed nil) )
    (config-load-all)
    (dolist (dep config-external-dependencies)
      (if (funcall (cdr (assoc 'predicate dep)))
          (push dep installed)
        (push dep not-installed)))

    (switch-to-buffer-other-window (get-buffer-create "*dependencies*")) (erase-buffer)
    (font-lock-mode)
    (insert "===============================MISSING===============================\n")
    (dolist (dep not-installed)
      (let ((name (symbol-name (cdr (assoc 'name dep))))
            (config-unit (cdr (assoc 'config-unit dep))) (reason (cdr (assoc 'reason dep)))
            (predicate (cdr (assoc 'predicate dep)))
            (installation (cdr (if (eq system-type 'windows-nt)
                                   (assoc 'installation-windows dep)
                                 (assoc 'installation-linux dep)))))
        (insert (propertize name 'font-lock-face '(:foreground "red")) " (" reason ")\n")
        (insert "Install with: "  installation "\n")
        (insert "Was checked with: "  (format "%S" predicate) "\n\n")))
    (insert "==============================INSTALLED==============================\n")
    (dolist (dep installed)
      (let ((name (symbol-name (cdr (assoc 'name dep))))
            (config-unit (cdr (assoc 'config-unit dep))) (reason (cdr (assoc 'reason dep)))
            (predicate (cdr (assoc 'predicate dep))))
        (insert (propertize name 'font-lock-face '(:foreground "green")) " (" reason ")\n")
        (insert "Was checked with: "  (format "%S" predicate) "\n\n")))
    (insert "--- done")))



;; --------------------------------------------------------------------------------
;; only load the bare-minimum if "-minimal" is passed as a command line argument
;; --------------------------------------------------------------------------------
(push (cons "minimal" (lambda (arg) (message "MINIMAL CONFIG"))) command-switch-alist)
(if (member "-minimal" command-line-args) (require 'init-minimal) (require 'init-default))

(defun start-minimal-emacs ()
  "Spawns a new emacs with minimal configuration. Same as passing `-minimal' as a command line argument."
  (interactive)
  (start-process-shell-command "minimal-emacs" "*minimal-emacs*"
                               (concat "\"" (car command-line-args) "\"" " -minimal "
                                       config-directory "/init/init-default.el")))

(defun start-default-emacs ()
  "Spawns a new emacs with minimal configuration. Same as passing `-minimal' as a command line argument."
  (interactive)
  (start-process-shell-command "default-emacs" "*default-emacs*"
                               (concat "\"" (car command-line-args) "\"" " -q")))

(provide 'init-config)
