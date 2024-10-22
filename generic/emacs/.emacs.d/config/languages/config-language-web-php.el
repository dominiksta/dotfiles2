(require-and-log 'config-language-web-general)

;; ----------------------------------------------------------------------
;; lsp
;; ----------------------------------------------------------------------

(straight-use-package 'php-mode) (require 'php-mode)

(defun fp/php-mode-hook ()
  (interactive)
  (fp/toggle-show-too-long-lines)
  (lsp))

(add-hook 'php-mode-hook 'fp/php-mode-hook)

(setq-default lsp-intelephense-stubs ["apache" "random" "bcmath" "bz2" "calendar" "com_dotnet" "Core" "ctype" "curl" "date" "dba" "dom" "enchant" "exif" "fileinfo" "filter" "fpm" "ftp" "gd" "hash" "iconv" "imap" "interbase" "intl" "json" "ldap" "libxml" "mbstring" "mcrypt" "meta" "mssql" "mysqli" "oci8" "odbc" "openssl" "pcntl" "pcre" "PDO" "pdo_ibm" "pdo_mysql" "pdo_pgsql" "pdo_sqlite" "pgsql" "Phar" "posix" "pspell" "readline" "recode" "Reflection" "regex" "session" "shmop" "SimpleXML" "snmp" "soap" "sockets" "sodium" "SPL" "sqlite3" "standard" "superglobals" "sybase" "sysvmsg" "sysvsem" "sysvshm" "tidy" "tokenizer" "wddx" "xml" "xmlreader" "xmlrpc" "xmlwriter" "Zend OPcache" "zip" "zlib" "gettext"])

(defun fp/web-mode-php-hook ()
  (when (string= web-mode-engine "php")
    (fp/toggle-show-too-long-lines)
    (lsp)))

(add-hook 'web-mode-hook 'fp/web-mode-php-hook)

;; ----------------------------------------------------------------------
;; Binds
;; ----------------------------------------------------------------------
(evil-leader/set-key-for-mode 'php-mode
  "md" 'lsp-ui-doc-show
  "mD" 'lsp-ui-doc-hide)

(provide 'config-language-web-php)
