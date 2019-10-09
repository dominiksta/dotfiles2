(require-and-log 'config-helpers)
(require-and-log 'config-editor)
(require-and-log 'config-window-management)
(require-and-log 'config-global-binds)
(load-theme 'adwaita)
(tool-bar-mode 0)

(setq initial-buffer-choice t)

(provide 'init-minimal)
