
(use-package elfeed :ensure t :demand t)
(use-package elfeed-protocol :ensure t :demand t)

(setq elfeed-feeds '(("ttrss+https://admin@rss.icyou.icu" :use-authinfo t)))
(elfeed-protocol-enable)

(evil-define-key 'normal elfeed-search-mode-map
  (kbd "RET") 'elfeed-search-show-entry
  "r" 'elfeed-update
  "O" 'elfeed-search-tag-all-unread
  "o" 'elfeed-search-untag-all-unread
  "s" 'elfeed-search-live-filter
  "y" 'elfeed-search-yank
  "q" 'elfeed-search-quit-window)

(evil-define-key 'normal elfeed-show-mode-map
  "y"  'elfeed-show-yank
  "u"  'elfeed-show-tag--unread
  "J"  'elfeed-show-next
  "K"  'elfeed-show-prev
  "gh" 'elfeed-show-visit
  "q"  'elfeed-kill-buffer)


(provide 'config-elfeed)