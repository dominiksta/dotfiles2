(require 'scratchy)

(autoload 'scratchy-open          "scratchy")
(autoload 'scratchy-open-dwim     "scratchy")
(autoload 'scratchy-open-archived "scratchy")
(autoload 'scratchy-archive       "scratchy")

(evil-leader/set-key
  "ss" 'scratchy-open-dwim
  "so" 'scratchy-open
  "sa" 'scratchy-open-archived
  "sA" 'scratchy-archive)

(provide 'config-scratchy)
