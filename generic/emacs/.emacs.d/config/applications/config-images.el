;; --------------------------------------------------------------------------------
;; image-mode
;; --------------------------------------------------------------------------------
(evil-set-initial-state 'image-mode 'normal)

;; (require-package 'eimp)
;; (require 'eimp)

;; TODO windows imagemagick
(config-add-external-dependency 'imagemagick 'config-images "viewing and modifying images"
                                (lambda () (executable-find "convert"))
                                "apt install imagemagick" "None")

(require 'image-dired)
(setq image-modes '(image-mode-map image-dired-display-image-mode-map))

(dolist (img-mode image-modes)
  (evil-define-key 'normal (eval img-mode)
    "h"  'image-backward-hscroll
    "j"  'image-next-line
    "k"  'image-previous-line
    "l"  'image-forward-hscroll
    "f"  'image-next-frame
    "b"  'image-previous-frame
    "zi" (lambda () (interactive) (image-increase-size current-prefix-arg))
    "+"  (lambda () (interactive) (image-increase-size current-prefix-arg))
    "zo" (lambda () (interactive) (image-decrease-size current-prefix-arg))
    "-"  (lambda () (interactive) (image-decrease-size current-prefix-arg))
    "W"  'image-transform-fit-to-width
    "P"  'image-transform-fit-to-width
    "H"  'image-transform-fit-to-height
    "O"  (lambda () (interactive) (image-transform-set-scale 1))
    "J"  (lambda () (interactive) (set-buffer-modified-p nil) (image-next-file 1))
    "K"  (lambda () (interactive) (set-buffer-modified-p nil) (image-previous-file 1))

    "gg" 'image-bob
    "G"  (lambda () (interactive) (image-eob) (image-bol nil))
    "$"  'image-eol
    "0"  'image-bol

    "q"  'volatile-kill-buffer
    "x"  (lambda () (interactive) (let ((buf (current-buffer))) (winner-undo) (kill-buffer buf)) )
    (kbd "s-Q") 'volatile-kill-buffer))


(define-key image-map (kbd "o") nil)
(define-key image-map (kbd "s") nil)
(define-key image-map (kbd "r") nil)

(provide 'config-images)
