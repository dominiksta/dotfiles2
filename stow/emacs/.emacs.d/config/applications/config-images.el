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

;; --------------------------------------------------------------------------------
;; image-dired
;; --------------------------------------------------------------------------------

(defun image-dired-execute-in-dired (fun)
  (with-current-buffer (image-dired-associated-dired-buffer)
    (funcall fun)))

(defun image-dired-execute-in-dired-and-refresh (fun)
  (save-excursion
    (switch-to-buffer (image-dired-associated-dired-buffer))
    (funcall fun)
    (image-dired-my-window-config)))

(defun image-dired-rotate-original-with-convert (degrees)
  "Rotate original image DEGREES degrees with convert."
  (image-dired--check-executable-exists
   'image-dired-cmd-rotate-original-program-convert)
  (if (not (image-dired-image-at-point-p))
      (message "No image at point")
    (let* ((file (image-dired-original-file-name))
           (spec
            (list
             (cons ?d degrees)
             (cons ?o (expand-file-name file))
             (cons ?t image-dired-temp-rotate-image-file))))
      (if (not (= 0 (apply #'call-process image-dired-cmd-rotate-original-program-convert
                           nil nil nil
                           (mapcar (lambda (arg) (format-spec arg spec))
                                   image-dired-cmd-rotate-original-options-convert))))
          (error "Could not rotate image")
        (image-dired-display-image image-dired-temp-rotate-image-file)
        (if (or (and image-dired-rotate-original-ask-before-overwrite
                     (y-or-n-p
		      "Rotate to temp file OK.  Overwrite original image? "))
                (not image-dired-rotate-original-ask-before-overwrite))
            (progn
              (copy-file image-dired-temp-rotate-image-file file t)
              (image-dired-refresh-thumb))
          (image-dired-display-image file))))))

(defun image-dired-rotate-original-left-with-convert ()
  "Rotate original image left (counter clockwise) 90 degrees."
  (interactive)
  (image-dired-rotate-original-with-convert "270"))

(defun image-dired-rotate-original-right-with-convert ()
  "Rotate original image right (clockwise) 90 degrees."
  (interactive)
  (image-dired-rotate-original-with-convert "90"))

(defun image-dired-my-window-config ()
  (interactive)
  (print (current-buffer))
  (delete-other-windows)
  (split-window-below) (split-window-right)
  (other-window 1) (switch-to-buffer (image-dired-create-display-image-buffer))
  (other-window 1) (switch-to-buffer (image-dired-create-thumbnail-buffer))
  (other-window 1) (image-dired default-directory)
  (image-dired-execute-in-dired 'dired-unmark-all-marks))

(defun image-dired-no-window-config ()
  (interactive)
  (image-dired default-directory)
  (image-dired-execute-in-dired 'dired-unmark-all-marks))

(setq image-dired-cmd-rotate-original-program-convert "convert"
      image-dired-cmd-rotate-original-options-convert '("-rotate" "%d" "%o" "%t"))

;; --------------------------------------------------------------------------------
;; bindings
;; --------------------------------------------------------------------------------

(setq image-dired-rotate-original-ask-before-overwrite nil)
(evil-define-key 'normal image-dired-thumbnail-mode-map
  "h" 'image-dired-backward-image
  "l" 'image-dired-forward-image
  "j" 'image-dired-next-line
  "k" 'image-dired-previous-line

  "m" 'image-dired-mark-thumb-original-file
  "d" 'image-dired-flag-thumb-original-file
  "x" (lambda () (interactive) (image-dired-execute-in-dired-and-refresh 'dired-do-flagged-delete))
  "v" (lambda () (interactive) (image-dired-execute-in-dired-and-refresh 'dired-do-rename))
  "c" (lambda () (interactive) (image-dired-execute-in-dired-and-refresh 'dired-do-copy))


  "U" (lambda () (interactive) (image-dired-execute-in-dired 'dired-unmark-all-marks))
  "u" 'image-dired-unmark-thumb-original-file
  "q" (lambda () (interactive) (switch-to-buffer (image-dired-associated-dired-buffer)) (delete-other-windows))

  "S"        'image-save
  "o"         'image-dired-display-thumbnail-original-image
  "L"         'image-dired-display-next-thumbnail-original
  (kbd "RET") 'image-dired-display-thumbnail-original-image
  "p"         'image-dired-display-thumbnail-original-image
  "s"         'image-dired-display-thumbnail-original-image
  "f"         (lambda () (interactive) (let ((img (image-dired-file-name-at-point)))
                                         (delete-other-windows) (find-file img)))

  "ee"         'image-dired-display-current-image-sized
  "eo"         'image-dired-display-current-image-full

  "rh" (lambda () (interactive) (image-dired-rotate-original-left-with-convert))
  "rl" (lambda () (interactive) (image-dired-rotate-original-right-with-convert)))

(provide 'config-images)
