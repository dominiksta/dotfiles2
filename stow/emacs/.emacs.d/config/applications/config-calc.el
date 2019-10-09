(defun fp/calc-eval-region (arg beg end)
  "Calculate the region and insert the result at the end of region.
With prefix ARG non-nil, simply display the result in the echo area."
  (interactive "P\nr")
  (let* ((expr (buffer-substring-no-properties beg end))
         (result (calc-eval expr)))
    (if (not (null arg))
        (message "%s = %s" expr result)
      (goto-char end)
      (kill-region beg end)
      (insert result))))

(provide 'config-calc)
