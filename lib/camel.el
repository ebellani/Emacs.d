(defun un-camelcase-region (beg end &optional yank-handler)
  "CamelCase string S to lower case with word separator '-'"
  (interactive "r")
  (let ((s (filter-buffer-substring beg end t)))
    (when s			;STRING is nil if BEG = END
      ;; Add that string to the kill ring, one way or another.
      (let ((case-fold-search nil))
        (while (string-match "[A-Z]" s)
          (setq s (replace-match (concat "-"
                                         (downcase (match-string 0 s)))
                                 t nil s)))
        (goto-char beg)
        (insert s)))))
