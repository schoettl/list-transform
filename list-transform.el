
;;; list-transform.el --- Transform a bullet list to a sentence and vice versa.  -*- lexical-binding: t; -*-

;; TODO see Conventional Headers
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Headers.html

;;; Code:

(defvar list-transform-and-word "and"
  "The word before the last list item in a sentence.  Set to nil to use a comma only.")

(defvar list-transform-oxford-comma t
  "Put a comma before `list-transform-and-word' in a sentence.")

(defun list-transform nil
  "Toggle between a bullet list to a sentence."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond ((re-search-forward "^ *[*+-] " (point-at-eol) t)
           (message "transforming to a sentence")
           (list-transform-to-sentence))
          (t
           (message "transforming to a list")
           (list-transform-to-list)))))

(defun list-transform-to-list nil
  "Transform a sentence to a bullet list."
  (interactive)
  (save-excursion
    ;; (evil-beginning-of-visual-line) ;; auto indent resulting list?
    (beginning-of-line)
    (when (re-search-forward "^ *" (point-at-eol) t)
      (replace-match "- "))
    (while (re-search-forward
            (concat (list-transform--comma-and) "\\|, ")
            (point-at-eol) t)
      (replace-match "\n- "))
    ))

(defun list-transform-to-sentence nil
  "Transform a bullet list to a sentence."
  (interactive)
  (save-excursion
    ;; goto begin of list
    (cond ((re-search-backward "^ *$" nil t)
           (forward-line)
           (beginning-of-line))
          (t (goto-char (point-min))))
    (let ((startpoint (point)))
      ;; delete first bullet point
      (when (re-search-forward "^ *[*+-] " (point-at-eol) t)
        (replace-match "")
        ;; goto end of list
        (cond ((re-search-forward "^ *$" nil t)
               (forward-line -1)
               (end-of-line))
              (t
               (goto-char (point-max))))
        (while (re-search-backward "^ *[*+-] " startpoint t)
          (replace-match ", ")
          (join-line))
        (end-of-line)
        (when (re-search-backward " , " startpoint t 1)
          (replace-match
           (list-transform--comma-and)))
        (while (re-search-backward " , " startpoint t)
          (replace-match ", "))
        )
      )
    )
  )

(defun list-transform--comma-and nil
  "Helper to make the search regex."
  (cond ((and list-transform-and-word list-transform-oxford-comma)
         (concat ", " list-transform-and-word " "))
        (list-transform-and-word ;; only and-word
         (concat " " list-transform-and-word " "))
        (t ", ")))

(provide 'list-transform)

;;; list-transform.el ends here
