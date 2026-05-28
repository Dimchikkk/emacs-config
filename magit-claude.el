;;; magit-claude.el --- Annotate magit diff hunks and review with Claude

(require 'json)

(defvar magit-claude--review-dir (expand-file-name "~/.magit/"))
(defvar magit-claude--current-batch 1)
(defvar magit-claude--overlays '())

;; --- File naming ---

(defun magit-claude--review-file (&optional n)
  "Return path for batch N (defaults to current batch)."
  (expand-file-name (format "magit-review-%d.json" (or n magit-claude--current-batch))
                    magit-claude--review-dir))

(defun magit-claude--next-free-batch ()
  "Find the next unused batch number."
  (let ((n 1))
    (while (file-exists-p (magit-claude--review-file n))
      (setq n (1+ n)))
    n))

;; --- JSON helpers ---

(defun magit-claude--read-entries ()
  (let ((file (magit-claude--review-file)))
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (condition-case nil
              (let ((json-array-type 'list))
                (json-read-from-string (buffer-string)))
            (error nil)))
      nil)))

(defun magit-claude--write-entries (entries)
  (with-temp-file (magit-claude--review-file)
    (insert (json-encode entries))))

;; --- Filename from magit section ---

(defun magit-claude--get-filename-from-magit ()
  (when (and (fboundp 'magit-current-section)
             (derived-mode-p 'magit-mode))
    (let ((section (magit-current-section)))
      (while (and section
                  (not (memq (oref section type) '(file))))
        (setq section (oref section parent)))
      (when (and section (stringp (oref section value)))
        (oref section value)))))

(defun magit-claude--get-filename-from-buffer ()
  "Repo-relative filename for the current buffer's file."
  (when buffer-file-name
    (let ((root (or (and (fboundp 'magit-toplevel) (magit-toplevel))
                    (vc-git-root buffer-file-name))))
      (if root
          (file-relative-name buffer-file-name (expand-file-name root))
        buffer-file-name))))

(defun magit-claude--get-filename ()
  (or (magit-claude--get-filename-from-magit)
      (magit-claude--get-filename-from-buffer)))

;; --- Hunk header ---

(defun magit-claude--find-hunk-header-in-diff ()
  (save-excursion
    (when (re-search-backward "^@@[^@]*@@" nil t)
      (buffer-substring-no-properties
       (line-beginning-position) (line-end-position)))))

(defun magit-claude--synthesize-hunk-header (beg end)
  "Build a fake hunk header from line numbers BEG..END in the source buffer."
  (let* ((start-line (line-number-at-pos beg))
         (end-line (line-number-at-pos end))
         (count (max 1 (1+ (- end-line start-line)))))
    (format "@@ -%d,%d +%d,%d @@" start-line count start-line count)))

(defun magit-claude--find-hunk-header (beg end)
  (or (magit-claude--find-hunk-header-in-diff)
      (magit-claude--synthesize-hunk-header beg end)))

;; --- Overlay management ---

(defun magit-claude--place-overlay (comment hunk _file batch &optional date answered)
  "Search buffer for HUNK header and place overlay with COMMENT after it."
  (let ((hunk-key (if (string-match "^@@[^@]+@@" hunk)
                      (match-string 0 hunk)
                    hunk))
        (prefix (format "  [%d]%s%s " batch
                        (if date (format " %s" date) "")
                        (if answered " ✓" ""))))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward hunk-key nil t)
        (end-of-line)
        (let ((ov (make-overlay (point) (point))))
          (overlay-put ov 'before-string
                       (propertize (concat prefix comment "\n")
                                   'face 'font-lock-comment-face))
          (overlay-put ov 'magit-claude t)
          (overlay-put ov 'magit-claude-batch batch)
          (push ov magit-claude--overlays))))))

(defun magit-claude--reapply-overlays ()
  "Remove all overlays and reapply from all existing batch files."
  (mapc #'delete-overlay magit-claude--overlays)
  (setq magit-claude--overlays '())
  (let ((n 1))
    (while (file-exists-p (magit-claude--review-file n))
      (dolist (entry (let ((magit-claude--current-batch n))
                       (magit-claude--read-entries)))
        (magit-claude--place-overlay
         (alist-get 'comment entry)
         (alist-get 'hunk entry)
         (alist-get 'file entry)
         n
         (alist-get 'date entry)
         (eq (alist-get 'answered entry) t)))
      (setq n (1+ n)))))

;; --- Comment input buffer ---

(defvar magit-claude--pending-entry nil)

(defun magit-claude--iso-date ()
  (format-time-string "%Y-%m-%dT%H:%M:%S"))

(defun magit-claude--confirm-comment ()
  (interactive)
  (let* ((start (or (next-single-property-change (point-min) 'read-only)
                    (point-min)))
         (end (point-max))
         (raw (buffer-substring-no-properties start end))
         (comment (string-trim
                   (replace-regexp-in-string "^> .*\n?" "" raw)))
         (entry `((file     . ,(alist-get 'file magit-claude--pending-entry))
                  (hunk     . ,(alist-get 'hunk magit-claude--pending-entry))
                  (diff     . ,(alist-get 'diff magit-claude--pending-entry))
                  (comment  . ,comment)
                  (date     . ,(magit-claude--iso-date))
                  (answered . :json-false)))
         (entries (append (magit-claude--read-entries) (list entry)))
         (magit-buf (alist-get 'magit-buf magit-claude--pending-entry)))
    (magit-claude--write-entries entries)
    (when (and magit-buf (buffer-live-p magit-buf))
      (with-current-buffer magit-buf
        (when (derived-mode-p 'magit-mode)
          (magit-claude--reapply-overlays))))
    (quit-window t)
    (message "Batch %d: comment saved (%d total). /magit in Claude to review."
             magit-claude--current-batch (length entries))))

(defun magit-claude--abort-comment ()
  (interactive)
  (quit-window t)
  (message "Comment aborted."))

;; --- Main commands ---

(defun magit-claude-add-comment ()
  "Annotate the selected diff region with a comment for Claude review."
  (interactive)
  ;; Ensure current batch file exists or start at next free
  (unless (file-exists-p (magit-claude--review-file))
    (setq magit-claude--current-batch (magit-claude--next-free-batch)))
  ;; If no region, use current line
  (unless (use-region-p)
    (beginning-of-line)
    (set-mark (point))
    (end-of-line)
    (activate-mark))
  (let* ((rbeg (region-beginning))
         (rend (region-end))
         (diff (buffer-substring-no-properties rbeg rend))
         (hunk (or (magit-claude--find-hunk-header rbeg rend) "unknown hunk"))
         (file (or (magit-claude--get-filename) "unknown file"))
         (buf  (current-buffer)))
    (deactivate-mark)
    (setq magit-claude--pending-entry
          `((file        . ,file)
            (hunk        . ,hunk)
            (diff        . ,diff)
            (magit-buf   . ,buf)))
    (let ((comment-buf (get-buffer-create "*magit-claude-comment*"))
          (preview (with-temp-buffer
                     (insert diff)
                     (goto-char (point-min))
                     (while (not (eobp))
                       (insert "> ")
                       (forward-line 1))
                     (buffer-string))))
      (with-current-buffer comment-buf
        (erase-buffer)
        (text-mode)
        (let ((start (point)))
          (insert (format "> %s\n> %s\n" file hunk))
          (insert preview)
          (unless (bolp) (insert "\n"))
          (insert "\n")
          (add-text-properties start (point)
                               '(face font-lock-comment-face read-only t rear-nonsticky t)))
        (use-local-map (copy-keymap (current-local-map)))
        (local-set-key (kbd "C-c 1") #'magit-claude--confirm-comment)
        (local-set-key (kbd "C-c 0") #'magit-claude--abort-comment))
      (display-buffer comment-buf '(display-buffer-below-selected (window-height . 16)))
      (select-window (get-buffer-window comment-buf))
      (goto-char (point-max))
      (message "Batch %d. Write comment. C-c 1 to save, C-c 0 to abort."
               magit-claude--current-batch))))

(defun magit-claude-new-batch ()
  "Start a new comment batch so previous one can be reviewed independently."
  (interactive)
  (setq magit-claude--current-batch (magit-claude--next-free-batch))
  (message "Started new batch %d." magit-claude--current-batch))

(defun magit-claude-clear ()
  "Clear current batch comments."
  (interactive)
  (let ((file (magit-claude--review-file))
        (n (length (magit-claude--read-entries))))
    (when (file-exists-p file)
      (delete-file file))
    ;; Remove overlays for current batch only
    (setq magit-claude--overlays
          (cl-remove-if (lambda (ov)
                          (when (eq (overlay-get ov 'magit-claude-batch)
                                    magit-claude--current-batch)
                            (delete-overlay ov)
                            t))
                        magit-claude--overlays))
    (message "Batch %d cleared (%d comments)." magit-claude--current-batch n)))

;; --- Keybindings and hooks ---

(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "C-c C-y") #'magit-claude-add-comment)
  (define-key magit-mode-map (kbd "C-c C-n") #'magit-claude-new-batch)
  (define-key magit-mode-map (kbd "C-c C-z") #'magit-claude-clear)
  (add-hook 'magit-refresh-buffer-hook #'magit-claude--reapply-overlays))

(global-set-key (kbd "C-c C-y") #'magit-claude-add-comment)
(global-set-key (kbd "C-c C-n") #'magit-claude-new-batch)
(global-set-key (kbd "C-c C-z") #'magit-claude-clear)

(provide 'magit-claude)
;;; magit-claude.el ends here
