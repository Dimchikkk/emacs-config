(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode Keymap.")

(defun my-copy-till-end-of-line ()
  (interactive)
  (let ((orig-point (point)))
    (end-of-line)
    (backward-char)
    (evil-visual-char)
    (goto-char orig-point)))

(defun xah-toggle-letter-case ()
  (interactive)
  (let ( (deactivate-mark nil) xp1 xp2)
    (if (region-active-p)
        (setq xp1 (region-beginning) xp2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alpha:]")
        (setq xp1 (point))
        (skip-chars-forward "[:alpha:]")
        (setq xp2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region xp1 xp2)
      (put this-command 'state 1))
     ((equal 1 (get this-command 'state))
      (upcase-region xp1 xp2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region xp1 xp2)
      (put this-command 'state 0)))))

(defun occur-thing-at-point()
  (interactive)
  (let ((term (thing-at-point 'symbol t))) (occur term)))

(defun counsel-git-grep-at-point()
  (interactive)
  (let ((term (thing-at-point 'symbol t))) (counsel-git-grep term)))

(define-key evil-normal-state-map  (kbd "C-r")         #'swiper-isearch-thing-at-point)
(define-key my-keys-minor-mode-map (kbd "C-s")         #'swiper-isearch)
(define-key my-keys-minor-mode-map (kbd "C-c )")       #'kmacro-end-macro)
(define-key my-keys-minor-mode-map (kbd "C-c (")       #'kmacro-start-macro)
(define-key my-keys-minor-mode-map (kbd "C-c ,")       #'kmacro-end-and-call-macro)
(define-key my-keys-minor-mode-map (kbd "C-c C-,")     #'apply-macro-to-region-lines)
(define-key my-keys-minor-mode-map (kbd "C-c /")       #'counsel-compilation-errors)
(define-key my-keys-minor-mode-map (kbd "C-/")         #'next-error)
(define-key my-keys-minor-mode-map (kbd "C-?")         #'previous-error)
(define-key my-keys-minor-mode-map (kbd "C--")         #'default-text-scale-decrease)
(define-key my-keys-minor-mode-map (kbd "C-=")         #'default-text-scale-increase)
(define-key my-keys-minor-mode-map (kbd "C-,")         #'evil-mc-make-and-goto-next-match)
(define-key my-keys-minor-mode-map (kbd "C-a")         #'evil-mc-make-all-cursors)
(define-key my-keys-minor-mode-map (kbd "C-;")         #'evil-mc-undo-all-cursors)
(define-key my-keys-minor-mode-map (kbd "C->")         #'evil-mc-make-cursor-in-visual-selection-end)
(define-key my-keys-minor-mode-map (kbd "C-<")         #'evil-mc-make-cursor-in-visual-selection-beg)
(define-key my-keys-minor-mode-map (kbd "M-RET")       #'evil-mode)
(define-key my-keys-minor-mode-map (kbd "C-c ;")       #'kill-other-buffers)
(define-key my-keys-minor-mode-map (kbd "C-c =")       #'sort-lines)
(define-key my-keys-minor-mode-map (kbd "C-c C-c M-x") #'execute-extended-command)
(define-key my-keys-minor-mode-map (kbd "C-c C-f")     #'ffap)
(define-key my-keys-minor-mode-map (kbd "C-c C-l")     #'shell)
(define-key my-keys-minor-mode-map (kbd "C-c RET")     #'projectile-switch-project)
(define-key my-keys-minor-mode-map (kbd "C-c SPC")     #'counsel-fzf)
(define-key my-keys-minor-mode-map (kbd "M-o")         #'find-grep-dired)
(define-key my-keys-minor-mode-map (kbd "C-c o")       #'occur-thing-at-point)
(define-key my-keys-minor-mode-map (kbd "C-c C-o")     #'occur)
(define-key my-keys-minor-mode-map (kbd "C-c r")       #'recentf-open-files)
(define-key my-keys-minor-mode-map (kbd "C-c a")       #'align-regexp)
(define-key my-keys-minor-mode-map (kbd "C-c c")       #'deadgrep)
(define-key my-keys-minor-mode-map (kbd "C-c e")       #'mark-defun)
(define-key my-keys-minor-mode-map (kbd "C-c d")       #'duplicate-dwim)
(define-key my-keys-minor-mode-map (kbd "C-c f")       #'ido-find-file)
(define-key my-keys-minor-mode-map (kbd "C-c g")       #'counsel-git-grep-at-point)
(define-key my-keys-minor-mode-map (kbd "C-c h")       #'lsp-execute-code-action)
(define-key my-keys-minor-mode-map (kbd "C-c i")       #'counsel-imenu)
(define-key my-keys-minor-mode-map (kbd "C-c j")       #'dired-jump)
(define-key my-keys-minor-mode-map (kbd "C-c k")       #'evil-join)
(define-key my-keys-minor-mode-map (kbd "C-c l")       #'shell-command)
(define-key my-keys-minor-mode-map (kbd "C-c m")       #'magit-status-here)
(define-key my-keys-minor-mode-map (kbd "C-c b")       #'toggle-full-window)
(define-key my-keys-minor-mode-map (kbd "C-l")         #'er/expand-region)
(define-key my-keys-minor-mode-map (kbd "C-c q")       #'ace-jump-mode)
(define-key my-keys-minor-mode-map (kbd "C-c t")       #'rename-buffer)
(define-key my-keys-minor-mode-map (kbd "C-c C-r")     #'query-replace-regexp)
(define-key my-keys-minor-mode-map (kbd "C-c s")       #'lsp-find-references)
(define-key my-keys-minor-mode-map (kbd "C-c n")       #'olivetti-mode)
(define-key my-keys-minor-mode-map (kbd "C-c v")       #'vundo)
(define-key my-keys-minor-mode-map (kbd "C-c x")       #'kill-buffer-and-window)
(define-key my-keys-minor-mode-map (kbd "C-c y")       #'browse-kill-ring)
(define-key my-keys-minor-mode-map (kbd "M-h")         #'drag-stuff-up)
(define-key my-keys-minor-mode-map (kbd "M-g")         #'drag-stuff-down)
(define-key my-keys-minor-mode-map (kbd "M-SPC")       #'ace-window)
(define-key my-keys-minor-mode-map (kbd "M-X")         #'smex-major-mode-commands)
(define-key my-keys-minor-mode-map (kbd "M-p")         #'xah-toggle-letter-case)
(define-key my-keys-minor-mode-map (kbd "M-l")         #'my-copy-till-end-of-line)
(define-key my-keys-minor-mode-map (kbd "M-u")         #'ido-switch-buffer)
(define-key my-keys-minor-mode-map (kbd "M-x")         #'smex)
(define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)

(defun move-to-new-line ()
  (interactive)
  (let ((col (current-column)))
    (beginning-of-line)
    (electric-newline-and-maybe-indent)
    (move-to-column col)))
(define-key my-keys-minor-mode-map (kbd "C-j")         #'move-to-new-line)

(define-key rustic-mode-map (kbd "C-4") #'rustic-cargo-build)
(define-key rustic-mode-map (kbd "C-5") #'rustic-cargo-run)
(define-key rustic-mode-map (kbd "C-7") #'rustic-cargo-test)

(define-key ido-file-completion-map (kbd "C-n") #'ido-next-match)
(define-key ido-file-completion-map (kbd "C-p") #'ido-prev-match)
(define-key ido-buffer-completion-map (kbd "C-n") #'ido-next-match)
(define-key ido-buffer-completion-map (kbd "C-p") #'ido-prev-match)

(define-key evil-normal-state-map (kbd "C-p") nil)
(define-key evil-motion-state-map (kbd "C-p") #'evil-jump-forward)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" #'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

(defun disable-my-keys ()
  (my-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'disable-my-keys)

(provide 'keys)
