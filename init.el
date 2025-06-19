(setq package-list '(
                     ace-jump-mode
                     browse-kill-ring
                     counsel
                     company
                     default-text-scale
                     deadgrep
                     dockerfile-mode
                     drag-stuff
                     doom-modeline
                     doom-themes
                     dumb-jump
                     exec-path-from-shell
                     gruber-darker-theme
                     multiple-cursors
                     rustic
                     flx-ido
                     go-mode
                     hl-todo
                     htmlize
                     js2-mode
                     lsp-mode
                     lsp-ui
                     magit
                     magit-todos
                     olivetti
                     pretty-mode
                     projectile
                     rainbow-mode
                     smex
                     swiper
                     sudo-edit
                     typescript-mode
                     vundo
                     vlf
                     yaml-mode
                     wgrep
                     which-key
                     )
      )
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(setq auto-save-visited-mode t)
(setq inhibit-startup-message t)

(defvar my-auto-save-folder "~/.emacs.d/auto-save/" "Directory to store auto-save files.")
(unless (file-exists-p my-auto-save-folder) (make-directory my-auto-save-folder t))
(setq auto-save-file-name-transforms `((".*" ,(concat my-auto-save-folder "\\1") t)))
(defvar my-lock-file-folder "~/.emacs.d/lockfiles/" "Directory to store lock files.")
(unless (file-exists-p my-lock-file-folder) (make-directory my-lock-file-folder t))
(setq lock-file-name-transforms `((".*" ,(concat my-lock-file-folder ".#\\1") t)))

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(require 'gruber-darker-theme)
(load-theme 'gruber-darker t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(customize-set-variable 'scroll-bar-mode nil)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(ido-mode t)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)

(set-frame-font "Ubuntu Mono 14" nil t)

(smex-initialize)

(setq lsp-rust-analyzer-completion-auto-import-enable t)

(setq rustic-analyzer-command
      (list (concat (getenv "HOME") "/.cargo/bin/rust-analyzer")))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))


(setq lsp-signature-render-documentation nil)
(setq lsp-auto-guess-root nil)

(projectile-mode +1)

(delete-selection-mode 1)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("package-lock.json" . text-mode))
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))

(require 'rustic)
(add-hook 'java-mode-hook #'lsp)
(add-hook 'js2-mode-hook #'lsp)
(add-hook 'rustic-mode-hook #'lsp)
(add-hook 'typescript-mode-hook #'lsp)

(setq buffer-save-without-query t)
(setq make-backup-files nil)

(electric-pair-mode t)

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(defun save-all() (interactive) (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

(defalias 'yes-or-no-p 'y-or-n-p)

(require #'dired-x)
(setq dired-omit-files "^\\...+$")
(setq dired-dwim-target t)
(setq dired-auto-revert-buffer t)
;; requires coreutils
(setq insert-directory-program "ls" dired-use-ls-dired t)
(setq dired-listing-switches "-alh --group-directories-first")

(drag-stuff-global-mode)

(exec-path-from-shell-initialize)
(setq-default shell-file-name (or (executable-find "fish") "/bin/bash"))
(setq-default indent-tabs-mode nil)

;; requires (nerd-icons-install-fonts)
(doom-modeline-mode)
(global-hl-todo-mode)

(setq-default indent-tabs-mode nil)

(which-key-mode)
(default-text-scale-mode)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(setq olivetti-body-width 100)
(setq compilation-ask-about-save nil)

(add-hook 'after-init-hook 'global-company-mode)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 100000000)
(setq vlf-application 'dont-ask)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
(setenv "FZF_DEFAULT_COMMAND" "rg --files")
(setq magit-git-executable (locate-file "git" exec-path))
(setq hl-todo-keyword-faces
      '(("TODO"   . "#A020F0")
        ("FIXME"  . "#A020F0")
        ("NOTE"  .  "#1E90FF")))
(magit-todos-mode 1)

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode Keymap.")

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun occur-thing-at-point()
  (interactive)
  (let ((term (thing-at-point 'symbol t))) (occur term)))

(defun counsel-git-grep-at-point()
  (interactive)
  (let ((term (thing-at-point 'symbol t))) (counsel-git-grep term)))

(defcustom bool-flip-alist
  '(("T"    . "F")
    ("t"    . "f")
    ("TRUE" . "FALSE")
    ("True" . "False")
    ("true" . "false")
    ("Y"    . "N")
    ("y"    . "n")
    ("YES"  . "NO")
    ("Yes"  . "No")
    ("yes"  . "no")
    ("1"    . "0"))
  "Alist mapping booleans to their opposites for `bool-flip`.")

(defun bool-flip()
  "Replace the boolean at point with its opposite."
  (interactive)
  (let* ((old (thing-at-point 'symbol t)) ;; Use `t` to strip properties
         (new (or (cdr (assoc old bool-flip-alist))
                  (car (rassoc old bool-flip-alist)))))
    (if new
        (let ((bounds (bounds-of-thing-at-point 'symbol)))
          (if bounds
              (let ((beg (car bounds))
                    (end (cdr bounds)))
                (let ((insert-after (= (point) beg)))
                  (delete-region beg end)
                  (insert new)
                  (when insert-after
                    (goto-char beg))))
            (user-error "No symbol at point")))
      (user-error "Nothing to flip here"))))

(defun duplicate-line ()
  "Duplicate current line by tsoding"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(define-key my-keys-minor-mode-map (kbd "M-<return>")  #'ace-window)
(define-key my-keys-minor-mode-map (kbd "M-X")         #'smex-major-mode-commands)
(define-key my-keys-minor-mode-map (kbd "M-x")         #'smex)
(define-key my-keys-minor-mode-map (kbd "C-.")         #'duplicate-line)
(define-key my-keys-minor-mode-map (kbd "C-c C-s")     #'swiper-isearch)
(define-key my-keys-minor-mode-map (kbd "C-c /")       #'counsel-compilation-errors)
(define-key my-keys-minor-mode-map (kbd "C-,")         #'mark-sexp)
(define-key my-keys-minor-mode-map (kbd "C--")         #'default-text-scale-decrease)
(define-key my-keys-minor-mode-map (kbd "C-=")         #'default-text-scale-increase)
(define-key my-keys-minor-mode-map (kbd "C-c C-k")     #'kill-other-buffers)
(define-key my-keys-minor-mode-map (kbd "C-c =")       #'sort-lines)
(define-key my-keys-minor-mode-map (kbd "C-c C-c M-x") #'execute-extended-command)
(define-key my-keys-minor-mode-map (kbd "C-c C-f")     #'ffap)
(define-key my-keys-minor-mode-map (kbd "C-c SPC")     #'recentf-open-files)
(define-key my-keys-minor-mode-map (kbd "C-c RET")     #'counsel-fzf)
(define-key my-keys-minor-mode-map (kbd "C-c o")       #'find-grep-dired)
(define-key my-keys-minor-mode-map (kbd "C-c C-o")     #'find-name-dired)
(define-key my-keys-minor-mode-map (kbd "C-c l")       #'occur-thing-at-point)
(define-key my-keys-minor-mode-map (kbd "C-<return>")  #'ido-switch-buffer)
(define-key my-keys-minor-mode-map (kbd "C-c a")       #'align-regexp)
(define-key my-keys-minor-mode-map (kbd "C-c c")       #'deadgrep)
(define-key my-keys-minor-mode-map (kbd "C-c g")       #'counsel-git-grep-at-point)
(define-key my-keys-minor-mode-map (kbd "C-c h")       #'lsp-execute-code-action)
(define-key my-keys-minor-mode-map (kbd "C-c i")       #'counsel-imenu)
(define-key my-keys-minor-mode-map (kbd "C-c j")       #'ace-jump-mode)
(define-key my-keys-minor-mode-map (kbd "C-c b")       #'rename-buffer)
(define-key my-keys-minor-mode-map (kbd "C-c q")       #'query-replace-regexp)
(define-key my-keys-minor-mode-map (kbd "C-c s")       #'lsp-find-references)
(define-key my-keys-minor-mode-map (kbd "C-c m")       #'olivetti-mode)
(define-key my-keys-minor-mode-map (kbd "C-c v")       #'vundo)
(define-key my-keys-minor-mode-map (kbd "C-c x")       #'kill-buffer-and-window)
(define-key my-keys-minor-mode-map (kbd "C-c y")       #'browse-kill-ring)
(define-key my-keys-minor-mode-map (kbd "C-c f")       #'bool-flip)
(define-key my-keys-minor-mode-map (kbd "M-<up>")      #'drag-stuff-up)
(define-key my-keys-minor-mode-map (kbd "M-<down>")    #'drag-stuff-down)
(define-key my-keys-minor-mode-map (kbd "M-o")         #'compile)
(define-key my-keys-minor-mode-map (kbd "C-S-c C-S-c") #'mc/edit-lines)
(define-key my-keys-minor-mode-map (kbd "C->")         #'mc/mark-next-like-this)
(define-key my-keys-minor-mode-map (kbd "C-<")         #'mc/mark-previous-like-this)
(define-key my-keys-minor-mode-map (kbd "C-c C-<")     #'mc/mark-all-like-this)
(define-key my-keys-minor-mode-map (kbd "M-p")         #'previous-error)
(define-key my-keys-minor-mode-map (kbd "M-n")         #'next-error)
(define-key projectile-mode-map (kbd "C-c p")          #'projectile-command-map)

(define-key rustic-mode-map (kbd "C-c 3") #'rustic-cargo-build)
(define-key rustic-mode-map (kbd "C-c 4") #'rustic-cargo-run)
(define-key rustic-mode-map (kbd "C-c 5") #'rustic-cargo-test)

(define-key ido-file-completion-map (kbd "C-n") #'ido-next-match)
(define-key ido-file-completion-map (kbd "C-p") #'ido-prev-match)
(define-key ido-buffer-completion-map (kbd "C-n") #'ido-next-match)
(define-key ido-buffer-completion-map (kbd "C-p") #'ido-prev-match)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" #'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

(defun disable-my-keys() (my-keys-minor-mode 0))
(add-hook 'minibuffer-setup-hook 'disable-my-keys)

(custom-set-faces
 '(company-tooltip-selection ((t (:background "#0000ff")))))

(setq-default js2-basic-offset 2
              js-indent-level 2)

(setq whitespace-style
      '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark))
(global-whitespace-mode)

(setq deadgrep-extra-arguments '("--multiline"))

(winner-mode 1)

;; C-x h - select whole file
;; C-x 0 - close active window
;; C-m instead of Return
;; C-, to make selection
;; C-. duplicate line
;; M-o: wget URL to download to current Dired directory
