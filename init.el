;; Faster startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
  (lambda () (setq file-name-handler-alist default-file-name-handler-alist)))

(setq package-list '(
                     ace-jump-mode
                     browse-kill-ring
                     consult
                     company
                     default-text-scale
                     deadgrep
                     dockerfile-mode
                     drag-stuff
                     doom-modeline
                     doom-themes
                     dumb-jump
                     embark
                     embark-consult
                     exec-path-from-shell
                     gruber-darker-theme
                     markdown-preview-mode
                     marginalia
                     multiple-cursors
                     orderless
                     rustic
                     go-mode
                     hl-todo
                     htmlize
                     js2-mode
                     lsp-mode
                     lsp-ui
                     magit
                     olivetti
                     pretty-mode
                     projectile
                     rainbow-mode
                     sudo-edit
                     typescript-mode
                     vertico
                     vundo
                     vlf
                     yaml-pro
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

(when (eq system-type 'darwin)
  (setq
   ns-command-modifier 'control
   ns-option-modifier 'meta
   ns-control-modifier 'super
   ns-function-modifier 'hyper))

(auto-save-visited-mode 1)
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

(save-place-mode 1)
(global-auto-revert-mode 1)
(pixel-scroll-precision-mode 1)

(require 'gruber-darker-theme)
(load-theme 'gruber-darker t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(customize-set-variable 'scroll-bar-mode nil)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(global-display-line-numbers-mode 1)

(if (eq system-type 'darwin)
    (set-frame-font "Ubuntu Mono 18" nil t)
  (set-frame-font "Ubuntu Mono 14" nil t))

(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t))
;;  (orderless-matching-styles '(orderless-flex)))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :config
  (setq consult-project-root-function #'projectile-project-root))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)))

(use-package embark-consult
  :after (embark consult))

(setq lsp-rust-analyzer-completion-auto-import-enable t)

(setq rustic-analyzer-command
      (list (concat (getenv "HOME") "/.cargo/bin/rust-analyzer")))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))


(setq lsp-signature-render-documentation nil)
(setq lsp-auto-guess-root nil)

(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-enable-caching t)
  (setq projectile-cache-file (expand-file-name "projectile.cache" user-emacs-directory))
  (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))
  (projectile-load-known-projects))


(delete-selection-mode 1)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("package-lock.json" . text-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-pro-mode))

(dolist (hook '(c-mode-hook))
  (add-hook hook
            (lambda ()
              (add-hook 'before-save-hook #'whitespace-cleanup nil t))))

(savehist-mode 1)
(setq savehist-additional-variables '(compile-command))
(use-package lsp-java
  :ensure t
  :config
  (setq lsp-java-jdt-download-url
        "https://download.eclipse.org/jdtls/milestones/1.44.0/jdt-language-server-1.44.0-202501221502.tar.gz")

  (setq lsp-java-vmargs
        '("-XX:+UseParallelGC"
          "-XX:GCTimeRatio=4"
          "-Xmx2G"
          "-Xms512m"))

  :hook (java-mode . lsp))

(require 'rustic)
(add-hook 'js2-mode-hook #'lsp)
(add-hook 'rustic-mode-hook #'lsp)
(add-hook 'typescript-mode-hook #'lsp)

(add-hook 'c-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)

(setq buffer-save-without-query t)
(setq make-backup-files nil)

(electric-pair-mode t)

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(defun save-all() (interactive) (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

(setq use-short-answers t)

(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(setq dired-dwim-target t)
(setq dired-auto-revert-buffer t)
;; requires coreutils

(if (eq system-type 'darwin)
    (setq insert-directory-program "gls")
  (setq insert-directory-program "ls"))

(setq dired-use-ls-dired t)
(setq dired-listing-switches "-alh --group-directories-first")

(drag-stuff-global-mode)

;; Speed up exec-path-from-shell on macOS
(when (eq system-type 'darwin)
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-arguments '("-l")))
(exec-path-from-shell-initialize)
(setq-default shell-file-name (or (executable-find "fish") "/bin/bash"))
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

(use-package magit
  :ensure t
  :custom
  (magit-git-executable (if (eq system-type 'darwin) "/opt/homebrew/bin/git" "git"))
  (magit-process-connection-type nil)
  :init
  (use-package with-editor :ensure t)

  ;; Have magit-status go full screen and quit to previous
  ;; configuration.  Taken from
  ;; http://whattheemacsd.com/setup-magit.el-01.html#comment-748135498
  ;; and http://irreal.org/blog/?p=2253
  (defun my/magit-status-fullscreen (orig-fun &rest args)
    (window-configuration-to-register :magit-fullscreen)
    (apply orig-fun args)
    (delete-other-windows))
  (advice-add 'magit-status :around #'my/magit-status-fullscreen)

  (defun my/magit-quit-restore (&rest _)
    (jump-to-register :magit-fullscreen))
  (advice-add 'magit-quit-window :after #'my/magit-quit-restore)
  :config
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent))

(setq hl-todo-keyword-faces
      '(("TODO"   . "#A020F0")
        ("FIXME"  . "#A020F0")
        ("NOTE"  .  "#1E90FF")))
;; (magit-todos-mode 1)

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode Keymap.")

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun occur-thing-at-point()
  (interactive)
  (let ((term (thing-at-point 'symbol t))) (occur term)))

(defun consult-git-grep-at-point()
  (interactive)
  (let ((term (thing-at-point 'symbol t))) (consult-git-grep nil term)))

(defun my-duplicate-dwim ()
  "Call `duplicate-dwim` and move cursor appropriately after duplication.
If duplicating a line, move point to the duplicated line preserving column.
If duplicating a region, move point to the new duplicated region and then remove the selection."
  (interactive)
  (let ((region-active (use-region-p))
        (col (current-column))
        beg end new-beg new-end)
    (if region-active
        (setq beg (region-beginning)
              end (region-end))
      (setq beg (line-beginning-position)
            end (line-end-position)))
    (duplicate-dwim)
    (if region-active
        ;; Region duplicated: new region inserted after original
        (progn
          (setq new-beg (+ end 0)) ;; duplicated text inserted right after original region
          (setq new-end (+ new-beg (- end beg)))
          (goto-char new-beg)
          ;; Remove selection after moving point
          (deactivate-mark))
      ;; Line duplicated: move to start of new line preserving column
      (progn
        (forward-line 1)
        (move-to-column col)))))
(defun my/open-repo ()
  "Open git repo in browser"
  (interactive)
  (let ((url (shell-command-to-string "git remote get-url origin")))
    (browse-url
     (replace-regexp-in-string
      "\\.git\n" ""
      (replace-regexp-in-string
       "git@github\\.com:" "https://github.com/" url)))))

(define-key my-keys-minor-mode-map (kbd "C-<return>")  #'compile)
(define-key my-keys-minor-mode-map (kbd "M-x")         #'execute-extended-command)
(define-key my-keys-minor-mode-map (kbd "C-,")         #'my-duplicate-dwim)
(define-key my-keys-minor-mode-map (kbd "C-s")         #'consult-line)
(define-key my-keys-minor-mode-map (kbd "C-c C-s")     #'isearch-forward)
(define-key my-keys-minor-mode-map (kbd "C-c /")       #'consult-compile-error)
(define-key my-keys-minor-mode-map (kbd "C-j")         #'mark-sexp)
(define-key my-keys-minor-mode-map (kbd "C--")         #'default-text-scale-decrease)
(define-key my-keys-minor-mode-map (kbd "C-=")         #'default-text-scale-increase)
(define-key my-keys-minor-mode-map (kbd "C-c C-k")     #'kill-other-buffers)
(define-key my-keys-minor-mode-map (kbd "C-c =")       #'sort-lines)
(define-key my-keys-minor-mode-map (kbd "C-c C-c M-x") #'execute-extended-command)
(define-key my-keys-minor-mode-map (kbd "C-c C-f")     #'ffap)
(define-key my-keys-minor-mode-map (kbd "C-c SPC")     #'consult-recent-file)
(define-key my-keys-minor-mode-map (kbd "M-o")         #'projectile-find-file)
(define-key my-keys-minor-mode-map (kbd "C-c t")       #'find-grep-dired)
(define-key my-keys-minor-mode-map (kbd "C-c C-t")     #'find-name-dired)
(define-key my-keys-minor-mode-map (kbd "C-c o")       #'occur-thing-at-point)
(define-key my-keys-minor-mode-map (kbd "C-c C-o")     #'occur)
(define-key my-keys-minor-mode-map (kbd "M-<return>")  #'consult-buffer)
(define-key my-keys-minor-mode-map (kbd "C-c a")       #'align-regexp)
(define-key my-keys-minor-mode-map (kbd "C-c d")       #'deadgrep)
(define-key my-keys-minor-mode-map (kbd "C-c c")       #'consult-ripgrep)
(define-key my-keys-minor-mode-map (kbd "C-c g")       #'consult-git-grep-at-point)
(define-key my-keys-minor-mode-map (kbd "C-c h")       #'lsp-execute-code-action)
(define-key my-keys-minor-mode-map (kbd "C-c i")       #'consult-imenu)
(define-key my-keys-minor-mode-map (kbd "C-c j")       #'ace-jump-mode)
(define-key my-keys-minor-mode-map (kbd "C-c r")       #'rename-buffer)
(define-key my-keys-minor-mode-map (kbd "C-c C-r")     #'my/open-repo)
(define-key my-keys-minor-mode-map (kbd "C-c q")       #'query-replace-regexp)
(define-key my-keys-minor-mode-map (kbd "C-c s")       #'lsp-find-references)
(define-key my-keys-minor-mode-map (kbd "C-c m")       #'olivetti-mode)
(define-key my-keys-minor-mode-map (kbd "C-c v")       #'vundo)
(define-key my-keys-minor-mode-map (kbd "C-c x")       #'kill-buffer-and-window)
(define-key my-keys-minor-mode-map (kbd "C-c y")       #'browse-kill-ring)
(define-key my-keys-minor-mode-map (kbd "M-<up>")      #'drag-stuff-up)
(define-key my-keys-minor-mode-map (kbd "M-<down>")    #'drag-stuff-down)
(define-key my-keys-minor-mode-map (kbd "C-S-c C-S-c") #'mc/edit-lines)
(define-key my-keys-minor-mode-map (kbd "C->")         #'mc/mark-next-like-this)
(define-key my-keys-minor-mode-map (kbd "C-<")         #'mc/mark-previous-like-this)
(define-key my-keys-minor-mode-map (kbd "C-c C-<")     #'mc/mark-all-like-this)
(define-key my-keys-minor-mode-map (kbd "M-p")         #'previous-error)
(define-key my-keys-minor-mode-map (kbd "M-n")         #'next-error)
(define-key my-keys-minor-mode-map (kbd "C-c f")       #'winner-redo)
(define-key my-keys-minor-mode-map (kbd "C-c b")       #'winner-undo)
(define-key projectile-mode-map (kbd "C-c p")          #'projectile-command-map)


(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys"
  :keymap my-keys-minor-mode-map)

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

(setq mc/always-run-for-all t)

(use-package multiple-cursors
  :init
  (use-package phi-search
    :init
    ;; credit to @jonebird for the following
    ;; Allow isearch functionality with multipl-cursors
    (add-hook 'multiple-cursors-mode-enabled-hook
              (lambda ()
                (interactive)
                (global-set-key (kbd "C-s") 'phi-search)
                (global-set-key (kbd "C-r") 'phi-search-backward)))

    (add-hook 'multiple-cursors-mode-disabled-hook
              (lambda ()
                (interactive)
                (global-set-key (kbd "C-c C-s") 'isearch-forward)
                (global-set-key (kbd "C-r") 'isearch-backward)))))

(setq markdown-command "pandoc")

(defun start-process@use-pipe (fn &rest args)
    ;; checkdoc-params: (fn args)
    "Advice to ensure that `start-process' uses a pipe rather than
a pty for the compilation command. This increases performance on OSX
by a factor of 10, as the default pty size is a pitiful 1024 bytes."
    (let ((process-connection-type nil))
      (apply fn args)))
(advice-add 'start-process :around #'start-process@use-pipe)

(setq native-comp-warning-on-missing-source nil
      native-comp-async-report-warnings-errors 'silent)

(setq bookmark-save-flag 1)
;; C-x h      - select whole file
;; C-x 0      - close active window
;; C-m        - instead of Return
;; C-j        - to make selection
;; C-,        - duplicate-dwim
;; C-x SPC    - rectangle selection
;; C-<return> - wget URL to download to current Dired directory
;; open magit, then press 'y', then b b RET to open local branch or b c RET to create local from remote
;; C-j in magit - visit source file
