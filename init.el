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
		     evil
                     evil-commentary
		     evil-collection
                     evil-numbers
                     evil-mc
		     exec-path-from-shell 
                     expand-region
                     gruber-darker-theme
                     rustic
                     flx-ido
                     go-mode
                     heaven-and-hell
                     highlight-indent-guides
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
 
(menu-bar-mode -1) 
(tool-bar-mode -1)
(customize-set-variable 'scroll-bar-mode nil)
(setq visible-bell t)
(setq ring-bell-function 'ignore)

(global-display-line-numbers-mode 1)

(use-package heaven-and-hell
  :ensure t
  :config
  (setq heaven-and-hell-theme-type 'dark) ;; Omit to use light by default
  (setq heaven-and-hell-themes
        '((light . whiteboard)
          (dark . gruber-darker)))
  ;; Optionall, load themes without asking for confirmation.
  (setq heaven-and-hell-load-theme-no-confirm t)
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c z" . heaven-and-hell-toggle-theme)))

(ido-mode t)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)

;; Set the default font and size
(set-frame-font "Ubuntu Mono 16" nil t)

(setq evil-want-keybinding nil)
(setq evil-want-C-i-jump nil)
(evil-mode)

(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.

(setq rustic-analyzer-command '("~/.cargo/bin/rust-analyzer"))
(use-package rustic
  :custom
  (rustic-analyzer-command '("rustup" "run" "nightly" "rust-analyzer")))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))

(setq lsp-auto-guess-root nil)
(setq lsp-ui-peek-always-show t)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(projectile-mode +1)
;; Recommended keymap prefix on macOS

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(when (eq system-type 'darwin)
  (setq
   ns-command-modifier 'control
   ns-option-modifier 'meta
   ns-control-modifier 'super
   ns-function-modifier 'hyper))

(evil-collection-init)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("package-lock.json" . text-mode))

(add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))

(add-hook 'java-mode-hook #'lsp)
(add-hook 'js2-mode-hook #'lsp)
(add-hook 'rustic-mode-hook #'lsp)
(add-hook 'typescript-mode-hook #'lsp)

(autoload 'kill-ring-search "kill-ring-search"
  "Search the kill ring in the minibuffer."
  (interactive))

(setq buffer-save-without-query t)
(setq make-backup-files nil)

(electric-pair-mode t)

(setq backup-directory-alist
   `(("." . ,(concat user-emacs-directory "backups"))))

(defun save-all ()
   (interactive)
   (save-some-buffers t))

(add-hook 'focus-out-hook 'save-all)

(defalias 'yes-or-no-p 'y-or-n-p)

(require #'dired-x)
(setq dired-omit-files "^\\...+$")
(setq dired-dwim-target t)
(setq dired-auto-revert-buffer t)
;; requires coreutils
(setq insert-directory-program "gls" dired-use-ls-dired t)
(setq dired-listing-switches "-alh --group-directories-first")

(drag-stuff-global-mode)

;; fixme: PATH hardcoded
(setq-default shell-file-name "/opt/homebrew/bin/fish") 

(setq-default indent-tabs-mode nil)

(set-face-attribute 'font-lock-comment-face nil :foreground "#5B6268" :slant 'italic)

;; requires (nerd-icons-install-fonts)
(doom-modeline-mode)

(evil-commentary-mode)
(global-hl-todo-mode)

;; Open compilation buffer bottom
;; (setq split-height-threshold nil)
;; (setq split-width-threshold most-positive-fixnum)
(setq org-html-validation-link nil)

(setq-default indent-tabs-mode nil)

(which-key-mode)

(default-text-scale-mode)

(add-to-list #'load-path (concat user-emacs-directory "src/") t)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(setq olivetti-body-width 100)
(setq compilation-ask-about-save nil)

(winner-mode 1)

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun toggle-full-window()
  "Toggle the full view of selected window"
  (interactive)
  (if (window-parent)
      (delete-other-windows)
    (winner-undo)))

(global-evil-mc-mode  1)

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
(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

(use-package highlight-indent-guides
  :ensure t
  :config
  (set-face-foreground 'highlight-indent-guides-character-face "#111111")
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character))

(require 'keys)

;; Uncomment line below for emacs debugging:
;; (setq debug-on-error t)
;; Notes: toggle mark Alt + Space
;; Install wget on system and use M-x shell: wget URL to download to current Dired directory
;; C-x r   - rectangle commands
;; M-o     - look for text in directory in dired-mode
;; C-c C-o - look for file in directory in dired-mode
;; C-3     - recompile

