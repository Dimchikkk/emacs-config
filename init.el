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
                     expand-region
                     gruber-darker-theme
                     multiple-cursors
                     rustic
                     flx-ido
                     go-mode
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

(require 'recentf)
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

(set-frame-font "Ubuntu Mono 20" nil t)

(smex-initialize)

(setq lsp-rust-analyzer-completion-auto-import-enable t)

(setq rustic-analyzer-command '("~/.cargo/bin/rust-analyzer"))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))

(setq lsp-auto-guess-root nil)
(setq lsp-ui-peek-always-show t)

(projectile-mode +1)

;; Recommended keymap prefix on macOS
(when (memq window-system '(mac ns x)) (exec-path-from-shell-initialize))
(when (eq system-type 'darwin)
  (setq
   ns-command-modifier 'control
   ns-option-modifier 'meta
   ns-control-modifier 'super
   ns-function-modifier 'hyper))

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

(defun save-all () (interactive) (save-some-buffers t))
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

;; requires (nerd-icons-install-fonts)
(doom-modeline-mode)
(global-hl-todo-mode)

(setq-default indent-tabs-mode nil)

(which-key-mode)
(default-text-scale-mode)
(add-to-list #'load-path (concat user-emacs-directory "src/") t)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(setq olivetti-body-width 100)
(setq compilation-ask-about-save nil)

(winner-mode 1)

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

(require 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)

(require 'keys)

;; Uncomment line below for emacs debugging:
;; (setq debug-on-error t)
;; Notes: toggle mark Alt + Space
;; Install wget on system and use M-x shell: wget URL to download to current Dired directory
;; C-x r     - rectangle commands
;; M-o       - look for text in directory in dired-mode
;; C-c C-o   - look for file in directory in dired-mode
;; C-4       - recompile
;; Shift + V - to select line
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(multiple-cursors yaml-mode which-key wgrep vundo vlf typescript-mode sudo-edit smex rustic rainbow-mode projectile pretty-mode olivetti magit-todos lsp-ui lsp-java js2-mode htmlize highlight-indent-guides heaven-and-hell gruber-darker-theme go-mode gherkin-mode flx-ido feature-mode expreg expand-region exec-path-from-shell evil-numbers evil-mc evil-commentary evil-collection dumb-jump drag-stuff doom-themes doom-modeline dockerfile-mode default-text-scale deadgrep counsel company browse-kill-ring ace-jump-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
