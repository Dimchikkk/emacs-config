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
                     rust-mode
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
          (dark . doom-xcode)))
  ;; Optionall, load themes without asking for confirmation.
  (setq heaven-and-hell-load-theme-no-confirm t)
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c z" . heaven-and-hell-toggle-theme)))

(ido-mode t)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)

;; Set the default font and size
(set-frame-font "Victor Mono 16" nil t)

(setq evil-want-keybinding nil)
(setq evil-want-C-i-jump nil)
(evil-mode)

(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.

(require 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(setq lsp-auto-guess-root nil)
(setq lsp-rust-analyzer-cargo-watch-command "")
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

(add-hook 'java-mode-hook #'lsp)
(add-hook 'js2-mode-hook #'lsp)
(add-hook 'rust-mode-hook #'lsp)
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

;; Install wget on system and use M-x shell: wget URL to download to current Dired directory

(global-evil-mc-mode  1)

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)

;; Load custom keybindings.
(require #'keys)

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
        ("IDEA"  .  "#1E90FF")))
(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(deeper-blue))
 '(custom-safe-themes
   '("4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8" "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882" "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290" "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700" "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7" "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" "9013233028d9798f901e5e8efb31841c24c12444d3b6e92580080505d56fd392" "c5878086e65614424a84ad5c758b07e9edcf4c513e08a1c5b1533f313d1b17f1" "b754d3a03c34cfba9ad7991380d26984ebd0761925773530e24d8dd8b6894738" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "4b6cc3b60871e2f4f9a026a5c86df27905fb1b0e96277ff18a76a39ca53b82e1" "37b6695bae243145fa2dfb41440c204cd22833c25cd1993b0f258905b9e65577" default))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(package-selected-packages
   '(rustic yaml-mode writeroom-mode which-key wgrep vundo vterm vlf typescript-mode tree-sitter-langs tabby-mode sudo-edit smex smartparens rust-mode projectile pretty-mode olivetti multiple-cursors magit-todos lsp-ui lsp-java language-id kill-ring-search jsonian js2-mode indent-guide htmlize highlight-thing highlight-indent-guides heaven-and-hell go-mode flx-ido fix-word expand-region exec-path-from-shell evil-numbers evil-multiedit evil-mc evil-commentary evil-collection eglot dumb-jump drag-stuff doom-themes doom-modeline dockerfile-mode default-text-scale deadgrep cursor-flash counsel corfu company-tabnine browse-kill-ring ace-jump-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
