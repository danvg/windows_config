;;; init.el --- My personal Emacs config

;;; Commentary:
;;; A vanilla but functional Emacs config

;;; Code:

;; User information

(setq user-full-name "Dan VG"
      user-mail-address "dava1000@student.miun.se")

;; Garbage collection optimization

(eval-and-compile
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6))

;; Options

;; Initial frame size
(add-to-list 'default-frame-alist '(width . 110))
(add-to-list 'default-frame-alist '(height . 38))

;; Default font settings
(add-to-list 'default-frame-alist '(font . "JetBrainsMono NF-11"))

;; Some sane defaults
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(setq sentence-end-double-space nil)

;; Write customize variables to another file, don't polute
;; the init.el file
(setq custom-file "~/.emacs.d/custom-vars.el")

;; Write backup files to another location
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Set the initial default buffer
(kill-buffer "*scratch*")
(setq default-directory "~/")

;; Disable visible scrollbar
(scroll-bar-mode -1)

;; Disable the toolbar
(tool-bar-mode -1)

;; Disable tooltips
(tooltip-mode -1)

;; Command delay
(set-fringe-mode 10)

;; Disable the menu bar
(menu-bar-mode -1)

;; Enable visual bell
(setq visible-bell t)

;; Display column number in the ruler
(column-number-mode t)

;; Display the line numbers
(global-display-line-numbers-mode t)

;; Typed text replaces selection
(delete-selection-mode t)

;; Disable line numbers in terminals
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Highlight the current cursor line
(hl-line-mode t)

;; Display a line at column 80
(setq display-fill-column-indicator-column 80)
(display-fill-column-indicator-mode 1)

;; Scrolling
(setq mouse-wheel-follow-mouse 't)

;; Auto pairs
(electric-pair-mode t)

;; Default encoding is UTF8 for everything
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(set-language-environment "UTF-8")
(setq-default buffer-file-coding-system 'utf-8-unix)

;; Indentation
(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)

;; Packages

;; Initialize package sources
(require 'package)

;; Package repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize packages
(package-initialize)

;; Refresh packages if needed
;;(unless package-archive-contents
;;  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Use use-package as package installer
(require 'use-package)
(setq use-package-always-defer t
      use-package-verbose t)

;; Vim emulation
(use-package evil
  :demand t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))
  ; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;; Add additional evil mode support
(use-package evil-collection
  :after evil
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (evil-collection-init))

;; Key menu
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Package to handle key bindings
(use-package general
  :after evil
  :config
  (general-evil-setup t)
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

;; Icons for the modeline. Need to install the fonts first with
;; all-the-icons-install-all-fonts
(use-package all-the-icons)

;; Fancy themes. Use M-x counsel-load-themes to change the theme.
(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Fancy mode line
(use-package doom-modeline
  :demand t
  :hook (after-init . doom-modeline-mode)
  :custom-face
  (mode-line ((t (:height 0.85))))
  (mode-line-inactive ((t (:height 0.85))))
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 8)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon t))

;; A generic completion frontend
(use-package ivy
  :demand t
  :diminish
  :bind (:map ivy-minibuffer-map
              ("TAB" . ivy-alt-done)
              ("C-l" . ivy-alt-done)
              ("C-j" . ivy-next-line)
              ("C-k" . ivy-previous-line)
              :map ivy-switch-buffer-map
              ("C-k" . ivy-previous-line)
              ("C-l" . ivy-done)
              ("C-d" . ivy-switch-buffer-kill)
              :map ivy-reverse-i-search-map
              ("C-k" . ivy-previous-line)
              ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1))

;; More friendly interface for ivy
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; Various completion functions using Ivy
(use-package counsel
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

(defun rune/lsp-mode-setup ()
  "Setup the LSP mode."
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;; Emacs LSP client
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . rune/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

;; An UI for the LSP mode
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show))

;; Provides an interactive ivy interface to the workspace symbol
;; functionality offered by lsp-mode
(use-package lsp-ivy)

;; Modular in-buffer completion framework
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;; A company front-end with icons
(use-package company-box
  :hook (company-mode . company-box-mode))

;; Alternative to the built-in Emacs help that provides much
;; more contextual information
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Snippets
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

;; Snippet collection
(use-package yasnippet-snippets
  :after yasnippet)

;; Linting
(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

;; Support for Java language
(use-package lsp-java
  :config
  (add-hook 'java-mode-hook 'lsp))

;; Support for the TypeScript language
(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(defun rune/set-js-indentation ()
  "The javascript indentation level."
  (setq js-indent-level 2)
  (setq evil-shift-width js-indent-level)
  (setq-default tab-width 2))

;; Support for JavaScript language
(use-package js2-mode
  :mode "\\.jsx?\\'"
  :config
  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)

  ;; Set up proper indentation in JavaScript and JSON files
  (add-hook 'js2-mode-hook #'rune/set-js-indentation)
  (add-hook 'json-mode-hook #'rune/set-js-indentation))

;; Pretty printer for web languages
(use-package prettier-js
  :hook ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode))
  :config
  (setq prettier-js-show-errors nil))

;; Support for some web languages
(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

;; Hide minor modes from the modeline
(use-package diminish
  :config (diminish 'eldoc-mode))

;; Key mapping helper
(use-package hydra
  :ensure t
  :config
  (defhydra hydra-common (:color blue)
    ("<ESC>" nil "quit")))

;; Multi-colored delimeters
(show-paren-mode t)
(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

;; Better syntax highlighting
(use-package tree-sitter
  :init (global-tree-sitter-mode)
  :hook ((js-mode . tree-sitter-hl-mode)
         (typescript-mode . tree-sitter-hl-mode)))

;; Tree-sitter Language Bundle for Emacs
(use-package tree-sitter-langs)

;; Bufferline
(use-package centaur-tabs
  :demand t
  :config
  (centaur-tabs-mode t)
  :custom
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-style "rounded")
  (centaur-tabs-height 36)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "?")
  (centaur-tabs-buffer-groups-function #'centaur-tabs-projectile-buffer-groups)
  :bind
  (("s-{" . #'centaur-tabs-backward)
   ("s-}" . #'centaur-tabs-forward)))

;; A simple directory drawer
(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar))

;; Git integration
(use-package magit
  :commands (magit-status magit-blame))

;; Project managment
(use-package projectile
  :demand t
  :init
  (projectile-mode +1)
  :bind
  (:map projectile-mode-map
        ("s-p" . projectile-command-map)
        ("C-c p" . projectile-command-map)))

;; Provides further ivy integration into projectile
(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode))

;; A startup dashboard
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-center-content t)
(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)))
(use-package dashboard
  :demand t
  :config
  (dashboard-setup-startup-hook))

;; Add language hooks for LSP and Linting
(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'flycheck-mode)

;; Key bindings

;; Make Esc key quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defhydra hydra-text-scale (:timeout 4)
  "Scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "t" '(:ignore t :which-key "toggles")
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; Daemon

(defun rune/font-setup ()
  "Setup the fonts."
  (set-frame-font "JetBrainsMono NF-12" nil t))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'rune/font-setup))

(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)

(provide 'init)
;;; init.el ends here
