;;; init.el --- My personal Emacs configuration

;;; Commentary:
;;; A vanilla but functional Emacs configuration

;;; Code:

;; User information
(setq user-full-name "Dan VG"
      user-mail-address "dava1000@student.miun.se")

;; Garbage collection optimization
(eval-and-compile
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6))

;; Options

;; Default frame properties
(add-to-list 'default-frame-alist '(width . 110))
(add-to-list 'default-frame-alist '(height . 38))
(add-to-list 'default-frame-alist '(font . "JetBrainsMono NF-13"))
(add-to-list 'default-frame-alist '(line-spacing . 0.2))

;; Some sane defaults
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(setq sentence-end-double-space nil)

;; Write customize variables to another file, don't pollute init.el
(setq custom-file "~/.emacs.d/custom-vars.el")

;; Write backup files to another location
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Persistent history
(savehist-mode)
(setq savehist-file "~/.emacs.d/history")

;; Set the initial default buffer
(kill-buffer "*scratch*")
(setq default-directory "~/")

;; Exclude from recent files
(setq recentf-exclude '("~/.emacs.d/elpa/"))

;; Disable visible scrollbar
(scroll-bar-mode -1)

;; Disable the tool bar
(tool-bar-mode -1)

;; Disable tool tips
(tooltip-mode -1)

;; Disable the menu bar
(menu-bar-mode -1)

;; Command delay
(set-fringe-mode 10)

;; Enable visual bell
(setq visible-bell t)

;; Typed text replaces selection
(delete-selection-mode t)

;; Display column number in the ruler
(column-number-mode t)

;; Display the line numbers
(global-display-line-numbers-mode t)

;; Disable line numbers in some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Highlight the current cursor line
(hl-line-mode 1)

;; Display a line at column 80
(setq display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Scrolling
(setq scroll-conservatively 101)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3)))
(setq mouse-wheel-progressive-speed t)
(setq mouse-wheel-follow-mouse 't)

;; Auto pairs
(electric-pair-mode t)

;; Default encoding is UTF8 for everything
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq default-process-coding-system 'utf-8-unix)
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
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Use use-package as package installer
(require 'use-package)
(setq use-package-always-defer t
      use-package-always-ensure t
      use-package-verbose t)

;; Vim emulation
(use-package evil
  :demand
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump nil)
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

;; Add additional evil mode support
(use-package evil-collection
  :after evil
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (evil-collection-init))

;; Undo tree system
(use-package undo-tree
  :after evil
  :diminish
  :config
  (global-undo-tree-mode t)
  (evil-set-undo-system 'undo-tree)
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

;; Key menu
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 0.3))

;; Package to handle key bindings
(use-package general
  :after evil
  :config
  (general-evil-setup t)
  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

;; Icons for the modeline. Need to install the fonts first with
;; all-the-icons-install-all-fonts
(use-package all-the-icons)

;; Icons for the dired mode
(use-package all-the-icons-dired
  :after all-the-icons
  :hook
  (dired-mode . all-the-icons-dired-mode))

;; Fancy themes. Use M-x counsel-load-themes to change the theme.
(use-package doom-themes
  :demand
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t))

;; Fancy mode line
(use-package doom-modeline
  :demand
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
  :demand
  :diminish
  :bind
  (:map ivy-minibuffer-map
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
  :bind
  (("M-x" . counsel-M-x)
   ("C-x b" . counsel-ibuffer)
   ("C-x C-f" . counsel-find-file)
   :map minibuffer-local-map
   ("C-r" . 'counsel-minibuffer-history))
  :custom
  (ivy-initial-inputs-alist nil))

;; Emacs LSP client
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (c++-mode . lsp-mode)
  :config
  (lsp-enable-which-key-integration t)
  (lsp-headerline-breadcrumb-mode)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)))

;; An UI for the LSP mode
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (lsp-ui-doc-show)
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-position 'at-point))

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
  :hook
  (lsp-mode . flycheck-mode)
  (emacs-lisp-mode . flycheck-mode))

;; Spell-checking
(use-package flyspell
  :ensure nil
  :bind
  (:map flyspell-mouse-map ([down-mouse-3] . flyspell-correct-word))
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  :custom
  (ispell-program-name "~/.emacs.d/hunspell/bin/hunspell")
  (ispell-hunspell-dict-paths-alist '(("en_US" "~/.emacs.d/hunspell/share/hunspell/en_US.aff")))
  (ispell-local-dictionary "en_US")
  (ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))

;; Support for Java language
(use-package lsp-java
  :hook (java-mode . lsp-mode))

;; Support for Markdown syntax
(use-package markdown-mode)

;; Support for the TypeScript language
(use-package typescript-mode
  :mode "\\.ts\\'"
  :custom
  (typescript-indent-level 2))

;; Support for JavaScript language
(use-package js2-mode
  :mode "\\.jsx?\\'"
  :custom
  ;; Don't use built-in syntax checking
  (js2-mode-show-strict-warnings nil)
  ;; Set up proper indentation in JavaScript and JSON files
  (js-indent-level 2)
  (evil-shift-width js-indent-level)
  (default tab-width 2))

;; Pretty printer for web languages
(use-package prettier-js
  :hook ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode))
  :custom
  (prettier-js-show-errors nil))

;; Support for some web languages
(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-attribute-indent-offset 2))

;; Hide minor modes from the modeline
(use-package diminish
  :config (diminish 'eldoc-mode))

;; Key mapping helper
(use-package hydra
  :config
  (defhydra hydra-common (:color blue)
    ("<ESC>" nil "quit")))

;; Multi-colored delimiters
(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode))
  :custom
  (show-paren-mode t))

;; Better syntax highlighting
(use-package tree-sitter
  :init (global-tree-sitter-mode))

;; Tree-sitter Language Bundle for Emacs
(use-package tree-sitter-langs
  :after tree-sitter)

;; Buffer-line
(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :custom
  (centaur-tabs-style "bar")
  (centaur-tabs-height 32)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "?")
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-set-bar 'under)
  (x-underline-at-descent-line t)
  (centaur-tabs-buffer-groups-function #'centaur-tabs-projectile-buffer-groups)
  :bind
  (("s-{" . #'centaur-tabs-backward)
   ("s-}" . #'centaur-tabs-forward))
  (:map evil-normal-state-map
        ("g t" . centaur-tabs-forward)
        ("g T" . centaur-tabs-backward))
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (dired-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode))

;; Git integration
(use-package magit
  :commands (magit-status magit-blame))

;; Project management
(use-package projectile
  :demand
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
(use-package dashboard
  :demand
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-items '((recents  . 5)
                     (bookmarks . 5)
                     (projects . 3)
                     (agenda . 5))))

;; Organizer
(use-package org
  :pin org
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-directory "~/.emacs.d/org"))

;; Nicer organizer bullets
(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

;; A directory drawer
(use-package treemacs)
(use-package treemacs-evil :after (treemacs evil))
(use-package treemacs-projectile :after (treemacs projectile))
(use-package treemacs-magit :after (treemacs magit))
(use-package treemacs-icons-dired :hook (dired-mode . treemacs-icons-dired-enable-once))

;; VCS gutter
(use-package git-gutter)

;; Key bindings

;; Make Esc key quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defhydra hydra-text-scale (:timeout 4)
  "Scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(my/leader-keys
  "t" '(:ignore t :which-key "toggles")
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; Daemon

(defun my/font-setup ()
  "Setup the fonts."
  (set-frame-font "JetBrainsMono NF-13" nil t))

(if (daemonp)
    (progn
      (add-hook 'after-make-frame-functions #'my/font-setup)
      (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))))

;; Revert garbage collector to sensible defaults
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)

(provide 'init)
;;; init.el ends here
