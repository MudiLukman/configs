;; Do not show the default "Welcome to GNU Emacs..." message
(setq inhibit-startup-message t)

(global-display-line-numbers-mode t) ; Display line numbers
;;Disbale line numbers for some modes i.e shell mode
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		pdf-view-mode-hook
		eshell-mode-hook))
 (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; All Files
(save-place-mode 1) ;; Remember to continue from previously open page
(setq save-place-forget-unreadable-files nil) ;; Don't forget place for missing files

;;PDF Config
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (pdf-continuous-scroll-mode 1) ; Enable continuous scrolling in PDF
  (setq pdf-view-display-size 'fit-page ;; Fit the page to the window
        pdf-view-resize-factor 1.1) ;; Optional: adjust zoom speed
  (add-hook 'pdf-view-mode-hook
            (lambda ()
	      ;; Remember Last page
              (setq-local save-place t))))

;;TODO: Disable word wrap for prog-mode i.e. source codes & text mode

;;Package Setup
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;install and configure lsp-mode (language server protocol) for my fave languages i.e. Java
(use-package lsp-mode
  :hook ((java-mode . lsp))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-keymap-prefix "C-c l"
	lsp-java-workspace-dir "~/IdeaProjects/"
	lsp-enable-snippet nil))

;; Company mode for autocompletion
(use-package company
  :hook (lsp-mode . company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))

;; Optional: UI improvements with lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-sideline-enable t))

;; Flycheck for real-time syntax checking
(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

;; Which-key for keybinding hints
(use-package which-key
  :config
  (which-key-mode))

;; Treemacs for project management
(use-package treemacs
  :commands treemacs
  :config
  (setq treemacs-is-never-other-window t))

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :config
  (lsp-treemacs-sync-mode 1))

;;Quick code commenting
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(scroll-bar-mode -1) ; Disable Scroll bar
(tool-bar-mode -1) ; Disable toobar
(tooltip-mode -1) ; Disable tooltips
(set-fringe-mode 10) ; Give some space

(menu-bar-mode -1) ; Disable menu bar

(set-face-attribute 'default nil :font "Fira Code Retina" :height 150) ;Font face & height
(load-theme 'tango-dark)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(pdf-tools evil-nerd-commenter lsp-treemacs treemacs which-key flycheck lsp-ui company lsp-mode))
 '(warning-suppress-log-types '((use-package)))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
