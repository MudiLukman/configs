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

;;Package setup
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

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


(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

;;install and configure lsp-mode (language server protocol) for Java
;; This wasn't straigth forward at all but after I cd into ~/.emacs.d/.cache/lsp/jdtls
;; Then I wget http://download.eclipse.org/jdtls/snapshots/jdt-language-server-1.44.0-202412192041.tar.gz
;; Then tar -xzf jdt-language-server-1.44.0-202412192041.tar.gz
;; Then set (setq lsp-java-server-install-dir "~/.emacs.d/.cache/lsp/jdtls")
(use-package lsp-mode
  :hook ((java-mode . lsp))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-keymap-prefix "C-c l"
        lsp-enable-snippet nil
        lsp-java-import-order '["java" "javax" "com" "org"]
        lsp-java-save-actions-organize-imports t
        lsp-java-completion-enabled t
        lsp-java-autobuild-enabled t
        lsp-java-format-enabled t))

;; Autocomplete with Company Mode
(use-package company
  :hook (lsp-mode . company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t
        company-backends '(company-capf)))

;; Optional: UI Enhancements with LSP-UI
(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-sideline-enable t
        lsp-ui-peek-enable t))

;; Flycheck for Real-Time Syntax Checking
(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

;; Debugging with DAP Mode
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  (require 'dap-java)
  (setq dap-java-test-runner "junit")) 

;; Project Management with Treemacs
(use-package treemacs
  :commands treemacs
  :config
  (setq treemacs-is-never-other-window t))

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :config
  (lsp-treemacs-sync-mode 1))

;; Keybinding Hints with Which-Key
(use-package which-key
  :config
  (which-key-mode))

;; Quick Commenting with Evil-Nerd-Commenter
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(setq lsp-java-java-path "/usr/lib/jvm/java-21-openjdk/bin/java")
(setq lsp-java-server-install-dir "~/.emacs.d/.cache/lsp/jdtls")

;; Refactoring Support
(use-package lsp-java
  :after lsp-mode
  :config
  (setq lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
        lsp-java-format-settings-profile "GoogleStyle"))

;; Snippets with YASnippet (Optional)
(use-package yasnippet
  :hook (java-mode . yas-minor-mode))

;; Additional Settings
(setq lsp-java-vmargs '("-Xmx2G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication")) 

(scroll-bar-mode -1) ; Disable Scroll bar
(tool-bar-mode -1) ; Disable toobar
(tooltip-mode -1) ; Disable tooltips
(set-fringe-mode 10) ; Give some space

(menu-bar-mode -1) ; Disable menu bar

(set-face-attribute 'default nil :font "Fira Code Retina" :height 120) ;Font face & height
(load-theme 'tango-dark)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-docker lsp-javacomp lsp-java dap-mode eterm-256color pdf-tools evil-nerd-commenter lsp-treemacs treemacs which-key flycheck lsp-ui company lsp-mode))
 '(warning-suppress-log-types '((use-package)))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
