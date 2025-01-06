;; Do not show the default "Welcome to GNU Emacs..." message
(setq inhibit-startup-message t)

(global-display-line-numbers-mode t) ; Display line numbers
;; Disbale line numbers for some modes i.e shell mode
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		pdf-view-mode-hook
		eshell-mode-hook))
 (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Disable word wrap
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'text-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'lsp-mode-hook (lambda () (setq truncate-lines t)))

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

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

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

(use-package which-key
   :config
   (which-key-mode))

;; Quick Commenting with Evil-Nerd-Commenter
(use-package evil-nerd-commenter
   :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package projectile)
(use-package flycheck)
(use-package yasnippet :config (yas-global-mode))
(use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration)))
(use-package hydra)
(use-package company)
(use-package lsp-ui)
(use-package which-key :config (which-key-mode))
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)
(use-package helm-lsp)
(use-package helm
  :config (helm-mode))
(use-package lsp-treemacs)

(use-package magit
  :custom
  (magit-display-buffer-fuction #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :after magit)

(setq lsp-java-java-path "/usr/lib/jvm/java-21-openjdk/bin/java")
(setq lsp-java-server-install-dir "~/.emacs.d/.cache/lsp/jdtls")
(setq lsp-java-vmargs (list "-javaagent:/home/kontrol/Projects/libs/lombok.jar"))

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
   '(evil-magit magit nerd-fonts all-the-icons-nerd-fonts doom-modeline helm helm-lsp lsp-docker lsp-javacomp lsp-java dap-mode eterm-256color pdf-tools evil-nerd-commenter lsp-treemacs treemacs which-key flycheck lsp-ui company lsp-mode))
 '(warning-suppress-log-types '((use-package)))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
