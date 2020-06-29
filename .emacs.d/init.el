;; -- PERSONAL INFO --
;(setq user-full-name "Tomas Lopez Brea"
;      user-mail-address "tomaslb@tutanota.com"
;      calendar-latitude 42.2
;      calendar-longitude -8.81
;      calendar-location-name "Vigo, GA")

;; -- INITIALIZING PACKAGES --
(require 'package)
(package-initialize)
; adds melpa to repository list
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

;; -- CLEANING UP DEFAULT CONFIGS --
(setq inhibit-startup-screen t)
; removes GUI stupidness from the interface
(menu-bar-mode 0)
(tool-bar-mode 0)

(scroll-bar-mode 0)
(setq tab-width 4)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
; starts scratchpad in org mode by default
(setq initial-major-mode 'org-mode)
; auto pair parens
(electric-pair-mode)

;; -- APEARANCE --
(set-frame-font "JetBrainsMono Nerd Font 11")
(global-display-line-numbers-mode)


;; -- COMPLETION (HELM!, ido, etc.) --
(require 'helm-config)
(helm-mode 1)
(ido-mode 1)
(global-key-binding (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(which-key-mode t)

;; -- SETTING UP SNIPPETS --
(require 'yasnippet)
(add-to-list 'load-path
	     "~/.emacs.d/plugins/yasnippet")
(yas-global-mode 1)

;; -- DOCUMENT MOBILITY SETTINGS --
; adds keybindings for resizing
(global-set-key (kbd "S-C-h") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-l") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-j") 'enlarge-window)
(global-set-key (kbd "S-C-k") 'shrink-window)
; adds keybinding for ace window, a plugin which switches between windows
(global-set-key (kbd "S-C-SPC") 'ace-window)

; adds multiple cursor support
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; -- SETTING UP FLYCHECK AND COMPANY
(add-hook 'after-init-hook 'global-company-mode)
;(add-hook 'after-init-hook #'global-flycheck-mode)
;(setq company-idle-delay 0.2)
;(setq company-minimum-prefix-length 1)

;; -- LANGUAGE SPECIFIC --
; haskell
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook (lambda ()
			       (custom-set-variables
				'(haskell-process-suggest-remove-import-lines t)
				'(haskell-process-auto-import-loaded-modules t)
				'(haskell-process-log t))

			       'haskell-indent-mode
			       'haskell-doc-mode
			       'hlint-refactor-mode
			       'interactive-haskell-mode
			       (local-set-key (kbd "C-c C-l") 'haskell-process-load-or-reload)
			       (local-set-key (kbd "C-`") 'haskell-interactive-bring)
			       (local-set-key (kbd "C-c C-t") 'haskell-process-do-type)
			       (local-set-key (kbd "C-c C-i") 'haskell-process-do-info)
			       (local-set-key (kbd "C-c C-c") 'haskell-process-cabal-build)
			       (local-set-key (kbd "C-c C-k") 'haskell-interactive-mode-clear)
			       (local-set-key (kbd "C-c c") 'haskell-process-cabal)
;			       (local-set-key (kbd "C-c C-f") 'hindent-reformat-buffer)
	    		       (local-set-key (kbd "C-c C-r") 'hlint-refactor-refactor-buffer)
			       (local-set-key (kbd "C-c C-f") 'hindent-reformat-buffer)))

(add-hook 'haskell-mode-hook #'hindent-mode)

;(add-to-list 'company-backends 'company-ghc)
;(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
;(setq flycheck-check-syntax-automatically '(save mode-enable))
;; the default value was '(save idle-change new-line mode-enabled)

; org
(setq org-hide-leading-stars t)
(add-hook 'org-mode-hook 'org-bullets-mode)
(require 'ob-go)
(require 'ob-rust)
(require 'ob-crystal)

(org-babel-do-load-languages 'org-babel-load-languages
			     '((go . t)
			       (shell . t)
			       (python . t)
			       (haskell . t)
			       (crystal . t)
			       (rust . t)))
; enables pdf viewer
(pdf-tools-install)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (base16-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "8c1dd3d6fdfb2bee6b8f05d13d167f200befe1712d0abfdc47bb6d3b706c3434" default)))
 '(package-selected-packages
   (quote
    (pdf-tools ob-crystal ob-rust ob-go org-bullets multiple-cursors hlint-refactor company which-key flycheck hindent haskell-snippets haskell-mode base16-theme helm ace-window yasnippet))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
