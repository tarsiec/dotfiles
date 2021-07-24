
;;; package -- Summary
;;; Commentary:
;;; Code:
;;; THEME
; (use-package nord-theme)
; (use-package base16-theme)
; (use-package zenburn-theme)
; (use-package color-theme-sanityinc-tomorrow)
; (use-package leuven-theme)
; (use-package dracula-theme)
; (use-package cyberpunk-theme)
; (use-package ample-theme)
; (use-package doom-themes)
; (use-package gruvbox-theme)
; (use-package poet-theme)
; (use-package anti-zenburn-theme)
; (use-package tao-theme)

;;; REMOVE GUI ;;;
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-startup-screen t)
(set-window-scroll-bars (minibuffer-window) nil nil)



;;; PACKAGE INITIALIZATION ;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

; use use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

; always install packages
(require 'use-package-ensure)
(setq use-package-always-ensure t)

; always compile and opt to newer package versions
(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)



;;; LOW-LEVEL UTILITIES ;;;
(electric-pair-mode 1)
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

; respect golden ratio
(use-package golden-ratio)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
; keep buffers up to date
(global-auto-revert-mode t)

(setq scroll-conservatively 100)


(use-package vterm
  :bind
  ("C-c t" . vterm))



;; Set up hydra & which-key
(use-package hydra)
(use-package which-key
  :config (which-key-mode))



;;; BASIC AESTHETICS ;;;
;; Font
(set-face-attribute 'default nil :family "Cascadia Code" :height 90)
(set-face-attribute 'fixed-pitch nil :family "JetBrains Mono")
(set-face-attribute 'variable-pitch nil :family "JetBrains Mono" :height 90)


(add-hook 'text-mode-hook
	  (lambda ()
	    (variable-pitch-mode 1)))

; display line number
(global-display-line-numbers-mode 1)
; display symbols
(global-prettify-symbols-mode)
; no ring
(setq ring-bell-function 'ignore)
; hl line
(global-hl-line-mode 1)



;;; COMPLETION ;;;
(use-package ivy
  :bind
  ("C-s" . swiper)
  ("C-s" . swiper-isearch)
  ("C-r" . ivy-resume)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

;;(use-package helm)

(use-package prescient)
(use-package ivy-prescient
  :init
  (ivy-prescient-mode))

(use-package counsel
  :bind
  ("M-x"     . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x c k" . counsel-yank-pop)
  ("C-c g f" . counsel-git)
  ("C-c g g" . counsel-git-grep)
  ("C-c k"   . counsel-ag)
  ("C-x l"   . counsel-locate)
  ("C-c -"   . counsel-dired))
  


;;; SYNTAX COMPLETION ;;;
(use-package company
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-limit-tooltip 20)
  (company-tooltip-align-annotations t)
  :config
  (add-hook 'prog-mode-hook 'global-company-mode))


;;; MOBILITY ;;;
; Move around the screen
(use-package avy
  :bind
  ("C-;" . avy-goto-char-2))

; Go to definition
(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))
  :bind
  ("C-x C-j" . dumb-jump-hydra/body))

; Move between windows
(use-package ace-window
  :bind
  ("M-o" . ace-window))

(use-package crux
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))



;;; MODE LINE
;; (use-package telephone-line
;;   :init
;;   (telephone-line-mode 1))

;;; FOR WRITING
(use-package zen-mode
  :bind
  ("C-M-z" . zen-mode))

;;; PROJECTS ;;;
(use-package projectile
  :bind
  ("C-x M-p" . projectile-command-map)
  ("C-c p"  . projectile-command-map))

; buffer title as projectile name
(setq frame-title-format '((:eval (projectile-project-name))))


; Search files
;; (use-package ag
;;   :bind
;;   ("C-x p s" . ag))


;; GIT
; diff-hl to show changes
(smerge-mode)

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))


;; Magit
(use-package magit)
(use-package with-editor)
(use-package forge)


;;; LANGUAGES ;;;
;; Flycheck
(use-package flycheck
  :init
  (global-flycheck-mode))

;; Yasnippet
(use-package yasnippet
  :config (yas-global-mode 1))

(use-package yasnippet-snippets)

;;; eldoc
(global-eldoc-mode)

;;; Attrap
;; (use-package attrap
;;   :ensure t
;;   :bind (("C-x /" . attrap-attrap)))

;; Org
(setq calendar-week-start-day 1)

(setq org-directory "~/docs/org"
      org-archive-location "~/docs/org/archive"
      org-hide-leading-stars 0)
(use-package org-bullets
  :config
  (org-bullets-mode))

(use-package org-brain
  :init
  (setq org-brain-path "~/docs/STEMBach/Org"))

(global-set-key (kbd "C-x C-a a") 'org-agenda)
(global-set-key (kbd "C-x C-a l") 'org-store-link)

;; LSP (eglot)
(use-package eglot
  :commands eglot
  :config
  (add-to-list 'eglot-server-programs '(haskell-mode . ("haskell-language-server-wrapper" ""))))


;; Haskell
(use-package haskell-mode
  :hook
  (haskell-mode . interactive-haskell-mode)
  (haskell-mode . haskell-doc-mode)
  :bind (:map haskell-mode-map)
              ("C-c C-c" . haskell-compile))

;; (use-package company-ghci
;;   :config
;;   (push 'company-ghci company-backends))

(use-package irony)

(use-package dante
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'dante-mode))

;; Rust
(use-package rust-mode
  :hook
  (rust-mode . racer-mode)
  (rust-mode . cargo-minor-mode))

(use-package flycheck-rust)

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package cargo
  :bind (:map cargo-mode-map
              ("C-c C-c" . my-cargo-run)
              ("C-c C-b" . cargo-process-build)
              ("C-c C-j" . cargo-process)))
  ;; TODO Make hydra

(use-package racer
  :defer t
  :bind (:map rust-mode-map)
              ("C-c C-d" . racer-describe)
              ("C-c C-f" . racer-find-definition)
              ("C-c C-r" . racer-debug)
  :config
  (define-key rust-mode-map (kbd "TAB")  #'company-indent-or-complete-common))

;; Python
;; (use-package company-jedi
  ;; :config
  ;; (setq jedi:setup-keys t)
  ;; (setq jedi:complete-on-dot t)
  ;; (add-hook 'python-mode-hook 'jedi:setup))
(use-package python-mode
  ;; (add-to-list 'company-backends 'company-jedi)
  :config
  (global-prettify-symbols-mode 0))

(use-package elpy
  :init
  (elpy-enable)
  (flymake-mode 0)
  :bind (:map elpy-mode-map)
              ("C-c C-j" . elpy-format-code)
  :config
  (setq elpy-formatter "autopep8"))













(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(beacon-color "#cc6666")
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#3cafa5")
 '(cua-mode t nil (cua-base))
 '(cua-overwrite-cursor-color "#c49619")
 '(cua-read-only-cursor-color "#93a61a")
 '(custom-enabled-themes '(zenburn))
 '(custom-safe-themes
   '("8c1dd3d6fdfb2bee6b8f05d13d167f200befe1712d0abfdc47bb6d3b706c3434" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "461e9e0d69636be8b5347a030f14b16c996c60a89e48c33f48bde51c32da3248" "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" "33ea268218b70aa106ba51a85fe976bfae9cf6931b18ceaf57159c558bbcd1e6" "b89a4f5916c29a235d0600ad5a0849b1c50fab16c2c518e1d98f0412367e7f97" "27a1dd6378f3782a593cc83e108a35c2b93e5ecc3bd9057313e1d88462701fcd" "549ccbd11c125a4e671a1e8d3609063a91228e918ffb269e57bd2cd2c0a6f1c6" "aba75724c5d4d0ec0de949694bce5ce6416c132bb031d4e7ac1c4f2dbdd3d580" "a44bca3ed952cb0fd2d73ff3684bda48304565d3eb9e8b789c6cca5c1d9254d1" "22a514f7051c7eac7f07112a217772f704531b136f00e2ccfaa2e2a456558d39" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "6c386d159853b0ee6695b45e64f598ed45bd67c47f671f69100817d7db64724d" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" "83e0376b5df8d6a3fbdfffb9fb0e8cf41a11799d9471293a810deb7586c131e6" "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" "28a104f642d09d3e5c62ce3464ea2c143b9130167282ea97ddcc3607b381823f" "2d035eb93f92384d11f18ed00930e5cc9964281915689fa035719cab71766a15" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "f99318b4b4d8267a3ee447539ba18380ad788c22d0173fc0986a9b71fd866100" "fede08d0f23fc0612a8354e0cf800c9ecae47ec8f32c5f29da841fe090dfc450" "5e2588d92543f5b40de864cd7abf36c72ff7d27a931ab4733aeb1b2ecf7ea22e" "5f824cddac6d892099a91c3f612fcf1b09bb6c322923d779216ab2094375c5ee" "e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" "bc4c89a7b91cfbd3e28b2a8e9e6750079a985237b960384f158515d32c7f0490" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "89b18e3da2f34aebb2917e1b1cca19713997fde8546f9f565bdec0dbc5a8baae" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" default))
 '(exwm-floating-border-color "#1c2028")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(highlight-changes-colors '("#e2468f" "#7a7ed2"))
 '(highlight-symbol-colors
   '("#3c6f408d329d" "#0c4a45f64ce3" "#486e33913532" "#1fac3bea568d" "#2ec943ac3324" "#449935a7314d" "#0b04411b5986"))
 '(highlight-symbol-foreground-color "#9eacac")
 '(highlight-tail-colors ((("#394147") . 0) (("#37424e") . 20)))
 '(hl-bg-colors
   '("#936d00" "#a72e01" "#ae1212" "#a81761" "#3548a2" "#0069b0" "#008981" "#687f00"))
 '(hl-fg-colors
   '("#002732" "#002732" "#002732" "#002732" "#002732" "#002732" "#002732" "#002732"))
 '(hl-paren-colors '("#3cafa5" "#c49619" "#3c98e0" "#7a7ed2" "#93a61a"))
 '(jdee-db-active-breakpoint-face-colors (cons "#191C25" "#81A1C1"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#191C25" "#A3BE8C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#191C25" "#434C5E"))
 '(line-spacing 0.2)
 '(lsp-ui-doc-border "#9eacac")
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(objed-cursor-color "#BF616A")
 '(org-agenda-files
   '("~/docs/STEMBach/Org/Trabajo.org" "~/docs/org/agenda.org"))
 '(package-selected-packages
   '(vterm nord-theme crux projectile golden-ratio racer cargo flycheck-rust rust-mode tao-theme anti-zenburn-theme anti-zenburn ample-theme cyberpunk-theme moe-theme dracula-theme leuven-theme doom-themes yasnippet-snippets elpy company-jedi python-mode poet-theme poet rainbow-delimiters rainbow-delimeters lsp-ui yasnippet eglot solarized-theme color-theme-sanityinc-tomorrow gruvbox-theme org-brain org-mode org-bullets night-owl-theme gruber-darker-theme company-ghci company-ghc zen-mode hs-lint flycheck-haskell hlint ivy-prescient helm dante telephone-line format-all format-all-the-code hindent ace-window zenburn-theme magit-forge forge which-key hydra lsp-mode dumb-jump avy ag diff-hl evil-comentary auto-compile company use-package-ensure counsel ivy use-package base16-theme))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(pos-tip-background-color "#01323d")
 '(pos-tip-foreground-color "#9eacac")
 '(rustic-ansi-faces
   ["#2E3440" "#BF616A" "#A3BE8C" "#EBCB8B" "#81A1C1" "#B48EAD" "#88C0D0" "#ECEFF4"])
 '(smartrep-mode-line-active-bg (solarized-color-blend "#93a61a" "#01323d" 0.2))
 '(term-default-bg-color "#002732")
 '(term-default-fg-color "#8d9fa1")
 '(vc-annotate-background-mode nil)
 '(weechat-color-list
   '(unspecified "#002732" "#01323d" "#ae1212" "#ec423a" "#687f00" "#93a61a" "#936d00" "#c49619" "#0069b0" "#3c98e0" "#a81761" "#e2468f" "#008981" "#3cafa5" "#8d9fa1" "#60767e"))
 '(window-divider-mode nil)
 '(xterm-color-names
   ["#01323d" "#ec423a" "#93a61a" "#c49619" "#3c98e0" "#e2468f" "#3cafa5" "#faf3e0"])
 '(xterm-color-names-bright
   ["#002732" "#db5823" "#62787f" "#60767e" "#8d9fa1" "#7a7ed2" "#9eacac" "#ffffee"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
