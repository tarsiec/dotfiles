;;; PACKAGES
;; initialize package list
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; ensure installation of packages
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer 1)



;;; SOURCE FILES
(load-file "~/.emacs.d/funcs.el")



;;; FUNCTIONS
;; Run C programs directly from within emacs
(defun execute-c-program ()
  (interactive)
  (defvar foo)
  (setq foo (concat "gcc " (buffer-name) " && ./a.out" ))
  (compile foo))




;;; BASIC APPEARANCE AND FUNCTIONALITY TWEAKS
;; startup screen and gui crap
(setq inhibit-startup-screen t)
(setq initial-major-mode 'org-mode)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)


;; parens
(electric-pair-mode)
(use-package rainbow-identifiers)
;; (rainbow-identifiers-mode)
;; (rainbow-mode)

;; tabs
(setq tab-width 4)

;; bells
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; scrolling
(setq scroll-conservatively 100)

;; keybinds that tree
(use-package hydra)

;;; APEARANCE
;; font
(set-frame-font "ypn envypn")

;; line & line numbers
(global-display-line-numbers-mode 1)
(global-hl-line-mode 0)

;;; INTERACTIVE SELECTION
;; ido mode
(ido-mode 1)

;; smex
;; (global-set-key (kbd "M-x") 'smex)

;; ivy & counsel
(use-package swiper
  :config
  (setq ivy-use-virtual-buffers 1
	 enable-recursive-minibuffers 1)
  :bind
  (("C-s" . swiper)
   ("C-c C-r" . ivy-resume)))
(ivy-mode 1)



;; ag
(use-package ag)

;;; VERSION CONTROL & PROJECTS
;; magit
(use-package magit
  :config
  (use-package with-editor)
  (setq git-commit-summary-max-length 50)
  :bind
  (("C-x g" . magit-status)))
(use-package forge)
(use-package ghub)

;; projectile
(use-package projectile
  :config
  ((define-key projectile-mode-map (kbd "C-c C-p p") 'projectile-command-map)
   (setq projectile-completion-system 'ivy))
  :bind
  ("C-c C-p f" . projectile-ag))

;; tree file map
(use-package treemacs
  :bind
  ("C-c C-p t" . treemacs))


;;; MOTION
;; keybind helper
(use-package which-key)
(which-key-mode)

;; between windows
(use-package ace-window
  :bind
  (("S-C-h" . enlarge-window-horizontally)
   ("S-C-l" . shrink-window-horizontally)
   ("S-C-j" . enlarge-window)
   ("S-C-k" . shrink-window)
   ("C-;"   . ace-window)))

;; multiple cursors
(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C-<"     . mc/mark-all-like-this)))

;; indentation guides
;; (use-package highlight-indent-guides
;;   :config
;;   (setq highlight-infent-guides-method 'character))
;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)


;;; COMPLETION AND IDE THINGS
;; jump to definition

(defhydra dumb-jump-hydra (:color blue :columns 3)
  "Dumb Jump"
  ("j" dumb-jump-go "Go")
  ("o" dumb-jump-go-other-window "Other window")
  ("e" dumb-jump-go-prefer-external "Go external")
  ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
  ("i" dumb-jump-go-prompt "Prompt")
  ("l" dumb-jump-quick-look "Quick look")
  ("b" dumb-jump-back "Back"))

(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'ivy)
  :bind
  ("C-c C-j" . dumb-jump-hydra/body))

;; undo tree
(use-package undo-tree)
; TODO understand further
(global-undo-tree-mode)

;; scroll through compilation output
(setq compilation-scroll-output 1)


;; auto-complete

(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)
  :bind
  ("C-," . company-complete))
;; (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; flycheck
(use-package flycheck)
(use-package flycheck-clang-tidy
  :hook c-mode-hook)


;; snippets
(use-package yasnippet)
(use-package yasnippet-snippets)
(yas-global-mode 1)
(yas/load-directory "~/.emacs.d/snippets")


;; semantic
(require 'semantic)

;; rtags

;; C/C++
(defhydra cc-hydra-actions (:color blue)
  "Dumbp Jump"
  ("r" execute-c-program "Compile file and output")
  ("c" compile "Compile with cmake"))


(setq-default c-basic-offset 4
	      c-default-style '((java-mode . "java")
				(awk-mode . "awk")
				(other . "bsd")))
(add-hook 'c-mode-hook 'semantinc)
(add-hook 'c-mode-hook 'company-mode)
(add-hook 'c-mode-hook (lambda ()
                         (interactive)
                         (c-toggle-comment-style -1)))

(add-hook 'c-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-r") 'cc-hydra-actions/body)))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


;;; GO
()


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#1d1f21" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#81a2be" "#c5c8c6"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes '(vscdark))
 '(custom-safe-themes
   '("f4c8f0b999a6407211a899401315a628e1a5ae2f408c04a33b14d7aa3ed86187" "e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "3380a2766cf0590d



50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" default))
 '(fci-rule-color "#383838")
 '(highlight-indent-guides-character 9474)
 '(highlight-indent-guides-method 'character)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(package-selected-packages
   '(vscdark-theme gruvbox-theme atom-one-dark-theme solarized-theme dracula-theme gruber-darker-theme go-snippets go-imports go-mode zenburn-theme ggtags dap-mode dap-cpptools dap lsp-mode auto-complete flycheck-clang-tidy yasnippet-snippets flycheck company treemacs counsel swiper projectile ag forge ghub magit undo-tree hydra dumb-jump highlight-indent-guides multiple-cursors ace-window which-key rainbow-identifiers auto-compile use-package base16-theme smex))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
