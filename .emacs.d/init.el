;; Configure package.el to include MELPA.
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Ensure that use-package is installed.
;;
;; If use-package isn't already installed, it's extremely likely that this is a
;; fresh installation! So we'll want to update the package repository and
;; install use-package before loading the literate configuration.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file "~/.emacs.d/config.org")
; (custom-set-variables
;  ;; custom-set-variables was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  '(ansi-color-names-vector
;    ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCDC"])
;  '(company-quickhelp-color-background "#4F4F4F")
;  '(company-quickhelp-color-foreground "#DCDCCC")
;  '(custom-enabled-themes '(doom-tomorrow-night))
;  '(custom-safe-themes
;    '("4b0e826f58b39e2ce2829fab8ca999bcdc076dec35187bf4e9a4b938cb5771dc" "f6665ce2f7f56c5ed5d91ed5e7f6acb66ce44d0ef4acfaa3a42c7cfe9e9a9013" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" default))
;  '(exwm-floating-border-color "#262626")
;  '(fci-rule-color "#dedede")
;  '(highlight-tail-colors ((("#454845") . 0) (("#474f4f") . 20)))
;  '(ispell-extra-args '("--sug-mode=ultra") t)
;  '(ispell-program-name "aspell" t)
;  '(jdee-db-active-breakpoint-face-colors (cons "#000000" "#8CD0D3"))
;  '(jdee-db-requested-breakpoint-face-colors (cons "#000000" "#7F9F7F"))
;  '(jdee-db-spec-breakpoint-face-colors (cons "#000000" "#494949"))
;  '(line-spacing 0.2)
;  '(nrepl-message-colors
;    '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
;  '(objed-cursor-color "#CC9393")
;  '(org-modules
;    '(ol-bbdb ol-bibtex org-crypt ol-docview ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m))
;  '(package-selected-packages
;    '(use-package treemacs org-re-reveal ox-twbs htmlize org-reveal company zen-mode yasnippet-snippets which-key vterm telephone-line tao-theme system-packages solarized-theme rainbow-delimiters racer python-mode projectile poet-theme pdf-tools org-bullets org-brain nord-theme night-owl-theme moe-theme lsp-ui lsp-haskell leuven-theme ivy-prescient hydra hindent helm gruvbox-theme gruber-darker-theme goto-chg golden-ratio frame-local format-all forge flycheck-rust flycheck-haskell elpy eglot dumb-jump dracula-theme diff-hl dash-functional dante cyberpunk-theme crux counsel company-jedi company-ghci color-theme-sanityinc-tomorrow cargo base16-theme auto-compile auctex anti-zenburn-theme annalist ample-theme ag ace-window))
;  '(pdf-view-midnight-colors (cons "#DCDCDC" "#3F3F3F"))
;  '(rustic-ansi-faces
;    ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCDC"])
;  '(vc-annotate-background "#3F3F3F")
;  '(vc-annotate-color-map
;    (list
;     (cons 20 "#7F9F7F")
;     (cons 40 "#a4b48f")
;     (cons 60 "#cac99f")
;     (cons 80 "#F0DFAF")
;     (cons 100 "#eacfa4")
;     (cons 120 "#e4bf99")
;     (cons 140 "#DFAF8F")
;     (cons 160 "#dea3a0")
;     (cons 180 "#dd97b1")
;     (cons 200 "#DC8CC3")
;     (cons 220 "#d68eb3")
;     (cons 240 "#d190a3")
;     (cons 260 "#CC9393")
;     (cons 280 "#ab8080")
;     (cons 300 "#8a6d6d")
;     (cons 320 "#695b5b")
;     (cons 340 "#4F4F4F")
;     (cons 360 "#4F4F4F")))
;  '(vc-annotate-very-old-color nil))
; (custom-set-faces
;  ;; custom-set-faces was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-scale-factor 1.2)
 '(org-agenda-files '("~/.emacs.d/config.org" "~/docs/org/agenda.org"))
 '(package-selected-packages
   '(all-the-icons-ivy flycheck-irony flycheck treemacs-all-the-icons all-the-icons-ivy-rich company-irony irony lsp-ivy diminish doom-modeline typescript-mode ox-reveal ox-twbs htmlize org-bullets yasnippet-snippets yasnippet gruvbox-theme treemacs diff-hl forge magit company-box company lsp-mode ace-window helpful counsel-projectile counsel ivy-rich smex ivy doom-themes zenburn-theme all-the-icons-dired all-the-icons rainbow-delimiters hydra which-key auto-compile use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
