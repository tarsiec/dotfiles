;;; INITIALIZING PACKAGES ----------------
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))


;;; CLEANING UP DEFAULTS -----------------
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq tab-width 8)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq initial-major-mode 'org-mode)

(electric-pair-mode)

(setq scroll-conservatively 100)

;;; SETTING UP USE-PACKAGE ---------------
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)


;;; APEARANCE ----------------------------
(set-frame-font "JetBrainsMono Nerd Font 11")
(global-display-line-numbers-mode)

(global-prettify-symbols-mode t)

(setq sml/no-confirm-load-theme t)
(use-package smart-mode-line
  :config
  (smart-mode-line-enable))


(global-hl-line-mode)



;;; UTILS --------------------------------
(use-package which-key)
(which-key-mode)

(use-package ace-window
  :bind
  (("S-C-h" . enlarge-window-horizontally)
   ("S-C-l" . shrink-window-horizontally)
   ("S-C-j" . enlarge-window)
   ("S-C-k" . shrink-window)
   ("C-;"   . ace-window)))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->"         . mc/mark-next-line-like-this)
	 ("C-<"         . mc/mark-previous-like-this)
	 ("C-c C-<"     . mc/mark-all-like-this)))

(use-package highlight-indent-guides
  :config
  (add-hook 'elpy-mode-hook 'highlight-indent-guides-mode))
;  (setq 'highlight-indent-guides-method 'character "|"))

(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'helm))

(use-package restclient)
(use-package company-restclient
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package undo-tree)

(use-package subword
  :config
  (global-subword-mode 1))

(setq compilation-scroll-output t)



;;; VC & PROJECTILE ----------------------
(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package ag)

(use-package magit
  :bind
  ("C-x g" . magit-status)
  :config
  (use-package with-editor)
  (setq git-commit-summary-max-length 50))

(use-package ghub)
(use-package forge)

(use-package projectile
  :bind
  ("C-c C-p f" . projectile-ag)

  :config
  (define-key projectile-mode-map (kbd "C-c C-p p") 'projectile-command-map)
  (setq projectile-completion-system 'helm
	projectile-switch-project-action 'projectile-dired
	projectile-require-project-root nil))

(use-package treemacs
  :bind ("C-c C-p t" . treemacs))


;;; COMPLETION FRAMEWORKS ---------------
(use-package helm
  :config
  (helm-mode 1)
  :bind
  ("M-x" . helm-M-x))

(ido-mode 1)


;;; COMPANY & FLYCHECK -------------------
(use-package company)
(add-hook 'after-init-hook 'global-company-mode)

;(global-set-key (kbd "TAB") 'company-complete-common)
(global-set-key (kbd "TAB") 'company-complete)

(use-package let-alist)
(use-package flycheck)

(use-package lsp-haskell
  :config
  (lsp-haskell-set-hlint-on)
  (setq lsp-haskell-process-path-hie "hie-wrapper"))

(use-package lsp-mode
  :hook ((rust-mode . lsp)
	 (haskell-mode . lsp)
	 (go-mode . lsp)))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package dap-mode)
(use-package lsp-treemacs)


;n;; GOLANG -------------------------------
(use-package go-mode)
(use-package go-errcheck)
(use-package company-go)

(setenv "GOPATH" "/home/tlb/code/projects/go")

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

;;; RUST ---------------------------------
(use-package rust-mode)
(add-hook 'before-save-hook
	  (lambda ()
	    (when (eq 'rust-mode major-mode)
	      (lsp-format-buffer))))


;;; HASKELL ------------------------------
(use-package haskell-mode)
(use-package ghc)


;;; PYTHON -------------------------------
(use-package python-mode)
(use-package elpy
  :config
  (elpy-enable)
  (setq global-prettify-symbols-mode -1)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;  (add-hook 'elpy-mode-hook 'flycheck-mode)
  )

(add-hook 'elpy-mode-hook (lambda ()
			      (highlight-indentation-mode -1)))

;(setq python-shell-interpreter "jupyter"
;      python-shell-interpreter-args "console --simple-prompt"
;      python-shell-prompt-detect-failure-warning nil)
;(add-to-list 'python-shell-completion-native-disabled-interpreters
;             "jupyter")

(use-package py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(use-package blacken)

(use-package ein)


;;; WEB MODE -----------------------------
;(use-package web-mode
;  :config
;  (emmet-mode))
;
;(add-hook 'after-save-hook 'lsp-format-buffer)
;
;(use-package emmet-mode
;  :config
;  (setq emmet-indentation 4
;	emet-move-cursor-between-quotes t
;	emmet-self-closing-tag-style " /"
;	lsp-ht)
;  :bind
;  ("TAB" . emmet-expand-line))















(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#1e1e1e" "#D16969" "#579C4C" "#D7BA7D" "#339CDB" "#C586C0" "#85DDFF" "#d4d4d4"])
 '(ansi-term-color-vector
   [unspecified "#2d2a2e" "#ff6188" "#a9dc76" "#ffd866" "#78dce8" "#ab9df2" "#a1efe4" "#fcfcfa"] t)
 '(beacon-color "#cc6666")
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes
   (quote
    ("2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "250268d5c0b4877cc2b7c439687f8145a2c85a48981f7070a72c7f47a2d2dc13" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "f984e2f9765a69f7394527b44eaa28052ff3664a505f9ec9c60c088ca4e9fc0b" "9c4acf7b5801f25501f0db26ac3eee3dc263ed51afd01f9dcfda706a15234733" "4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "82360e5f96244ce8cc6e765eeebe7788c2c5f3aeb96c1a765629c5c7937c0b5b" "6c3b5f4391572c4176908bb30eddc1718344b8eaff50e162e36f271f6de015ca" "0cb1b0ea66b145ad9b9e34c850ea8e842c4c4c83abe04e37455a1ef4cc5b8791" "5d09b4ad5649fea40249dd937eaaa8f8a229db1cec9a1a0ef0de3ccf63523014" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "dde8c620311ea241c0b490af8e6f570fdd3b941d7bc209e55cd87884eb733b0e" "5b809c3eae60da2af8a8cfba4e9e04b4d608cb49584cb5998f6e4a1c87c057c4" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "2a749c20af891c16571527d07976bbcf2bf31819fa7d322942b73386019f4d58" "6177ecbffb8f37756012c9ee9fd73fc043520836d254397566e37c6204118852" "34b3219ae11acd81b2bb7f3f360505019f17d7a486deb8bb9c1b6d13c6616d2e" "9b272154fb77a926f52f2756ed5872877ad8d73d018a426d44c6083d1ed972b1" "e2acbf379aa541e07373395b977a99c878c30f20c3761aac23e9223345526bcc" "632694fd8a835e85bcc8b7bb5c1df1a0164689bc6009864faed38a9142b97057" "a92e9da0fab90cbec4af4a2035602208cebf3d071ea547157b2bfc5d9bd4d48d" "3d3807f1070bb91a68d6638a708ee09e63c0825ad21809c87138e676a60bda5d" "9b01a258b57067426cc3c8155330b0381ae0d8dd41d5345b5eddac69f40d409b" "fe94e2e42ccaa9714dd0f83a5aa1efeef819e22c5774115a9984293af609fce7" "d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086" "3577ee091e1d318c49889574a31175970472f6f182a9789f1a3e9e4513641d86" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "527df6ab42b54d2e5f4eec8b091bd79b2fa9a1da38f5addd297d1c91aa19b616" "b89a4f5916c29a235d0600ad5a0849b1c50fab16c2c518e1d98f0412367e7f97" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "cfd51857f5e80eddece7eb5d30b9afce81f442707207e0d636250d03978a66ec" "2d035eb93f92384d11f18ed00930e5cc9964281915689fa035719cab71766a15" "f490984d405f1a97418a92f478218b8e4bcc188cf353e5dd5d5acd2f8efd0790" "28a104f642d09d3e5c62ce3464ea2c143b9130167282ea97ddcc3607b381823f" "35c096aa0975d104688a9e59e28860f5af6bb4459fd692ed47557727848e6dfe" "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" "801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" "d4131a682c4436bb5a61103d9a850bf788cbf793f3fd8897de520d20583aeb58" "1de8de5dddd3c5138e134696180868490c4fc86daf9780895d8fc728449805f3" "c1c459af570241993823db87096bc775506c378aa02c9c6cd9ccaa8247056b96" "bb28b083fe1c61848c10c049be076afc572ea9bee6e1f8dc2631c5ee4f7388c8" "6a0d7f41968908e25b2f56fa7b4d188e3fc9a158c39ef680b349dccffc42d1c8" "c342ef444e7aca36f4b39a8e2848c4ba793d51c58fdb520b8ed887766ed6d40b" "10845272b6fa47a6cdfc49816748bdb1dc1cb9be647801c25c054a8e6a27ef72" "643b4d181b6afa4306d65db76889d8b987e095ae8f262a4c71dd5669d39c9b09" "7e5d400035eea68343be6830f3de7b8ce5e75f7ac7b8337b5df492d023ee8483" "c0fef082e36bb01efb44c8becead9f1d56234d61d84a849370195ca26d09cfa0" "9089d25e2a77e6044b4a97a2b9fe0c82351a19fdd3e68a885f40f86bbe3b3900" "c499bf4e774b34e784ef5a104347b81c56220416d56d5fd3fd85df8704260aad" "fc0fe24e7f3d48ac9cf1f87b8657c6d7a5dd203d5dabd2f12f549026b4c67446" "8ce796252a78d1a69e008c39d7b84a9545022b64609caac98dc7980d76ae34e3" "9f9450547564423166a7d2de976c9ca712293170415ec78ed98d198748b44a90" "7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "b5fff23b86b3fd2dd2cc86aa3b27ee91513adaefeaa75adc8af35a45ffb6c499" "71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "912cac216b96560654f4f15a3a4d8ba47d9c604cbc3b04801e465fb67a0234f0" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "5846b39f2171d620c45ee31409350c1ccaddebd3f88ac19894ae15db9ef23035" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "5d59bd44c5a875566348fa44ee01c98c1d72369dc531c1c5458b0864841f887c" "d4f8fcc20d4b44bf5796196dbeabec42078c2ddb16dcb6ec145a1c610e0842f3" "fede08d0f23fc0612a8354e0cf800c9ecae47ec8f32c5f29da841fe090dfc450" "aded61687237d1dff6325edb492bde536f40b048eab7246c61d5c6643c696b7f" "8c1dd3d6fdfb2bee6b8f05d13d167f200befe1712d0abfdc47bb6d3b706c3434" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "f56eb33cd9f1e49c5df0080a3e8a292e83890a61a89bceeaa481a5f183e8e3ef" default)))
 '(fci-rule-color "#373b41")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(global-prettify-symbols-mode t)
 '(helm-completion-style (quote emacs))
 '(highlight-indent-guides-method (quote character))
 '(hl-sexp-background-color "#1c1f26")
 '(jdee-db-active-breakpoint-face-colors (cons "#171F24" "#237AD3"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#171F24" "#579C4C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#171F24" "#777778"))
 '(line-spacing 0.2)
 '(linum-format " %5i ")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(objed-cursor-color "#D16969")
 '(package-selected-packages
   (quote
    (solarized-theme highlight-indent-guides multiple-cursors cyberpunk-theme nord-theme naysayer-theme color-theme-sanityinc-solarized monokai-pro-theme doom-themes kaolin-themes tao-theme poet-theme oceanic-theme ein blacken py-autopep8 material-theme ace-jump elpy web-mode python-mode ghc gruvbox-theme lsp-haskell haskell-mode dap-mode lsp-ui lsp-mode rust-mode company-go go-errcheck go-mode eglot undo-tree company-restclient restclient magit flycheck helm dumb-jump company ag diff-hl smart-mode-line which-key base16-theme color-theme-sanityinc-tomorrow auto-compile autocompile use-package zenburn-theme)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#2a2931")
 '(pos-tip-foreground-color "#d4d4d6")
 '(rustic-ansi-faces
   ["#1e1e1e" "#D16969" "#579C4C" "#D7BA7D" "#339CDB" "#C586C0" "#85DDFF" "#d4d4d4"])
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
