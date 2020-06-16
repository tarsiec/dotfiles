;; SETTING UP PACKAGES
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))


;; CLEANING UP DEFAULT CONFIG
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq tab-width 4)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; initial buffer in org mode
(setq initial-major-mode 'org-mode)

;; HELPER FUNC(S)
(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
      (funcall (cdr my-pair)))))

;;SETTING UP AUTOCOMPLETE
(require 'auto-complete-config)
(define-key ac-mode-map (kbd "M-TAB") 'autocomplete)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)
(global-set-key (kbd "TAB") #'company-indent-or-complete-common) ;
(setq company-begin-commands '(self-insert-command))
(global-set-key (kbd "C-c /") 'company-files)

;; SETTING UP TERMINAL (AND MULTITERM)
(global-set-key (kbd "C-c t") 'multi-term)
;(setq multi-term-program-switches "--login")
(setq system-uses-terminfo nil)


(add-hook 'term-mode-hook
          (lambda ()
            (goto-address-mode)
            (define-key term-raw-map (kbd "C-y") 'hrs/term-paste)
            (define-key term-raw-map (kbd "<mouse-2>") 'hrs/term-paste)
            (define-key term-raw-map (kbd "M-o") 'other-window)
            (setq yas-dont-activate t)))


;; PATH AND GOPATH
;; Snag the user's PATH and GOPATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;; PERSONAL INFO
(setq user-full-name "Tomás López Brea"
      user-mail-adress "tomaslb@tutanota.com"
      calendar-latitude 42.2
      calendar-longitude -8.81
      calendar-location-name "Vigo, GA")

;; APPEARANCE
(set-default-font "Fira Code Retina 12")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (base16-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" default)))
 '(package-selected-packages
   (quote
    (fira-code-mode helm-go-package magithub magit-todos evil-magit magit helm-fuzzier helm-emmet ac-helm helm ac-emmet web-beautify web-mode auto-rename-tag ac-html prettier-js sane-term eterm-256color skewer-mode company-web flymake-jslint js2-mode yasnippet multi-term flycheck-rust flycheck racer company-racer company-go company flymake-go flymake go-rename go-guru go-autocomplete go-playground rust-playground cargo rust-auto-use evil which-key org-pdftools org-notebook org-msg org-caldav org-board org-alert ob-rust ob-go org-bullets go-mode smex))))
(global-display-line-numbers-mode)

;; COMPLETION (HELM!)
(ido-mode 1)
(helm-mode 1)
(global-key-binding (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require 'helm-fuzzier)
(helm-fuzzier-mode 1)

;; LANGS
;; HASKELL MODE
(setq interactive-haskell-mode t)
;; ORG MODE
(require 'org)
; pretty bullets
(add-hook 'org-mode-hook
	  (lambda ()
	    (org-bullets-mode t)))
(setq org-hide-leading-stars t)

; org-babel
(require 'ob-go)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((go . t)
   (shell . t)
   (python . t)
   (rust . t)
   (haskell . t)
   (ruby . t)
   (R . t)))

; done date
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

; org-alert (WIP)
(setq alert-default-style 'libnotify)
(setq org-alert-notification-title "ORG Agenda")

;; GO MODE
(autoload 'helm-go-package "helm-go-package") ;; Not necessary if using ELPA package
(eval-after-load 'go-mode
  '(substitute-key-definition 'go-import-add 'helm-go-package go-mode-map))

(require 'go-autocomplete)
(require 'go-guru)
(add-to-list 'load-path "/home/tlb/code/projects/go/src/github.com/dougm/goflymake")

(require 'go-flymake)
(add-hook 'go-mode-hook (lambda ()
			  ;; godef-describe -> C-c C-d
			  ;; godef-jump     -> C-c C-j [C-x <LEFT>] <-back to prev buffer
			  (local-set-key (kbd "C-c C-c") 'go-remove-unused-imports)
			  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
			  (local-set-key (kbd "C-c C-g") 'go-goto-imports)
			  (local-set-key (kbd "C-c C-f") 'go-fmt)
			  (local-set-key (kbd "C-c C-k") 'godoc)))
(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook (lambda ()
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)))
(add-hook 'before-save-hook 'gofmt-before-save)
(with-eval-after-load 'go-mode-hook 'my-go-mode-hook)

;; RUST MODE
(setq racer-cmd "/usr/bin/racer")
(setq racer-rust-src-path "/home/tlb/code/resources/rust/rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")

(add-hook 'rust-mode-hook (lambda ()
			  ;; godef-describe -> C-c C-d
			  ;; godef-jump     -> C-c C-j [C-x <LEFT>] <-back to prev buffer
			  (local-set-key (kbd "C-c C-c") 'rust-compile)
			  (local-set-key (kbd "C-c C-r") 'rust-run)
			  (local-set-key (kbd "C-c C-f") 'rust-format-buffer)
			  (rust-enable-format-on-save)))

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; set up configs when loaded rust mode
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(setq company-tooltip-align-annotations t)

;; SET UP WEBDEV
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 3)

(require 'company)                                   ; load company mode
(require 'company-web-html)                          ; load company mode html backend
(require 'company-web-jade)                          ; load company mode jade backend
(require 'company-web-slim)                          ; load company mode slim backend

(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook #'(lambda ()
			      'skewer-html-mode
			      '(auto-rename-tag-mode t)))

(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)

(setq prettier-js-args '(
  "--trailing-comma" "all"
  "--bracket-spacing" "false"
))

(add-hook 'web-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.jsx?\\'" . prettier-js-mode))
))

;; POWERLINE
(powerline-default-theme)

;; BUFFER SELECTION
(global-set-key (kbd "C-x C-b") 'bs-show)
(require 'bs)
    (add-to-list 'bs-configurations
                 '("channels" nil nil "^[^#]" nil nil))
    (add-to-list 'bs-configurations
                 '("targets" nil nil nil
                   (lambda (buf)
                      (with-current-buffer buf
                        (not (erc-default-target)))) nil))

;; SETING UP PATH
(defun set-exec-path-from-shell-PATH ()
  "Sets the exec-path to the same value used by the user shell"
  (let ((path-from-shell
         (replace-regexp-in-string
          "[[:space:]\n]*$" ""
          (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;; SETTING UP PDF-TOOLS
(pdf-loader-install)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;  WHICH KEY (awesome)
(which-key-mode t)

;; SAVE PLACE MODE
(save-place-mode t)

;; EVIL MODE
(setq evil-want-abbrev-expand-on-insert-exit nil
      evil-want-keybinding nil)

(evil-mode 1)
