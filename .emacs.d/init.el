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

;; PERSONAL INFO
(setq user-full-name "Tomás López Brea"
      user-mail-adress "tomaslb@tutanota.com"
      calendar-latitude 42.2
      calendar-longitude -8.81
      calendar-location-name "Vigo, GA")

;; APPEARANCE
(set-default-font "JetBrains Mono 12")
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
    (evil which-key org-pdftools org-notebook org-msg org-caldav org-board org-alert ob-rust ob-go org-bullets go-mode smex))))
(global-display-line-numbers-mode)

;; IDO & SMEX
(ido-mode 1)
(global-key-binding (kbd "M-x") #'smex)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

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
