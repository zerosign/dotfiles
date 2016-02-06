;; @author : Yuri Setiantoko <yuri.setiantoko@gmail.com>

(provide 'my-general-settings)

(setq inhibit-splash-screen t)
(setq magit-last-seen-setup-instructions "1.4.0")
(setq backup-directory-alist '(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      version-control t)

(setq ring-bell-function 'ignore)

;; Configure view

; disable toolbar
(tool-bar-mode -1)
; disable menu bar
(menu-bar-mode -1)
; enable line number on the right
(global-linum-mode t)

(projectile-global-mode)
(setq projectile-completion-system 'helm)

(global-set-key (kbd "C-x C-[") 'shrink-window-horizontally)
(global-set-key (kbd "C-x C-]") 'enlarge-window-horizontally)
(scroll-bar-mode -1)

;; (gist-view-gist t)
;; (tab-width 3)
;; (merlin-command "/home/zerosign/.opam/system/bin/ocamlmerlin")
;; (graphviz-dot-view-command "xdot %s")


;; Configure themes
(set-face-attribute 'default nil :height 94)
(setq default-frame-alist '((width . 120) (height . 64)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "adobe" :slant normal :weight normal :height 100 :width normal)))))

;; (require 'color-theme)
;; (require 'color-theme-solarized)
;; (color-theme-solarized-dark)
;; (require 'powerline)
;; (powerline-default-theme)
;; (load-theme 'flatland-black t)
;; (load-theme 'fogus t)
(load-theme 'material t)

;; Configure the faces
;(set-face-attribute 'default nil :height 90)


;; Configure temporary files
;; (setq backup-by-copying t
;;       backup-directory-alist '(("/home/zerosign/.backups/"))
;;       delete-old-versions t
;;       kept-new-versions t
;;       kept-new-versions t
;;       version-control t
;; )

;; Configure global shortcut

(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "M-[") 'align)
(global-set-key (kbd "M-,") 'tabbar-backward)

(global-set-key (kbd "C-x m") 'flymake-compile)

;; (require 'auto-complete)
;; Set global auto complete
;; (global-auto-complete-mode)
(require 'company)
(global-company-mode)

(setq-default tab-width 3)

(ido-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
