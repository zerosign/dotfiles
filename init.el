(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)

;; (benchmark-init/activate)

(add-to-list 'load-path "~/Repositories/dotfiles/emacs/")
(add-to-list 'exec-path "~/.cabal/bin/")

(require 'autoload)

;; (require 'benchmark-init-loaddefs)
;; (benchmark-init/activate)

(require 'my-general-settings)

(require 'my-rust-mode)
(require 'my-haskell-mode)
(require 'my-ocaml-mode)
(require 'my-javascript-mode)

(add-to-list 'auto-mode-alist '("\\.ml\\'" . tuareg-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	(quote
	 ("fa11f855b5f606f84e50106a7360c72aac88fee5f6fb8084aa4329009b61c5a2" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "0aa12caf6127772c1a38f7966de8258e7a0651fb6f7220d0bbb3a0232fba967f" "12b7ed9b0e990f6d41827c343467d2a6c464094cbcc6d0844df32837b50655f9" "870a63a25a2756074e53e5ee28f3f890332ddc21f9e87d583c5387285e882099" "dc54983ec5476b6187e592af57c093a42790f9d8071d9a0163ff4ff3fbea2189" "ce557950466bf42096853c6dac6875b9ae9c782b8665f62478980cc5e3b6028d" "d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "0155b3b94f6d5bce0275a15bc662be4f6f9f3284f9e469ca4ab1bd67ff9b5222" "dcf229d4673483cb7b38505360824fa56a0d7b52f54edbcdca98cf5059fa1662" "3b24f986084001ae46aa29ca791d2bc7f005c5c939646d2b800143526ab4d323" default)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "adobe" :slant normal :weight normal :height 100 :width normal)))))
