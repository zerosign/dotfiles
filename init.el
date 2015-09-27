(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)

(add-to-list 'load-path "~/Repositories/dotfiles/emacs/")
(add-to-list 'exec-path "~/.cabal/bin/")

(require 'autoload)
(require 'my-general-settings)

(require 'my-rust-mode)
(require 'my-haskell-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	(quote
	 ("d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "36affb6b6328d2bfa7a31b3183cd65d6dd1a8c0945382f94de729233b9737359" "f04122bbc305a202967fa1838e20ff741455307c2ae80a26035fbf5d637e325f" "6d401fa3c899170ce88eba7000fdefe11dcf24cfcec14d766fa4812a46d6bcf1" "76626efc044daee1c402e50f185bd633d1a688c332bc15c8fd5db4cdf2966b79" "4f2ede02b3324c2f788f4e0bad77f7ebc1874eff7971d2a2c9b9724a50fb3f65" "86c109e93d1d18834f0ddc2ec16a6575e8c10bfdd15b666cda5f5b81fcd788ee" "3d5307e5d6eb221ce17b0c952aa4cf65dbb3fa4a360e12a71e03aab78e0176c5" "118717ce0a2645a0cf240b044999f964577ee10137b1f992b09a317d5073c02d" "b4d289faa4d7d3ce7add2f2c84a827cf7646fb7863c52b724620fda9e3dcf587" default)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "adobe" :slant normal :weight normal :height 100 :width normal)))))

(global-set-key (kbd "C-?") 'comment-or-uncomment-region) ; Ctrl+?
