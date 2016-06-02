(provide 'my-python-mode)

;; (require 'auto-complete)
(require 'jedi)
;; (require 'flymake)
;; (require 'flymake-python-pyflakes)

;(add-hook 'python-mode-hook 'jedi:setup)
;(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

;; (flymake-python-pyflakes-load)

;; (setq jedi:tooltip-method nil)
;; (setq jedi:setup-keys t)
;; (setq jedi:complete-on-dot t)

;; (jedi:setup)

(add-to-list 'company-backends '(company-jedi company-files))
;; (py-autopep8-enable-on-save)

;; (sphinx-doc)
;; (sphinx-doc-mode t)

; (require 'yasnippet)
; (yas/initialize)
; (yas/load-directories "~/Repositories/dotfiles/emacs/yasnippet/python")
