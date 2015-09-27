(provide 'my-golang-mode)

(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "GOPATH")

(require 'company)
(require 'company-go)

(add-to-list 'company-backends '(company-go company-files))


;; (setq company-tooltip-limit 20)
;; (setq company-idle-delay .3)
;; (setq company-echo-delay 0)
;; (setq company-begin-commands '(self-insert-command))

;; (require 'auto-complete-config)


;; (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

;; (require 'go-autocomplete)
;; (require 'auto-complete-config)


;; (setq ac-sources '(ac-source-go))
;; (add-to-list 'ac-modes 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)
;; (setq-default tab-width 4 standard-indent 4 indent-tabs-mode nil)
