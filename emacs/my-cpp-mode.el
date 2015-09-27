(provide 'my-cpp-mode)

;; (add-to-list 'load-path (expand-file-name "/home/zerosign/Repositories/irony-mode/elisp"))

; (require 'auto-complete)
(require 'yasnippet)
(require 'irony)

;; (defun my-irony-mode-hook ()
;;   (define-key irony-mode-map [remap completion-at-point]
;;     'irony-completion-at-point-async)
;;   (define-key irony-mode-map [remap complete-symbol]
;;     'irony-completion-at-point-async))

;; (my-irony-mode-hook)

(yas/minor-mode-on)
;; (auto-complete-mode 1)
;; (when (member major-mode irony-known-modes)
;;   (irony-mode 1)
;;   )
;; (when (file-exists-p "/usr/lib/clang/3.4/include/")
;;   (setq irony-libclang-additional-flags
;; 	'("-isystem" "/usr/lib/clang/3.4/include/")))

(add-to-list 'company-backends 'company-irony)
(company-irony-setup-begin-commands)

;; (irony-enable 'company)
(irony-cdb-autosetup-compile-options)
;; (message "Enable autocomplete in irony-mode")

(irony-mode)
