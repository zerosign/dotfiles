(provide 'my-lisp-mode)

; (load (expand-file-name "~/quicklisp/slime-helper.el"))
(require 'slime-autoloads)
(setq inferior-lisp-program "/usr/bin/ccl")
(setq slime-contribs '(slime-fancy))
(load (expand-file-name "~/quicklisp/slime-helper.el"))
  ;; Replace "sbcl" with the path to your implementation

(slime-setup '(slime-company))
(local-set-key [?\C-x ?\C-\'] 'slime-eval-region)

(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-.") 'company-show-location)
