(provide 'my-haml-mode)

(defun my-javascript-mode-hook ()
  (setq indent-tabs-mode t tab-width 2 js-indent-level 2))

(add-hook 'javascript-mode-hook 'my-javascript-mode-hook)
