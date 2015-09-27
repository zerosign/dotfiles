(provide 'my-html-mode)

(require 'emmet-mode)
(emmet-mode)

(add-to-list 'company-backends 'company-web-html)
(add-to-list 'company-backends 'company-web-jade)
(add-to-list 'company-backends 'company-web-slim)
