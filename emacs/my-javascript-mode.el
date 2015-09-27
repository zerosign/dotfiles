(provide 'my-javascript-mode)

(require 'js-doc)
(require 'tern-auto-complete)

(setq js-doc-mail-address "yuri.setiantoko@gmail.com"
      js-doc-author (format "Yuri Setiantoko <%s>" js-doc-mail-address)
      js-doc-url ""
      js-doc-license "MIT")
(doc-mode)

(add-hook 'js-mode-hook 'flymake-jshint-load)
(add-hook 'js-mode-hook 'tern-ac-setup)
