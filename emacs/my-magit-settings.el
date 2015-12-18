(provide 'my-magit-settings)
(require 'magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
(add-to-list 'magit-commit-arguments "--verbose")
