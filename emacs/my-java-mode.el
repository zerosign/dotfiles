(provide 'my-java-mode)

(custom-set-variables
 '(eclim-eclipse-dirs '("/usr/share/eclipse"))
 '(eclim-executable "/usr/share/eclipse/eclim")
)

(require 'eclim)
(global-eclim-mode)
(require 'eclimd)

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)
