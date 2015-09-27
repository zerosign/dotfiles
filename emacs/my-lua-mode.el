;; @author : Yuri Setiantoko <yuri.setiantoko@gmail.com>

(provide 'my-lua-mode)

(autoload 'lua-mode "lua-mode" "Lua editing mode" t)
(add-to-list 'auto-mode-alist '("\\.lua$" .lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" .lua-mode))

(setq 'lua-default-application "love")


