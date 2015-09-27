(provide 'my-rust-mode)

(setq racer-cmd "~/Repositories/rust/racer/target/release/racer")
(setq racer-rust-src-path "~/Repositories/rust/rust/src")


(unless (getenv "RUST_SRC_PATH")
      (setenv "RUST_SRC_PATH" (expand-file-name "~/Repositories/rust/rust/src")))


(setenv "PATH" (concat (getenv "PATH") ":~/Repositories/rust/rustfmt/target/release"))
(setq exec-path (append exec-path '("~/Repositories/rust/rustfmt/target/release")))

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)
(setq company-idle-delay .3)
