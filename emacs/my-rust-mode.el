(provide 'my-rust-mode)

(setq exec-path (append exec-path '("/opt/rust/build/multirust/bin")))
(setenv "PATH" (concat (getenv "PATH") "/opt/rust/build/multirust/bin"))
(setq exec-path (append exec-path '("/home/zerosign/.multirust/toolchains/nightly/cargo/bin/")))
(setenv "PATH" (concat (getenv "PATH") "/home/zerosign/.multirust/toolchains/nightly/cargo/bin/"))

(setq racer-cmd "~/Repositories/rust/racer/target/release/racer")
(setq racer-rust-src-path "~/Repositories/rust/rust/src")

(unless (getenv "RUST_SRC_PATH")
      (setenv "RUST_SRC_PATH" (expand-file-name "/opt/rust/source/rust/src")))

;; (setenv "PATH" (concat (getenv "PATH") ":~/Repositories/rust/rustfmt/target/release"))
;; (setq exec-path (append exec-path '("~/Repositories/rust/rustfmt/target/release")))

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)
(setq company-idle-delay .3)

(rustfmt-enable-on-save)
