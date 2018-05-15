(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(defconst default-snapshot-dir (expand-file-name "snapshots" user-emacs-directory))

;; settings
(global-auto-revert-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 3)
(setq inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'text-mode
      make-backup-files nil
      scroll-error-top-bottom t
      use-package-always-ensure t
      ring-bell-function 'ignore)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; envs
(setq extra-paths
      (list "/opt/racket/bin"
            "/opt/lein/bin"
            "/opt/sbt/bin"
            (concat (getenv "HOME") "/go/bin")
            (concat (getenv "HOME") "/.cargo/bin")
            (concat (getenv "HOME") "/.local/bin")))

(defun set-external-paths (externals)
  (let ((paths (append (delete-dups (split-string (getenv "PATH") ":")) externals)))
    (setenv "PATH"
     (seq-reduce
      '(lambda (lhs rhs) (concat lhs path-separator rhs)) paths ""))
    (setq exec-path (append exec-path externals))))

(set-external-paths extra-paths)

(setenv "LANG"   "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

(use-package quelpa :ensure t :demand t :pin melpa)
(use-package quelpa-use-package :ensure t :after quelpa :pin melpa)

;; themes
(use-package soothe-theme        :defer t :ensure t :pin melpa)
(use-package github-theme        :defer t :ensure t :pin melpa)
(use-package lenlen-theme        :defer t :ensure t :pin melpa)
(use-package kaolin-themes       :defer t :ensure t :pin melpa)
(use-package flatui-theme        :defer t :ensure t :pin melpa)
(use-package apropospriate-theme :defer t :ensure t :pin melpa)
(use-package foggy-night-theme   :defer t :ensure t :pin melpa)
;; set default theme
(load-theme 'apropospriate-light t)

(use-package smart-mode-line
  :ensure t
  :pin melpa
  :config
  (setq sml/theme 'respectful)
  (setq sml/name-width 40)
  (setq sml/mode-width 'full))

(global-linum-mode t)

;; (require 'autoload)

;; ivy-mode
(use-package ivy :ensure t :demand t :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (setq ivy-extra-directories nil)
  :config
  (define-key ivy-minibuffer-map (kbd "C-l") (kbd "DEL"))
  (use-package smex :ensure t :init
    (setq-default smex-history-length 32
                  smex-save-file (expand-file-name "smex-items" default-snapshot-dir)))
  (use-package flx :ensure t)
  (ivy-mode)
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>"    . ivy-resume)
         ("s-b"     . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("TAB"     . ivy-alt-done)
         ("<escape>" . minibuffer-keyboard-quit)
         ("C-j"      . ivy-next-line)
         ("C-k"      . ivy-previous-line)))

(use-package projectile :ensure t :diminish projectile-mode :init
  (setq projectile-enable-caching t)
  (setq projectile-cache-file (expand-file-name "projectile.cache" default-snapshot-dir))
  (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" default-snapshot-dir))
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action 'projectile-dired)
  :config (projectile-mode))

(use-package counsel :ensure t)

(use-package projectile-ripgrep :ensure t :defer t)

(use-package counsel-projectile :ensure t :diminish counsel-projectile-mode :config (counsel-projectile-mode) :init
  (setq projectile-switch-project-action 'projectile-dired))

(use-package pdf-tools :ensure t :defer t)

;; (use-package org-projectile :ensure t :defer t :pin melpa)

;; magit
(use-package magit
  :commands magit-status magit-blame
  :bind (("C-c C-]" . magit-status)
         ("C-c C-[" . magit-checkout)
         ("C-c C-/" . magit-commit)
         ("C-c C-*" . magit-push-matching)
         ("C-c C-\\" . magit-blame)))

(use-package swiper :ensure t :defer t
  :init (ivy-mode)
  :config (setq ivy-use-virtual-buffers t
                enable-recursive-minibuffers t)
  :bind (("C-s"     . swiper)
         ("C-c C-r" . ivy-resume)
         ("<f6>"    . ivy-resume)
         ("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c k"   . ounsel-rg)))


(use-package erc :commands erc erc-tls
  :init (setq erc-prompt-for-password nil
              erc-hide-list '("JOIN" "PART" "QUIT")
              erc-autojoin-channels-alist
              '(("irc.freenode.net" "#emacs")
                ("irc.gitter.im" "#ensime/ensime-server" "#ensime/ensime-emacs"))))

(use-package tramp :init
  (setq tramp-default-method "ssh"))
  ;; (setq tramp-persistency-file-name (expand-file-name "tramp" default-snapshot-dir)))

;; undo history
(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :bind ("s-/" . undo-tree-visualize))

;; highlight symbols
(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  :bind ("s-h" . highlight-symbol))

;; org-modes
(use-package org-bullets
  :ensure t
  :defer  t
  :after org-mode
  :pin melpa
  :hook (org-mode-hook . org-bullets-mode))

(use-package org-projectile
  :ensure t
  :pin melpa
  :bind (("C-c n p" . org-projectile-project-todo-completing-read)
         ("C-c c" . org-capture))
  :config
  (progn
    (org-projectile-per-project)
    (setq org-projectile-per-project-filepath ".notes/todo.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  :ensure t))

;; publishing & books
(use-package pandoc-mode :ensure t :defer t :pin melpa)

;; code search
(use-package ripgrep :ensure t :defer t :pin melpa)
(use-package ag :ensure t :defer t :pin melpa)

;; elscreen
(use-package elscreen
  :ensure t :defer t
  :pin melpa
  :init (elscreen-start))

;; non major mode (minor modes)
(use-package dockerfile-mode
  :ensure t :defer t
  :pin melpa)

(use-package docker-compose-mode
  :ensure t :defer t
  :pin melpa)

(use-package meson-mode
  :ensure t
  :pin melpa
  :hook (meson-mode . company-mode))

;; cmake mode
(use-package cmake-ide :ensure t :pin melpa)
(use-package cmake-font-lock :ensure t :pin melpa)
(use-package cmake-project :ensure t :pin melpa)

;; java
(use-package jdecomp
  :ensure t
  :defer t
  :pin melpa
  :config (setq jdecomp-decompiler-type 'fernflower
                jdecomp-decompiler-paths '((cfr . "/opt/jde/cfr.jar")
                                           (fernflower . "/opt/jde/fernflower.jar")
                                           (procyon . "/opt/jde/procyon.jar"))))


;; eldoc
(use-package eldoc
  :ensure t
  :pin melpa
  :diminish eldoc-mode :commands eldoc-mode)

;; editorconfig
(use-package editorconfig
  :ensure t
  :pin melpa
  :config (editorconfig-mode t))

(use-package editorconfig-charset-extras
  :ensure t :defer t :pin melpa)

;; language modes
(use-package toml-mode :ensure t :defer t :pin melpa)
(use-package yaml-mode :ensure t :defer t :pin melpa)

(use-package typescript-mode
  :ensure t
  :pin melpa
  :hook (typescript-mode .  tide-init))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :pin melpa
  :init (setq markdown-command "multimarkdown"))

(use-package js2-mode
  :ensure t
  :mode ("\\.js$" "\\.json$")
  :pin melpa
  :config
  (progn
    (setq-default js2-basic-offset 2)
    (setq-default js2-show-parse-errors t)
    (setq-default js2-strict-missing-semi-warning t)
    (setq-default js2-strict-trailing-comma-warning t)))

(use-package elixir-mode
  :commands elixir-mode
  :hook (elixir-mode . alchemist-mode)
  :pin melpa)

(use-package rust-mode
  :ensure t
  :pin melpa
  :config (setq rust-format-on-save t))

(use-package protobuf-mode :ensure t :defer t :pin melpa)

(use-package waf-mode :ensure t :defer t :pin melpa)

(use-package yard-mode :ensure t :defer t :pin melpa)

;; company
(use-package company :diminish company-mode :commands company-mode
  :config (setq company-dabbrev-ignore-case nil
                company-dabbrev-code-ignore-case nil
                company-dabbrev-downcase nil
                company-idle-delay 0
                company-minimum-prefix-length 4)
  :init (add-hook 'after-init-hook 'global-company-mode))

;; lsp
(use-package lsp-mode :ensure t :pin melpa
  :hook (lsp-mode . flycheck-mode))

(use-package lsp-ui :ensure t :pin melpa
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-flycheck-enable t
        lsp-ui-imenu-enable t
        lsp-ui-peek-enable t
        lsp-ui-peek-always-show t
        lsp-ui-sideline-enable t))

(use-package company-lsp
  :ensure t
  :after company lsp-mode
  :config
  (push 'company-lsp company-backends)
  ;; (push 'company-lsp company-backends)
  (setq company-lsp-cache-candidates 'auto)
  (setq company-lsp-async t)
  (setq company-lsp-enable-snippet t)
  (setq company-lsp-enable-recompletion t))

(use-package lsp-scala
  :quelpa ((lsp-scala :fetcher github :repo "rossabaker/lsp-scala") :upgrade t)
  :after scala-mode
  :hook  (scala-mode . lsp-scala-enable)
  :config
  (setq lsp-scala-server-command '("metals" "0.1.0-M1+138-4625a657")))

;; javascript + typescript
(use-package tide :ensure t
  :config
  :after js2-mode
  (setq company-tooltip-align-annotations t
        flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package nvm :ensure t :defer t)

(use-package tern
  :ensure t
  :after js2-mode
  :hook (js2-mode . tern-mode))

(use-package company-tern
  :after tern
  :ensure t
  :config (add-to-list 'company-backends company-tern))

(use-package indium
  :after company-tern
  :ensure t
  :defer t
  :hook (js-mode . (lambda ()
                     (require 'indium)
                     (indium-interaction-mode))))

;; elixir
(use-package alchemist
  :ensure t
  :pin melpa
  :after elixir-mode
  :defer t :commands alchemist-mode)

;; slime
(use-package slime-company
  :ensure t
  :pin melpa
  :after slime
  :defer t)

(use-package slime
  :ensure t :defer t
  :pin melpa
  :after elisp-mode
  :hook (elisp-mode . (lambda () (slime-setup '(slime-fancy slime-company))))
  :config
  (setq slime-net-coding-system 'utf-8-unix
        slime-contribs '(slime-fancy slime-repl slime-scratch slime-trace-dialog)
        lisp-loop-forms-indentation 6)
  (show-paren-mode 1))

;; Rust toolize
;; cargo
;; (use-package company-racer
;;   :ensure t
;;   :pin melpa
;;   :after company racer
;;   :hook (add-to-list 'company-backends company-racer))

(use-package rust-playground :ensure t :defer t :pin melpa)

(use-package cargo
  :ensure t :defer t :pin melpa
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))

(use-package lsp-rust
  :ensure t :pin melpa
  :hook (rust-mode . lsp-rust-enable))

;; (use-package racer :ensure t :after rust-mode
;;   :init
;;   (with-eval-after-load 'company
;;     (add-to-list 'company-backends 'company-racer))
;;   :config
;;   (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
;;   (setq company-tooltip-align-notations t)
;;   (add-hook 'rust-mode-hook  #'racer-mode)
;;   (add-hook 'racer-mode-hook #'eldoc-mode)
;;   :bind
;;   (("M-<RET>" . racer-describe)))

;; scala toolize
;; (use-package ensime :ensure t :pin melpa
;;   :config
;;   (setq
;;    ensime-sbt-command "/opt/sbt/bin/sbt"
;;    sbt:program-name   "/opt/sbt/bin/sbt"
;;    ensime-search-interface 'ivy))

(use-package epg
  :defer t
  :config (setq epg-gpgconf-program "gpg"))

(use-package sbt-mode :ensure t :pin melpa)

;; scala
(use-package scala-mode :ensure t :defer t :pin melpa)

;; ocaml
(use-package tuareg :ensure t :defer t :pin melpa)

(use-package utop
  :ensure t :pin melpa
  :config
  (autoload 'utop-minor-mode "utop" "minor mode for utop" t)
  (add-hook 'tuareg-mode-hook 'utop-minor-mode))

(use-package merlin :ensure t :defer t :pin melpa
  :after tuareg-mode
  :hook (tuareg-mode . merlin-mode)
  :config
  (add-to-list 'company-backends merlin-company-backend)
  (setq merlin-error-after-save t))

(use-package restclient :ensure t :defer t :pin melpa)
(use-package restclient-helm :ensure t :defer t :pin melpa)

(use-package rg :ensure t :defer t :pin melpa)

(use-package ssh-agency :ensure t :defer t :pin melpa)
(use-package ssh-config-mode :ensure t :defer t :pin melpa)

(use-package sudo-edit :ensure t :defer t :pin melpa)
(use-package systemd :ensure t :defer t   :pin melpa)

(use-package term-manager :ensure t :defer t :pin melpa)
(use-package term-projectile :ensure t :defer t :pin melpa)

(use-package vlf :ensure t :defer t :pin melpa)


;; (use-package lsp-flycheck :ensure t)

;; lsp-mode
;; (use-package lsp-mode
;;   :ensure t
;;   :config
;;   ;; (global-lsp-mode t)
;;   (with-eval-after-load 'lsp-mode
;;     (require 'lsp-flycheck)))

;; (use-package lsp-mode :ensure t)
;; ;; lsp-mode
;; (use-package lsp-java :ensure t :after lsp-mode)
;; (use-package lsp-go   :ensure t :after lsp-mode)
;; (use-package lsp-rust :ensure t :after lsp-mode)
;; (use-package lsp-python :ensure t :after lsp-mode)
;; (use-package lsp-haskell :ensure t :after lsp-mode)

;; ;; dotty
;; (use-package lsp-dotty :requires lsp-mode
;;   :quelpa (lsp-dotty :fetcher github :repo "smarter/emacs-lsp-dotty")
;;   (add-hook 'scala-mode-hook '(lambda () (setq-local flycheck-disable-checkers '(scala))
;;                                 (lsp-mode)
;;                                 (flycheck-mode))))

;; racket-mode
(use-package geiser        :ensure t :defer t)
(use-package racket-mode   :ensure t :defer t)
(use-package scribble-mode :ensure t :defer t)

(use-package coffee-mode :ensure t :defer t :init
  (setq-local whitespace-style '(trailing space-before-tab indentation empty space-after-tab))
  (custom-set-variables '(coffee-tab-width 2)))


(use-package anaconda-mode :ensure t :commands anaconda-mode :diminish anaconda-mode :init
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'eldoc-mode)
    (add-hook 'python-mode-hook (lambda ()
                                  (flycheck-mode t)
                                  (setq flycheck-checker 'python-pylint
                                        flycheck-checker-error-threshold 900)))))

(use-package company-anaconda
  :ensure t
  :config
  (push 'company-anaconda company-backends))

(use-package rubocop    :ensure t :defer t)
(use-package rspec-mode :ensure t :defer t)
(use-package robe       :ensure t :defer t :after company :config (add-to-list 'company-backends 'company-robe))
(use-package rvm        :ensure t :defer t)

;; ruby config start
(use-package ruby-mode :init
  (lambda () ((rvm-activate-corresponding-ruby)
              (add-hook 'ruby-mode-hook 'rubocop-mode)
              (add-hook 'ruby-mode-hook 'rspec-mode)
              (add-hook 'ruby-mode-hook 'robe-mode))))

;; golang config start
(use-package go-mode :ensure t :init
  (progn
    (add-hook 'before-save-hook 'gofmt-before-save)
    (bind-key [remap find-tag] #'godef-jump))
  :config (add-hook 'go-mode-hook 'electric-pair-mode))

(use-package company-go :ensure t :defer t :init
  (with-eval-after-load 'company
    (push 'company-go company-backends)))

(use-package go-eldoc :ensure t :defer t :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))
