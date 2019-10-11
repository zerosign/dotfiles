;;; package -- Summary
;;; Commentary: this is zerosign init.el
;;; Code:

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'default-frame-alist '(font . "Source Code Pro-9"))
(set-face-attribute 'default t :font "Source Code Pro-9")
(set-face-attribute 'default nil :font "Source Code Pro-9")
(set-frame-font "Source Code Pro-9" nil t)

(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-defer t
      use-package-always-ensure t)

(setq gnutls-log-level 2)

;; settings
(global-auto-revert-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
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
(global-linum-mode t)

;; begin envs setup

;; nvm env variable setups
(defconst nvm-dir (concat (getenv "HOME") "/.nvm"))

(defun nvm-version (path)
  "Return nvm current nvm version."
  (substring (with-temp-buffer
               (insert-file-contents (concat path "/alias/default"))
               ;; eliminate newline (hack)
               (buffer-string)) 0 -1))

(defun nvm-bin-path (path)
  "Return nvm current bin path"
  (concat nvm-dir "/versions/node/" (nvm-version nvm-dir) "/bin"))

(defconst default-snapshot-dir (expand-file-name "snapshots" user-emacs-directory))
(setq create-lockfiles nil)

(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq extra-paths
      (list "/opt/racket/bin"
            "/opt/lein/bin"
            "/opt/scala/bin"
            "/opt/mu/bin"
            (concat (getenv "HOME") "/go/bin")
            (concat (getenv "HOME") "/.cargo/bin")
            (concat (getenv "HOME") "/.local/bin")
            (nvm-bin-path nvm-dir)))

(add-to-list 'load-path "/opt/mu/share/emacs/site-lisp/mu4e")

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

(setenv "GO111MODULE" "on")

;; end envs setup

;; add supports for why3
;;(add-to-list 'load-path "~/Repositories/ocaml/why3/share/emacs/")
;; (load "why3")

(add-to-list 'load-path "~/Repositories/emacs/tla-mode")
(require 'tla-mode)
;; (use-package tla-mode :mode "\\.tla$")


(add-to-list 'load-path "~/Repositories/ocaml/dune/editor-integration/emacs")
(require 'dune)

(use-package company :demand t :pin melpa
  :init (global-company-mode))

(use-package company-lsp :demand t :pin melpa)
(use-package lsp-treemacs :demand t :pin melpa)

;; begin projectile & ivy setup
;;
(use-package ivy :demand t :pin melpa :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (setq ivy-extra-directories nil)
  :config
  (define-key ivy-minibuffer-map (kbd "C-l") (kbd "DEL"))
  (use-package smex :ensure t :pin melpa :init
    (setq-default smex-history-length 32
                  smex-save-file (expand-file-name "smex-items" default-snapshot-dir)))
  (use-package flx :ensure t :pin melpa)
  (ivy-mode)
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>"    . ivy-resume)
         ("s-b"     . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("TAB"     . ivy-alt-done)
         ("<escape>" . minibuffer-keyboard-quit)
         ("C-j"      . ivy-next-line)
         ("C-k"      . ivy-previous-line)))

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

(use-package projectile :diminish projectile-mode :pin melpa :init
  (setq projectile-enable-caching t)
  (setq projectile-cache-file (expand-file-name "projectile.cache" default-snapshot-dir))
  (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" default-snapshot-dir))
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action 'projectile-dired)
  :config (projectile-mode)
  :bind (("C-c f" . projectile-find-file)
         ("C-c p" . projectile-switch-project)))

(use-package counsel :pin melpa)

;; code search
(use-package rg :pin melpa)
(use-package ag :pin melpa)

(use-package projectile-ripgrep :pin melpa
  :bind (("C-c C-s" . projectile-ripgrep)))

(use-package counsel-projectile
  :pin melpa
  :diminish counsel-projectile-mode
  :config (counsel-projectile-mode) :init
  (setq projectile-switch-project-action 'projectile-dired))

;; end projectile & ivy setup

(use-package gruvbox-theme :pin melpa)
(use-package flatui-theme :pin melpa)
(use-package brutalist-theme :pin melpa)

;; (load-theme 'brutalist-dark t)
(load-theme 'gruvbox-light-medium t)
;; (load-theme 'leuven t)

;; multiple major modes in emacs
(use-package polymode :pin melpa)
(use-package poly-markdown :pin melpa :mode "\\.md$")
(use-package poly-rst :pin melpa :mode "\\.rst$")
(use-package poly-slim :pin melpa :mode "\\.slim$")

(use-package markdown-mode :pin melpa
  :init
  (setq markdown-command "multimarkdown"))

(use-package mermaid-mode :pin melpa)

(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

;; magit
(use-package magit :pin melpa :commands magit-status magit-blame)
(use-package magit-todos :pin melpa)
(use-package magit-lfs :pin melpa)
(use-package forge :pin melpa :after magit)

;; tramp
(use-package tramp :init
  (setq tramp-default-method "ssh"))

;; elscreen
(use-package elscreen
  :pin melpa
  :init (elscreen-start))

(use-package phabricator :pin melpa)

;; eldoc
(use-package eldoc
  :pin melpa
  :diminish eldoc-mode :commands eldoc-mode)

(use-package bnf-mode :pin melpa)

;; editorconfig
(use-package editorconfig
  :pin melpa
  :config (editorconfig-mode t))

(use-package editorconfig-charset-extras
   :pin melpa)

;; java
(use-package jdecomp
  :pin melpa
  :config (setq jdecomp-decompiler-type 'procyon
                jdecomp-decompiler-paths '((cfr . "/opt/jde/cfr.jar")
                                           (fernflower . "/opt/jde/fernflower.jar")
                                           (procyon . "/opt/jde/procyon.jar"))))

(use-package flycheck :pin melpa :config (global-flycheck-mode))

(use-package epg
  :config (setq epg-gpgconf-program "gpg"))

(use-package restclient :pin melpa)
(use-package restclient-helm :pin melpa)

(use-package ssh-agency :pin melpa)
(use-package ssh-config-mode :pin melpa)

(use-package sudo-edit :pin melpa)
(use-package systemd :pin melpa)

(use-package gitignore-mode :pin melpa)
(use-package gitignore-templates :pin melpa)

(use-package persistent-scratch :pin melpa
  :config (persistent-scratch-setup-default))

(use-package scratch :pin melpa)

(use-package rvm :pin melpa)
(use-package nvm :pin melpa)

;; language supports

;; docker supports
(use-package dockerfile-mode :pin melpa)
(use-package docker-compose-mode :pin melpa)

;; rfc mode supports
(use-package rfc-mode :pin melpa)

;; R mode supports
(use-package ess :pin melpa)

;; rust supports
(use-package rust-mode :pin melpa :mode "\\.rs$"
  :config (setq rust-format-on-save t))

(use-package rust-playground :pin melpa )
(use-package cargo :pin melpa)

;; jsonnet
(use-package jsonnet-mode :pin melpa)

;; go supports
(use-package go-mode :pin melpa :mode "\\.go$"
  :hook (before-save . gofmt-before-save))

;; ocaml supports
(use-package tuareg :pin melpa)

;; elixir supports
(use-package elixir-mode :pin melpa)
;; (use-package elixir-mix :pin melpa)
(use-package flycheck-elixir :pin melpa)

;; scala supports
(use-package scala-mode :pin melpa :mode "\\.s\\(c\\|cala\\|bt\\)$")

;; elixir supports
;; (use-package lsp-elixir :pin melpa :mode "\\.exs$")

(use-package pug-mode :pin melpa)

(use-package pkgbuild-mode :pin melpa)

(global-auto-revert-mode t)

;; (use-package auto-complete :pin melpa
;;   :config
;;   (ac-config-default)
;;   (global-autocomplete-mode t))

;; init lsp
(use-package yasnippet :pin melpa)
(use-package lsp-mode :pin melpa
  :hook
  (scala-mode . lsp)
  (rust-mode . lsp)
  (tuareg-mode . lsp)
  (go-mode . lsp)
  :commands lsp)

(use-package lsp-ui :pin melpa :commands lsp-ui-mode)
(use-package company-lsp :pin melpa :commands company-lsp)
(use-package lsp-treemacs :pin melpa :commands lsp-treemacs-errors-list)
(use-package dap-mode :pin melpa)
