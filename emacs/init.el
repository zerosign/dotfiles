;;; package -- Summary
;;; Commentary: this is zerosign init.el
;;; Code:

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; begin envs setup
(defconst repository-dir (expand-file-name "Repositories" (getenv "HOME")))
;; (defconst blog-dir (expand-file-name "blog/zerosign.github.io" repository-dir))
;;(defconst twitter-dir (expand-file-name "twitter" (expand-file-name ".config" (concat (getenv "HOME")))))
(defconst nvm-dir (concat (getenv "HOME") "/.nvm"))
(defconst caches-dir (expand-file-name "caches" user-emacs-directory))

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

;; set opam environment
(defun opam-env ()
  (interactive nil)
  (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var))))

;; use define-inline or defsubst
;; ('home . "go/bin") .
;; ('home . ".cargo/bin") .
;; ('home . ".local/bin") .
;; ('home . ".local/share/coursier/bin") .
;; ('home . 'nvm-path) .
;; ('home . 'opam-path)
;; ('aux  . "dotty/current/bin")

;; ('home . (getenv "HOME")) .
;; ('aux  . "/opt") .
;; ('cache . '(path-of 'user-emacs-directory "caches"))
;; ('repo . '(path-of 'home "Repositories"))

(setq extra-paths
      (list (concat (getenv "HOME") "/go/bin")
            (concat (getenv "HOME") "/.cargo/bin")
            ;; (concat (getenv "HOME") "/.opam/4.09.0+fp+flambda/bin")
            (concat (getenv "HOME") "/.local/bin")
            "/opt/dotty/current/bin"
            (concat (getenv "HOME") "/.local/share/coursier/bin")
            (nvm-bin-path nvm-dir)))

;; (add-to-list 'load-path "/opt/mu/share/emacs/site-lisp/mu4e")

(defun set-external-paths (externals)
  (let ((paths (append (delete-dups (split-string (getenv "PATH") ":")) externals)))
    (setenv "PATH"
     (seq-reduce
      '(lambda (lhs rhs) (concat lhs path-separator rhs)) paths ""))
    (setq exec-path (append exec-path externals))
    (opam-env)))

(set-external-paths extra-paths)

(defun zerosign--setup-envs ()
  (setenv "LANG"   "en_US.UTF-8")
  (setenv "LC_ALL" "en_US.UTF-8")
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (setenv "GO111MODULE" "on"))

(defun zerosign--setup-tls ()
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"
        gnutls-log-level 0))

(zerosign--setup-tls)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")
                         ;; ("org"   . "https://orgmode.org/elpa")
                         ("ublt" . "https://elpa.ubolonton.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq-default use-package-always-defer t
	      use-package-always-ensure t)

(use-package quelpa :demand t
  :init
  (quelpa
   '(quelpa-use-package
     :fetcher git
     :url "https://github.com/quelpa/quelpa-use-package"))
  (require 'quelpa-use-package))

(use-package ivy :demand t :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        ivy-extra-directories nil)
  :config
  (define-key ivy-minibuffer-map (kbd "C-l") (kbd "DEL"))
  (use-package smex
    :init (setq-default smex-history-length 32
			smex-save-file (expand-file-name "smex-items" default-snapshot-dir)))
  (use-package flx)
  (ivy-mode)
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>"    . ivy-resume)
         ("s-b"     . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("TAB"     . ivy-alt-done)
         ("<escape>" . minibuffer-keyboard-quit)
         ("C-j"      . ivy-next-line)
         ("C-k"      . ivy-previous-line)))

(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

(use-package swiper
  ;; :init (ivy-mode)
  :config (setq ivy-use-virtual-buffers t
                enable-recursive-minibuffers t)
  :bind (("C-s"     . swiper)
         ("C-c C-r" . ivy-resume)
         ("<f6>"    . ivy-resume)
         ("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c k"   . ounsel-rg)))

(use-package projectile :diminish projectile-mode
  :init (setq projectile-enable-caching t
	      projectile-cache-file (expand-file-name "projectile.cache" default-snapshot-dir)
	      projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" default-snapshot-dir)
	      projectile-completion-system 'ivy
	      projectile-switch-project-action 'projectile-dired)
  :config (projectile-mode)
  :bind (("C-c f" . projectile-find-file)
         ("C-c p" . projectile-switch-project)))
(use-package counsel)
;; (use-package spinner :pin elpa :ensure t)
(use-package deadgrep
  :bind (("C-c C-s"  . deadgrep)))
(use-package counsel-projectile
  :diminish counsel-projectile-mode
  :config (counsel-projectile-mode) :init
  (setq projectile-switch-project-action 'projectile-dired))
(use-package pass)
(use-package password-store)
(use-package password-store-otp)
(use-package auth-source-pass
  :init (auth-source-pass-enable))
(use-package tramp :init
  (setq tramp-default-method "ssh"))
(use-package company
  :demand t
  :config (setq company-dabbrev-downcase nil
                company-show-numbers t
                company-minimum-prefix-length 2
                company-dabbrev-other-buffers t
                company-idle-delay 0)
  :init (global-company-mode))
(use-package epg :config (setq epg-gpgconf-program "gpg"))
(use-package restclient)
(use-package restclient-helm)
(use-package ssh-agency)
(use-package ssh-config-mode)
(use-package sudo-edit)
(use-package systemd)
(global-auto-revert-mode t)
(use-package yasnippet)
(use-package verb)
;; eldoc
(use-package eldoc
  :diminish eldoc-mode :commands eldoc-mode)
(use-package flycheck :config (global-flycheck-mode))

(defun zerosign--setup-layouts ()
  (interactive)
  (setq auto-window-vscroll nil
		  indent-tabs-mode nil
		  tab-width 3
		  inhibit-startup-screen t
		  initial-scratch-message nil
		  initial-major-mode 'org-mode
		  make-backup-files nil
		  scroll-error-top-bottom t
		  ring-bell-function 'ignore)
  (global-linum-mode t)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(defun zerosign--setup-defaults ()
  (setq backup-directory-alist `((".*" . ,temporary-file-directory))
        auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
        delete-old-versions t
        create-lockfiles nil
        kept-new-versions 6
        kept-old-versions 2
        version-control t))

(defun zerosign--config-font (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (set-face-attribute 'default t :font "Source Code Variable-10:Semibold")
    (set-face-attribute 'default nil :font "Source Code Variable-10:Semibold")))

(defun zerosign--setup-hooks ()
  (add-hook 'after-make-frame-functions #'zerosign--config-font)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(zerosign--setup-hooks)
(zerosign--setup-layouts)
(zerosign--setup-envs)
(zerosign--setup-defaults)

;;(defun zerosign--docs ()
;;  (interactive)
(use-package adoc-mode
  :mode "\\.adoc\\$")
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
(use-package mermaid-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode)))

;;(defun zerosign--themes ()
;;  (interactive)
(use-package gruvbox-theme)
(use-package flatui-theme)
(use-package brutalist-theme)
;; (use-package zerodark-theme)
(use-package cloud-theme)
(use-package poet-theme)
(use-package faff-theme)
(use-package mood-one-theme)
(use-package silkworm-theme)
(use-package hydandata-light-theme)
;; (use-package eziam-theme)
(use-package parchment-theme)
(use-package nord-theme )
(use-package github-theme)
(use-package brutal-theme)
(use-package night-owl-theme)
(use-package github-modern-theme)
;; (use-package almost-mono-theme)
(use-package darktooth-theme)
(use-package flucui-themes)

;;(defun zerosign--setup-vc ()
;;(interactive)
(use-package magit :commands magit-status magit-blame)
(use-package magit-todos )
(use-package magit-lfs )
(use-package forge  :after magit)

;;(defun zerosign--pl-basic ()
;; scala supports
(use-package scala-mode  :mode "\\.s\\(c\\|cala\\|bt\\)$")
(use-package groovy-mode)
(use-package erlang)
(use-package php-mode)
(use-package typescript-mode)
(use-package haskell-mode)
;; rust supports
(use-package rust-mode  :mode "\\.rs$"
  :config (setq rust-format-on-save t
                ;; https://github.com/rust-lang/rust-mode/issues/288#issuecomment-469276567
                rust-match-angle-brackets nil))
(use-package zig-mode)
;; go supports
(use-package go-mode  :mode "\\.go$"
  :hook (before-save . gofmt-before-save))
;; ocaml supports
(use-package tuareg)
(use-package dune)
;; elixir supports
(use-package elixir-mode)
;; futhark supports
(use-package futhark-mode)
;; fish-shell supports
(use-package fish-mode)
(use-package kotlin-mode)
(use-package tree-sitter
  :pin ublt
  :after tree-sitter-langs
  :demand t
  :hook ((js-mode . tree-sitter-hl-mode)
         (typescript-mode . tree-sitter-hl-mode)
         (python-mode . tree-sitter-hl-mode)
         (rust-mode . tree-sitter-hl-mode)
         (go-mode . tree-sitter-hl-mode)
         (scala-mode . tree-sitter-hl-mode)
         (clojure-mode . tree-sitter-hl-mode)
         (java-mode . tree-sitter-hl-mode)
         (ocaml-mode . tree-sitter-hl-mode)
         (tuareg-mode . tree-sitter-hl-mode))
  :config (progn
            (require 'tree-sitter-query)
            (require 'tree-sitter-hl)
            (global-tree-sitter-mode)))

(use-package tree-sitter-langs :pin ublt)

;;(defun zerosign--flychecks ()
;;  (interactive)
(use-package flycheck-elixir)
(use-package gitlab-ci-mode-flycheck)
(use-package flycheck-plantuml)
(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
(use-package flycheck-gradle
  :commands (flycheck-gradle-setup)
  :init (mapc (lambda (x)
                (add-hook x #'flycheck-gradle-setup))
	      '(java-mode-hook kotlin-mode-hook)))
(flycheck-define-checker scala
			 "A Scala syntax checker using the Scala compiler.
    See URL `http://www.scala-lang.org/'."
			 :command ("scalac" "-target:1.12" "-encoding" "UTF-8" "-deprecation" "-opt-warnings" source)
			 :error-patterns
			 ((error line-start (file-name) ":" line ": error: " (message) line-end))
			 :modes scala-mode
			 :next-checkers ((warning . scala-scalastyle)))

;;(defun zerosign--pl-aux ()
;;  (interactive)
(use-package graphviz-dot-mode)
(use-package gitlab-ci-mode)
(use-package gradle-mode)
(use-package meson-mode)
(use-package jsonnet-mode )
(use-package json-mode)
(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package rfc-mode)
(use-package jq-mode)
(use-package pest-mode)
(use-package terraform-mode)
(use-package gitignore-mode)
(use-package protobuf-mode)
(use-package pug-mode)
(use-package gnuplot)
(use-package nginx-mode)
(use-package pkgbuild-mode)
(use-package plantuml-mode)
(use-package editorconfig
  :config (editorconfig-mode t))
(use-package bnf-mode)
(use-package editorconfig-charset-extras)

;;(defun zerosign--devops ()
;;  (interactive)
(use-package ansible)

(defun zerosign-confirm-babel-evaluate (lang body)
  (member lang '("plantuml" "ditaa")))

;;(defun zerosign--setup-org ()
;;  (interactive)
(use-package org
  :mode (("\\.org$" . org-mode))
  :hook ((org-shiftleft-final . windmove-left)
         (org-shiftdown-final . windmove-down)
         (org-shiftright-final . windmove-right))
  :config
  (setq org-src-tab-acts-natively t
        org-support-shift-select 'always
        org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar"
        org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"
        org-confirm-babel-evaluate 'zerosign-confirm-babel-evaluate))
(use-package ox-hugo)
(use-package org-projectile

  :bind (("C-c n p" . org-projectile-project-todo-completing-read)
         ("C-c c" . org-capture))
  :config
  (progn
    (org-projectile-per-project)
    (setq org-log-done 'note)
    (setq org-projectile-per-project-filepath ".notes/todo.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates)))

(use-package ob-ammonite
  :after org
  :init
  (add-to-list 'org-babel-load-languages '((ammonite . t))))

(use-package ob-restclient
  :after org
  :init
  (add-to-list 'org-babel-load-languages '((restclient . t))))
(use-package ob-mermaid
  :after org
  :init
  (add-to-list 'org-babel-load-languages '((mermaid . t))))
(use-package ob-prolog
  :after org
  :init
  (add-to-list 'org-babel-load-languages '((prolog . t))))
(use-package org-jira)
(use-package ox-rfc)
(use-package ox-epub)
(use-package ox-gfm)
(use-package ox-rst)
(use-package org-index)
(use-package org-kanban)
;; (use-package org-trello)
;; (use-package ox-ioslide)
(use-package org-tree-slide)
(use-package ob-kotlin)
(use-package org-ref)
(use-package org-brain)
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
;; (use-package polymode)
(use-package orgit)
(global-set-key (kbd "C-c l") 'org-store-link)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (java . t)
   (clojure . t)
   (C . t)
   (lilypond . t)
   (ruby . t)
   ;; (rust . t)
   ;; (rustic . t)
   (python . t)
   (dot . t)
   (screen . t)
   (fortran . t)
   (prolog . t)
   (kotlin . t)
   ;; (http . t)
   (R . t)
   (ditaa . t)
   (js . t)
   (ammonite . t)
   (shell . t)
   (sql . t)
   (plantuml . t)
   (ocaml . t)))

;;(defun zerosign--devs ()
;;  (interactive)
(use-package jdecomp
  :config (setq jdecomp-decompiler-type 'fernflower
                jdecomp-decompiler-paths '((cfr . "/opt/jde/cfr.jar")
                                           (fernflower . "/opt/jde/fernflower.jar")
                                           (procyon . "/opt/jde/procyon.jar"))))
(use-package gitignore-templates)
(use-package persistent-scratch
  :config (persistent-scratch-setup-default))
(use-package scratch)
(use-package rvm)
(use-package nvm)
(use-package direnv)
(use-package rust-playground)
(use-package cargo)
(use-package proof-general)

;;(defun zerosign--comp ()
;;  (interactive)
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs `(tuareg-mode . ("ocamllsp"))))
(use-package rustic
  :config
  (setq rustic-lsp-client 'eglot
        rustic-lsp-server 'rust-analyzer))

;; (zerosign--setup-packages)
;;(zerosign--init-essentials)
;;(zerosign--themes)
(load-theme 'poet t)
;;(zerosign--setup-vc)
;;(zerosign--pl-basic)
;;(zerosign--pl-aux)
;;(zerosign--flychecks)
;;(zerosign--setup-org)
;;(zerosign--devs)
;;(zerosign--comp)

(use-package wat-mode :quelpa (wat-mode :fetcher github :repo "devonsparks/wat-mode"))
(use-package tla-pcal-mode :quelpa (tla-pcal-mode :fetcher github :repo "mrc/tla-tools"))
;; (use-package ob-rust :quelpa (ob-rust :fetcher gitlab :repo "ajyoon/ob-rust")
;;     :after org
;;     :init
;;     (add-to-list 'org-babel-load-languages '((rust . t))))

(use-package ob-go :quelpa (ob-go :fetcher gitlab :repo "pope/ob-go")
    :after org
    :init
    (add-to-list 'org-babel-load-languages '((go . t))))

(use-package rmsbolt :quelpa (rmsbolt :fetcher gitlab :repo "jgkamat/rmsbolt"))

(use-package doct :commands (doct))
