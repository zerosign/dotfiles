(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

;; (unless (require 'quelpa nil t)
;;   (with-temp-buffer
;;     (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
;;     (eval-buffer)))

(defconst default-snapshot-dir (expand-file-name "snapshots" user-emacs-directory))

(global-auto-revert-mode t)
(menu-bar-mode -1)

(setenv "PATH" (concat (getenv "PATH") ":/opt/racket/bin"))
(setq exec-path (append exec-path '("/opt/racket/bin")))

;; (quelpa '(quelpa-use-package :fetcher github :repo "quelpa/quelpa-use-package"))
;; (require 'quelpa-use-package)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 3)

(setq inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'text-mode
      make-backup-files nil
      scroll-error-top-bottom t
      use-package-always-ensure t
      ring-bell-function 'ignore)

;; themes
(use-package github-theme        :defer t :ensure t :pin melpa)
(use-package lenlen-theme        :defer t :ensure t :pin melpa)
(use-package kaolin-themes       :defer t :ensure t :pin melpa)
(use-package flatui-theme        :defer t :ensure t :pin melpa)
(use-package apropospriate-theme :defer t :ensure t :pin melpa)
(use-package foggy-night-theme   :defer t :ensure t :pin melpa)

(load-theme 'kaolin-dark t)

(use-package smart-mode-line
  :ensure t
  :config
  (progn
    (setq sml/theme 'respectful)
    (setq sml/name-width 40)
    (setq sml/mode-width 'full)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-linum-mode t)

(require 'autoload)

(setenv "LANG"   "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

;; org-bullets
(use-package org-bullets :ensure t :config (add-hook 'org-mode-hook #'org-bullets-mode))

(use-package epresent :ensure t :defer t)

;; ox
(use-package ox-asciidoc :ensure t :defer t)
(use-package ox-epub :ensure t :defer t)
(use-package ox-jira :ensure t :defer t)
(use-package ox-hugo :ensure t :defer t)
(use-package ox-pandoc :ensure t :defer t)
(use-package ox-rst :ensure t :defer t)
(use-package pandoc-mode :ensure t :defer t)

;; code search
(use-package ripgrep :ensure t :defer t)
(use-package ag      :ensure t :defer t)

;; elscreen
(use-package elscreen :ensure t :defer t :init (elscreen-start))

;; docker
(use-package dockerfile-mode :ensure t :defer t :pin melpa)
(use-package docker-compose-mode :ensure t :defer t :pin melpa)

;; ;; spaceline
;; (use-package spaceline :ensure t :init
;;   (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
;;   (setq powerline-default-separator 'bar)
;;   :config (require 'spaceline-config)
;;   (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

;; eldoc
(use-package eldoc :ensure t :diminish eldoc-mode :commands eldoc-mode)

;; editorconfig
(use-package editorconfig :ensure t :config (editorconfig-mode t))
(use-package editorconfig-charset-extras :ensure t :defer t)

;; company
(use-package company :diminish company-mode :commands company-mode
  :config (setq company-dabbrev-ignore-case nil
                company-dabbrev-code-ignore-case nil
                company-dabbrev-downcase nil
                company-idle-delay 0
                company-minimum-prefix-length 4)
  :init (add-hook 'after-init-hook 'global-company-mode))

;; java
(use-package jdecomp :ensure t :defer t
  :config (setq jdecomp-decompiler-type 'fernflower
                jdecomp-decompiler-paths '((cfr . "/opt/jde/cfr.jar")
                                           (fernflower . "/opt/jde/fernflower.jar")
                                           (procyon . "/opt/jde/procyon.jar"))))

;; cmake mode
(use-package cmake-ide :ensure t)
(use-package cmake-font-lock :ensure t)
(use-package cmake-project :ensure t)

;; javascript + typescript
(use-package tide :ensure t :config (setq company-tooltip-align-annotations t
                                          flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package typescript-mode :ensure t :init (add-hook 'typescript-mode-hook #'tide-init))

(use-package nvm :ensure t :defer t)

(use-package markdown-mode :ensure t :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package js2-mode
  :ensure t
  :mode ("\\.js$" "\\.json$")
  :config
  (progn
    (setq-default js2-basic-offset 2)
    (setq-default js2-show-parse-errors t)
    (setq-default js2-strict-missing-semi-warning t)
    (setq-default js2-strict-trailing-comma-warning t)))

(use-package tern
  :ensure t
  :after js2-mode
  :config
  (add-hook 'js2-mode-hook (lambda () (tern-mode t))))

(use-package company-tern
  :after tern
  :ensure t
  :config (add-to-list 'company-backends 'company-tern))

(use-package indium
  :after company-tern
  :ensure t
  :defer t
  :init
  (add-hook 'js-mode-hook (lambda ()
                            (require 'indium)
                            (indium-interaction-mode))))

;; toml-mode
(use-package toml-mode :ensure t)

;; yaml-mode
(use-package yaml-mode :ensure t)

;; elixir
(use-package alchemist :defer t :commands alchemist-mode)
(use-package elixir-mode :commands elixir-mode :config (add-hook 'elixir-mode-hook 'alchemist-mode))


;; slime
(use-package slime-company :ensure t :defer t)

(use-package slime :ensure t :defer t
  :init (add-hook 'elisp-mode (slime-setup '(slime-fancy slime-company)))
  :config (setq slime-net-coding-system 'utf-8-unix
                slime-contribs '(slime-fancy slime-repl slime-scratch slime-trace-dialog)
                lisp-loop-forms-indentation   6)
  ;; :bind (( "C-n" . company-select-next)
  ;;        ( "C-p" . company-select-previous)
  ;;        ( "C-d" . company-show-doc-buffer)
  ;;        ( "M-." . company-show-location))
  (show-paren-mode 1))

;; Rust toolize
;; cargo
(use-package cargo :ensure t :defer t :init (add-hook 'rust-mode-hook 'cargo-minor-mode))
(use-package rust-mode :init (setq rust-format-on-save t))
(use-package racer :ensure t :after rust-mode
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-racer))
  :config
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-notations t)
  (add-hook 'rust-mode-hook  #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  :bind
  (("M-<RET>" . racer-describe)))

;; Scala toolize
(use-package ensime :ensure t :pin melpa
  :config
  (setq
   ensime-sbt-command "/usr/bin/sbt"
   sbt:program-name   "/usr/bin/sbt"
   ensime-search-interface 'ivy))

(use-package sbt-mode :ensure t :pin melpa)

;; scala
(use-package scala-mode :ensure t :init
  (add-hook 'scala-mode-hook '(lambda () ((company-mode)
                                          (ensime-mode)))))

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

(use-package waf-mode :ensure t :defer t :pin melpa)
(use-package yard-mode :ensure t :defer t :pin melpa)


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
  ;; (projectile-register-project-type
  ;;  'npm   '("package.json")
  ;;  :compile     "npm install"
  ;;  :test        "npm test"
  ;;  :run         "npm start"
  ;;  :test-suffix ".spec")
  ;; (projectile-register-project-type
  ;;  'cargo '("Cargo.toml")
  ;;  :compile     "cargo build"
  ;;  :test        "cargo test"
  ;;  :run         "cargo run")
  ;; (projectile-register-project-type
  ;;  'bundle '("Gemfile")
  ;;  :compile     "bundle install"
  ;;  :test        "bundle exec rspec")
  ;; (projectile-register-project-type
  ;;  'sbt    '("build.sbt")
  ;;  :compile     "sbt compile"
  ;;  :test        "sbt test"
  ;;  :run         "sbt run")
  ;; (projectile-register-project-type
  ;;  'cmake  '("CMakefile.txt")
  ;;  :compile     "cmake .; make"
  ;;  :test        "make test")
  :config (projectile-mode))

(use-package projectile-ripgrep :ensure t :defer t)

(use-package counsel-projectile :ensure t :init
  (defun counsel-projectile-ag ()
    (interactive)
    (counsel-ag nil (projectile-project-root)))
  :bind (("C-c p p"   . counsel-projectile-switch-project)
         ("C-c p b"   . counsel-projectile-switch-to-buffer)
         ("C-c p f"   . counsel-projectile-find-file)
         ("C-c p s s" . counsel-projectile-ag)))

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

(use-package coffee-mode :ensure t :defer t :init
  (setq-local whitespace-style '(trailing space-before-tab indentation empty space-after-tab))
  (custom-set-variables '(coffee-tab-width 2)))

;; magit
(use-package magit
  :commands magit-status magit-blame
  :bind (("C-c C-]" . magit-status)
         ("C-c C-[" . magit-checkout)
         ("C-c C-/" . magit-commit)
         ("C-c C-*" . magit-push-matching)
         ("C-c C-\\" . magit-blame)))

(use-package anaconda-mode :ensure t :commands anaconda-mode :diminish anaconda-mode :init
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'eldoc-mode)
    (add-hook 'python-mode-hook (lambda ()
                                  (flycheck-mode t)
                                  (setq flycheck-checker 'python-pylint
                                        flycheck-checker-error-threshold 900)))))

(use-package company-anaconda :ensure t :init
  (add-to-list 'company-backends 'company-anaconda))

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
    (add-to-list 'company-backends 'company-go)))

(use-package go-eldoc :ensure t :defer t :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("649ca960922e2176a451db44624bc4dbcd282bc1660d2621793145232f688836" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "8e23afd1939fb702db93df19271f48d44b22db8683fa9d1dab87a1f84a6484dc" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "d606ac41cdd7054841941455c0151c54f8bff7e4e050255dbd4ae4d60ab640c1" "3b5ce826b9c9f455b7c4c8bff22c020779383a12f2f57bf2eb25139244bb7290" "ddc7c847e2327389ac0b5a155ff9358a84b7292978c0d2a63f561918c5738d6f" default)))
 '(package-selected-packages
   (quote
    (markdown-mode protobuf-mode epresent editorconfig-charset-extras cmake-project cmake-font-lock cmake-ide apropospriate-theme flatui-themes docker-compose-mode dockerfile-mode yarn-mode vala-mode ag ripgrep yaml-mode editorconfig use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
