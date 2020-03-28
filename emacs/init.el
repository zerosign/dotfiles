;;; package -- Summary
;;; Commentary: this is zerosign init.el
;;; Code:

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; TODO : evaluate emacs-tree-sitter
;; (add-to-list 'load-path "/home/zerosign/Repositories/rust/emacs-tree-sitter")

(add-to-list 'default-frame-alist '(font . "Cascadia Code-10:Regular"))
(set-face-attribute 'default t :font "Cascadia Code-10:Regular")
(set-face-attribute 'default nil :font "Cascadia Code-10:Regular")
(set-frame-font "Cascadia Code-10:Regular" nil t)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")
                         ("org"   . "https://orgmode.org/elpa")
                         ("ublt" . "https://elpa.ubolonton.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-defer t
      use-package-always-ensure t)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq gnutls-log-level 2)

;; settings
(setq auto-window-vscroll nil)
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
(defconst repository-dir (expand-file-name "Repositories" (getenv "HOME")))
(defconst blog-dir (expand-file-name "blog/zerosign.github.io" repository-dir))
(defconst twitter-dir (expand-file-name "twitter" (expand-file-name ".config" (concat (getenv "HOME")))))
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
(setq create-lockfiles nil)

(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq extra-paths
      (list (concat (getenv "HOME") "/go/bin")
            (concat (getenv "HOME") "/.cargo/bin")
            (concat (getenv "HOME") "/.opam/4.09.0+fp+flambda/bin")
            (concat (getenv "HOME") "/.local/bin")
            (nvm-bin-path nvm-dir)))

;; (add-to-list 'load-path "/opt/mu/share/emacs/site-lisp/mu4e")


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

(use-package quelpa :demand t :pin melpa
  :init (quelpa
          '(quelpa-use-package
            :fetcher git
            :url "https://github.com/quelpa/quelpa-use-package"))
         (require 'quelpa-use-package))

;; add supports for why3
;;(add-to-list 'load-path "~/Repositories/ocaml/why3/share/emacs/")
;; (load "why3")

(use-package company :demand t :pin melpa
  :config (setq company-dabbrev-downcase nil
                company-show-numbers t
                company-minimum-prefix-length 2
                company-dabbrev-other-buffers t
                company-idle-delay 0)
  :init (global-company-mode))

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

(use-package centaur-tabs
  :ensure t
  :pin melpa
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

(use-package swiper :ensure t :defer t
  :pin melpa
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

(use-package spinner :pin elpa :ensure t)
(use-package deadgrep :pin melpa
  :bind (("C-c C-s"  . deadgrep)))

(use-package counsel-projectile
  :pin melpa
  :diminish counsel-projectile-mode
  :config (counsel-projectile-mode) :init
  (setq projectile-switch-project-action 'projectile-dired))

(use-package annotate :ensure t :pin melpa)
(use-package browse-at-remote :ensure t :pin melpa)
(use-package cask :pin melpa :ensure t)

;;; theme packages
(use-package gruvbox-theme :pin melpa)
(use-package flatui-theme :pin melpa)
(use-package brutalist-theme :pin melpa)
(use-package zerodark-theme :pin melpa)
(use-package cloud-theme :pin melpa)
(use-package poet-theme :pin melpa)
(use-package faff-theme :pin melpa)
(use-package mood-one-theme :pin melpa)
(use-package silkworm-theme :pin melpa)
(use-package hydandata-light-theme :pin melpa)
;; (use-package eziam-theme :pin melpa)
(use-package parchment-theme :pin melpa)
(use-package nord-theme :pin melpa)
(use-package github-theme :pin melpa)
(use-package github-modern-theme :pin melpa)
;; (use-package almost-mono-theme :pin melpa)
(use-package darktooth-theme :pin melpa)

;; (load-theme 'poet-dark-monochrome t)
;; (load-theme 'nord t)
(load-theme 'github-modern t)

(zerodark-setup-modeline-format)

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

;; ;; elscreen
;; (use-package elscreen
;;   :pin melpa
;;   :init (elscreen-start))

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

;; nginx mode supports
(use-package nginx-mode :pin melpa)

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

(use-package groovy-mode :pin melpa :ensure t)

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

(use-package json-mode :pin melpa)

;; language supports

;; docker supports
(use-package dockerfile-mode :pin melpa)
(use-package docker-compose-mode :pin melpa)

;; rfc mode supports
(use-package rfc-mode :pin melpa)

;; R mode supports
(use-package ess :pin melpa)

;; php mode supports
(use-package php-mode :ensure t :pin melpa)

;; fsharp mode supports
(use-package fsharp-mode :ensure t :pin melpa
  :hook (fsharp . (lambda () (require 'eglot-fsharp))))

;; rust supports
(use-package rust-mode :pin melpa :mode "\\.rs$"
  :config (setq rust-format-on-save t
                ;; https://github.com/rust-lang/rust-mode/issues/288#issuecomment-469276567
                rust-match-angle-brackets nil))

(use-package zig-mode :pin melpa)
(use-package direnv :pin melpa)
(use-package jq-mode :pin melpa)
(use-package pest-mode :pin melpa)

;; futhark supports
(use-package futhark-mode :pin melpa)

;; fish-shell supports
(use-package fish-mode :pin melpa)

(use-package rust-playground :pin melpa :ensure t)
(use-package cargo :pin melpa)

;; jsonnet
(use-package jsonnet-mode :pin melpa)

;; go supports
(use-package go-mode :pin melpa :mode "\\.go$"
  :hook (before-save . gofmt-before-save))

;; ocaml supports
(use-package tuareg :pin melpa)
(use-package dune :pin melpa)

;; elixir supports
(use-package elixir-mode :pin melpa)
;; (use-package elixir-mix :pin melpa)
(use-package flycheck-elixir :pin melpa)
(use-package flycheck-rust :pin melpa)

(use-package flycheck-gradle :pin melpa
  :ensure t
  :commands (flycheck-gradle-setup)
  :init (mapc (lambda (x)
                (add-hook x #'flycheck-gradle-setup))
              '(java-mode-hook kotlin-mode-hook)))

;; scala supports
(use-package scala-mode :pin melpa :mode "\\.s\\(c\\|cala\\|bt\\)$")

(use-package pug-mode :pin melpa)

(use-package pkgbuild-mode :pin melpa)

(global-auto-revert-mode t)

(use-package yasnippet :pin melpa)

;; (use-package dap-mode :pin melpa)

;; abandon lsp-mode, let's try eglot
(use-package eglot :pin melpa
  :config (add-to-list 'eglot-server-programs '((caml-mode tuareg-mode) . ("ocamllsp"))))

;; follow https://github.com/ubolonton/emacs-tree-sitter
(use-package tree-sitter :pin ublt :ensure t
  :config
  (add-hook 'rust-mode-hook #'tree-sitter-mode)
  (add-hook 'clojure-mode-hook #'tree-sitter-mode)
  (add-hook 'java-mode-hook #'tree-sitter-mode)
  (add-hook 'ocaml-mode-hook #'tree-sitter-mode)
  (add-hook 'tuareg-mode-hook #'tree-sitter-mode))


(use-package gif-screencast :pin melpa :ensure t)

(use-package pass :pin melpa)
(use-package password-store :pin melpa)
(use-package password-store-otp :pin melpa)
(use-package auth-source-pass :pin melpa
  :init (auth-source-pass-enable))

;; (use-package realgud :pin melpa)
;; (use-package realgud-byebug :pin melpa)
;; (use-package realgud-ipdb :pin melpa)
;; (use-package realgud-lldb :pin melpa)
;; (use-package realgud-node-debug :pin melpa)
;; (use-package realgud-node-inspect :pin melpa)
;; (use-package realgud-pry :pin melpa)

(use-package gradle-mode :pin melpa)
(use-package kotlin-mode :pin melpa)

(use-package webfeeder :quelpa (webfeeder :fetcher gitlab :repo "ambrevar/emacs-webfeeder"))


; (use-package irfc :quelpa (irfc :fetcher url "https://www.emacswiki.org/emacs/download/irfc.el"))

(quelpa '(irfc :url "https://www.emacswiki.org/emacs/download/irfc.el" :fetcher url))

(use-package tla-pcal-mode :quelpa (tla-pcal-mode :fetcher github :repo "mrc/tla-tools"))
(use-package proof-general :pin melpa :ensure t)
(use-package wat-mode :quelpa (wat-mode :fetcher github :repo "devonsparks/wat-mode"))
(use-package plantuml-mode :ensure t :pin melpa)
(use-package flycheck-plantuml :ensure t :pin melpa)

(use-package org :pin melpa
  :ensure t
  :mode (("\\.org$" . org-mode))
  :hook ((org-shiftleft-final . windmove-left)
         (org-shiftdown-final . windmove-down)
         (org-shiftright-final . windmove-right))
  ;; :init
  ;; (add-to-list 'org-src-lang-modes
  ;;              '(("plantuml" . plantuml)
  ;;                ("scala" . ammonite)
  ;;                ("ocaml" . tuareg)))
  :config
  (setq org-src-tab-acts-natively t
        org-support-shift-select 'always
        org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar"
        org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t)
     (emacs-lisp . t)
     (ruby . t)
     (ammonite . t)
     (shell . t)
     (sql . t)
     (plantuml . t)
     (ocaml . t))))

(use-package org-projectile
  :pin melpa
  :bind (("C-c n p" . org-projectile-project-todo-completing-read)
         ("C-c c" . org-capture))
  :config
  (progn
    (org-projectile-per-project)
    (setq org-projectile-per-project-filepath ".notes/todo.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates))
  :ensure t)

(use-package ob-ammonite :pin melpa
  :after org
  :init
  (add-to-list 'org-babel-load-languages '((ammonite . t))))

(use-package ob-rust :quelpa (ob-rust :fetcher gitlab :repo "ajyoon/ob-rust")
  :after org
  :init
  (add-to-list 'org-babel-load-languages '((rust . t))))

(use-package ob-go :quelpa (ob-go :fetcher gitlab :repo "pope/ob-go")
  :after org
  :init
  (add-to-list 'org-babel-load-languages '((go . t))))

(use-package ob-restclient :pin melpa
  :after org
  :init
  (add-to-list 'org-babel-load-languages '((restclient . t))))

(use-package ob-mermaid :pin melpa :ensure t
  :after org
  :init
  (add-to-list 'org-babel-load-languages '((mermaid . t))))

(use-package ob-prolog :ensure t :pin melpa
  :after org
  :init
  (add-to-list 'org-babel-load-languages '((prolog . t))))

;; (use-package org-ql :pin melpa :ensure t)
(use-package org-jira :pin melpa :ensure t)
(use-package ox-rfc :pin melpa :ensure t)
(use-package ox-epub :pin melpa :ensure t)
(use-package ox-gfm :pin melpa :ensure t)
(use-package ox-rst :pin melpa :ensure t)
;; (use-package ob-diagram :pin melpa :ensure t)
(use-package org-index :pin melpa :ensure t)
;; (use-package org-ioslide :pin melpa :ensure t)
(use-package org-kanban :pin melpa :ensure t)
(use-package org-trello :pin melpa :ensure t)
(use-package ox-ioslide :pin melpa :ensure t)
(use-package org-tree-slide :pin melpa :ensure t)
(use-package verb :pin melpa :ensure t)

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


;; (add-hook 'server-after-make-frame-hook '((let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;                                                                        ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;                                                                        ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;                                                                        ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;                                                                        (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;                                                  (base-font-color     (face-foreground 'default nil 'default))
;;                                                  (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
;;                                             (custom-theme-set-faces 'user
;;                                                                     `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;                                                                     `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;                                                                     `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;                                                                     `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;                                                                     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;                                                                     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
;;                                                                     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
;;                                                                     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
;;                                                                     `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))))

(use-package doct :ensure t :commands (doct))

(use-package gitlab-ci-mode :pin melpa :ensure t)
(use-package gitlab-ci-mode-flycheck :pin melpa :ensure t)

(use-package orgit :ensure t :pin melpa)

;; (setq org-export-latex-listings 'minted)
;; (add-to-list 'org-export-latex-packages-alist '("" "minted"))

(defun org-blog-prepare (project-plist)
  "With help from `https://github.com/howardabrams/dot-files'.
  Touch `index.org' to rebuilt it.
  Argument `PROJECT-PLIST' contains information about the current project."
  (let* ((base-directory (plist-get project-plist :base-directory))
         (buffer (find-file-noselect (expand-file-name "index.org" base-directory) t)))
    (with-current-buffer buffer
      (set-buffer-modified-p t)
      (save-buffer 0))
    (kill-buffer buffer)))

(defun org-blog-sitemap-format-entry (entry _style project)
  "Return string for each ENTRY in PROJECT."
  (when (s-starts-with-p "posts/" entry)
    (format "@@html:<span class=\"archive-item\"><span class=\"archive-date\">@@ %s @@html:</span>@@ [[file:%s][%s]] @@html:</span>@@"
            (format-time-string "%h %d, %Y" (org-publish-find-date entry project))
            entry
            (org-publish-find-title entry project))))


(defun org-blog-sitemap-function (title list)
  "Return sitemap using TITLE and LIST returned by `org-blog-sitemap-format-entry'."
  (concat "#+TITLE: " title "\n\n"
          "\n#+begin_archive\n"
          (mapconcat (lambda (li)
                       (format "@@html:<li>@@ %s @@html:</li>@@" (car li)))
                     (seq-filter #'car (cdr list))
                     "\n")
          "\n#+end_archive\n"))
