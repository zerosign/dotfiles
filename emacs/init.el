(add-to-list 'term-file-aliases '("alacritty" . "xterm"))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq straight-recipes-gnu-elpa-use-mirror    t
      straight-repository-branch              "develop"
      straight-enable-use-package-integration t)

(defvar bootstrap-version)

(defconst default-snapshot-dir (expand-file-name "snapshots" user-emacs-directory))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-linum-mode t)

(setq auto-window-vscroll nil)
(global-auto-revert-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq-default indent-tabs-mode nil
              tab-width 3)
(setq inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'text-mode
      make-backup-files nil
      scroll-error-top-bottom t
      ;; use-package-always-ensure t
      ring-bell-function 'ignore
      create-lockfiles nil
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setenv "LANG"   "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(setenv "GO111MODULE" "on")

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Automatically :ensure each use-package.
;; (setq use-package-always-pin "melpa")
(use-package poet-theme :straight t)
(use-package ayu-theme :straight t)

;; (load-theme 'poet t)
;; (load-theme 'ayu-light t)
(load-theme 'leuven t)

;; (straight-use-package 'deadgrep)
(use-package deadgrep :straight t :bind (("C-c C-s"  . deadgrep)))

;; (straight-use-package 'company)
(use-package company
  :straight t
  :diminish
  :config
  (global-company-mode t)
  (setq ;; Only 2 letters required for completion to activate.
        company-minimum-prefix-length 2
        ;; Search other buffers for completion candidates
        company-dabbrev-other-buffers t
        company-dabbrev-code-other-buffers t
        ;; Allow (lengthy) numbers to be eligible for completion.
        company-complete-number t
        ;; M-⟪num⟫ to select an option according to its number.
        company-show-numbers t
        ;; Edge of the completion list cycles around.
        company-selection-wrap-around t
        ;; Do not downcase completions by default.
        company-dabbrev-downcase nil
        ;; Even if I write something with the ‘wrong’ case,
        ;; provide the ‘correct’ casing.
        company-dabbrev-ignore-case t
        ;; Immediately activate completion.
        company-idle-delay 0))

(add-hook 'prog-mode-hook 'company-mode)

(use-package prescient :straight t :defer t
  :config (prescient-persist-mode +1))

;; (ivy-prescient-mode +1)

;; (straight-use-package 'company-prescient)
(use-package company-prescient :straight t
  :after company-mode
  :defer t
  :config (company-prescient-mode +1))

(use-package protobuf-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode)))

;; (use-package selectrum-prescient)

(use-package counsel :straight t :demand t
  :bind (("M-x" . counsel-M-x)))

(use-package ivy :straight t :demand t :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        ivy-extra-directories nil)
  :config
  (define-key ivy-minibuffer-map (kbd "C-l") (kbd "DEL"))
  (use-package flx :straight t :defer t)
  (use-package ivy-prescient :straight t :defer t)
  (ivy-mode)
  (ivy-prescient-mode)
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>"    . ivy-resume)
         ("s-b"     . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("TAB"     . ivy-alt-done)
         ("<escape>" . minibuffer-keyboard-quit)
         ("C-j"      . ivy-next-line)
         ("C-k"      . ivy-previous-line)))

(use-package swiper :straight t)
(global-set-key "\C-s" 'swiper)

;; (straight-use-package 'magit)
(use-package magit
  :straight t
  :defer t
  :commands magit-status
  :hook ((magit-popup-mode-hook . no-trailing-whitespace)
	 (git-commit-mode . goto-address-mode)))

(use-package exec-path-from-shell :straight t :defer t)

(when (daemonp)
  (exec-path-from-shell-initialize))

;; (straight-use-package 'magit-todos)
(use-package magit-todos :straight t :defer t)

(use-package magit-delta :straight t :defer t)

(require 'subr-x)
(use-package git :straight t)

(straight-use-package '(org-plus-contrib :includes (org)))

;; (defun org-git-version ()
;;   "The Git version of org-mode.
;; Inserted by installing org-mode or when a release is made."
;;   (require 'git)
;;   (let ((git-repo (expand-file-name
;;                    "straight/repos/org/" user-emacs-directory)))
;;     (string-trim
;;      (git-run "describe"
;;               "--match=release\*"
;;               "--abbrev=6"
;;               "HEAD"))))
;;
;; (defun org-release ()
;;   "The release version of org-mode.
;; Inserted by installing org-mode or when a release is made."
;;   (require 'git)
;;   (let ((git-repo (expand-file-name
;;                    "straight/repos/org/" user-emacs-directory)))
;;     (string-trim
;;      (string-remove-prefix
;;       "release_"
;;       (git-run "describe"
;;                "--match=release\*"
;;                "--abbrev=0"
;;                "HEAD")))))

(use-package plantuml-mode :straight t
  :config
  (setq plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"
        plantuml-default-exec-mode 'jar))

(use-package flycheck-plantuml :straight t
  :hook (plantuml . flycheck))

;; (use-package org-plus-contrib :straight t
;;   :mode (("\\.org$" . org-mode))
;;   :init
;;   (require 'ob-dot)
;;   (require 'ob-plantuml)
;;   (setq org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
;;   :config
;;   (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
;;   (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((clojure . t)
;;      (emacs-lisp . t)
;;      (gnuplot . t)
;;      (java . t)
;;      (clojure . t)
;;      (dot . t)
;;      (C . t)
;;      (lilypond . t)
;;      (ruby . t)
;;      ;; (rust . t)
;;      ;; (rustic . t)
;;      (python . t)
;;      (dot . t)
;;      (screen . t)
;;      (fortran . t)
;;      ;; (prolog . t)
;;      ;; (kotlin . t)
;;      ;; (http . t)
;;      ;; (R . t)
;;      (ditaa . t)
;;      (js . t)
;;      (ammonite . t)
;;      (shell . t)
;;      (sql . t)
;;      (plantuml . t)
;;      (ocaml . t)))
;;   :bind (("C-c l" . org-store-link)
;;          ("C-c a" . org-agenda)))

(use-package org-projectile
  :straight t
  :bind (("C-c n p" . org-projectile-project-todo-completing-read)
         ("C-c c" . org-capture))
  :config
  (progn
    (setq org-projectile-per-project-filepath ".notes/notes.org"
          org-agenda-files (append agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates)))

;; (use-package ob-ammonite :straight t)
;; ;; (use-package org
;; ;;   :straight org-plus-contrib
;; ;;   :mode (("\\.org$" . org-mode))
;; ;;   ("C-c l" . org-store-link)
;; ;;   ("C-c a" . org-agenda))

(use-package lsp-metals :straight t)
(use-package lsp-java :straight t)

(use-package lsp-mode
  :straight t
  :init
  (setq lsp-rust-server 'rust-analyzer
        read-process-output-max (* 1024 1024)
        lsp-idle-delay 0.500
        lsp-enable-file-watchers nil
        gc-cons-threshold 100000000)
  :hook ((crystal-mode . lsp)
         (tuareg-mode . lsp)
         (kotlin-mode . lsp)
         (typescript-mode . lsp)
         (rust-mode . lsp)
         (go-mode . lsp)
         (scala-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :straight t :commands lsp-ui-mode)

;; if you are ivy user
(use-package lsp-ivy :straight t :commands lsp-ivy-workspace-symbol)
;; ;; (org-babel-do-load-languages
;; ;;  'org-babel-load-languages
;; ;;  '((clojure . t)
;; ;;    (emacs-lisp . t)
;; ;;    (gnuplot . t)
;; ;;    (java . t)
;; ;;    (clojure . t)
;; ;;    (dot . t)
;; ;;    (C . t)
;; ;;    (lilypond . t)
;; ;;    (ruby . t)
;; ;;    ;; (rust . t)
;; ;;    ;; (rustic . t)
;; ;;    (python . t)
;; ;;    (dot . t)
;; ;;    (screen . t)
;; ;;    (fortran . t)
;; ;;    ;; (prolog . t)
;; ;;    ;; (kotlin . t)
;; ;;    (http . t)
;; ;;    ;; (R . t)
;; ;;    (ditaa . t)
;; ;;    (js . t)
;; ;;    (ammonite . t)
;; ;;    (shell . t)
;; ;;    (sql . t)
;; ;;    (plantuml . t)
;; ;;    (ocaml . t)))

(use-package polymode :straight t)
(use-package poly-markdown :straight t)
(use-package poly-org :straight t)

(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.org" . poly-org-mode))

(use-package org-ql :straight t :defer t)
(use-package magit-org-todos :straight t :defer t :config (magit-org-todos-autoinsert))

(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(use-package forge :straight t)

(use-package projectile :straight t :diminish projectile-mode
  :init
  (setq projectile-enable-caching t
	projectile-cache-file (expand-file-name "projectile.cache" default-snapshot-dir)
	projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" default-snapshot-dir)
	projectile-completion-system 'ivy
	projectile-switch-project-action 'projectile-dired)
  :config (projectile-mode)
  :bind (("C-c f" . projectile-find-file)
	 ("C-c p" . projectile-switch-project)))

(straight-register-package
 '(tsc :host github
       :repo "ubolonton/emacs-tree-sitter"
       :files ("core/*.el")))

(straight-use-package
 '(tree-sitter :host github
               :repo "ubolonton/emacs-tree-sitter"
               :files ("lisp/*.el")))

(straight-use-package
 '(tree-sitter-langs :host github
                     :repo "ubolonton/emacs-tree-sitter"
                     :files ("langs/*.el" "langs/queries")))

(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)
(add-to-list 'tree-sitter-major-mode-language-alist '(tuareg-mode . ocaml))

(require 'tree-sitter-debug)
(require 'tree-sitter-query)

(add-hook 'tree-sitter-mode-hook #'tree-sitter-hl-mode)

(global-tree-sitter-mode)

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode :straight t)

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package rust-mode :straight t)
(use-package scala-mode :straight t)
(use-package tuareg :straight t)

(use-package flycheck-mercury :straight t)
(use-package verb :straight t)

(use-package racket-mode :straight t)

(use-package crystal-mode :straight t)

(use-package rmsbolt :straight t)

;; optionally if you want to use debugger
(use-package treemacs :straight t)

;; optional if you want which-key integration
(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package flycheck :straight t
  :config (global-flycheck-mode))

(use-package epg :straight t :config (setq epg-gpgconf-program "gpg"))

(use-package gradle-mode :straight t)
(use-package flycheck-gradle :straight t)

(use-package kotlin-mode :straight t)

(use-package persistent-scratch :straight t :config (persistent-scratch-setup-default))
(use-package dockerfile-mode :straight t)
(use-package docker-compose-mode :straight t)
(use-package dune :straight t)

(use-package pass :straight t)
(use-package password-store :straight t)
(use-package password-store-otp :straight t)
(use-package auth-source-pass :straight t :config (auth-source-pass-enable))

(use-package graphviz-dot-mode :straight t)

(use-package ripgrep :straight t)

(use-package bnf-mode :straight t)

(use-package emacsql-psql :straight t)
(use-package emacsql-mysql :straight t)
(use-package pcap-mode :straight t)

(use-package magithub :straight t)
(use-package ghub :straight t)
(use-package ghub+ :straight t)
(use-package github-browse-file :straight t)

(use-package typescript-mode :straight t)

(setq wl-copy-process nil)

(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "wl-copy"
                                      :buffer nil
                                      :command '("wl-copy" "-f" "-n")
                                      :connection-type 'pipe))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))

;; (defun wl-paste ()
;;   (if (and wl-copy-process (process-live-p wl-copy-process))
;;       nil
;;     (shell-command-to-string "wl-paste -n | tr -d \r")))

;; (setq interprogram-cut-function 'wl-copy)
;; (setq interprogram-paste-function 'wl-paste)

;; (use-package gist :straight t
;;   :config
;;   (setq gist-list-format '((created "Created" 15 nil "%D" "%R")
;;                            (id      "Id"      10 nil identity)
;;                            (files   "Files"   35 nil (lambda (files) (mapconcat 'identity files ", ")))
;;                            (visibility "Private" 8 nil (lambda (public) (or (and public "") "   Y")))
;;                            (description "Description" 0 nil identity))))

(use-package browse-at-remote :straight t)

(use-package xclip :straight t :init (xclip-mode))

(straight-use-package
 '(spthy-mode :type git
              :host github :repo "tamarin-prover/tamarin-prover"
              :branch "develop"
              :files (("etc/spthy-mode.el" . "spthy-mode.el"))))

(straight-use-package
 '(tla+-mode :type git
             :repo "https://git.sdf.org/bch/tlamode"
             :branch "master"
             :files (("lisp/tla+-mode.el" . "tla+-mode.el"))))

(add-to-list 'auto-mode-alist '("\\.spthy" . spthy-mode))
(add-to-list 'auto-mode-alist '("\\.tla" . tla+-mode))

(global-set-key (kbd "C-c C-_") 'comment-or-uncomment-region)

(use-package json-mode :straight t)

(use-package jdecomp :straight t
  :config
  (setq
   jdecomp-decompiler-paths '((fernflower . "/home/zerosign/.m2/repository/org/jetbrains/fernflower/fernflower/1.0/fernflower-1.0.jar"))
   jdecomp-decompiler-type 'fernflower))


(use-package editorconfig :straight t :config (editorconfig-mode 1))

(use-package deft :straight t
  :custom
  (deft-extensions '("org" "md" "txt"))
  (deft-directory  "~/Repositories/notes/zerodevs")
  (deft-use-filename-as-title t))

(use-package zetteldeft :straight t :after deft config (zettledeft-set-classic-keybindings))

(use-package julia-mode :straight t)

(use-package hcl-mode :straight t)

(use-package meson-mode :straight t)

(unless (display-graphic-p)
  (defun scroll-mouse-up ()
    (interactive)
    (scroll-up 15))
  (defun scroll-mouse-down ()
    (interactive)
    (scroll-down 15))
  ;; activate mouse-based scrolling
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-mouse-down)
  (global-set-key (kbd "<mouse-5>") 'scroll-mouse-up))
