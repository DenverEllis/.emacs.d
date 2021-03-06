;;; init10-face.el
;; Visual Customizations to be run on start of emacs

;; Copyright (c) 2015 Denver S. Ellis

;;Author: Denver Ellis <ellis.denver@yahoo.com>
;;Maintained By: Denver Ellis <ellis.denver@yahoo.com>
;;Created: 29 May 2019
;;Last Updated: 29 May 2019

;; Keywords: configuration, zenburn
;; Homepage: https://github.com/DenverEllis/.emacs.d.git

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

(require 'use-package)

;; === Theme Choice ===
(use-package zenburn-theme
  :ensure t
  :init
  (load-theme 'zenburn t))


;; === Sane Defaults ===
;; Starting Splash Screen
(setq inhibit-splash-screen   t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; Remove Scrollbar, Toolbar, and Menu, full screen
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(toggle-frame-fullscreen)

;; Set default fill column
(setq-default fill-column 80)

;; A quiet emacs is a happy developer...
(setq visible-bell nil)
(setq ring-bell-function 'ignore)


;; === Mode Line Customizations ===
(setq line-number-mode 1)
(setq column-number-mode 1)


;; === Helm Mode ===
(require 'helm)
(require 'helm-config)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ;rebind TAB
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ;make TAB work in terminal
(define-key helm-map (kbd "C-z") 'helm-select-action) ;list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

;; Helm Sane Defaults
(setq helm-split-window-in-side-p           t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-scroll-amount                    8
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line        t)

;; Helm Fuzzy Settings
(setq helm-M-x-fuzzy-match       t
      helm-buffers-fuzzy-match   t
      helm-recentf-fuzzy-match   t
      helm-semantic-fuzzy-match  t
      helm-imenu-fuzzy-match     t
      helm-locate-fuzzy-match    t
      helm-apropos-fuzzy-match   t
      helm-lisp-fuzzy-completion t)

(setq helm-autoresize-max-height  0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

;; Helm key bindings
(global-set-key (kbd "M-x") 'helm-M-x)                ;; Replaces standard M-x
(global-set-key (kbd "M-y") 'helm-show-kill-ring)     ;; Replaces "cycle-kill-ring"
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)     ;; Replaces "find-file" command
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)

(helm-mode 1) ;; have this at the bottom of the helm initializer
