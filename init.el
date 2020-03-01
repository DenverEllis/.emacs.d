;;; init.el
;; Start up code for emacs

;; Copyright (c) 2015 Denver S. Ellis

;;Author: Denver Ellis <ellis.denver@yahoo.com>
;;Maintained By: Denver Ellis <ellis.denver@yahoo.com>
;;Created: 05 Mar 2019
;;Last Updated: 05 Mar 2019

;; Keywords: configuration
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

;; User Info
(setq user-full-name "Denver Ellis")
(setq user-mail-address "dsellis@ualr.edu")

;; Required packages
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives (append package-archives
                         '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("elpy" . "http://jorgenschaefer.github.io/packages/"))))
(package-initialize)


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package diminish :ensure t)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)


;; === Face Customization ==
(load-file "~/.emacs.d/config/init10-face.el")


;; === Interface ===


;; === Document Editing ===
(load-file "~/.emacs.d/config/init30-org.el")


;; === Programming & Coding Functions ===
(load-file "~/.emacs.d/config/init40-codingGen.el")
(load-file "~/.emacs.d/config/init41-lisp.el")
(load-file "~/.emacs.d/config/json-reformat.el")


;; === Misc. ===


;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("54f2d1fcc9bcadedd50398697618f7c34aceb9966a6cbaa99829eb64c0c1f3ca" default)))
 '(org-export-backends
   (quote
    (ascii beamer html icalendar latex man md odt org texinfo)))
 '(package-selected-packages
   (quote
    (slime-repl slime rust-mode php-mode flycheck zenburn-theme use-package exec-path-from-shell diminish))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
