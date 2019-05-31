;;; init40-codingGen.el
;; General programming preferances

;; Copyright (c) 2019 Denver S. Ellis

;;Author: Denver Ellis <ellis.denver@yahoo.com>
;;Maintained By: Denver Ellis <ellis.denver@yahoo.com>
;;Created: 30 May 2019
;;Last Updated: 30 May 2019

;; Keywords: configuration, magit, flycheck
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

;; == Company-mode ==
(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)

  :config
  (use-package company-irony :ensure t :defer t)
  (setq company-idle-delay            nil
	company-minimum-prefix-length 3
	company-show-numbers          t
	company-tooltip-limit         20
	company-dabbrev-downcase      nil))

;; == YASnippet ==
(use-package yasnippet
  :ensure t
  :defer t
  :config (yas-global-mode t))

;; == WS-butler ==
;; WS is for White Space
(use-package ws-butler
  :ensure t
  :init
  (ws-butler-global-mode)
  :diminish ws-butler-mode)

  
;;; init40-codingGen.el end
