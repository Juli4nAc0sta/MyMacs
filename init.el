;;; init.el --- My emacs config
;;; -*- coding: utf-8 -*-

;; Version: 0.0.1
;; Author: Julian Acosta <me@julianacosta.co>
;; Maintainer: Julian Acosta
;; Package-Requires: ((emacs "25.1.1"))
;; Repository: https://github.com/Juli4nAc0sta/MyMacs
;; Created: March 20
;; License: GNU General Public License >= 3
;; Distribution: This file is not part of Emacs

;;; Commentary:

;;==============================================================================
;; My Emacs config
;;==============================================================================

;;; Code:

;;==============================================================================
;; Global
;;==============================================================================

(require 'package)

(add-to-list 'package-archives
             '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(setq backup-inhibited t)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

(setq column-number-mode t)
(show-paren-mode 1)
(setq-default show-paren-delay 0)
(setq-default indent-tabs-mode nil)
(global-hl-line-mode t)

;;==============================================================================
;; Customization
;;==============================================================================

(load-theme 'rebecca t)

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;;==============================================================================
;; Modes
;;==============================================================================

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))))


(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
