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
(setq-default flycheck-temp-prefix ".flycheck")
;; use local eslint from node_modules before global
(defun eslint-from-node-modules ()
  "Find executable file named eslint.js from the node_modules directory."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'eslint-from-node-modules)

(require 'web-mode)

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-style-padding 0)
(setq web-mode-script-padding 0)
(setq web-mode-block-padding 0)

(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
