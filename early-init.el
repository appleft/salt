;;; early-init.el --- I exist before everything!! -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 cervelleless
;;
;; Author: cervelle <http://github.com/cervelleless>
;; Maintainer: cervelle <cervelleless@vivaldi.net>
;; Created: November 17, 2020
;; Modified: November 17, 2020
;; Keywords:
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  I exist before everything!!
;;
;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before the package system and GUI is initialized.
;;
;;; Code:

;; Defer GC further back in the startup process.
(setq gc-cons-threshold 402653184)
(add-hook 'after-emacs-hook '(lambda ()
			       ;; restore after startup
			       (setq gc-cons-threshold 16777216)))

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. We must prevent Emacs from doing
;; it early!
(setq package-enable-at-startup nil)

;; Prevent unwanted runtime builds in gccemacs (native-comp); packages are
;; compiled ahead-of-time when they are installed and site files are compiled
;; when gccemacs is installed.
(setq comp-deferred-compilation nil)

;;; early-init.el ends here
