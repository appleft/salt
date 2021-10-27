;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(let (file-name-handler-alist)
  ;; Ensure Emacs is running out of this file's directory
  (setq user-emacs-directory (file-name-directory load-file-name)))

(defvar salt-dir-private (expand-file-name "private" user-emacs-directory)
  "All files you create under the private folder are yours for personalization.")

;;; .local folder
(defvar salt-dir-local (expand-file-name ".local" user-emacs-directory)
  "Folder of configure files, backup files, template files and so on.")

;;; Cache folder
(defvar salt-dir-cache (expand-file-name "cache" salt-dir-local))

;;; Template folder
(defvar salt-dir-template (expand-file-name "template" salt-dir-local)
  "Folder path of template files.")

;; the load-path of core settings
(add-to-list 'load-path (expand-file-name "elisp/" user-emacs-directory))

;;; Create folders
(dolist (dir (list salt-dir-private
                   salt-dir-local
                   salt-dir-cache
                   salt-dir-template))
  (when (not (file-directory-p dir))
    (make-directory dir)))

;;; Font and frame size
;; frame size
(setq initial-frame-alist
      (quote ((fullscreen . maximized))))

;;; Remove menu-bar, scroll-bar and tool-bar.
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq visible-bell t)

(defvar bootstrap-version)
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

;;; install use-package
(straight-use-package 'use-package)
(eval-and-compile
  (setq use-package-always-ensure nil)   ;; ESSENTIAL for `straight.el'
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
  (setq use-package-compute-statistics nil))

;;; provides `straight-x-clean-unused-repos' (part of `straight.el')
(use-package straight-x
  :defer t)

;;; modus themes
(use-package modus-themes
  :straight t
  :init
  (setq modus-themes-no-mixed-fonts t
        modus-themes-slanted-constructs t
        modus-themes-bold-constructs t
        modus-themes-fringes 'subtle    ; {nil,'subtle,'intense}
        modus-themes-mode-line '3d      ; {nil,'3d,'moody}
        modus-themes-syntax 'yellow-comments-green-strings ; Lots of options---continue reading the manual
        modus-themes-intense-hl-line nil
        modus-themes-paren-match 'subtle-bold ; {nil,'subtle-bold,'intense,'intense-bold}
        modus-themes-links 'neutral-underline ; Lots of options---continue reading the manual
        modus-themes-prompts 'intense         ; {nil,'subtle,'intense}
        modus-themes-completions 'moderate      ; {nil,'moderate,'opinionated}
        modus-themes-region nil ; {nil,'no-extend,'bg-only,'bg-only-no-extend}
        modus-themes-diffs nil  ; {nil,'desaturated,'fg-only,'bg-only}
        modus-themes-org-blocks 'grayscale      ; {nil,'grayscale,'rainbow}
        modus-themes-headings ; Lots of options---continue reading the manual
        '((1 . section)
          (2 . section-no-bold)
          (3 . rainbow-line)
          (t . rainbow-line-no-bold))
        modus-themes-variable-pitch-headings nil
        modus-themes-scale-headings nil
        modus-themes-scale-1 1.1
        modus-themes-scale-2 1.15
        modus-themes-scale-3 1.21
        modus-themes-scale-4 1.27
        modus-themes-scale-5 1.33)
  (set-face-attribute 'default nil :family "M+ 1mn Light" :height 200)
  (set-face-attribute 'variable-pitch nil :family "M+ 1mn Light" :height 200)
  (set-face-attribute 'variable-pitch nil :family "M+ 1mn Light" :height 200)
  :config
  (load-theme 'modus-vivendi t))

;;; display line number
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :hook (text-mode . display-line-numbers-mode))

;;; TODO: deal with word-wrap issue of language-mixed style.
;;; visual-fill-column
(use-package visual-fill-column
  :straight t
  :init
  (setq visual-fill-column-inhibit-sensible-window-split t
        visual-fill-column-width 120)
  :hook
  (text-mode . visual-line-mode)
  (prog-mode . visual-line-mode)
  (visual-line-mode . visual-fill-column-mode)
  :config
  (add-hook 'visual-line-mode-hook #'(lambda ()
                                       (setq-local word-wrap nil)))
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))
