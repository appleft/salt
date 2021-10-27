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
