;; -*- lexical-binding: t; -*-
;;; Bootstrap
;; Speed up startup
;; Following https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly

;; max memory available for gc on startup
(defvar me/gc-cons-threshold 16777216)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold me/gc-cons-threshold
                  gc-cons-percentage 0.1)))

(setq emacs-load-start-time (current-time))

;; max memory available for gc when opening minibuffer
(defun me/defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun me/restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold me/gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'me/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'me/restore-garbage-collection-h)


(defvar me/-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist me/-file-name-handler-alist)))

(setq inhibit-compacting-font-caches t)

;; Set up straight.el
(setq debug-on-error t)
(setq straight-use-package-by-default t
      use-package-always-demand t
      straight-cache-autoloads t)
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

(require 'straight-x)
;; (setq straight-check-for-modifications '(check-on-save))
(setq vc-follow-symlinks t)

(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

(straight-use-package 'use-package)

(use-package benchmark-init
  :demand t
  :hook (after-init . benchmark-init/deactivate))

(use-package org)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

(when (require 'time-date nil t)
  (message "Emacs startup time: %d seconds."
           (time-to-seconds (time-since emacs-load-start-time))))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; byte-compile-warnings: (not free-vars)
;; End:
