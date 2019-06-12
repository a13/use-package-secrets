;;; use-package-secrets.el --- :secret keyword for use-package. -*- lexical-binding: t -*-

;; Homepage: https://github.com/a13/use-package-secrets
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (use-package "2.1"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'subr-x)
(require 'use-package)

(defvar use-package-secrets--files nil
  "List of loaded secret files.")

(defgroup use-package-secrets nil
  ":secret keyword"
  :group 'use-package)

(defcustom use-package-secrets-default-directory nil
  "Path to secrets storage."
  :group 'use-package-secrets
  :type 'directory)

(defun use-package-secrets-file-exists-p (file &optional default-directory)
  "Check if FILE exists in DEFAULT-DIRECTORY and return its full name."
  (let ((filename (expand-file-name file default-directory)))
    (and (file-exists-p filename)
         filename)))

(defun use-package-secrets-locate-file (file)
  "Try to locate FILE."
  (or (and use-package-secrets-default-directory
           (use-package-secrets-file-exists-p file use-package-secrets-default-directory))
      (use-package-secrets-file-exists-p file)
      (use-package-secrets-file-exists-p file user-emacs-directory)
      (use-package-secrets-file-exists-p file "~/")))

(defun use-package-secrets-load-files (&rest files)
  "Try to load secret FILES."
  (dolist (file files)
    (unless (member file use-package-secrets--files)
      (if-let ((path (use-package-secrets-locate-file file)))
          (progn
            ;; TODO: add support to non-lisp password files
            (load-file path)
            (add-to-list 'use-package-secrets--files file))
        (warn "file %s not found, skipped" file)))))

(defun use-package-normalize/:secret (name keyword args)
  (use-package-as-one (symbol-name keyword) args
    (lambda (label arg)
      (cond
       ((and (listp arg) (stringp (car arg)))
        arg)
       (t (list arg))))))

(defun use-package-handler/:secret (name keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (mapcar #'(lambda (secret-arg)
                 `,(pcase secret-arg
                     (`(,fn . ,files)
                      `(define-advice ,fn
                           (:before (&optional args))
                         (use-package-secrets-load-files ,@files)))
                     (file
                      `(use-package-secrets-load-files ,file))))
             arg)
     body)))

(defun use-package-secrets-find-file ()
  "Find secret file."
  (interactive)
  (find-file (use-package-secrets-locate-file
              (completing-read
               "List of loaded secret files."
               use-package-secrets--files))))

(add-to-list 'use-package-keywords :secret t)

(provide 'use-package-secrets)

;;; use-package-secrets.el ends here
