;;; use-package-secrets.el --- A :secret keyword for use-package -*- lexical-binding: t -*-

;; Homepage: https://github.com/a13/use-package-secrets
;; Version: 0.0.2
;; Package-Requires: ((emacs "25.1") (use-package "2.1") (dash "2.11"))

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

;; A package to advice .el(.gpg) file loadind to commands.

;;; Code:

(require 'subr-x)
(require 'use-package)
(require 'dash)

(defvar use-package-secrets--files nil
  "List of loaded secret files.")

(defgroup use-package-secrets nil
  ":secret keyword"
  :group 'use-package)


(defcustom use-package-secrets-directories nil
  "Paths to secrets storage."
  :group 'use-package-secrets
  :type '(repeat directory))

(defun use-package-secrets-file-exists-p (file &optional directory)
  "Check if FILE exists in DIRECTORY and return its full name."
  (let ((filename (expand-file-name file directory)))
    (and (file-exists-p filename)
         filename)))

(defun use-package-secrets-locate-file (file)
  "Try to locate FILE."
  (-some
   (apply-partially #'use-package-secrets-file-exists-p file)
   (cons nil use-package-secrets-directories)))


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

(defun use-package-normalize/:secret (_name keyword args)
  (use-package-as-one (symbol-name keyword) args
    (lambda (_label arg)
      (cond
       ((and (listp arg) (stringp (car arg)))
        arg)
       (t (list arg))))))

(defun use-package-handler/:secret (name _keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (mapcar #'(lambda (secret-arg)
                 `,(pcase secret-arg
                     (`(,fn . ,files)
                      `(define-advice ,fn
                           (:before (&optional _args))
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
               "Find secret file: "
               use-package-secrets--files))))

(add-to-list 'use-package-keywords :secret t)

(provide 'use-package-secrets)

;;; use-package-secrets.el ends here
