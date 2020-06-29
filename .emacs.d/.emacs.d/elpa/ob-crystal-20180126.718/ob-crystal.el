;;; ob-crystal.el --- org-babel functions for Crystal evaluation

;; Copyright (C) 2017 Brantou

;; Author: Brantou <brantou89@gmail.com>
;; URL: https://github.com/brantou/ob-crystal
;; Keywords: crystal, literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version:  0.0.1
;; Package-Requires: ((emacs "24.3"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Org-Babel support for evaluating crystal-lang code.
;;
;; It was created based on the usage of ob-template.
;;

;;; Requirements:
;;
;; - crystal :: https://crystal-lang.org/
;;

;;; TODO
;;
;; - Provide better error feedback.
;;
;; - Find better way to handle table and list
;;

;;; Code:
(require 'ob)
(require 'ob-eval)
(require 'ob-tangle)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("crystal" . "cr"))

(defvar org-babel-default-header-args:crystal '()
  "Default header arguments for crystal code blocks.")

(defcustom org-babel-crystal-command "crystal"
  "Name of command used to evaluate crystal blocks."
  :group 'org-babel
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-babel-crystal-nil-to 'hline
  "Replace nil in crystal tables with this before returning."
  :group 'org-babel
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'symbol)

(defvar org-babel-crystal-function-wrapper
  "
def main()
%s
end

results = main()
File.write(\"%s\", (results.class == String) ? results : results.inspect)
")

(defun org-babel-execute:crystal (body params)
  "Execute a block of Crystal code with org-babel.
 This function is called by `org-babel-execute-src-block'"
  (message "executing Crystal source code block")
  (let* ((org-babel-crystal-command
          (or (cdr (assq :crystal params))
              org-babel-crystal-command))
         (result-params (cdr (assq :result-params params)))
         (result-type (cdr (assq :result-type params)))
         (full-body (org-babel-expand-body:generic
                     body params (org-babel-variable-assignments:crystal params)))
         (result (org-babel-crystal-evaluate-external-process
                  full-body result-type result-params)))
    (org-babel-reassemble-table
     result
     (org-babel-pick-name (cdr (assq :colname-names params))
                          (cdr (assq :colnames params)))
     (org-babel-pick-name (cdr (assq :rowname-names params))
                          (cdr (assq :rownames params))))))

(defun org-babel-crystal-evaluate-external-process
    (body &optional result-type result-params)
  "Evaluate BODY in external crystal process.
If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value of the
last statement in BODY, as elisp."
  (let ((result
         (let* ((script-file (org-babel-temp-file "crystal-code-" ".cr"))
                (tmp-file (org-babel-temp-file "crystal-")))
           (with-temp-file script-file
             (insert
              (if (string= result-type "value")
                  (format org-babel-crystal-function-wrapper
                          body
                          (org-babel-process-file-name tmp-file 'noquote))
                full-body)))
           (let ((eval-cmd
                  (format "%s run %s"
                          org-babel-crystal-command
                          (org-babel-process-file-name script-file))))
             (pcase result-type
               (`output (org-babel-eval eval-cmd ""))
               (`value (when (org-babel-eval eval-cmd "")
                         (org-babel-eval-read-file tmp-file))))))))
    (org-babel-result-cond result-params
      result
      (org-babel-crystal-table-or-string (org-trim result)))))

(defun org-babel-prep-session:crystal (_session _params)
  "This function does nothing as crystal is a compiled language with no
support for sessions"
  (error "Crystal is a compiled language -- no support for sessions"))

(defun org-babel-load-session:crystal (_session _body _params)
  "This function does nothing as crystal is a compiled language with no
support for sessions"
  (error "Crystal is a compiled language -- no support for sessions"))

;; helper functions

(defun org-babel-variable-assignments:crystal (params)
  "Return list of crystal statements assigning the block's variables."
  (mapcar
   (lambda (pair)
     (format "%s=%s"
             (car pair)
             (org-babel-crystal-var-to-crystal (cdr pair))))
   (org-babel-crystal-get-vars params)))

(defun org-babel-crystal-get-vars (params)
  "org-babel-get-header was removed in org version 8.3.3"
  (if (fboundp 'org-babel-get-header)
      (mapcar #'cdr (org-babel-get-header params :var))
    (org-babel--get-vars params)))

(defun org-babel-crystal-var-to-crystal (var)
  "Convert VAR into a crystal variable.
Convert an elisp value into a string of crystal source code
specifying a variable of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-crystal-var-to-crystal var ", ") "]")
    (if (eq var 'hline)
        org-babel-crystal-hline-to
      (format "%S" var))))

(defun org-babel-crystal-table-or-string (results)
  "Convert RESULTS into an appropriate elisp value.
If RESULTS look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (let ((res (org-babel-script-escape results)))
    (if (listp res)
        (mapcar (lambda (el) (if (not el)
                                 org-babel-crystal-nil-to el))
                res)
      res)))

(provide 'ob-crystal)
;;; ob-crystal.el ends here
