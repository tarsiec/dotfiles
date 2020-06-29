;;; test-ob-crystal.el --- tests for ob-crystal.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'ert)
(require 'org-id)

(defconst ob-crystal-test-dir
  (expand-file-name (file-name-directory (or load-file-name buffer-file-name))))

(defconst org-id-locations-file
  (expand-file-name ".test-org-id-locations" ob-crystal-test-dir))

(defun ob-crystal-test-update-id-locations ()
  (let ((files (directory-files
                ob-crystal-test-dir 'full
                "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*\\.org$")))
    (org-id-update-id-locations files)))

(defmacro org-test-at-id (id &rest body)
  "Run body after placing the point in the headline identified by ID."
  (declare (indent 1))
  `(let* ((id-location (org-id-find ,id))
	  (id-file (car id-location))
	  (visited-p (get-file-buffer id-file))
	  to-be-removed)
     (unwind-protect
	 (save-window-excursion
	   (save-match-data
	     (org-id-goto ,id)
	     (setq to-be-removed (current-buffer))
	     (condition-case nil
		 (progn
		   (org-show-subtree)
		   (org-show-block-all))
	       (error nil))
	     (save-restriction ,@body)))
       (unless (or visited-p (not to-be-removed))
	 (kill-buffer to-be-removed)))))

(def-edebug-spec org-test-at-id (form body))

(unless (featurep 'ob-crystal)
  (signal 'missing-test-dependency "Support for Crystal code blocks"))

(ert-deftest ob-crystal/crystal-executable ()
  (should (executable-find org-babel-crystal-command)))

(ert-deftest ob-crystal/ns-rt-value ()
  "Test no session return-type: value."
  (if (executable-find org-babel-crystal-command)
      (org-test-at-id "977ba858-a4aa-4108-8e61-43dd880d5b08"
                      (org-babel-next-src-block 1)
                      (should
                       (string-equal
                        "ob-crystal" (org-babel-execute-src-block))))))

(ert-deftest ob-crystal/ns-rt-output ()
  "Test no session return-type: output."
  (if (executable-find org-babel-crystal-command)
      (org-test-at-id "977ba858-a4aa-4108-8e61-43dd880d5b08"
                      (org-babel-next-src-block 2)
                      (should
                       (string-equal
                        "ob-crystal\n" (org-babel-execute-src-block))))))

(ert-deftest ob-crystal/ns-variable-int ()
  (if (executable-find org-babel-crystal-command)
      (org-test-at-id "1f5d82ee-93a4-4821-85fb-c855188beb65"
                      (org-babel-next-src-block 1)
                      (should
                       (equal
                        5 (org-babel-execute-src-block))))))

(ert-deftest ob-crystal/ns-variable-str ()
  (if (executable-find org-babel-crystal-command)
      (org-test-at-id "1f5d82ee-93a4-4821-85fb-c855188beb65"
                      (org-babel-next-src-block 2)
                      (should
                       (string-equal "ob-crystal" (org-babel-execute-src-block))))))

(ert-deftest ob-crystal/ns-variable-list ()
  (if (executable-find org-babel-crystal-command)
      (org-test-at-id "1f5d82ee-93a4-4821-85fb-c855188beb65"
                      (org-babel-next-src-block 3)
                      (should
                       (string-equal "(\"a\" \"b\" \"c\")" (org-babel-execute-src-block))))))

(ert-deftest ob-crystal/ns-variable-tb ()
  (if (executable-find org-babel-crystal-command)
      (org-test-at-id "1f5d82ee-93a4-4821-85fb-c855188beb65"
                      (org-babel-next-src-block 4)
                      (should
                       (string-equal "((1 2) (3 4))" (org-babel-execute-src-block))))))

(ert-deftest ob-crystal/ns-multi-variables ()
  (if (executable-find org-babel-crystal-command)
      (org-test-at-id "1f5d82ee-93a4-4821-85fb-c855188beb65"
                      (org-babel-next-src-block 5)
                      (should
                       (equal 12 (org-babel-execute-src-block))))))

(defun ob-crystal-test-runall ()
  (progn
    (ob-crystal-test-update-id-locations)
    (ert t)))

(provide 'ob-crystal-test)
