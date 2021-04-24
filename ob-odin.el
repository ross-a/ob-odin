;;; ob-odin.el --- org-babel functions for odin evaluation

;; Copyright (C) 2011-2015 Free Software Foundation, Inc.

;; Author: Ross
;; based on:
;; Original Author: Eric Schulte (ob-java.el)
;; Author: thomas "at" friendlyvillagers.com
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

;; This file is NOT YET part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Currently this only supports the external compilation and execution
;; of odin code blocks (i.e., no session support).

;;; Code:
(require 'ob)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("odin" . "odin"))

(defcustom org-babel-odin-command "odin"
  "Name of the odin command.
May be either a command in the path, like odin
or an absolute path name, like /usr/local/bin/odin
parameters may be used, like mono -verbose"
  :group 'org-babel
  :version "24.3"
  :type 'string)

(defcustom org-babel-odin-compiler "odin"
  "Name of the odin compiler.
May be either a command in the path, like odin
or an absolute path name, like /usr/local/bin/odin
parameters may be used, like odin -vet"
  :group 'org-babel
  :version "24.3"
  :type 'string)


(defun org-babel-odin-ensure-main-wrap (body)
  "Wrap BODY in a \"main\" function call if none exists."
  (if (string-match "^[ \t\n\r]*main[ \t]*(.*)" body)
      body
    (format "import \"core:fmt\"\nmain :: proc() {\n%s\n}\n" body)))

(defun org-babel-odin-expand-odin (body params)
  "Expand a block of C or C++ code with org-babel according to
its header arguments."
  (let ((vars (org-babel--get-vars params))
	(colnames (cdr (assq :colname-names params)))
	(main-p (not (string= (cdr (assq :main params)) "no")))
	(includes (org-babel-read
		   (cdr (assq :includes params))
		   nil))
	(defines (org-babel-read
		  (cdr (assq :defines params))
		  nil))
	(namespaces (org-babel-read
		     (cdr (assq :namespaces params))
		     nil)))
    (when (stringp includes)
      (setq includes (split-string includes)))
    (when (stringp namespaces)
      (setq namespaces (split-string namespaces)))
    (when (stringp defines)
      (let ((y nil)
	    (result (list t)))
	(dolist (x (split-string defines))
	  (if (null y)
	      (setq y x)
	    (nconc result (list (concat y " " x)))
	    (setq y nil)))
	(setq defines (cdr result))))
    (mapconcat 'identity
							 (list
		;; package
		(mapconcat
		 (lambda (inc) (format "package %s;" "obodin"))
		 "." "\n")
		;; includes / imports
		(mapconcat
		 (lambda (inc) (format "import \"%s\"" inc))
		 includes "\n")
		;; defines
		(mapconcat
		 (lambda (inc) (format "%s" inc))
		 (if (listp defines) defines (list defines)) "\n")
		;; namespaces
		(mapconcat
		 (lambda (inc) (format "using %s;" inc))
		 namespaces
		 "\n")
		;; variables TODO
		;(mapconcat 'org-babel-C-var-to-C vars "\n")
		;; table sizes TODO
		;(mapconcat 'org-babel-C-table-sizes-to-C vars "\n")
		;; tables headers utility TODO
		;(when colnames
		;  (org-babel-C-utility-header-to-C))
		;; tables headers
		;(mapconcat 'org-babel-C-header-to-C colnames "\n")
		;; body
		(if main-p
		    (org-babel-odin-ensure-main-wrap body)
		  body) "\n") "\n")))

(defun org-babel-expand-body:odin (body params)
  "Expand a block of odin code with org-babel according to its
header arguments."
  (let ((org-babel-odin-variant 'odin)) (org-babel-odin-expand-odin body params)))

(defun org-babel-execute:odin (body params)
  (let* ((full-body (org-babel-expand-body:odin body params))
         (cmpflag (or (cdr (assoc :cmpflag params)) ""))
         (cmdline (or (cdr (assoc :cmdline params)) ""))
         (src-file (org-babel-temp-file "odin-src-" ".odin"))
         (exe-file (concat (file-name-sans-extension src-file)  ".exe"))
         (compile
          (progn (with-temp-file  src-file (insert full-body))
                 (org-babel-eval
                  (concat org-babel-odin-compiler " check " cmpflag " "  src-file) ""))))
    (let ((results (org-babel-eval (concat org-babel-odin-command " run " cmdline " " src-file) "")))
      (org-babel-reassemble-table
       (org-babel-result-cond (cdr (assoc :result-params params))
         (org-babel-read results)
         (let ((tmp-file (org-babel-temp-file "odin-")))
           (with-temp-file tmp-file (insert results))
           (org-babel-import-elisp-from-file tmp-file)))
       (org-babel-pick-name
        (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
       (org-babel-pick-name
        (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params)))))))

(defun org-babel-prep-session:odin (session params)
  "Return an error because odin does not support sessions. TODO double check this"
  (error "Sessions are not (yet) supported for Odin"))


(provide 'ob-odin)
;;; ob-odin.el ends here
