;;; ob-odin.el --- org-babel functions for odin evaluation

;; Copyright (C) 2011-2015 Free Software Foundation, Inc.

;; Author: RL
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

(defcustom org-babel-odin-clang-compiler "clang"
  "Name of the clang compiler. To compile .ll from the -build-mode:llvm output.
May be either a command in the path, like odin
or an absolute path name, like /usr/local/bin/odin
parameters may be used, like odin -vet"
  :group 'org-babel
  :version "24.3"
  :type 'string)

(defun org-babel-odin-ensure-main-wrap (body)
  "Wrap BODY in a \"main\" function call if none exists."
  (if (string-match "^[ \t\n\r]*main[ \t]* ::" body)
      body
    (format "import \"core:fmt\"\nmain :: proc() {\n%s\n}\n" body)))

(defun org-babel-odin-val-to-base-type (val)
  "Determine the base type of VAL which may be
`integerp' if all base values are integers
`floatp' if all base values are either floating points or integers
`stringp' otherwise."
  (cond
   ((integerp val) 'integerp)
   ((floatp val) 'floatp)
   ((or (listp val) (vectorp val))
    (let ((type nil))
      (mapc (lambda (v)
	      (pcase (org-babel-odin-val-to-base-type v)
		(`stringp (setq type 'stringp))
		(`floatp
		 (when (or (not type) (eq type 'integerp))
		   (setq type 'floatp)))
		(`integerp
		 (unless type (setq type 'integerp)))))
	    val)
      type))
   (t 'stringp)))

(defun org-babel-odin-val-to-odin-type (val)
  "Determine the type of VAL.
Return a list (TYPE-NAME FORMAT).  TYPE-NAME should be the name of the type.
FORMAT can be either a format string or a function which is called with VAL."
  (let* ((basetype (org-babel-odin-val-to-base-type val))
	 (type
	  (pcase basetype
	    (`integerp '("int" "%d"))
	    (`floatp '("double" "%f"))
	    (`stringp
	     (list
	      "string"
	      "\"%s\""))
	    (_ (error "unknown type %S" basetype)))))
    (cond
     ((integerp val) type) ;; an integer declared in the #+begin_src line
     ((floatp val) type) ;; a numeric declared in the #+begin_src line
     ((and (listp val) (listp (car val))) ;; a table
      `(,(car type)
	(lambda (val)
	  (cons
	   (format "[%d][%d]" (length val) (length (car val)))
	   (concat
	    "{\n"
	    (mapconcat
	     (lambda (v)
	       (concat
		" {"
		(mapconcat (lambda (w) (format ,(cadr type) w)) v ",")
		"}"))
	     val
	     ",\n")
	    "\n}")))))
     ((or (listp val) (vectorp val)) ;; a list declared in the #+begin_src line
      `(,(car type)
	(lambda (val)
	  (cons
	   (format "[%d]" (length val))
	   (concat
	    "{"
	    (mapconcat (lambda (v) (format ,(cadr type) v)) val ",")
	    "}")))))
     (t ;; treat unknown types as string
      type))))

(defun org-babel-odin-format-val (type val)
  "Handle the FORMAT part of TYPE with the data from VAL."
  (let ((format-data (cadr type)))
    (if (stringp format-data)
	(cons "" (format format-data val))
      (funcall format-data val))))

(defun org-babel-odin-var-to-odin (pair)
  "Convert an elisp val into a string of odin code specifying a var
of the same value."
  ;; TODO list support ???
  (let ((var (car pair))
	(val (cdr pair)))
    (when (symbolp val)
      (setq val (symbol-name val))
      (when (= (length val) 1)
	(setq val (string-to-char val))))
    (let* ((type-data (org-babel-odin-val-to-odin-type val))
	   (type (car type-data))
	   (formatted (org-babel-odin-format-val type-data val))
	   (suffix (car formatted))
	   (data (cdr formatted)))
      (format "%s : %s%s = %s;"
	      var
	      suffix
	      type
	      (replace-regexp-in-string "\n" "" data)))))

(defun org-babel-odin-expand-odin (body params)
  "Expand a block of odin code with org-babel according to
its header arguments."
  (let ((vars (org-babel--get-vars params))
				(colnames (cdr (assq :colname-names params)))
				(main-p (not (string= (cdr (assq :main params)) "no")))
				(includes (org-babel-read
									 (cdr (assq :includes params))
									 nil))
				(defines (org-babel-read
									(cdr (assq :defines params))
									nil)))
    (when (stringp includes)
      (setq includes (split-string includes)))
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
		;; package .. TODO some default package name that the user can change
		(if (string-match "^[ \t\n\r]*package[ \t]*.*" body)
				(let* ((package (format "%s" (match-string 0 body)))
							 (new-body (concat (substring body 0 (match-beginning 0)) (substring body (match-end 0)))))
					(setq body new-body)
					package)
			(format "package %s" "obodin"))
		;; includes / imports
		(mapconcat
		 (lambda (inc) (format "import \"%s\"" inc))
		 includes "\n")
		;; defines
		(mapconcat
		 (lambda (inc) (format "%s" inc))
		 (if (listp defines) defines (list defines)) "\n")
		;; variables
		(mapconcat 'org-babel-odin-var-to-odin vars "\n")
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

(defun org-babel-odin-async-shell-command (command)
	(save-excursion
		(setq odin-shell "*Odin-shell*")
		(if (window-right (get-buffer-window (current-buffer)))
				t
			(progn (split-window-right)))
		(let* ((first-buffer (current-buffer))
					 (output-buffer (get-buffer-create odin-shell))
					 (proc (progn
									 (with-current-buffer output-buffer
										 (message command)
										 (async-shell-command command output-buffer)
										 (setq buffer-face-mode-face '(:family "monospace" :height 80))
										 (buffer-face-mode)
										 )))))))

(defun org-babel-execute:odin (body params)
  (let* ((full-body (org-babel-expand-body:odin body params))
				 (test (or (cdr (assoc :test params)) ""))
				 (norun (or (cdr (assoc :norun params)) test))
         (cmpflag (or (cdr (assoc :cmpflag params)) ""))
         (cmdline (or (cdr (assoc :cmdline params)) ""))
				 (tangle-src (or (cdr (assoc :tangle params)) ""))
         (src-file  (if (string-equal tangle-src "no") (org-babel-temp-file "odin-src-" ".odin") (concat default-directory tangle-src)))
         (src-file2 (concat default-directory (file-name-base src-file) ".odin"))
         (exe-file (concat default-directory (file-name-base src-file) (when (eq system-type "windows-nt") ".exe") ".bin"))
         (ll-file (concat default-directory (file-name-base src-file) ".ll"))
         (compile
          (progn
						(if (not (file-directory-p exe-file)) (delete-file exe-file) ())
						(delete-file ll-file)
						(with-temp-file  src-file (insert full-body))
						(if (string= test "")
								(org-babel-eval (concat org-babel-odin-compiler " build " src-file " " cmpflag " -file") "")
							(org-babel-odin-async-shell-command (concat org-babel-odin-compiler " test " src-file " " cmpflag " -file")))
						"")))
    (let* ((results (if (file-exists-p exe-file)
												(if (string= norun "")
														(org-babel-eval (concat exe-file " " cmdline) "")
													exe-file)
											(progn
												(when (eq system-type "windows-nt")
													(if (file-exists-p ll-file)
															(progn                                                                                 ;; v-- fix me!
																(org-babel-eval (concat org-babel-odin-clang-compiler " -g " ll-file " -o " exe-file " " cmdline) "") (org-babel-eval (concat "mv " src-file " " src-file2) "") exe-file)
														))
												(progn
													(let ((buf (get-buffer-create org-babel-error-buffer-name)))
														(with-current-buffer buf
															(concat "build error :\n" (buffer-string)))))
												)))
					 (build-ok (file-exists-p exe-file)))
			(if build-ok
					(org-babel-reassemble-table
					 (org-babel-result-cond (cdr (assoc :result-params params))
						 (org-babel-read results)
						 (let ((tmp-file (org-babel-temp-file "odin-")))
							 (with-temp-file tmp-file (insert results))
							 (org-babel-import-elisp-from-file tmp-file)))
					 (org-babel-pick-name
						(cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
					 (org-babel-pick-name
						(cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))
				results
				))))

(defun org-babel-prep-session:odin (session params)
  "Return an error because odin does not support sessions. TODO double check this"
  (error "Sessions are not (yet) supported for Odin"))


(provide 'ob-odin)
;;; ob-odin.el ends here
