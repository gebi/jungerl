(defpackage :a
  (:nicknames :atom))

(defpackage :core-compiler
  (:use :common-lisp)
  (:export
   :module :exports :attributes
   :value-list :tuple :am :integer :var :a-string
   :function-name :fdef :fun))

(in-package :core-compiler)

;; Code generators

(defun module (name exports attributes defs)
  `(progn
    (defpackage ,name
      (:export ,@(loop for symbol in exports
                       collect (symbol-name symbol))))
    (in-package ,name)
    (defvar ,(intern "*attributes*" (find-package name)) ',attributes)
    ,@defs))

(defun exports (fnames) fnames)
(defun attributes (attrs) attrs)

(defun value-list (values)
  `(VALUES ,@values))

(defun tuple (elems)
  `(TUPLE ,@elems))

(defun am (name)
  (intern name :a))
  `(ATOM ,name))

(defun integer (i)
  i)

(defun var (v)
  (intern v))

(defun a-string (s)
  `(STRING ,s))

(defun function-name (name arity)
  (intern (format nil "~a/~a" name arity)))

(defun fdef (name fun)
  ;; We pull apart the FUN's lambda expression to create a DEFUN
  (destructuring-bind (lambda args expr) fun
    `(defun ,name ,args ,expr)))

(defun fun (args body)
  `(lambda ,args ,body))

