;; Compiler for Core Erlang.
;;
;; The basic strategy is macro-driven recursive descent translation
;; into Lisp. Hopefully it can stay this way, though once processes
;; are added we will probably have to do CPS-conversion, and then
;; probably do last-call optimisation by hand. I dunno, I haven't done
;; this stuff before :-)
;;
;; Anyway, the translation is as direct as possible. Erlang modules
;; are mapped onto Lisp packages, variables onto variables, function
;; definitions onto DEFUNs. Core Erlang's multiple-values features are
;; also conveniently handled by Lisp's own multiple values support,
;; which appear to be a pefect match.

(defpackage :lang
  (:nicknames :e)
  (:shadow :let :values :case :apply)
  (:use :common-lisp)
  (:export :module :exports :attributes
           :let :values :fdef :case :apply :call
           :true :false))

(in-package :lang)

(defmacro module (name &body body)
  "Compile a module declaration."
  (declare (type symbol name))
  `(progn
    (in-package ,(symbol-name name))
    ,@body))

(defmacro exports (symbols)
  "Compile an export declaration."
  (declare (type list symbols))
  `(export ',symbols))

(defmacro attributes (attrs)
  "Compile an attributes declaration."
  (declare (ignore attrs))
  nil)

(defmacro fdef (name args &body body)
  "Compile a function definition."
  (declare (type symbol name)
           (type list args))
  `(defun ,name ,args ,@body))

(defmacro let (vars e &body body)
  "Compile a LET binding.
The syntax is:

  (LET (V1 .. Vn) E
     BODY..)

Where E is some expression that returns <n> values."
  (declare (type list vars))
  `(multiple-value-bind ,vars ,e
    ,@body))

(defmacro values (&rest exprs)
  "Compile a multiple-values expression."
  `(cl:values ,@exprs))

(defmacro call (fn args)
  "Compile a function call."
  (declare (type symbol fn)
           (type list args))
  `(,fn ,@args))

(defmacro apply (fn args)
  "Compile a function application."
  `(cl:apply (function ,fn) (list ,@args)))

(defmacro case (e &rest clauses)
  "Compile a multiple-value case expression.
The CASE syntax is:

  (CASE E
     ((PAT0 .. PATn) GUARD BODY..)
      ..)

Only two types of pattern are currently supported: integers, which are
matched by value, and symbols, which are bound as variables."
  (labels ((var (i)
             ;; Name of the variable bound to the value of the ith EXPR
             ;; (VAR 42) => E::V42
             (intern (format nil "~A~S" "V" i) :lang))
           (temporaries (n)
             ;; List of temporaries to hold the values of the expression.
             ;; (TEMPORARIES 3) => (E::V0 E::V1 E::V2)
             (loop for i from 0 below n
                   collect (var i)))
           (clause (c)
             ;; Generate a case clause that will either just return
             ;; (if it doesn't match) or THROW a result (if it does)
             ;;
             ;;    (CLAUSE ((X 0) GUARD . BODY))
             ;; 1:   => (CL:LET ((X E::V1))
             ;; 2:        (WHEN (AND (EQUAL E::V0 0)
             ;; 3:                   (EQUAL GUARD 'E:TRUE))
             ;; 4:         (THROW 'E::RESULT (PROGN . BODY))))
             ;;
             ;; Line 1 binds the first pattern's variable to the
             ;; corresponding value.
             ;;
             ;; Lines 2-3 make sure the value in the pattern in the
             ;; second pattern matches its value and that the guard
             ;; succeeds.
             ;;
             ;; If the clause matches, line 4 throws the result.
             (destructuring-bind (pats guard &rest body) c
               `(cl:let ,(pattern-bindings pats)
                 (when (and ,@(pattern-tests pats)
                            (equalp ,guard 'true))
                   (throw 'result (progn ,@body))))))
           (pattern-bindings (pats)
             ;; LET list to bind each pattern variable to a Vi value
             ;; (PATTERN-BINDINGS (0 X)) => ((X E::V1))
             (loop for x in pats
                   for i from 0
                   when (symbolp x)
                   collect (list x (var i))))
           (pattern-tests (pats)
             ;; List of predicate expressions to test matches in patterns
             ;; (PATTERN-TESTS (0 X)) => ((EQUAL E::V0 0))
             (loop for x in pats
                   for i from 0
                   when (integerp x)
                   collect `(equalp ,x ,(var i)))))
    ;; Bind the expressions' values to some variables and then try the
    ;; clauses one-by-one until one of them THROW's the result.
    (cl:let ((arity (length (caar clauses))))
      `(multiple-value-bind ,(temporaries arity) ,e
        (catch 'result
          (progn ,@(mapcar #'clause clauses)
                 ;; This will never be run for correct inputs: case
                 ;; clauses must be exhaustive in Core Erlang
                 (error "case-clause")))))))

