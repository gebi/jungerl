(defpackage :claw
  (:use :common-lisp)
  (:export :erlang-variable :erlang-atom :quoted-erlang-atom
           :atom? :variable?
           :test :file
           :call :match-case :primop)
  (:documentation "Main package for the CLAW compiler."))

(defpackage :var
  (:use)
  (:documentation "Symbols in this package represent Erlang variables."))

;; Package for Erlang atoms
(defpackage :atom
  (:use)
  (:documentation "Symbols in this package represent Erlang atoms."))

(defpackage :|erlang|
  (:nicknames :bif)
  (:shadow :< :> :* :- :+ :and :or)
  (:export :< :> :* :- :+ :and :or
           :display)
  (:use :common-lisp)
  (:documentation "Erlang built-in-functions (BIFs.)"))

(in-package :claw)

(declaim (inline atom? variable?))

;; ----------------------------------------------------------------------
;; Scanning and parsing
;; ----------------------------------------------------------------------

;; Scanner

(clex:deflexer core
    ((sign (or "+" "-"))
     (digit (range "0" "9"))
     (uppercase (or (range "A" "Z")
                    (range #.(code-char #xC0) #.(code-char #xD6))
                    (range #.(code-char #xD8) #.(code-char #xDE))))
     (lowercase (or (range "a" "z")
                    (range #.(code-char #xDF) #.(code-char #xF6))
                    (range #.(code-char #xF8) #.(code-char #xFF))))
     ;; input-char is [^\r\n], not convenient to specify in clex
     (input-char (or (range #\null #.(code-char (1- (char-code #\linefeed))))
                     #.(code-char (1+ (char-code #\linefeed)))
                     (range #.(code-char (1+ (char-code #\newline)))
                            #.(code-char #xFF))))
     (control (range #\null #.(code-char #x1F)))
     (space   #x20)
     (namechar (or uppercase lowercase digit "@_<>+-*=:/^"))
     (varchar  (or uppercase lowercase digit "_"))
     (escape (and "\\" (or octal
                           (and "^" ctrlchar)
                           escapechar)))
     (octaldigit (range "0" "7"))
     (octal (and octaldigit (? octaldigit (? octaldigit))))
     (ctrlchar (range #.(code-char #x40) #.(code-char #x5F)))
     (ecapechar (or "bdefnrstv\"'\\")))
  ;; integer
  ((and (? sign) (+ digit))
   (return (list :integer (parse-integer clex:bag))))
  ;; float
  ((and (? sign) (+ digit) "." (+ digit)
        (? (or "E" "e") (? sign) (+ digit)))
   ;; FIXME: parse
   (return (list :float clex:bag)))
  ;; atom
  ((and "'" (* namechar) "'")
   (return (list :atom (subseq clex:bag 1 (1- (length clex:bag))))))
  ;; string
  ((and #\" (* namechar) #\")
   (return (list :string (subseq clex:bag 1 (1- (length clex:bag))))))
  ;; variable
  ((and (or uppercase (and "_" varchar))
        (* varchar))
   (return (list :variable clex:bag)))
  ((and "->")
   (return :arrow))
  ((and "-|")
   (return :annotation))
  ((or "{}[]()=,|/<>/:")
   (return (char clex:bag 0)))
  ;; keywords
  ((or (and "after")
       (and "apply")
       (and "attributes")
       (and "call")
       (and "case")
       (and "catch")
       (and "do")
       (and "end")
       (and "fun")
       (and "in")
       (and "let")
       (and "letrec")
       (and "module")
       (and "of")
       (and "primop")
       (and "receive")
       (and "try")
       (and "when"))
   (return (intern (string-upcase clex:bag) :keyword)))
  ;; skip whitespace
  ((or #\space #\tab #\newline #\return)))

(defun core-tokens (stream)
  (let ((lexer (make-core-lexer stream)))
    (loop for token = (funcall lexer)
          collect token
          until (eq token :eof))))

(defun scan-file (filename)
  (with-open-file (stream filename :direction :input)
    (core-tokens stream)))

;; Parser

(defvar *just-parse* nil
  "When non-nil, create a regular parse tree instead of a program.")

(defmacro action (&rest body)
  (let ((args (gensym)))
    `#'(lambda (&rest ,args)
	 (declare (ignorable ,args))
	 (symbol-macrolet ((%1 (nth 0 ,args))
			   (%2 (nth 1 ,args))
			   (%3 (nth 2 ,args))
			   (%4 (nth 3 ,args))
			   (%5 (nth 4 ,args))
			   (%6 (nth 5 ,args))
			   (%7 (nth 6 ,args))
                           (%8 (nth 7 ,args))
                           (%9 (nth 8 ,args))
                           (%10 (nth 9 ,args)))
             ,@body))))

(defmacro comp ((exp-type &rest args))
  (let ((compiler-function (intern (format nil "~a~a" 'compile- exp-type))))
    `(action (if (or *just-parse*
                     (not (fboundp ',compiler-function)))
                 (list ',exp-type ,@args)
                 (,compiler-function ,@args)))))

(lalr:define-grammar core-parser
    ;; terminals
    (:arrow :annotation :atom :string :variable :float :integer
            :after :apply :attributes :call :case :catch :do
            :end :fun :in :let :letrec :module :of :primop
            :receive :try :when
            #\{ #\} #\[ #\] #\( #\) #\= #\, #\| #\/ #\< #\> #\/ #\:)
  (start --> module-definition
         #'identity)

  (module-definition --> :module :atom module-export module-attribute module-defs :end
                     (comp (module %2 %3 %4 %5)))

  ;; attrs

  (module-attribute --> :attributes #\[ #\]
                    (comp (module-attributes '())))

  (module-attribute --> :attributes #\[ attribute-list #\]
                    (comp (module-attributes %2)))

  (attribute-list --> attribute
                  (action (list %1)))
  (attribute-list --> attribute #\, attribute-list
                  (action (cons %1 %3)))

  ;; exports

  (module-export --> #\[ #\]
                 (comp (module-export '())))
  (module-export --> #\[ exported-names #\]
                 (comp (module-export %2)))

  (exported-names --> exported-name
                  (action (list %1)))
  (exported-names --> exported-name #\, exported-names
                  (action (cons %1 %3)))

  (exported-name --> function-name
                 (action %1))

  (function-name --> :atom #\/ :integer
                 (comp (function-name %1 %3)))

  (module-defs --> function-def             #'list)
  (module-defs --> function-def module-defs #'cons)

  (function-def --> function-name #\= fun
                (comp (fdef %1 %3)))

  (fun --> :fun arglist :arrow expr
       (comp (fun %2 %4)))

  (arglist --> #\( #\)  (lambda (&rest i) i  '()))
  (arglist --> #\( var-list #\)  (lambda (i0 vs i1) i0 i1  vs))

  (expr --> value-list  #'identity)
  (expr --> annotated-single-expr #'identity)

  (value-list --> #\< #\>
              (comp (value-list '())))
  (value-list --> #\< values #\>
              (comp (value-list %2)))

  (values --> annotated-single-expr
          #'list)
  (values --> annotated-single-expr #\, values
          (action (cons %1 %3)))

  (annotated-single-expr --> #\( single-expr :annotation single-expr #\)
                         (args-in-list #'second))
  (annotated-single-expr --> single-expr #'identity)

;;  (single-expr --> nil #'identity)
  (single-expr --> atomic #'identity)
  (single-expr --> variable #'identity)
  (single-expr --> function-ref #'identity)
  (single-expr --> tuple #'identity)
  (single-expr --> list #'identity)
  (single-expr --> let #'identity)
  (single-expr --> case #'identity)
  (single-expr --> fun #'identity)
  (single-expr --> letrec #'identity)
  (single-expr --> application #'identity)
  (single-expr --> remote-call #'identity)
  (single-expr --> primop-call #'identity)
  (single-expr --> try #'identity)
;  (single-expr --> receive #'identity)
;  (single-expr --> sequencing #'identity)
;  (single-expr --> catch #'identity)
  (single-expr --> do #'identity)

  (function-ref --> :atom #\/ :integer
                (comp (function-ref %1 %3)))

  (letrec --> :letrec local-fdefs :in expr
          (comp (letrec %2 %4)))

  (local-fdefs --> local-fdef
               (action (list %1)))
  (local-fdefs --> local-fdef local-fdefs
               (action (cons %1 %2)))

  (local-fdef --> function-name #\= fun
              (comp (local-fdef %1 %3)))

  (try --> :try expr :catch #\( variable #\, variable #\) :arrow expr
       (comp (try %2 %5 %7 %10)))

  (do --> :do annotated-single-expr annotated-single-expr
      (comp (do %2 %3)))

  (atomic --> :integer
          (comp (integer %1)))
  (atomic --> :atom
          (comp (atom %1)))
  (atomic --> :string
          (comp (string %1)))
;;  (atomic --> nil
;;          #'identity)

;;  (nil --> #\[ #\]     (lambda (&rest i) i  '()))

  (tuple    --> #\{ #\}
            (comp (tuple '())))
  (tuple    --> #\{ expr-list #\}
            (comp (tuple %2)))

  (expr-list --> annotated-single-expr   #'list)
  (expr-list --> annotated-single-expr #\, expr-list   (lambda (e i0 es)
                                                         (cons e es)))

  (list --> #\[ list-exprs #\]
        (comp (list %2)))
  (list --> #\[ #\]
        (action '()))
  (list-exprs --> annotated-single-expr #'list)
  (list-exprs --> expr #\, list-exprs   (lambda (e i0 es)  i0  (cons e es)))
  (list-exprs --> expr #\| expr         (lambda (e1 i0 e2) i0  (cons e1 e2)))

  (let --> :let variables #\= expr :in expr
       (comp (let %2 %4 %6)))

  (variables --> variable
             (action (list %1)))
  (variables --> #\< #\>
             (action '()))
  (variables --> #\< var-list #\>
             (action %2))

  (var-list  --> variable
             (action (list %1)))
  (var-list  --> variable  #\, var-list
             (action (cons %1 %3)))

  (variable  --> :variable
             (comp (variable %1)))

  (case --> :case expr :of clauses :end
        (comp (case-expr %2 %4)))

  (clauses --> clause          #'list)
  (clauses --> clause clauses  #'cons)

  (clause --> patterns guard :arrow expr
          (action (list %1 %2 %4)))

  (patterns --> pattern
            (action (list %1)))
  (patterns --> #\< #\>
            (action '()))
  (patterns --> #\< pattern-list #\>
            (action %2))

  (pattern-list --> pattern #'list)
  (pattern-list --> pattern #\, pattern-list
                (lambda (p i0 ps) i0  (cons p ps)))

  (pattern --> atomic
           (action %1))
  (pattern --> #\{ pattern-list #\}
           (comp (tuple %2)))
  (pattern --> #\[ pattern-cons-list #\]
           (action %2))
  (pattern --> #\[ #\]
           (action '()))
  (pattern --> variable
           (action %1))
  (pattern --> variable #\= pattern
           (action `(:alias ,%1 ,%3)))

  (pattern-cons-list --> pattern  #'list)
  (pattern-cons-list --> pattern #\, pattern-cons-list
                     (lambda (p i0 ps) i0  (cons p ps)))
  (pattern-cons-list --> pattern #\| pattern
                     (lambda (p1 i0 p2) i0  (cons p1 p2)))

  (guard --> :when annotated-single-expr  (args-in-list #'second))

  (remote-call --> :call expr #\: expr #\( expr-list #\)
               (comp (call %2 %4 %6)))

  (application --> :apply expr #\( expr-list #\)
               (comp (application %2 %4)))

  (primop-call --> :primop :atom #\( expr-list #\)
               (lambda (i0 name i1 args i2)
                 i0 i1 i2
                 `(PRIMOP ,name ,@args)))

)

(defun args-in-list (f)
  (lambda (&rest args) (funcall f args)))

;; Driver

(defun core-compile (module-name input)
  (ignore-errors (delete-package module-name))
  (let* ((*package* (make-package module-name))
         (*module-package* *package*)
         (lexer (make-core-lexer input)))
    (use-package '(:claw))
    (flet ((next-input ()
             (let ((x (funcall lexer)))
;;               (print x)
               (cond ((eq x :eof) (values :eof :eof))
                     ((atom x) (values x x))
                     (t (values (first x) (second x))))))
           (parse-error ()
             (format T "Parse-Error! at pos = ~D"
                     (file-position input))))
      (core-parser #'next-input #'parse-error))))

(defun test ()
  (with-open-file (s "fact.core" :direction :input)
    (claw::core-compile "fact" s)))

(defun file (name &rest compile-keywords)
  (let ((infile  (concatenate 'string name ".core"))
        (outfile (concatenate 'string name ".cl"))
        module)
    (with-open-file (s infile :direction :input)
      (setq module (core-compile name s)))
    (format t "Converted to Lisp..~%")
    (with-open-file (out outfile :direction :output)
      (let ((*print-case* :downcase)
            (*package* (find-package name)))
        (mapc (lambda (form)
                (format out "~s~%" form)
                (terpri out))
              (cdr module))))
    (format t "Wrote ~s, compiling..~%" outfile)
    (let ((*readtable* *claw-readtable*))
      (apply #'compile-file outfile compile-keywords))))

;; ----------------------------------------------------------------------
;; Compiler
;; ----------------------------------------------------------------------

(defvar *module-package* nil
  "Bound to the package of the module being compiled.")

;; Code generators

(defun compile-module (name exports attributes defs)
  `(progn
    (defpackage ,name
      (:use :claw :cl)
      (:export ,@(mapcar #'symbol-name exports)))
    (in-package ,name)
    (defvar ,(intern "*attributes*" (find-package name)) ',attributes)
    ,@defs
    ,name))

(defun compile-module-export (fnames) fnames)
(defun compile-module-attributes (attrs) attrs)

(defun compile-value-list (values)
  `(VALUES ,@values))

(defun compile-tuple (elems)
  (apply #'vector elems))

(defun compile-let (vars vexp exp)
  (if (= (length vars) 1)
      `(let ((,(car vars) ,vexp)) ,exp)
      `(multiple-value-bind ,vars ,vexp ,exp)))

(defun compile-atom (name)
  `(quote ,(make-atom name)))

(defun compile-integer (i)
  i)

(defun compile-variable (v)
  (make-variable v))

(defun compile-function-name (name arity)
  (intern (format nil "~a/~a" name arity)))

(defun compile-function-ref (name arity)
  `(function ,(compile-function-name name arity)))

(defun compile-fdef (name fun)
  ;; We pull apart the FUN's lambda expression to create a DEFUN
  (destructuring-bind (lambda args expr) fun
    (declare (ignore lambda))
    `(defun ,name ,args ,expr)))

(defun compile-fun (args body)
  `(lambda ,args ,body))

(defun compile-case-expr (e clauses)
  `(match-case ,e ,@clauses))

(defun compile-application (fexp args)
  (if (local-function-ref? fexp)
      `(,(cadr fexp) ,@args)
      `(funcall ,fexp ,@args)))

(defun compile-call (module function args)
  `(call ,module ,function ,@args))

(defun remote-fname (module function)
  (let ((package (find-package (symbol-name module))))
    (and package
         (find-symbol (symbol-name function) package))))

(defun compile-do (e1 e2)
  (if (eq (car e2) 'progn)
      `(progn ,e1 ,@(cdr e2))
      `(progn ,e1 ,e2)))

(defun compile-letrec (named-funs e)
  `(labels ,named-funs ,e))

(defun compile-local-fdef (name fun)
  (destructuring-bind (lambda args &rest body) fun
    (declare (ignore lambda))
    `(,name ,args ,@body)))

(defun local-function-ref? (x)
  (and (consp x)
       (eq (car x) 'function)
       (symbolp (cadr x))
       (eq (symbol-package (cadr x)) *module-package*)))

(defun compile-list (x)
  (if (consp x)
      `(cons ,(car x) ,(compile-list (cdr x)))
      x))

(defun compile-try (exp1 v1 v2 exp2)
  ;; Not properly implemented, since there is no "throw" yet. FIXME.
  exp1)

;; ----------------------------------------------------------------------
;; Core^2 language
;; ----------------------------------------------------------------------

(defmacro match-case (e &body clauses)
  "Match the result(s) of an expression against a series of clauses.

  (MATCH-CASE E CLAUSE1 ... CLAUSEn)
  CLAUSE = ((<PATTERN0 ... PATTERNn>) GUARD BODY-EXPR)
  PATTERN = (PATTERN) | #(PATTERN) | atom"
  `(let ((it (multiple-value-list ,e)))
    (catch 'result
      ,@(loop for c in clauses
              collect (destructuring-bind (pat guard body) c
                        `(match-clause ,pat ,guard (throw 'result ,body))))
      (error "case-clause"))))

(defmacro match-clause (pattern guard body)
  "Compile a pattern into a Common Lisp predicate expression."
  (let ((*variables* '()))
    (declare (special *variables*))
    (labels ((pat (e)
               (cond ((variable? e)
                      (push e *variables*)
                      `(prog1 t (setq ,e it)))
                     ((numberp e)
                      `(equalp it ,e))
                     ((symbolp e)
                      `(equalp it ',e))
                     ((and (consp e)
                           (eq (car e) :alias))
                      (destructuring-bind (alias v p) e
                        (declare (ignore alias))
                        (push v *variables*)
                        `(when ,(pat p)
                          (setq ,v it)
                          t)))
                     ((consp e)
                      `(and (consp it)
                            (let ((it (car it)))  ,(pat (car e)))
                            (let ((it (cdr it)))  ,(pat (cdr e)))))
                     ((vectorp e)
                      `(and (vectorp it)
                            (= (length it) ,(length e))
                            ,@(loop for exp across e
                                    for i from 0
                                    collect `(let ((it (elt it ,i)))
                                              ,(pat exp))))))))
      (let* ((test (pat pattern)))
        `(let ,*variables*
          (when (and ,test (eq ,guard 'atom::|true|))
            ,body))))))

(defmacro call (mod fun &rest args)
  `(funcall (remote-fname ,mod ,fun) ,@args))

;; (match-clause '(+ 1 2) (var::foo 3 #(1 2 3)) (< x 1) yes no)

;; ----------------------------------------------------------------------
;; Runtime system
;; ----------------------------------------------------------------------

(defun make-variable (name)
  (intern name :var))

(defun variable? (x)
  (and (symbolp x)
       (equal (symbol-package x) (find-package :var))))

(defun make-atom (name)
  (intern name :atom))

(defun atom? (x)
  (and (symbolp x)
       (equal (symbol-package x) (find-package :atom))))

(defun atom-literal? (x)
  (typep x 'quoted-erlang-atom))

(defun atom-literal-symbol (x)
  (cadr x))

(defun primop (name arg)
  (error "primop: ~s ~s" name arg))

;; ----------------------------------------------------------------------
;; Syntactic sugar
;;
;; Atom syntax:     @foo <=> 'atom::foo
;; Variable syntax: $foo <=> var::foo
;; ----------------------------------------------------------------------

(defvar *claw-readtable* nil)
(defvar *claw-symbol-readtable* nil
  "Readtable for atoms and variables - case sensitive.")

(setq *claw-symbol-readtable* (copy-readtable))
(setf (readtable-case *claw-symbol-readtable*) :preserve)

(defun read-atom (stream char)
  (declare (ignore char))
  `(quote ,(read-symbol-with-package :atom stream t nil t)))

(defun read-variable (stream char)
  (declare (ignore char))
  (read-symbol-with-package :var stream t nil t))

(defun read-symbol-with-package (package-name &rest read-args)
  (let ((*package* (find-package package-name))
        (*readtable* *claw-symbol-readtable*))
    (apply #'read read-args)))

(defun print-quoted-atom (stream x)
  (print-atom stream (cadr x)))

(defun print-atom (stream atom)
  (let ((*package* (find-package :atom))
        (*print-pretty* nil))
    (princ "@" stream)
    (let ((*readtable* *claw-symbol-readtable*))
      (prin1 atom stream))))

(defun print-variable (stream var)
  (let ((*package* (find-package :var))
        (*print-pretty* nil))
    (princ "$" stream)
    (let ((*readtable* *claw-symbol-readtable*))
      (prin1 var stream))))

(deftype erlang-atom     () '(satisfies atom?))
(deftype erlang-variable () '(satisfies variable?))
(deftype quoted-erlang-atom ()
  '(cons (eql quote) (cons erlang-atom null)))

(defun customize-syntax ()
  (setq *claw-readtable* (copy-readtable nil))
  (set-macro-character #\@ #'read-atom     t *claw-readtable*)
  (set-macro-character #\$ #'read-variable t *claw-readtable*)
  (setq *readtable* *claw-readtable*)
  (set-pprint-dispatch 'erlang-variable    #'print-variable)
  ;; too confusing?
;  (set-pprint-dispatch 'erlang-atom        #'print-atom)
  (set-pprint-dispatch 'quoted-erlang-atom #'print-quoted-atom))

(customize-syntax)

;; ----------------------------------------------------------------------
;; BIFs
;; ----------------------------------------------------------------------

(in-package :|erlang|)

(defvar *true*  'atom::|true|)
(defvar *false* 'atom::|false|)

;; wrappers to enforce number of arguments
(defun < (x y) (true/false (cl:< x y)))
(defun > (x y) (true/false (cl:> x y)))
(defun + (x y) (cl:+ x y))
(defun - (x y) (cl:- x y))
(defun * (x y) (cl:* x y))
(defun display (x)
  (print x))
(defun and (x y)
  (unless (member x (list *true* *false*)) (error "badarg ~s" x))
  (unless (member y (list *true* *false*)) (error "badarg ~s" x))
  (true/false (cl:and (eq x *true*) (eq y *true*))))

;; support

(defun true/false (x) (if x *true* *false*))
(defun true?  (x) (eq x 'atom::|true|))
(defun false? (x) (eq x 'atom::|true|))

;; for ilisp
(in-package :claw)

