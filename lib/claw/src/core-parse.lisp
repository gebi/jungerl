(defpackage :core-parse
  (:use :common-lisp :core-compiler))

(in-package :core-parse)

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
     (namechar (or uppercase lowercase digit "@_<>+-*"))
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
	 (symbol-macrolet (($1 (nth 0 ,args))
			   ($2 (nth 1 ,args))
			   ($3 (nth 2 ,args))
			   ($4 (nth 3 ,args))
			   ($5 (nth 4 ,args))
			   ($6 (nth 5 ,args))
			   ($7 (nth 6 ,args)))
	   ,@body))))

(defmacro expand ((function-name &rest subexps))
  `(action (if *just-parse*
               (list ',function-name ,@subexps)
               (,function-name ,@subexps))))

(lalr:define-grammar core-parser
    ;; terminals
    (:arrow :annotation :atom :string :variable :float :integer
            ;; keywords
            :after :apply :attributes :call :case :catch :do
            :end :fun :in :let :letrec :module :of :primop
            :receive :try :when
            #\{ #\} #\[ #\] #\( #\) #\= #\, #\| #\/ #\< #\> #\/ #\:)
  (start --> module-definition
         #'identity)

  (module-definition --> :module :atom module-export module-attribute module-defs :end
                     (expand (module $2 $3 $4 $5)))

  ;; attrs

  (module-attribute --> :attributes #\[ #\]
                    (expand (attributes '())))

  (module-attribute --> :attributes #\[ attribute-list #\]
                    (expand (attributes $2)))

  (attribute-list --> attribute #'identity)
  (attribute-list --> attribute #\, attribute-list
                  (lambda (attr i0 rest)
                    i0
                    (cons attr rest)))

  ;; exports

  (module-export --> #\[ #\]
                 (expand (exports '())))
  (module-export --> #\[ exported-names #\]
                 (expand (exports $2)))

  (exported-names --> exported-name #\, exported-names
                  (lambda (name i0 names)
                    i0
                    (cons name names)))
  (exported-names --> exported-name #'list)

  (exported-name --> function-name  #'identity)

  (function-name --> :atom #\/ :integer
                 (expand (function-name $1 $3)))

  (module-defs --> function-def             #'list)
  (module-defs --> function-def module-defs #'cons)

  (function-def --> function-name #\= fun
                (expand (fdef $1 $3)))

  (fun --> :fun arglist :arrow expr
       (expand (fun $2 $4)))

  (arglist --> #\( #\)  (lambda (&rest i) i  '()))
  (arglist --> #\( var-list #\)  (lambda (i0 vs i1) i0 i1  vs))

  (expr --> value-list  #'identity)
  (expr --> annotated-single-expr #'identity)

  (value-list --> #\< #\>
              (expand (value-list '())))
  (value-list --> #\< values #\> -->
              (expand (value-list $2)))

  (values --> annotated-single-expr        #'list)
  (values --> annotated-single-expr values #'cons)

  (annotated-single-expr --> #\( single-expr :annotation single-expr #\)
                         (args-in-list #'second))
  (annotated-single-expr --> single-expr #'identity)

  (single-expr --> atomic #'identity)
  (single-expr --> variable #'identity)
  (single-expr --> function-name #'identity)
  (single-expr --> tuple #'identity)
  (single-expr --> list #'identity)
  (single-expr --> let #'identity)
  (single-expr --> case #'identity)
  (single-expr --> fun #'identity)
;  (single-expr --> letrec #'identity)
  (single-expr --> application #'identity)
  (single-expr --> remote-call #'identity)
  (single-expr --> primop-call #'identity)
;  (single-expr --> try #'identity)
;  (single-expr --> receive #'identity)
;  (single-expr --> sequencing #'identity)
;  (single-expr --> catch #'identity)

  (atomic --> :integer
          (expand (integer $1)))
  (atomic --> :atom
          (expand (am $1)))
  (atomic --> :string
          (expand (a-string $1)))
  (atomic --> nil
          #'identity)

  (nil --> #\( #\)     (lambda (&rest i) i  '()))

  (variable --> :variable
            (expand (var $1)))
  (tuple    --> #\{ #\}
            (expand (tuple '())))
  (tuple    --> #\{ expr-list #\}
            (expand (tuple $2)))

  (expr-list --> annotated-single-expr   #'list)
  (expr-list --> annotated-single-expr #\, expr-list   (lambda (e i0 es)
                                                         (cons e es)))

  (list --> #\[ list-exprs #\]  (args-in-list #'second))
  (list --> #\[ #\]             (lambda (&rest i) i  '()))
  (list-exprs --> annotated-single-expr #'list)
  (list-exprs --> expr #\, list-exprs   (lambda (e i0 es)  i0  (cons e es)))
  (list-exprs --> expr #\| expr         (lambda (e1 i0 e2) i0  (cons e1 e2)))

  (let --> :let variables #\= expr :in expr
       (lambda (i0 vars i1 bexpr i2 expr)
         `(LET ,vars ,bexpr ,expr)))

  (variables --> :variable #'identity)
  (variables --> #\< #\>  (lambda (&rest i) i  '()))
  (variables --> #\< var-list #\>  (args-in-list #'second))

  (var-list  --> variable
             (action (list $1)))
  (var-list  --> variable  #\, var-list
             (action (cons $1 $3)))

  (variable  --> :variable
             (action (var $1)))

  (case --> :case expr :of clauses :end
        (lambda (i0 expr i1 clauses i2)
          i0 i1 i2
          `(CASE ,expr ,@clauses)))

  (clauses --> clause          #'list)
  (clauses --> clause clauses  #'cons)

  (clause --> patterns guard :arrow expr
          (lambda (ps g i0 e) i0  (list ps g e)))

  (patterns --> pattern #'identity)
  (patterns --> #\< #\>               (lambda (&rest i) i  '()))
  (patterns --> #\< pattern-list #\>
            (expand (value-list $2)))

  (pattern-list --> pattern #'identity)
  (pattern-list --> pattern #\, pattern-list
                (lambda (p i0 ps) i0  (cons p ps)))

  (pattern-cons-list --> pattern  #'identity)
  (pattern-cons-list --> pattern #\, pattern-list
                     (lambda (p i0 ps) i0  (cons p ps)))
  (pattern-cons-list --> pattern #\| pattern
                     (lambda (p1 i0 p2) i0  (cons p1 p2)))

  (pattern --> atomic  #'identity)
  (pattern --> #\{ pattern-list #\} (lambda (i0 ps i1)
                                      i0 i1
                                      `(TUPLE ,ps)))
  (pattern --> #\[ pattern-cons-list #\] (lambda (i0 ps i1)
                                           i0 i1
                                           `(LIST ,ps)))
  (pattern --> :variable (lambda (v) `(VAR ,v)))

  (guard --> :when annotated-single-expr  (args-in-list #'second))

  (remote-call --> :call expr #\: expr #\( expr-list #\)
               (lambda (i0 m i1 f i2 args i3)
                 i0 i1 i2 i3
                 `(CALL ,m ,f ,@args)))

  (application --> :apply expr #\( expr-list #\)
               (lambda (i0 e i1 args i2)
                 i0 i1 i2
                 `(APPLY ,e ,@args)))

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
  (let ((*package* (make-package module-name))
        (lexer (make-core-lexer input)))
    (flet ((next-input ()
             (let ((x (funcall lexer)))
               (print x)
               (cond ((eq x :eof) (values :eof :eof))
                     ((atom x) (values x x))
                     (t (values (first x) (second x))))))
           (parse-error ()
             (format T "Parse-Error! at pos = ~D"
                     (file-position input))))
      (print (core-parser #'next-input #'parse-error))
      (values))))

