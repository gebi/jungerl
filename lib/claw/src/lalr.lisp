;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LALR; -*-

(provide 'lalr)

(defpackage "LALR"
  (:export "DEFINE-GRAMMAR"))

(in-package "LALR")

(declaim (optimize (speed 3)))
;;;  lalr.lisp
;;;
;;;  This is an LALR parser generator.
;;;  (c) 1988 Mark Johnson. mj@cs.brown.edu
;;;  This is *not* the property of Xerox Corporation!

;;;  Modified to cache the first terminals, the epsilon derivations
;;;  the rules that expand a category, and the items that expand
;;;  a category

;;;  There is a sample grammar at the end of this file.
;;;  Use your text-editor to search for "Test grammar" to find it.

;;; (in-package 'LALR)
;;; (export '(make-parser lalr-parser *lalr-debug* grammar lexforms $ parse))

;;; (shadow '(first rest))
;;; (defmacro first (x) `(car ,x))
;;; (defmacro rest (x) `(cdr ,x))

;;;  The external interface is MAKE-PARSER.  It takes three arguments, a
;;;  CFG grammar, a list of the lexical or terminal categories, and an
;;;  atomic end marker.  It produces a list which is the Lisp code for
;;;  an LALR(1) parser for that grammar.  If that list is compiled, then
;;;  the function LALR-PARSER is defined.  LALR-PARSER is a function with 
;;;  two arguments, NEXT-INPUT and PARSE-ERROR. 
;;;
;;;  The first argument to LALR-PARSER, NEXT-INPUT must be a function with 
;;;  zero arguments; every time NEXT-INPUT is called it should return
;;;  two values, the first is the category of the next lexical
;;;  form in the input and the second is the value of that form.
;;;  Each call to NEXT-INPUT should advance one lexical item in the
;;;  input.  When the input is consumed, NEXT-INPUT should return a
;;;  CONS whose CAR is the atomic end marker used in the call to MAKE-PARSER.
;;;
;;;  The second argument to LALR-PARSER, PARSE-ERROR will be called
;;;  if the parse fails because the input is ill-formed.
;;;
;;;
;;;  There is a sample at the end of this file.

;;; definitions of constants and global variables used

(defconstant *TOPCAT* '$Start)
(defvar      *ENDMARKER*)
(defvar      glex)
(defvar      grules)
(defvar      gstart)
(defvar      gstarts)
(defvar      gcats)
(defvar      gfirsts)
(defvar      gepsilons)
(defvar      gexpansions)
(defvar      *lalr-debug* NIL "Inserts debugging code into parser if non-NIL")
(defvar      stateList '())

(defvar      *first-terminals-cache* nil)

(defmacro fixnum= (x y) `(eq ,x ,y))

(defun make-parser (grammar lex endMarker &key (name 'lalr-parser))
  "Takes a grammar and produces the Lisp code for a parser for that grammar"
  (setq *ENDMARKER* endMarker)

  ;;;  cache some data that will be useful later
  (setq glex lex)
  (setq gstart (caar grammar))
  (setq grules (let ((i 0)) 
                 (mapcar #'(lambda (r) (transform-rule r (incf i)))
                         grammar)))
  (setq gcats (get-all-cats))

  (progn
    (setq gexpansions (make-hash-table :test #'eq))
    (setq *first-terminals-cache* (make-hash-table :test #'equal))
    (dolist (cat gcats)
      (setf (gethash cat gexpansions) (compute-expansion cat))))

  (setq gepsilons (remove-if-not #'derives-eps gcats))

  (progn
    (setq gstarts (make-hash-table :test #'eq))
    (setf (gethash *ENDMARKER* gstarts) (list *ENDMARKER*))
    (dolist (cat gcats)
      (setf (gethash cat gstarts) (first-terms (list cat)))) )

  ;;; now actually build the parser
  (build-table)
  (when (and (listp *lalr-debug*) (member 'print-table *lalr-debug*))
    (Print-Table stateList))
  (format T "~%; Table ready (total of ~R rules --> ~R states)."
	  (length grammar)
	  (length stateList))
  (format T "~%; Dumping:")
  (build-parser name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                    Rules and Grammars
;;;

(defstruct rule no mother daughters action)

(defun transform-rule (rule no)
  (make-rule :no no
             :mother (first rule)
             :daughters (butlast (cddr rule))
             :action (car (last rule))))

(defun compute-expansion (cat)
  (remove-if-not #'(lambda (rule)
                     (eq (rule-mother rule) cat))
                 grules))

(defmacro expand (cat)
  `(gethash ,cat gexpansions) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                    Properties of grammars

(defun get-all-cats ()
  (labels ((try (dejaVu cat)
                (if (find cat dejaVu)
                  dejaVu
                  (tryRules (cons cat dejaVu) (compute-expansion cat))))
           (tryRules (dejaVu rules)
                     (if rules
                       (tryRules (tryCats dejaVu (rule-daughters (car rules)))
                                 (cdr rules))
                       dejaVu))
           (tryCats (dejaVu cats)
                    (if cats
                      (tryCats (try dejaVu (car cats)) (cdr cats))
                      dejaVu)))
    (try '() gstart)))

(defun derives-eps (c)
  "t if c can be rewritten as the null string"
  (labels ((try (dejaVu cat)
             (unless (find cat dejaVu)
               (some #'(lambda (r) 
                         (every #'(lambda (c1) (try (cons cat dejaVu) c1))
                                (rule-daughters r)))
                     (expand cat)))))
    (try '() c)))

(defmacro derives-epsilon (c)
  "looks up the cache to see if c derives the null string"
  `(member ,c gepsilons))

(defun first-terms (catList)
  "the leading terminals of an expansion of catList"
  (labels ((firstDs (cats)
                    (if cats
                      (if (derives-epsilon (car cats))
                        (cons (car cats) (firstDs (cdr cats)))
                        (list (car cats)))))
           (try (dejaVu cat)
                (if (member cat dejaVu)
                  dejaVu
                  (tryList (cons cat dejaVu) 
                           (mapcan #'(lambda (r) 
                                       (firstDs (rule-daughters r)))
                                   (expand cat)))))
           (tryList (dejaVu cats)
                    (if cats
                      (tryList (try dejaVu (car cats)) (cdr cats))
                      dejaVu)))
    (remove-if-not #'(lambda (term)
                       (or (eq *ENDMARKER* term)
                           (find term glex))) 
                   (tryList '() (firstDs catList)))))

(defun first-terminals (cat-list)
  (if cat-list
    (if (derives-epsilon (first cat-list))
      (union (gethash (first cat-list) gstarts)
             (first-terminals (rest cat-list)))
      (gethash (first cat-list) gstarts))
    '()))

#+IGNORE
(defun first-terminals* (cat-list-0 cat-1)
  (let ((key (cons cat-list-0 cat-1)))
    (multiple-value-bind (v found?) (gethash key *first-terminals-cache*)
      (cond (found? v)
	    (t (setf (gethash key *first-terminals-cache*)
		     (block foo
		       (let ((res nil))
			 (dolist (c0 cat-list-0)
			   (setf res (union res (gethash c0 gstarts)))
			   (unless (derives-epsilon c0)
			     (return-from foo res)))
			 (union res (gethash cat-1 gstarts)) )))) ))))

(defmacro first-terminals* (cat-list-0 cat-1)
  `(let ((cat-list-0 ,cat-list-0)
	 (cat-1 ,cat-1))
     (let ((key (cons cat-list-0 cat-1)))
       (multiple-value-bind (v found?) (gethash key *first-terminals-cache*)
	 (cond (found? v)
	       (t (setf (gethash key *first-terminals-cache*)
			(block foo
			  (let ((res nil))
			    (dolist (c0 cat-list-0)
			      (setf res (union res (gethash c0 gstarts)))
			      (unless (derives-epsilon c0)
				(return-from foo res)))
			    (union res (gethash cat-1 gstarts)) )))) )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                  LALR(1) parsing table constructor
;;;

(defstruct item rule pos la)

(defmacro item-daughters (i) `(rule-daughters (item-rule ,i)))

(defmacro item-right (i) `(nthcdr (item-pos ,i) (item-daughters ,i)))

(defmacro item-equal (i1 i2)
  `(and (eq (item-rule ,i1) (item-rule ,i2))
        (fixnum= (item-pos ,i1) (item-pos ,i2))
        (eq (item-la ,i1) (item-la ,i2))))

(defmacro item-core-equal (c1 c2)
  "T if the cores of c1 and c2 are equal"
  `(and (eq (item-rule ,c1) (item-rule ,c2))
        (fixnum= (item-pos ,c1) (item-pos ,c2))))

(defun close-items (items)    
  "computes the closure of a set of items"
  (declare (optimize (speed 3)))
  (do ((to-do items))
      ((null to-do) items)
    (let ((i (pop to-do)))
      (let ((rgt (item-right i)))
	(when rgt
	  (dolist (la (first-terminals* (rest rgt) (item-la i) ))
	    (dolist (r (expand (first rgt)))
              (unless (dolist (i items)
                        (if (and (eq r (item-rule i))
                                 (eq (item-la i) la)
                                 (fixnum= (item-pos i) 0))
                          (return t)))
                (let ((new (make-item :rule r :pos 0 :la la)))
                  (push new items)
                  (push new to-do))))))))))

(defun shift-items (items cat)
  "shifts a set of items over cat"
  (labels ((shift-item (item)
                       (if (eq (first (item-right item)) cat)
                         (make-item :rule (item-rule item)
                                    :pos (1+ (item-pos item))
                                    :la (item-la item)))))
    (let ((new-items '()))
      (dolist (i items)
        (let ((n (shift-item i)))
          (if n
            (push n new-items))))
      new-items)))

(defun items-right (items)
  "returns the set of categories appearing to the right of the dot"
  (let ((right '()))
    (dolist (i items)
      (let ((d (first (item-right i))))
        (if (and d (not (find d right)))
          (push d right))))
    right))

(defun compact-items (items)
  "collapses items with the same core to compact items" 
  (let ((soFar '()))
    (dolist (i items)
      (let ((ci (dolist (s soFar)
                  (if (item-core-equal s i)
                    (return s)))))
        (if ci
          (push (item-la i) (item-la ci))
          (push (make-item :rule (item-rule i)
                           :pos (item-pos i)
                           :la (list (item-la i)))
                soFar))))
    (sort soFar #'< 
          :key #'(lambda (i) (rule-no (item-rule i))))))

(defmacro expand-citems (citems)
  "expands a list of compact items into items"
  `(let ((items '()))
     (dolist (ci ,citems)
       (dolist (la (item-la ci))
         (push (make-item :rule (item-rule ci)
                          :pos (item-pos ci)
                          :la la)
               items)))
     items))

(defun subsumes-citems (ci1s ci2s)
  "T if the sorted set of items ci2s subsumes the sorted set ci1s"
  (and (fixnum= (length ci1s) (length ci2s))
       (every #'(lambda (ci1 ci2)
                  (and (item-core-equal ci1 ci2)
                       (subsetp (item-la ci1) (item-la ci2))))
              ci1s ci2s)))

(defun merge-citems (ci1s ci2s)
  "Adds the las of ci1s to ci2s.  ci2s should subsume ci1s"
  (mapcar #'(lambda (ci1 ci2)
              (setf (item-la ci2) (nunion (item-la ci1) (item-la ci2))))
          ci1s ci2s)
  ci2s)

;;;  The actual table construction functions

(defstruct state name citems shifts conflict)
(defstruct shift cat where)

(defparameter nextStateNo -1)

;(defun lookup (citems)
;  "finds a state with the same core items as citems if it exits"
;  (find-if #'(lambda (state)
;               (and (= (length citems) (length (state-citems state)))
;                    (every #'(lambda (ci1 ci2)
;                               (item-core-equal ci1 ci2))
;                            citems (state-citems state))
;                    ))
;           stateList))

(defun lookup (citems)
  "finds a state with the same core items as citems if it exits"
  (dolist (state stateList)
    (if (and (fixnum= (length citems) (length (state-citems state)))
             (do ((ci1s citems (cdr ci1s))
                  (ci2s (state-citems state) (cdr ci2s)))
                 ((null ci1s) t)
               (unless (item-core-equal (car ci1s) (car ci2s))
                 (return nil))))
      (return state))))

(defun add-state (citems)
  "creates a new state and adds it to the state list"
  (let ((newState 
         (make-state :name (intern (format nil "STATE-~D" (incf nextStateNo)))
                     :citems citems)))
    (push newState stateList)
    newState))

(defun get-state-name (items)
  "returns the state name for this set of items"
  (let* ((citems (compact-items items))
         (state (lookup citems)))
    (cond ((null state)
           (setq state (add-state citems))
           (build-state state items))
          ((subsumes-citems citems (state-citems state))
           nil)
          (t
           (merge-citems citems (state-citems state))
           (follow-state items)))
    (state-name state)))

      
(defun build-state (state items)
  "creates the states that this state can goto"
  (let ((closure (close-items items)))
    (dolist (cat (items-right closure))
      (push (make-shift :cat cat
                        :where (get-state-name (shift-items closure cat)))
            (state-shifts state)))))

(defun follow-state (items)
  "percolates look-ahead onto descendant states of this state"
  (let ((closure (close-items items)))
    (dolist (cat (items-right closure))
      (get-state-name (shift-items closure cat)))))

(defun build-table ()
  "Actually builds the table"
  (setq stateList '())
  (setq nextStateNo -1)
  (get-state-name (list (make-item :rule (make-rule :no 0
						    :mother *TOPCAT*
						    :daughters (list gstart))
				   :pos 0
				   :la *ENDMARKER*)))
  (setq stateList (nreverse stateList)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                  LALR(1) parsing table printer
;;;

(defun print-table (stateList)
  "Prints the state table"
  (dolist (state stateList)
    (format t "~%~%~a:" (state-name state))
    (dolist (citem (state-citems state))
      (format t "~%  ~a -->~{ ~a~} .~{ ~a~}, ~{~a ~}"
              (rule-mother (item-rule citem))
              (subseq (rule-daughters (item-rule citem)) 0 (item-pos citem))
              (subseq (rule-daughters (item-rule citem)) (item-pos citem))
              (item-la citem)))
    (dolist (shift (state-shifts state))
      (format t "~%    On ~a shift ~a" (shift-cat shift) (shift-where shift)))
    (dolist (reduce (compact-items 
                     (delete-if #'(lambda (i) (item-right i))
                                (close-items 
                                 (expand-citems (state-citems state))))))
      (format t "~%    On~{ ~a~} reduce~{ ~a~} --> ~a"
              (item-la reduce)
              (rule-daughters (item-rule reduce))
              (rule-mother (item-rule reduce)))))
  (format t "~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                  LALR(1) parser constructor
;;;

(defun translate-State (state)
  "translates a state into lisp code that could appear in a labels form"
  (format T " ~(~S~)" (state-name state))

  (let ((reduces (compact-items 
                  (delete-if #'(lambda (i) (item-right i))
                             (close-items 
                              (expand-citems (state-citems state))))))
        (symbolsSoFar '()))   ; to ensure that a symbol never occurs twice
       (labels ((translateShift (shift)
                                (push (shift-cat shift) symbolsSoFar)
                                `(,(shift-cat shift)
                                  ,@(when *lalr-debug*
                                      `((when *lalr-debug*
                                          (princ ,(format nil "Shift ~a to ~a~%" 
                                                          (shift-cat shift) (shift-where shift))))))
                                  (shift-from #',(state-name state))
                                  (,(shift-where shift))))
                (translateReduce (item)
                                 (when (intersection (item-la item) symbolsSoFar)
                                   (format t "Warning, Not LALR(1)!!: ~a, ~a --> ~{~a ~}~%"
                                           (state-name state) 
                                           (rule-mother (item-rule item))
                                           (rule-daughters (item-rule item)))
                                   (setf (item-la item) 
                                         (nset-difference (item-la item) 
                                                          symbolsSoFar)))
                                 (dolist (la (item-la item))
                                   (push la symbolsSoFar))
                                 `(,(item-la item)
                                   ,@(when *lalr-debug*
                                       `((when *lalr-debug*
                                           (princ ,(format nil "Reduce ~{~a ~} --> ~a~%"
                                                           (rule-daughters (item-rule item))
                                                           (rule-mother (item-rule item)))))))
                                   (reduce-cat #',(state-name state)
                                               ',(rule-mother (item-rule item))
                                               ,(item-pos item)
                                               ,(rule-action (item-rule item))))))
         `(,(state-name state) ()
           (case (input-peek)
             ,@(mapcar #'translateShift (state-shifts state))
             ,@(mapcar #'translateReduce reduces)
             (otherwise (funcall parse-error)))))))

;;;  next-input performs lexical analysis.  It must return two values.
;;;  the category and the value.

(defun build-parser (name)
  "returns an lalr(1) parser.  next-input must return 2 values!"
  `(defun ,name (next-input parse-error)
     (let ((cat-la '())          ; category lookahead
           (val-la '())          ; value lookahead
           (val-stack '())       ; value stack
           (state-stack '()))    ; state stack
       (labels ((input-peek ()
                            (unless cat-la
			      (setf (values cat-la val-la) (funcall next-input)
				    cat-la (list cat-la)
				    val-la (list val-la)))
                            (first cat-la))
                (shift-from (name)
                            (push name state-stack)
                            (pop cat-la)
                            (push (pop val-la) val-stack))
                (reduce-cat (name cat ndaughters action)
                            (if (eq cat ',*TOPCAT*)
                              (pop val-stack)
                              (let ((daughter-values '())
                                    (state name))
                                (dotimes (i ndaughters)
                                  (push (pop val-stack) daughter-values)
                                  (setq state (pop state-stack)))
                                (push cat cat-la)
                                (push (apply action daughter-values) val-la)
                                (funcall state))))
                ,@(mapcar #'translate-State stateList))
         (,(state-name (first stateList)))))))
                
(defmacro define-grammar (name lex-forms &rest grammar)
  (make-parser grammar lex-forms :eof :name name))





