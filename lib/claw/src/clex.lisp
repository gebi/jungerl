;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLEX; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: A flex like scanner generator for Common LISP
;;;   Created: 1997-10-12
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; --------------------------------------------------------------------------------------
;;;  (c) copyright 1997-1999 by Gilbert Baumann

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

(defpackage :clex
  (:use :common-lisp)
  (:export
   #:deflexer #:backup #:begin #:initial #:bag))

(in-package :CLEX)

;; This code should burn!
;; (declaim (optimize (space 1) (speed 3)))

;;; NOTE -- It turns out that this code is a magintude slower under CMUCL
;;; compared to CLISP or ACL. Probably they do not have a good implementation of
;;; bit vectors.

;;; We encode our FSA's directly as linked datastructures; A state is represented by:

(defstruct (state (:type vector))
  (final 0)
  transitions                           ;simple alist of (sigma . next-state)
  id                                    ;numeric id of state
  eps-transitions)                      ;list of all states reached by epsilon (empty transitions)

(defun state-add-link (this char that)
  "Add a transition to state `this'; reading `char' proceeds to `that'."
  (cond ((eq char 'eps)
         (pushnew that (state-eps-transitions this)))
        (t
         (dolist (k (state-transitions this)
                   (push (cons (list char) that) (state-transitions this)))
           (when (eq (cdr k) that)
             (pushnew char (car k))
             (return nil))) )))

;;; When constructing FSA's from regular expressions we abstract by the notation
;;; of FSA's as boxen with an entry and an exit state.

(defstruct fsa
  start                                 ;entry state
  end)                                  ;exit state

(defun fsa-empty ()
  "Accepts the empty word."
  (let ((q (make-state)))
    (make-fsa :start q :end q)))

(defun fsa-trivial (char)
  "Accepts the trivial word consisting out of exactly one `char'."
  (let ((q0 (make-state))
        (q1 (make-state)))
    (state-add-link q0 char q1)
    (make-fsa :start q0 :end q1)))

(defun fsa-concat (a1 a2)
  "Concatenation of `a1' and `a2'. Hence `a1 a2'."
  (state-add-link (fsa-end a1) 'eps (fsa-start a2))
  (make-fsa :start (fsa-start a1)
            :end   (fsa-end   a2)))

(defun fsa-iterate (a)
  "Iteration of `a'. Hence `a*'"
  (let ((q0 (make-state))
        (q1 (make-state)))
    (state-add-link q0 'eps (fsa-start a))
    (state-add-link q0 'eps q1)
    (state-add-link q1 'eps q0)
    (state-add-link (fsa-end a) 'eps q1)
    (make-fsa :start q0 :end q1)))

(defun fsa-branch (&rest as)
  "Alternation of a0..an; Hence `a0 | a1 | ... | an'."
  (let ((q0 (make-state))
        (q1 (make-state)))
    (dolist (a as)
      (state-add-link q0 'eps (fsa-start a))
      (state-add-link (fsa-end a) 'eps q1))
    (make-fsa :start q0 :end q1)))

;;;; ----------------------------------------------------------------------------------------------------
;;;;  Converting regular expressions to (ND)FSA 
;;;;

;;; However we choose here a Lispy syntax for regular expressions:

;;; a                   singelton
;;; (and a0 .. an)      concatation
;;; (or a0 .. an)       alternation
;;; (* a)               iteration

;;; Further the abbrevs.:
;;;   (+ a) == (and a (* a))
;;;   (? a) == (or a (and))
;;;   (a0 ... an) == (and a0 ... an)

;;; When a string embeded into a regular expression is seen, the list
;;; of characters is spliced in. So formally:
;;;   (a0 .. ai "xyz" aj .. an) == (a0 .. ai #\x #\y #\z aj .. an)
;;;
;;; This is useful for matching words:
;;;   "foo" --> (and "foo") --> (and #\f #\o #\o) == The word 'foo'
;;; or for denoting small sets:
;;;   (or "+-") --> (or #\+ #\-) == One of '+' or '-'

(defun loose-eq (x y)
  (cond ((eq x y))
        ((and (symbolp x) (symbolp y))
         (string= (symbol-name x) (symbol-name y)))))

(defun regexp->fsa (term)
  (setf term (regexp-expand-splicing term))
  (cond ((and (atom term) (not (stringp term)))
         (fsa-trivial term))
        ((loose-eq (car term) 'AND) (regexp/and->fsa term))
        ((loose-eq (car term) 'OR)  (regexp/or->fsa term))
        ((loose-eq (car term) '*)   (fsa-iterate (regexp->fsa (cadr term))))
        ((loose-eq (car term) '+)   (regexp->fsa `(AND ,(cadr term) (* ,(cadr term)))))
        ((loose-eq (car term) '?)   (regexp->fsa `(OR (AND) ,(cadr term))))
        ((loose-eq (car term) 'RANGE)
         (regexp->fsa `(OR .,(loop for i from (char-code (cadr term)) to (char-code (caddr term)) 
                                 collect (code-char i)))))
        (t 
         (regexp->fsa `(AND .,term))) ))

(defun regexp/or->fsa (term)
  ;; I optimize here a bit: I recognized, that ORs are mainly just
  ;; (large) sets of characters. The extra epsilon transitions are not
  ;; neccessary on single atoms, so I omit them here. -- This reduces the
  ;; number of states quite a bit in the first place.
  (let ((q0 (make-state))
        (q1 (make-state)))
    (dolist (a (cdr term))
      (cond ((atom a)
             (state-add-link q0 a q1))
            ((let ((a (regexp->fsa a)))
               (state-add-link q0 'eps (fsa-start a))
               (state-add-link (fsa-end a) 'eps q1)))))
    (make-fsa :start q0 :end q1)))

(defun regexp/and->fsa (term)
  (cond ((null (cdr term)) (fsa-empty))
        ((null (cddr term)) (regexp->fsa (cadr term)))
        ((fsa-concat (regexp->fsa (cadr term)) (regexp->fsa `(and .,(cddr term)))))))

(defun regexp-expand-splicing (term)
 (cond ((consp term)
        (mapcan #'(lambda (x)
                    (cond ((stringp x) (coerce x 'list))
                          ((list x))))
                term))
       (t term)))

;;;; ----------------------------------------------------------------------------------------------------
;;;;  Converting a ND-FSA to a D-FSA
;;;;

;;; Since we have to compare and unionfy sets of states a lot, I use bit-vectors
;;; to represent these sets for speed. However let me abstract that a bit:

;;; (All these are defined as macros simply for speed. Inlining would be an
;;; option here, when it would be reliable. With defining macros I enforce
;;; inlining).

(defmacro make-empty-set (n)
  "Create the empty set on the domain [0,n)."
  `(make-array ,n :element-type 'bit :initial-element 0))

(defmacro nset-put (bag new)
  "Destructively calculate bag = bag U {new}."
  `(setf (sbit (the (simple-array bit (*)) ,bag) (the fixnum ,new)) 1))

(defmacro element-of-set-p (elm set)
  "Determine whether `elm' is element of the set `set'."
  `(eq 1 (sbit (the (simple-array bit (*)) ,set) (the fixnum ,elm))))

(defmacro set-size (set)
  "Return the upper bound of the domain of `set'."
  `(length ,set))

(defmacro do-bits ((var set &optional result) &body body)
  "Iterate body with `var' over all elements of `set'."
  (let ((g/set (gensym)))
    `(let ((,g/set ,set))
       (dotimes (,var (set-size ,g/set) ,result)
         (when (element-of-set-p ,var ,g/set)
           ,@body)))))

;;; Since the sets we defined above only take non-negative integers, we have to
;;; number our states. This is done once by NUMBER-STATES.

(defun number-states (starts)
  "Number all state reachable form `starts', continuosly from 0. Each state got
   it's number stuck into the `id' slot.
   Returns two values: `n' the number of states and `tab' a table to lookup a
   state given the number it got attached to."
  (let ((n 0)
        (tab (make-array 0 :adjustable t :fill-pointer 0 :initial-element nil)))
    (labels ((walk (x)
               (unless (state-id x)
                 (vector-push-extend x tab 300)
                 (setf (state-id x) (prog1 n (incf n)))
                 (dolist (tr (state-transitions x))
                   (walk (cdr tr)))
                 (dolist (y (state-eps-transitions x))
                   (walk y)))))
      (dolist (s starts) (walk s))
      (values n tab))))

;;; We need to calculate the epsilon closure of a given state. Due to the
;;; precise workings of our algorithm below, we only need this augmenting
;;; version.

(defun fsa-epsilon-closure/set (x state-set)
  "Augment the epsilon closure of the state `state' into `state-set'."
  (unless (element-of-set-p (state-id x) state-set)
    (nset-put state-set (state-id x))
    (dolist (k (state-eps-transitions x))
      (fsa-epsilon-closure/set k state-set))))

(defun ndfsa->dfsa (starts)
  (let ((batch nil)
        (known nil))
    (multiple-value-bind (n tab) (number-states starts)
      (labels ((name-state-set (state-set)
                 (or (cdr (assoc state-set known :test #'equal))
                     (let ((new (make-state)))
                       (push (cons state-set new) known)
                       (push state-set batch)
                       new)))
               (add-state-set (state-set)
                 (let ((new-tr nil)
                       (new-tr-real nil)
                       (name   (name-state-set state-set))
                       (new-final  0))
                   (do-bits (s0 state-set)
                     (let ((s (aref tab s0))) 
                       (setf new-final (max new-final (state-final s)))
                       (dolist (tr (state-transitions s))
                         (let ((to (cdr tr)))
                           (dolist (z (car tr))
                             (let ((looked (getf new-tr z nil)))
                               (if looked
                                   (fsa-epsilon-closure/set to looked)
                                 (let ((sts (make-empty-set n)))
                                   (fsa-epsilon-closure/set to sts)
                                   (setf (getf new-tr z) sts) ))))))))
                   (setq new-tr (frob2 new-tr))
                   (do ((q new-tr (cddr q)))
                       ((null q))
                     (let ((z (car q))
                           (to (cadr q)))
                       (push (cons z (name-state-set to)) new-tr-real)))
                   (setf (state-transitions name) new-tr-real
                         (state-final name) new-final))))
        (prog1
            (mapcar #'(lambda (s)
                        (name-state-set (let ((sts (make-empty-set n)))
                                          (fsa-epsilon-closure/set s sts)
                                          sts)))
                    starts)
          (do ()
              ((null batch))
            (add-state-set (pop batch)))) ))))

(defun frob2 (res &aux res2)
  (do ((q res (cddr q)))
      ((null q) res2)
    (do ((p res2 (cddr p)))
        ((null p)
         (setf res2 (list* (list (car q)) (cadr q) res2)))
      (when (equal (cadr q) (cadr p))
        (setf (car p) (cons (car q) (car p)))
        (return)))))

;;;; ----------------------------------------------------------------------------------------------------
;;;;  API
;;;;

;;; Features to think about:
;;; - case insensitive scanner
;;; - compression of tables
;;; - debugging aids
;;; - non-interactive high speed scanning?
;;; - make BAG a macro? So that non used bags are not considered?
;;; - REJECT?
;;; - support for include?
;;; - support for putting back input?
;;; - count lines/columns? Track source?
;;; - richer set of regexp primitives e.g. "[a-z]" style sets
;;; - could we offer complement regexp?
;;; - trailing context
;;; - sub-state stacks?
;;; - user variables to include ['global' / 'lexical']
;;; - identifing sub-expression of regexps (ala \(..\) and \n)
;;;

#-(OR CMU GCL)
(defun loadable-states-form (starts) 
  `',starts)  

#+(OR CMU GCL)
;; Leider ist das CMUCL so dumm, dass es scheinbar nicht faehig ist die
;; selbstbezuegliche Structur ',starts in ein FASL file zu dumpen ;-(
;; Deswegen hier dieser read-from-string Hack.
(defun loadable-states-form (starts)
  `(LET ((*PACKAGE* (FIND-PACKAGE ',(package-name *package*))))
        (READ-FROM-STRING ',(let ((*print-circle* t)
                                  (*print-readably* t)
                                  (*print-pretty* nil))
                              (prin1-to-string starts)))))

(defmacro old/deflexer (name macro-defs &rest rule-defs)
  (let ((macros nil) starts clauses (n-fin 0))
    (dolist (k macro-defs)
      (push (cons (car k) (sublis macros (cadr k))) macros))
    ;;canon clauses -- each element of rule-defs becomes (start expr end action)
    (setq rule-defs
      (mapcar #'(lambda (x)
                  (cond ((and (consp (car x)) (eq (caar x) 'in))
                         (list (cadar x) (sublis macros (caddar x)) (progn (incf n-fin) n-fin) (cdr x)))
                        ((list 'initial (sublis macros (car x)) (progn (incf n-fin) n-fin) (cdr x)))))
              (reverse rule-defs)))
    ;;collect all start states in alist (<name> . <state>)
    (setq starts (mapcar #'(lambda (name)
                             (cons name (make-state)))
                         (remove-duplicates (mapcar #'car rule-defs))))
    ;;build the nd-fsa's
    (dolist (r rule-defs)
      (destructuring-bind (start expr end action) r
        (let ((q0 (cdr (assoc start starts)))
              (fsa (regexp->fsa `(and ,expr))))
          ;;link start state
          (state-add-link q0 'eps (fsa-start fsa))
          ;;mark final state
          (setf (state-final (fsa-end fsa)) end)
          ;; build a clause for CASE
          (push `((,end) .,action) clauses))))
    ;; hmm... we have to sort the final states after building the dfsa
    ;; or introduce fixnum identifier and instead of union take the minimum 
    ;; above in ndfsa->dfsa.
    (progn
     (mapcar #'(lambda (x y) (setf (cdr x) y))
             starts (ndfsa->dfsa (mapcar #'cdr starts))))
    ;;    (print (number-states starts))
    `(DEFUN ,(intern (format nil "MAKE-~A-LEXER" name)) (INPUT)
        (LET* ((STARTS ,(loadable-states-form starts))
               (SUB-STATE 'INITIAL)
               (STATE NIL)
               (LOOK-AHEAD NIL)
               (BAGG/CH (MAKE-ARRAY 100 :FILL-POINTER 0 :ADJUSTABLE T :ELEMENT-TYPE ',(ARRAY-ELEMENT-TYPE "")))
               (BAGG/STATE (MAKE-ARRAY 100 :FILL-POINTER 0 :ADJUSTABLE T))
               (CH NIL))
              #'(LAMBDA ()
                   (BLOCK NIL
                     (LABELS ((BEGIN (X)
                                     (SETQ SUB-STATE X))
                              (BACKUP (CH)
                                      (COND ((STRINGP CH)
                                             (WHEN (> (LENGTH CH) 0)
                                                   (PUSH (CONS 0 CH) LOOK-AHEAD)))
                                            (T (PUSH CH LOOK-AHEAD))))
                              (PUSH* (CH STATE)
                                     (VECTOR-PUSH-EXTEND CH BAGG/CH 10)
                                     (VECTOR-PUSH-EXTEND STATE BAGG/STATE 10) )
                              (POP*/CH ()
                                       (LET ((FP (LENGTH BAGG/CH)))
                                            (PROG1 (AREF BAGG/CH (1- FP))
                                                   (SETF (FILL-POINTER BAGG/STATE) (1- FP))
                                                   (SETF (FILL-POINTER BAGG/CH) (1- FP)))))
                              (TOS*/STATE ()
                                          (AREF BAGG/STATE (1- (LENGTH BAGG/STATE))) )
                              (EMPTY*? ()
                                       (= (LENGTH BAGG/CH) 0))
                              (REWIND* ()
                                       (SETF (FILL-POINTER BAGG/CH) 0)
                                       (SETF (FILL-POINTER BAGG/STATE) 0) )
                              (STRING* ()
                                       (COPY-SEQ BAGG/CH))
                              #+(OR)
                              (FIND-NEXT-STATE (CH STATE)
                                               (DOLIST (K (STATE-TRANSITIONS STATE))
                                                       (WHEN (MEMBER CH (CAR K))
                                                             (RETURN (CDR K)))))
                              (GETCH ()
                                     (COND ((NULL LOOK-AHEAD) (READ-CHAR INPUT NIL NIL))
                                           ((CONSP (CAR LOOK-AHEAD))
                                            (LET ((S (CDAR LOOK-AHEAD)))
                                                 (PROG1
                                                  (AREF S (CAAR LOOK-AHEAD))
                                                  (INCF (CAAR LOOK-AHEAD))
                                                  (WHEN (= (CAAR LOOK-AHEAD) (LENGTH S))
                                                        (POP LOOK-AHEAD)))))
                                           (T (POP LOOK-AHEAD)) )))
                             (DECLARE (INLINE BACKUP GETCH))
                             (TAGBODY 
                              START (SETQ STATE (CDR (ASSOC SUB-STATE STARTS)))
                                    (WHEN (NULL STATE)
                                          (ERROR "Sub-state ~S is not defined." SUB-STATE))
                                    (REWIND*)
                              LOOP  (SETQ CH (GETCH))
                                    (LET ((NEXT-STATE 
                                           (BLOCK FOO
                                                  (DOLIST (K (STATE-TRANSITIONS STATE))
                                                          (DOLIST (Q (CAR K))
                                                                  (WHEN (EQL CH Q)
                                                                        (RETURN-FROM FOO (CDR K)))))) ))
                                         (COND ((NULL NEXT-STATE)
                                                (BACKUP CH)
                                                (DO ()
                                                    ((OR (EMPTY*?) (NOT (EQ 0 (TOS*/STATE)))))
                                                    (BACKUP (POP*/CH)))
                                                (COND ((AND (EMPTY*?) (NULL CH))
                                                       (RETURN :EOF))
                                                      ((EMPTY*?)
                                                       (ERROR "oops ~S ~S" ch (mapcar #'car (state-transitions state))))
                                                      (T
                                                       (LET ((HALTING-STATE (TOS*/STATE)))
                                                            (LET ((BAG* NIL))
                                                                 (SYMBOL-MACROLET ((BAG (IF BAG*
                                                                                         BAG*
                                                                                         (SETF BAG* (STRING*)))))
                                                                   (CASE HALTING-STATE
                                                                         ,@clauses)))
                                                            (GO START)))))
                                               (T
                                                (PUSH* CH (STATE-FINAL NEXT-STATE))
                                                (SETQ STATE NEXT-STATE)
                                                (GO LOOP))))))))))))

;;;; ----------------------------------------------------------------------------------------------------
;;;;

(defun parse-char-set (string i)
  (let ((res nil)
        (complement-p nil))
    (incf i)                            ;skip '['
    ;;the first char is special
    (cond ((char= (char string i) #\]) (incf i) (push #\] res))
          ((char= (char string i) #\^) (incf i) (setq complement-p t))
          ((char= (char string i) #\-) (incf i) (push #\- res)))
    (do ()
        ((char= (char string i) #\])
         (values (if complement-p (cons 'cset res) (cons 'set res)) (+ i 1)))
      (cond ((char= (char string (+ i 1)) #\-)
             ;;it's a range
             (push (cons (char string i) (char string (+ i 2))) res)
             (incf i 3))
            (t
             ;;singleton
             (push (char string i) res)
             (incf i))))))

;;;; ------------------------------------------------------------------------------------------

(defparameter *full-table-p* t)

(defun mungle-transitions (trs)
  (if *full-table-p*
      (let ((res (make-array 256 :initial-element nil)))
        (dolist (tr trs)
          (dolist (ch (car tr))
            (setf (aref res (char-code ch)) (cdr tr))))
        res)
    trs))

(defun over-all-states (fun starts)
  ;; Apply `fun' to each state reachable from starts.
  (let ((yet nil))
    (labels ((walk (q)
               (unless (member q yet)
                 (push q yet)
                 (let ((trs (state-transitions q)))
                   (funcall fun q)
                   (dolist (tr trs)
                     (walk (cdr tr)))))))
      (mapc #'walk starts))))

(defmacro deflexer (name macro-defs &rest rule-defs)
  (let ((macros nil) starts clauses (n-fin 0))
    (dolist (k macro-defs)
      (push (cons (car k) (sublis macros (cadr k))) macros))
    ;;canon clauses -- each element of rule-defs becomes (start expr end action)
    (setq rule-defs
      (mapcar #'(lambda (x)
                  (cond ((and (consp (car x)) (eq (caar x) 'in))
                         (list (cadar x) (sublis macros (caddar x)) (progn (incf n-fin) n-fin) (cdr x)))
                        ((list 'initial (sublis macros (car x)) (progn (incf n-fin) n-fin) (cdr x)))))
              (reverse rule-defs)))
    ;;collect all start states in alist (<name> . <state>)
    (setq starts (mapcar #'(lambda (name)
                             (cons name (make-state)))
                         (remove-duplicates (mapcar #'car rule-defs))))
    ;;build the nd-fsa's
    (dolist (r rule-defs)
      (destructuring-bind (start expr end action) r
        (let ((q0 (cdr (assoc start starts)))
              (fsa (regexp->fsa `(and ,expr))))
          ;;link start state
          (state-add-link q0 'eps (fsa-start fsa))
          ;;mark final state
          (setf (state-final (fsa-end fsa)) end)
          ;; build a clause for CASE
          (push `((,end) .,action) clauses))))
    ;; hmm... we have to sort the final states after building the dfsa
    ;; or introduce fixnum identifier and instead of union take the minimum 
    ;; above in ndfsa->dfsa.
    (progn
     (mapcar #'(lambda (x y) (setf (cdr x) y))
             starts (ndfsa->dfsa (mapcar #'cdr starts))))
    ;;(terpri)(princ `(,(number-states starts) states))(finish-output)
    (let ((n 0))
      (over-all-states (lambda (state)
                         (incf n)
                         (setf (state-transitions state)
                               (mungle-transitions (state-transitions state))))
                       (mapcar #'cdr starts))
      (format T "~&~D states." n))
    `(DEFUN ,(intern (format nil "MAKE-~A-LEXER" name)) (INPUT)
        (LET* ((STARTS ,(loadable-states-form starts))
               (SUB-STATE 'INITIAL)
               (STATE NIL)
               (LOOK-AHEAD NIL)
               (BAGG/CH (MAKE-ARRAY 100 :FILL-POINTER 0 :ADJUSTABLE T :ELEMENT-TYPE ',(ARRAY-ELEMENT-TYPE "")))
               (BAGG/STATE (MAKE-ARRAY 100 :FILL-POINTER 0 :ADJUSTABLE T))
               (CH NIL))
              #'(LAMBDA ()
                   (BLOCK NIL
                     (LABELS ((BEGIN (X)
                                     (SETQ SUB-STATE X))
                              (BACKUP (CH)
                                      (COND ((STRINGP CH)
                                             (WHEN (> (LENGTH CH) 0)
                                                   (PUSH (CONS 0 CH) LOOK-AHEAD)))
                                            (T (PUSH CH LOOK-AHEAD))))
                              (PUSH* (CH STATE)
                                     (VECTOR-PUSH-EXTEND CH BAGG/CH 10)
                                     (VECTOR-PUSH-EXTEND STATE BAGG/STATE 10) )
                              (POP*/CH ()
                                       (LET ((FP (LENGTH BAGG/CH)))
                                            (PROG1 (CHAR BAGG/CH (1- FP))
                                                   (SETF (FILL-POINTER BAGG/STATE) (1- FP))
                                                   (SETF (FILL-POINTER BAGG/CH) (1- FP)))))
                              (TOS*/STATE ()
                                          (AREF BAGG/STATE (1- (LENGTH BAGG/STATE))) )
                              (EMPTY*? ()
                                       (= (LENGTH BAGG/CH) 0))
                              (REWIND* ()
                                       (SETF (FILL-POINTER BAGG/CH) 0)
                                       (SETF (FILL-POINTER BAGG/STATE) 0) )
                              (STRING* ()
                                       (COPY-SEQ BAGG/CH))
                              (GETCH ()
                                     (COND ((NULL LOOK-AHEAD) (READ-CHAR INPUT NIL NIL))
                                           ((CONSP (CAR LOOK-AHEAD))
                                            (LET ((S (CDAR LOOK-AHEAD)))
                                                 (PROG1
                                                  (CHAR S (CAAR LOOK-AHEAD))
                                                  (INCF (CAAR LOOK-AHEAD))
                                                  (WHEN (= (CAAR LOOK-AHEAD) (LENGTH S))
                                                        (POP LOOK-AHEAD)))))
                                           (T (POP LOOK-AHEAD)) ))
                              ,(if *full-table-p*
                                 `(FIND-NEXT-STATE (STATE CH)
                                    (IF (CHARACTERP CH)
                                        (SVREF (STATE-TRANSITIONS STATE) (CHAR-CODE CH))
                                        NIL))
                                 `(FIND-NEXT-STATE (STATE CH)
                                     (BLOCK FOO
                                       (DOLIST (K (STATE-TRANSITIONS STATE))
                                          (DOLIST (Q (CAR K))
                                                  (WHEN (CHAR= CH Q)
                                                        (RETURN-FROM FOO (CDR K)))))))) )
                             (DECLARE (INLINE BACKUP GETCH FIND-NEXT-STATE))
                             (TAGBODY 
                              START (SETQ STATE (CDR (ASSOC SUB-STATE STARTS)))
                                    (WHEN (NULL STATE)
                                          (ERROR "Sub-state ~S is not defined." SUB-STATE))
                                    (REWIND*)
                              LOOP  (SETQ CH (GETCH))
                                    (LET ((NEXT-STATE (FIND-NEXT-STATE STATE CH)) )
                                         (COND ((NULL NEXT-STATE)
                                                (BACKUP CH)
                                                (DO ()
                                                    ((OR (EMPTY*?) (NOT (EQ 0 (TOS*/STATE)))))
                                                    (BACKUP (POP*/CH)))
                                                (COND ((AND (EMPTY*?) (NULL CH))
                                                       (RETURN :EOF))
                                                      ((EMPTY*?)
                                                       (ERROR "oops ~S ~S" ch (mapcar #'car (state-transitions state))))
                                                      (T
                                                       (LET ((HALTING-STATE (TOS*/STATE)))
                                                            (LET ((BAG* NIL))
                                                                 (SYMBOL-MACROLET ((BAG (IF BAG*
                                                                                         BAG*
                                                                                         (SETF BAG* (STRING*)))))
                                                                   (CASE HALTING-STATE
                                                                         ,@clauses)))
                                                            (GO START)))))
                                               (T
                                                (PUSH* CH (STATE-FINAL NEXT-STATE))
                                                (SETQ STATE NEXT-STATE)
                                                (GO LOOP))))))))))))
