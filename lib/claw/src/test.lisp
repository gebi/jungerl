;; Simple test script: compiles and loads CLAW, then uses it to
;; compile and run the factorial program.
;;
;; Works in CMUCL. In the Lisp shell, type: (load "test")
;; After this has run, the file "fact.cl" contains the compiled program.
;;
;; It can take a very long time to compile the code generated for the
;; parser. I'd like to improve this somehow. Meanwhile, a workaround
;; for this script is to compile CLAW to bytecode instead of native
;; code. Then it compiles about 10x faster, but bytecode isn't as nice
;; to work with (e.g. in the Lisp debugger.)

(compile-file "clex" :load t)           ; lexer generator
(compile-file "lalr" :load t)           ; parser generator
(compile-file "claw" :load t :byte-compile t)

(claw:file "fact" :load t)
(format t "~%The factorial of 100 is ~s" (|fact|:|fact/1| 100)))

