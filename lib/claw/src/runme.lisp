;; Just saying (load "runme") in a fresh Lisp should load the
;; compiler, compile the factorial program, and call the compiled
;; Erlang function.

(format t "~%Loading compiler..")
(load "language")
(load "erlang")
(load "erlc")

(format t "~%Compiling factorial program from 'fact.lerl'")
(erlc:erlc-file "fact")

(format t "~%The factorial of 10 is ~A."
        (|fact|:|fact/1| 10))
