(defpackage :erlc
  (:use :common-lisp)
  (:export :erlc-file))

(in-package :erlc)

(defun erlc-file (name)
  "Compile a Core Erlang program (in Lispy syntax) called NAME.
The source is read from NAME.lerl.
The result is loaded directly into the Lisp system in a package called
NAME."
  (let ((filename (concatenate 'string name ".lerl")))
    (with-open-file (s filename :direction :input)
      (when (find-package name)
        (delete-package name))
      (let ((*package* (make-package name)))
        ;; We READ the source file in the new module's package so that
        ;; its identifiers all end up in the right place. Then we just
        ;; EVAL it and let the macros (from language.lisp) do the
        ;; compilation.
        (eval (read s))))))

