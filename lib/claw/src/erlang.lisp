;; BIFs

(defpackage :erlang
  (:shadow ">" "-" "*")
  (:export ">" "-" "*"))

(defun erlang:> (x y)
  (if (cl:> x y) 'lang:true 'lang:false))

(defun erlang:- (x y)
  (cl:- x y))

(defun erlang:* (x y)
  (cl:* x y))

