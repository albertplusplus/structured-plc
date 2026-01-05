;;;; parser.lisp

(in-package :structured-sim)

;;; Given a list of tokens from lexer such as:
;;;   ((:ident "bMotor") (:assign ":=") (:boolean "True") (:semicolon ";"))
;;; Perform recursive descent parsing to turn into
;;;   (setf bMotor-ident t)
;;; The -ident addition is incase a LISP keyword is used as a variable name
;;;
;;; But that basic idea is not enough. We need to turn it into more than just a setf s-expr
;;; We need to change the value of an HTML element so it can show that variable as true in
;;; the simulator.
;;;
;;; I will do this by, instead of the setf s-expr above, i will turn it into
;;;   (setf (text-value html-element) "True")
;;; So we can be visually changing the value of the data in HTML
