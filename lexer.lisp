;;;; lexer.lisp

(in-package :structured-sim)


(defparameter *tokens* nil)

(defstruct token
  type
  lexeme
  line)

(defun lex (str)
  (let ((curr 0))
    (loop
          ())
