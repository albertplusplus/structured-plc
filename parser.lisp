;;;; parser.lisp

(in-package :structured-sim)

(defun parse (tokens)
  (let ((id (second (first tokens))))
    (if (eql (first (second tokens)) :assign)
        (format t "(setf ~a ~a)~%" id (second (third tokens))))))
