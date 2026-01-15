;; ast.lisp
(in-package #:structured-sim)


(defun ast-to-lisp (ast)
  (cond
    ((numberp ast) ast)

    ((listp ast)
     (let ((type (first ast)))
       (case type
         (:assignment
          `(setf ,(intern (string-upcase (second ast)))
                 ,(ast-to-lisp (third ast))))

         (:add `(+ ,@(mapcar #'ast-to-lisp (rest ast))))
         (:sub `(- ,@(mapcar #'ast-to-lisp (rest ast))))
         (:mul `(* ,@(mapcar #'ast-to-lisp (rest ast))))
         (:div `(/ ,@(mapcar #'ast-to-lisp (rest ast))))

         (t (error "Unknown node: ~A" type)))))))

