;;;; parser.lisp

(in-package :structured-sim)

(defclass parser ()
  ((tokens :initarg :tokens :accessor tokens)
   (parse-pos :initform 0 :accessor parse-pos))
  (:documentation "A parser has tokens and a parse position, which is
    the index of the token list"))

(defmethod consume-token ((p parser))
  "Consume the next token in the parser and return it."
  (let ((next-token (and (tokens p) (pop (tokens p)))))
    (incf (parse-pos p))
    (if next-token
        next-token
        (error "Empty input"))))

(defmethod peek-token ((p parser))
  "Peek the next token in the parser but dont consume it."
  (let ((peek-token (and (tokens p) (first (tokens p)))))
    (if peek-token
        peek-token
        (error "Empty input"))))


(defun parse (tokens)
  (let* ((p (make-instance 'parser :tokens tokens))
         (fst (consume-token p)))
    (if p
        (case (first fst)
          (:ident (format t "Hello ~A~%" (first fst)))))))


(defun parse-assign (tokens)
  (let ((lhs (expect (first tokens) :ident))
        (opp (expect (second tokens) :assign))
        (rhs (expect (third tokens) :ident :int :real)))
    (if (and lhs opp rhs)
        (format t "(~A ~A ~A)" (second lhs) (second opp) (second rhs))
        nil)))


(defun parse-expr (tokens)
  ())


(defun expect (token &rest symbs)
  (if (and token (member (car token) symbs))
      token
      (error (format nil "Expected one of ~a got ~a" symbs (if token (car token) nil)))))
