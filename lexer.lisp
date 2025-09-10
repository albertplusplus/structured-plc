;;;; lexer.lisp

(in-package :structured-sim)


(defparameter *tokens* nil)
(defvar *reserved* (list "true" "false" "if" "then" "for" "by" "do" "to" "end_if" "end_for"))

(defstruct token
  type
  lexeme
  col)

(defun lex (code-to-lex)
  (let ((curr 0)
        (tokens nil))
    (with-input-from-string (code code-to-lex)
      (loop for ch = (peek code) then (peek code)
            until (null ch) do
              (cond
                ((alpha-char-p ch) (setf tokens (cons (lex-ident code) tokens)))
                ((alphanumericp ch) (setf tokens (cons (lex-num code) tokens)))
                ((member ch '(#\Tab #\Space)) (loop while (and (peek code)
                                                               (member (peek code) str:*whitespaces*))
                                                    do (read-char code nil nil)))
    ;             (setf tokens (cons (list :whitespace " ") tokens)))
                ((eql ch #\:) (setf tokens (cons (lex-assign code) tokens)))
                ((eql ch #\=) (setf tokens (cons (lex-equal code) tokens)))
                ((eql ch #\Newline) (setf tokens (cons (list :newline ch) tokens)))
                ((eql ch #\<) (setf tokens (cons (lex-less-angle code) tokens)))
                ((eql ch #\>) (setf tokens (cons (lex-greater-angle code) tokens)))
                (t (read-char code nil nil)))))
    (nreverse tokens)))


(defun lex-assign (code)
  (read-char code nil nil)
  (let ((next (peek code)))
    (cond
      ((eql next #\=) (read-char code nil nil) (list :assign ":="))
      (t (list :undefined ":")))))

(defun lex-equal (code)
  (read-char code nil nil)
  (let ((next (peek code)))
    (cond
      ((eql next #\=) (read-char code nil nil) (list :comparison "=="))
      (t (list :undefined "=")))))

(defun lex-less-angle (code)
  (read-char code nil nil)
  (let ((next (peek code)))
    (cond
      ((eql next #\=) (read-char code nil nil) (list :less-equal "<="))
      ((eql next #\>) (read-char code nil nil) (list :not-equal "<>"))
      (t (list :less-than "<")))))

(defun lex-greater-angle (code)
  (read-char code nil nil)
  (let ((next (peek code)))
    (cond
      ((eql next #\=) (read-char code nil nil) (list :greater-equal ">="))
      (t (list :greater ">")))))


(defun lex-ident (code)
  (let* ((tmp (lex-token code :token-key :ident :stop-pred #'(lambda (x) (or (alpha-char-p x) (eql x #\_)))))
         (ident-str (str:downcase tmp))
         (is-reserved (member ident-str *reserved* :test #'string=)))
    (if is-reserved
        (list (intern (str:upcase ident-str) :keyword) tmp)
        (list :ident tmp))))


(defun lex-num (code)
  (let ((fstr (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t))
        (is-real nil))
    (with-output-to-string (s fstr)
      (princ (glob-digits code) s)
      (if (eql #\. (peek code))
          (progn
            (princ (read-char code nil nil) s)
            (princ (glob-digits code) s)
            (setf is-real t))))
    (if is-real (list :real fstr) (list :int fstr))))


(defun glob-digits (code)
  (loop while (digit-char-p (peek code))
        collect (read-char code nil nil) into ret
        finally (return (coerce ret 'string))))


(defun lex-token (st &key token-key stop-pred)
  (loop for ch = (peek st)
        until (not (and ch (funcall stop-pred ch)))
        collect (read-char st nil nil) into res
        finally (return (coerce res 'string))))


(defun peek (strm)
  (peek-char nil strm nil nil))
