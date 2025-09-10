;;;; lexer.lisp

(in-package :structured-sim)


(defparameter *tokens* nil)

(defvar *reserved-table* (alexandria:alist-hash-table
                          '(("if"      . :if)
                            ("then"    . :then)
                            ("end_if"  . :end-if)
                            ("for"     . :for)
                            ("by"      . :by)
                            ("do"      . :do)
                            ("to"      . :to)
                            ("end_for" . :end-for)
                            ("true"    . :true)
                            ("false"   . :false)
                            ("case" . :case)
                            ("of" . :of)
                            ("end_case" . :end-case))))


(defvar *reserved* (list "true" "false" "if" "then" "for" "by" "do" "to" "end_if" "end_for"))

(defstruct token
  type
  lexeme
  col)

(defun new-token (tp le co)
  (make-token :type tp :lexeme le :col co))

(defun read-ch (curr)
  (incf curr))

(defun lex (code-to-lex)
  (let ((curr-line 0)
        (tokens nil))
    (with-input-from-string (code code-to-lex)
      (loop for ch = (peek code) then (peek code)
            until (null ch) do
              (cond
                ((alpha-char-p ch) (push (lex-ident code) tokens))
                ((alphanumericp ch) (push (lex-num code) tokens))
                ((member ch '(#\Tab #\Space)) (loop while (and (peek code)
                                                               (member (peek code) str:*whitespaces*))
                                                    do (read-char code nil nil)))
    ;             (setf tokens (cons (list :whitespace " ") tokens)))
                ((eql ch #\:) (push (lex-assign code) tokens))
                ((eql ch #\=) (push (lex-equal code) tokens))
                ((eql ch #\Newline) (incf curr-line) (read-char code nil nil) (push (list :newline "\n") tokens))
                ((eql ch #\;) (read-char code nil nil) (push (list :semicolon ";") tokens))
                ((eql ch #\<) (push (lex-less-angle code) tokens))
                ((eql ch #\") (push (lex-string code #\") tokens))
                ((eql ch #\') (push (lex-string code #\') tokens))
                ((eql ch #\/) (push (lex-slash code) tokens))
                ((eql ch #\() (push (lex-open-paren code) tokens))
                ((eql ch #\>) (push (lex-greater-angle code) tokens))
                (t (read-char code nil nil)))))
    (nreverse tokens)))

(defun lex-slash (code)
  "Lex a forward slash which can be the beginning of a double
   slash which is a comment, or a division if a single slash"
  (read-char code nil nil)
  (if (eql (peek code) #\/)
      (progn
        (loop while (and (peek code) (not (eql (peek code) #\Newline))) do
            (read-char code nil nil))
        (read-char code nil nil)
        (list :comment ""))
      (list :division "/")))

(defun lex-open-paren (code)
  (read-char code nil nil)
  (if (eql (peek code) #\*)))

(defun lex-block-comment (code)
  ())

(defun lex-string (code delim)
  (read-char code nil nil)
  (let ((fstr (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s fstr)
    (loop while (not (eql (peek code) delim))
          do (princ (read-char code nil nil) s))
    (read-char code nil nil)
    (list :string fstr))))

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
         (is-reserved (gethash ident-str *reserved-table*)))
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
  (loop while (and (peek code) (digit-char-p (peek code)))
        collect (read-char code nil nil) into ret
        finally (return (coerce ret 'string))))


(defun lex-token (st &key token-key stop-pred)
  (loop for ch = (peek st)
        until (not (and ch (funcall stop-pred ch)))
        collect (read-char st nil nil) into res
        finally (return (coerce res 'string))))


(defun peek (strm)
  (peek-char nil strm nil nil))
