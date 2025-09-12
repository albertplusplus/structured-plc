;;;; lexer.lisp

(in-package :structured-sim)


(defparameter *reserved-table* (alexandria:alist-hash-table
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
                            ("end_case" . :end-case)) :test #'equal))

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
  "Lex an open paren. If followed by asterisk, it is a block comment."
  (read-char code nil nil)
  (if (eql (peek code) #\*)
      (lex-block-comment code)
      (list :open-paren "(")))

(defun lex-block-comment (code)
  "An open paren followed by asterisk is a block comment."
  (read-char code nil nil)
  (let ((last-peek nil))
    (loop while (peek code) do
      (if (and (eql last-peek #\*)
               (eql (peek code) #\)))
          (progn
            (read-char code nil nil)
            (return (list :block-comment "")))
          (setf last-peek (read-char code nil nil))))))

(defun lex-string (code delim)
  "Lex a string, which is text inside of single or double quotes (#\' or #\")"
  (read-char code nil nil)
  (let ((fstr (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s fstr)
    (loop while (not (eql (peek code) delim))
          do (princ (read-char code nil nil) s))
    (read-char code nil nil)
    (list :string fstr))))

(defun lex-assign (code)
  "Lex an assignment, which is a colon followed by equal sign :="
  (read-char code nil nil)
  (let ((next (peek code)))
    (cond
      ((eql next #\=) (read-char code nil nil) (list :assign ":="))
      (t (list :undefined ":")))))

(defun lex-equal (code)
  "Lex an equal sign, which is used for comparison"
  (read-char code nil nil)
  (list :comparison "="))

(defun lex-less-angle (code)
  "Lex a left angle, which can either be less than (<), less than or equal to (<=),
   or not equal to (<>)."
  (read-char code nil nil)
  (let ((next (peek code)))
    (cond
      ((eql next #\=) (read-char code nil nil) (list :less-equal "<="))
      ((eql next #\>) (read-char code nil nil) (list :not-equal "<>"))
      (t (list :less-than "<")))))

(defun lex-greater-angle (code)
  "Lex a right angle, which can be greater than (>) or greater than or equal to (>=)"
  (read-char code nil nil)
  (let ((next (peek code)))
    (cond
      ((eql next #\=) (read-char code nil nil) (list :greater-equal ">="))
      (t (list :greater ">")))))


(defun lex-ident (code)
  "Lex an identifier, which is a stream of alphanumeric characters and/or underscores,
   but must start with an alpha character."
  (let* ((tmp (lex-token code :token-key :ident :stop-pred #'(lambda (x) (or (alpha-char-p x) (eql x #\_)))))
         (ident-str (str:downcase tmp))
         (is-reserved (gethash ident-str *reserved-table*)))
    (if is-reserved
        (list is-reserved tmp)
        (list :ident tmp))))


(defun lex-num (code)
  "Lex a number, which is a collection of digits, which may have a decimal (.),
   making it a real"
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
