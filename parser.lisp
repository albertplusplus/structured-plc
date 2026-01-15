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


(defstruct (parser (:constructor make-parse))
  tokens
  position)

(defun make-parser (tokens)
  (make-parse :tokens tokens :position 0))

(defun current-token (parser)
  (when (< (parser-position parser) (length (parser-tokens parser)))
    (nth (parser-position parser) (parser-tokens parser))))

(defun peek-type (parser)
  (let ((tok (current-token parser)))
    (when tok (token-type tok))))

(defun advance-parser (parser)
  (incf (parser-position parser)))

(defun expect (parser expected-type)
  (let ((tok (current-token parser)))
    (unless tok
      (error "Unexpected end of input"))
    (unless (eql (token-type tok) expected-type)
      (error "Expected ~A but got ~A at line ~A"
             expected-type (token-type tok) (token-line tok)))
    (advance-parser parser)
    tok))

(defun parse-number (parser)
  (let ((type (peek-type parser)))
    (cond
      ((eql type :int)
       (let ((tok (expect parser :int)))
         (parse-integer (token-value tok))))
      ((eql type :real)
       (let ((tok (expect parser :real)))
         (read-from-string (token-value tok))))
      (t (error "Expected number but got ~A" type)))))

(defun parse-assignment (parser)
  (let* ((ident-tok (expect parser :ident))
         (ident-name (token-value ident-tok)))
    (expect parser :assign)
    (let ((value (parse-expression parser)))
      (list :assignment ident-name value))))


(defun parse-factor (parser)
  (let ((type (peek-type parser)))
    (cond

      ((eql type :not)
       (expect parser :not)
       (list :not (parse-factor parser)))

      ((eql type :open-paren)
       (expect parser :open-paren)
       (let ((expr (parse-expression parser)))
         (expect parser :close-paren)
         expr))

      ((or (eql type :int) (eql type :real))
       (parse-number parser))

      ((eql type :ident)
       (let* ((tok (current-token parser)))
         (advance-parser parser)
         (let ((name (token-value tok)))
           (if (eql (peek-type parser) :open-bracket)
               (progn
                 (expect parser :open-bracket)
                 (let ((index (parse-expression parser)))
                   (expect parser :close-bracket)
                   (list :array-ref name index)))
             name))))
      (t (error "Expected number or ( but got ~A" type)))))

(defun parse-term (parser)
  (let ((left (parse-factor parser)))
    (loop while (member (peek-type parser) '(:op :division))
          do (let ((op-type (peek-type parser)))
               (cond
                 ((eql op-type :division)
                  (expect parser :division)
                  (setf left (list :div left (parse-factor parser))))
                 ((eql op-type :op)
                  (let ((tok (current-token parser)))
                    (if (string= (token-value tok) "*")
                      (progn
                        (advance-parser parser)
                        (setf left (list :mul left (parse-factor parser))))
                      (return)))))))
    left))

(defun parse-expression (parser)
  (let ((left (parse-term parser)))
    (loop while (eql (peek-type parser) :op)
          do (let ((tok (current-token parser)))
               (cond
                 ((string= (token-value tok) "+")
                  (advance-parser parser)
                  (setf left (list :add left (parse-term parser))))
                 ((string= (token-value tok) "-")
                  (advance-parser parser)
                  (setf left (list :sub left (parse-term parser))))
                 (t (return)))))
    left))

(defun parse-if (parser)
  (expect parser :if)
  (let ((condition (parse-comparison parser)))
    (expect parser :then)
    (let ((then-branch (parse-statement parser)))
      (expect parser :end-if)
      (list :if condition then-branch))))

(defun parse-statement (parser)
  (let ((type (peek-type parser)))
    (cond
      ((eql type :if) (parse-if parser))

      ((eql type :ident)
       (let* ((ident-tok (expect parser :ident))
              (ident-name (token-value ident-tok)))
         (cond
           ((eql (peek-type parser) :open-bracket)
            (expect parser :open-bracket)
            (let ((index (parse-expression parser)))
              (expect parser :close-bracket)
              (expect parser :assign)
              (let ((value (parse-expression parser)))
                (list :array-assign ident-name index value))))

           ((eql (peek-type parser) :assign)
            (expect parser :assign)
            (let ((value (parse-expression parser)))
              (list :assignment ident-name value)))

           (t (error "Expected := or [ after identider")))))

      (t (error "Expected a statement but got ~A" type)))))


(defun parse-comparison (parser)
  (let ((left (parse-expression parser)))
    (let ((type (peek-type parser)))
      (cond
        ((eql type :comparison)
         (expect parser :comparison)
         (list :eq left (parse-expression parser)))

        ((eql type :less-than)
         (expect parser :less-than)
         (list :lt left (parse-expression parser)))

        ((eql type :greater)
         (expect parser :greater)
         (list :gt left (parse-expression parser)))

        (t left)))))

(defun parse-array-index (parser ident-name)
  (expect parser :open-bracket)
  (let ((index (parse-expression parser)))
    (expect parser :close-bracket)
    (list :array-ref ident-name index)))
