;;;; lexer.lisp

(in-package :structured-sim)

(defstruct token
  type
  value
  line
  column)

(defstruct lexer-state
  stream
  line
  column
  current-char)

(defun make-lexer (input-string)
  (let ((stream (make-string-input-stream input-string)))
    (make-lexer-state :stream stream
                      :line 1
                      :column 0
                      :current-char (read-char stream nil nil))))

(defun advance (lexer)
  (let ((ch (lexer-state-current-char lexer)))
    (when ch
      (when (char= ch #\Newline)
        (incf (lexer-state-line lexer))
        (setf (lexer-state-column lexer) 0))
      (incf (lexer-state-column lexer))
      (setf (lexer-state-current-char lexer)
            (read-char (lexer-state-stream lexer) nil nil)))))

(defun peek-char-lexer (lexer)
  (lexer-state-current-char lexer))

(defun make-token-here (lexer type value)
  (make-token :type type
              :value value
              :line (lexer-state-line lexer)
              :column (lexer-state-column lexer)))

(defun whitespace-p (ch)
  (and ch (member ch '(#\Space #\Tab))))

(defun newline-p (ch)
  (and ch (member ch '(#\Newline))))

(defun skip-whitespace (lexer)
  (loop while (whitespace-p (peek-char-lexer lexer))
        do (advance lexer)))

(defun lex-identifier (lexer)
  (let ((start-line (lexer-state-line lexer))
        (start-col (lexer-state-column lexer))
        (chars nil))
    (loop for ch = (peek-char-lexer lexer)
          while (and ch (or (alpha-char-p ch)
                            (digit-char-p ch)
                            (char= ch #\_)))
          do (progn
               (advance lexer)
               (push ch chars)))
    (let* ((text (coerce (nreverse chars) 'string))
           (text-upper (string-upcase text))
           (keyword-type (cond
                           ((string= text-upper "IF") :if)
                           ((string= text-upper "THEN") :then)
                           ((string= text-upper "END_IF") :end-if)
                           ((string= text-upper "FOR") :for)
                           ((string= text-upper "TO") :to)
                           ((string= text-upper "BY") :by)
                           ((string= text-upper "NOT") :not)
                           ((string= text-upper "TRUE") :true)
                           ((string= text-upper "FALSE") :false)
                           ((string= text-upper "CASE") :case)
                           ((string= text-upper "END_CASE") :end-case)
                           ((string= text-upper "TON") :ton)
                           ((string= text-upper "IN") :in)
                           ((string= text-upper "PT") :pt)
                           (t :ident))))
      (make-token :type keyword-type
                  :value text
                  :line start-line
                  :column start-col))))

(defun peek-next-char (lexer)
  (let ((stream (lexer-state-stream lexer)))
    (peek-char nil stream nil nil)))

(defun lex-number (lexer)
  (let ((start-line (lexer-state-line lexer))
        (start-col (lexer-state-column lexer))
        (chars nil)
        (has-dot nil))
    (loop for ch = (peek-char-lexer lexer)
          while (and ch (digit-char-p ch))
          do (progn
               (advance lexer)
               (push ch chars)))
    (when (and (eql (peek-char-lexer lexer) #\.)
               (and (peek-next-char lexer) (digit-char-p (peek-next-char lexer))))
      (setf has-dot t)
      (push #\. chars)
      (advance lexer)
      (loop for ch = (peek-char-lexer lexer)
            while (and ch (digit-char-p ch))
            do (progn
                 (advance lexer)
                 (push ch chars))))
    (let ((text (coerce (nreverse chars) 'string)))
      (make-token :type (if has-dot :real :int)
                  :value text
                  :line start-line
                  :column start-col))))

(defun lex-string (lexer quote-char)
  (let ((start-line (lexer-state-line lexer))
        (start-col (lexer-state-column lexer))
        (chars nil))
    (advance lexer)
    (loop for ch = (peek-char-lexer lexer)
          until (or (null ch) (char= ch quote-char))
          do (progn
               (advance lexer)
               (push ch chars)))
    (unless (peek-char-lexer lexer)
      (error "Unterminated string at line"))
    (advance lexer)
    (make-token :type :string
                :value (coerce (nreverse chars) 'string)
                :line start-line
                :column start-col)))



(defun lex-next-token (lexer)
  (skip-whitespace lexer)
  (let ((ch (peek-char-lexer lexer)))
    (cond
      ((null ch) nil)

      ((newline-p ch)
       (let ((tok (make-token-here lexer :newline "\\n")))
         (advance lexer)
         tok))

      ((alpha-char-p ch)
       (lex-identifier lexer))

      ((digit-char-p ch)
       (lex-number lexer))

      ((or (char= ch #\") (char= ch #\'))
       (lex-string lexer ch))

      ((char= ch #\;)
       (advance lexer)
       (make-token-here lexer :semicolon ";"))

      ((char= ch #\()
       (advance lexer)
       (make-token-here lexer :open-paren "("))

      ((char= ch #\))
       (advance lexer)
       (make-token-here lexer :close-paren ")"))

      ((char= ch #\[)
       (advance lexer)
       (make-token-here lexer :open-bracket "["))

      ((char= ch #\])
       (advance lexer)
       (make-token-here lexer :close-bracket "]"))

      ((char= ch #\/)
       (advance lexer)
       (if (eql (peek-char-lexer lexer) #\/)
           (progn
             (loop while (let ((c (peek-char-lexer lexer)))
                           (and c (not (newline-p c))))
                   do (advance lexer))
             (make-token-here lexer :comment "//"))
           (make-token-here lexer :division "/")))

      ((char= ch #\:)
       (advance lexer)
       (if (eql (peek-char-lexer lexer) #\=)
           (progn (advance lexer)
                  (make-token-here lexer :assign ":="))
           (make-token-here lexer :colon ":")))

      ((char= ch #\+)
       (advance lexer)
       (make-token-here lexer :op "+"))

      ((char= ch #\-)
       (advance lexer)
       (make-token-here lexer :op "-"))

      ((char= ch #\*)
       (advance lexer)
       (make-token-here lexer :op "*"))

      ((char= ch #\<)
       (advance lexer)
       (make-token-here lexer :less-than "<"))

      (t
       (let ((tok (make-token-here lexer :undefined (string ch))))
         (advance lexer)
         tok)))))

(defun tokenize (input-string)
  (let ((lexer (make-lexer input-string))
        (tokens nil))
    (loop for token = (lex-next-token lexer)
          while token
          do (push token tokens))
    (nreverse tokens)))
