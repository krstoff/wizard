(in-package :wizard)

(defstruct expr type)
(defstruct (binop (:include expr)) op left right)
(defstruct (dot-call (:include expr)) left name args)
(defstruct (call (:include expr)) left args)
(defstruct (index-op (:include expr)) left index)
(defstruct (member-op (:include expr)) left property)
(defstruct (list-literal (:include expr)) items)
(defstruct (property-list-literal (:include expr)) props)
(defstruct (block (:include expr)) statements)
(defstruct (if-expr (:include expr)) condition block else-block)
(defstruct (while-expr (:include expr)) condition block)
(defstruct (loop-expr (:include expr)) expr)
(defstruct (cond-expr (:include expr)) left clauses)
(defstruct break-stmt expr)
(defstruct return-stmt expr)
(defstruct continue-stmt)
(defstruct let-stmt bindings expr)

(defstruct fun-decl name args type block)
(defstruct type-decl name fields)

(defun expect-token (expected stream)
  (let* ((token (advance-token stream))
         (relevant
           (case expected
             ((sym keyword num str) (type-of token))
             (t token))))
    (if (eq relevant expected)
      token
      (error "Expected ~S, but found ~S" expected token))))

(defun expect-ident (stream)
  (let ((token (expect 'sym stream)))
    (if (null (sym-path token))
      (sym-name token)
      (error "Expected an unqualified symbol, found ~S instead." token))))

(defun expect-keyword (stream)
  (let ((token (expect 'keyword stream)))
    (if (null (keyword-path token))
      (keyword-name token)
      (error "Expected an unqualified symbol, found ~S instead." token))))

(defun binary-op-p (token)
  (case token
    ((MUL DIV MOD ADD SUB LESS-THAN LESS-THAN-EQUALS GREATER-THAN
      GREATER-THAN-EQUALS AND OR EQUALS OF AS PERIOD) t)))

(defun parse-call-args (stream)
  (prog1
   (iterate
     (with rest-args = nil)
     (when (eq (peek-token stream) 'RIGHT-PAREN) (finish))
     (unless (first-time-p)
       (expect-token 'COMMA stream))
     (cond
       ((eq 'ELLIPSIS (peek-token stream))
        (if rest-args
            (error "You can only have one rest parameter.")
            (progn
              (advance-token stream)
              (setq rest-args (expression 0 stream)))))
       ((eq 'KEYWORD (type-of (peek-token stream)))
        (let ((keyword (advance-token stream)))
          (if (or (eq 'comma (peek-token stream))
                  (eq 'right-paren (peek-token stream)))
            (collect keyword into args)
            (collect (cons (keyword-name keyword) (expression 0 stream))
              into keywords))))
       (t
        (collect (expression 0 stream) into args)))
     (finally
       (return (list :args args :keywords keywords :rest rest-args))))
   (expect-token 'RIGHT-PAREN stream)))

(defun parse-list-items (stream &aux first-keyword)
  (if (and (eq 'keyword (type-of (peek-token stream)))
           (setq first-keyword (advance-token stream))
           (not (eq 'comma (peek-token stream))))
    ;; property list
    (iterate
      (when (first-iteration-p)
        (collect (cons first-keyword (expression 0 stream)) into props)
        (next-iteration))
      (when (eq (peek-token stream) 'RIGHT-BRACKET) (finish))
      (unless (first-iteration-p)
        (expect-token 'COMMA stream))
      (collect (cons (expect-token 'keyword stream) (expression 0 stream))
        into props)
      (finally
        (return
          (prog1
            (make-property-list-literal :props props)
            (expect-token 'RIGHT-BRACKET stream)))))
    ;; regular list
    (iterate
      (when (and (first-iteration-p) first-keyword)
        (collect first-keyword into items)
        (next-iteration))
      (when (eq (peek-token stream) 'RIGHT-BRACKET) (finish))
      (unless (first-iteration-p)
        (expect-token 'COMMA stream))
      (collect (expression 0 stream) into items)
      (finally
        (return
          (prog1 (make-list-literal :items items)
                 (expect-token 'RIGHT-BRACKET stream)))))))

(defun parse-cond-clause (stream)
  (let* ((pattern
          (unless (eq (peek-token stream) 'IF)
            (expression 0 stream)))
         (condition
           (when (eq (peek-token stream) 'IF)
             (advance-token stream)
             (expression 0 stream)))
         (block
           (progn
             (expect-token 'ARROW stream)
             (expect-token 'LEFT-BRACE stream)
             (parse-block stream))))
    (list :pattern pattern :condition condition :block block)))

(defun parse-cond-clauses (stream)
  (expect-token 'LEFT-BRACE stream)
  (iterate
    (until (eq (peek-token stream) 'RIGHT-BRACE))
    (collect (parse-cond-clause stream))
    (finally (advance-token stream))))

(defun lbp (token)
  (case token
    ((PERIOD LEFT-PAREN) 80)
    ((NOT) 70)
    ((MUL DIV MOD) 60)
    ((ADD SUB) 50)
    ((LESS-THAN LESS-THAN-EQUALS GREATER-THAN GREATER-THAN-EQUALS) 30)
    ((AND OR) 30)
    ((COND) 20)
    ((EQUALS RANGE) 10)
    (t 0)))

(defun nud (token stream)
  (typecase token
    ((or sym keyword num str) token)
    (symbol
      (case token
        ((LEFT-PAREN)
         (when (eq (peek-token stream) 'RIGHT-PAREN)
           (error "Can't have empty expression ()"))
         (prog1
           (expression 0 stream)
           (expect-token 'RIGHT-PAREN stream)))
        ((LEFT-BRACKET) (parse-list-items stream))
        ((LEFT-BRACE) (parse-block stream))
        ((IF)
         (let ((condition (expression 0 stream))
               (discard (expect-token 'LEFT-BRACE stream))
               (block (parse-block stream))
               (else-block (if (eq 'ELSE (peek-token stream))
                               (progn (advance-token stream)
                                      (expect-token 'LEFT-BRACE stream)
                                      (parse-block stream)))))
           (make-if-expr :condition condition :block block :else-block else-block)))
        ((WHILE)
         (let ((condition (expression 0 stream))
               (discard (expect-token' LEFT-BRACE stream))
               (block (parse-block stream)))
           (make-while-expr :condition condition :block block)))
        ((LOOP)
         (make-loop-expr :expr (expression 0 stream)))
        (t (error "Expected a unary operator but found ~S" token))))
    (t (error "Expected a unary operator but found ~S" token))))

(defun led (token left stream)
  (cond
    ((eq token 'PERIOD)
     (make-dot-call
       :left left
       :name (expect-token 'sym stream)
       :args (and (expect-token 'LEFT-PAREN stream) (parse-call-args stream))))
    ((eq token 'LEFT-PAREN)
     (make-call
       :left left
       :args (parse-call-args stream)))
    ((eq token 'MEMBER)
     (make-member-op
       :left left
       :property (expect-token 'sym stream)))
    ((eq token 'COND)
     (make-cond-expr
       :left left
       :clauses (parse-cond-clauses stream)))
    ((eq token 'LEFT-BRACKET)
     (make-index-op
       :left left
       :index (prog1 (expression 0 stream) (expect-token 'RIGHT-BRACKET stream))))
    ((binary-op-p token)
     (make-binop :op token :left left :right (expression (lbp token) stream)))

    (t
     (error "Expected a binary operator but found ~S" token))))

(defun expression (rbp stream)
  (iterate
    (with token = (advance-token stream))
    (with left = (nud token stream))

    (while (< rbp (lbp (peek-token stream))))
    (setq token (advance-token stream))
    (setq left (led token left stream))

    (finally (return left))))

(defun parse-block (stream)
  (iterate
    (until (eq 'right-brace (peek-token stream)))
    (collect (statement stream) into statements)
    (finally
      (expect-token'right-brace stream)
      (return (make-block :statements statements)))))

(defun parse-let (stream)
  (expect-token'LET stream)
  (let ((bindings
          (iterate
            (until (eq 'EQUALS (peek-token stream)))
            (unless (first-iteration-p)
              (expect-token 'COMMA stream))
            (collect (expect-token'SYM stream))
            (finally (advance-token stream))))
        (expr (expression 0 stream)))
    (make-let-stmt :bindings bindings :expr expr)))

;; TODO: IDEA: Control flow can only appear at the end of a block. Genius. Genius!
(defun statement (stream)
  (case (peek-token stream)
    ((LET) (parse-let stream))
    ((BREAK)
     (advance-token stream)
     (if (eq 'LEFT-PAREN (peek-token stream))
       (progn
         (advance-token stream)
         (if (eq 'RIGHT-PAREN (peek-token stream))
           (prog2 (advance-token stream) (make-break-stmt))
           (prog1 (make-break-stmt :expr (expression 0 stream))
                  (expect-token 'RIGHT-PAREN stream))))
       (make-break-stmt)))
    ((RETURN)
     (advance-token stream)
     (if (eq 'LEFT-PAREN (peek-token stream))
       (progn
         (advance-token stream)
         (if (eq 'RIGHT-PAREN (peek-token stream))
           (prog2 (advance-token stream) (make-return-stmt))
           (prog1 (make-return-stmt :expr (expression 0 stream))
                  (expect-token 'RIGHT-PAREN stream))))
       (make-return-stmt)))
    ((CONTINUE) (prog2 (advance-token stream) (make-continue-stmt)))
    (t (expression 0 stream))))

(defun parse-type (stream)
  (expect-token 'sym stream))

(defun parse-fun-args (stream)
  (expect-token 'LEFT-PAREN stream)
  (iterate
    (with rest-arg = nil)
    (until (eq 'RIGHT-PAREN (peek-token stream)))
    (unless (first-iteration-p)
      (expect-token 'COMMA stream))
    (when (eq 'ELLIPSIS (peek-token stream))
      (when rest-arg (error "Cannot have more than one rest parameter."))
      (advance-token stream)
      (let ((name (expect-token 'sym stream))
            (type (when (eq (peek-token stream) 'of)
                        (advance-token stream)
                        (parse-type stream))))
        (setq rest-arg (cons name type))
        (next-iteration)))
    (let* ((name (if (keyword-p (peek-token stream))
                   (advance-token stream)
                   (expect-token 'sym stream)))
           (type (when (eq (peek-token stream) 'of)
                   (advance-token stream)
                   (parse-type stream)))
           (arg (cons name type)))
      (if (keyword-p name) (collect arg into keywords)
                           (collect arg into args)))
    (finally
      (expect-token 'RIGHT-PAREN stream)
      (return (list :args args :keywords keywords :rest-arg rest-arg)))))

(defun parse-fun-decl (stream)
  (let ((name (expect-token 'sym stream))
        (args (parse-fun-args stream))
        (type (when (eq (peek-token stream) 'of)
                (advance-token stream)
                (parse-type stream)))
        (block (prog2 (expect-token 'LEFT-BRACE stream) (parse-block stream))))
    (make-fun-decl :name name :args args :type type :block block)))

(defun parse-fields (stream)
  (expect-token 'LEFT-PAREN stream)
  (iterate
    (until (eq 'RIGHT-PAREN (peek-token stream)))
    (unless (first-iteration-p)
      (expect-token 'COMMA stream))
    (let ((name (expect-token 'SYM stream))
          (type (when (eq 'OF (peek-token stream))
                  (advance-token stream)
                  (parse-type stream))))
      (collect (cons name type))
      (finally (expect-token 'RIGHT-PAREN stream)))))

(defun parse-type-decl (stream)
  (let ((name (expect-token 'sym stream))
        (fields (parse-fields stream)))
    (make-type-decl :name name :fields fields)))

(defun decl (stream)
  (case (advance-token stream)
    ((FUN)
     (parse-fun-decl stream))
    ((USE) (error "Unimplemented declaration 'use'"))
    ((TYPE)
     (parse-type-decl stream))
    (T (error "Unknown declaration."))))

(defun parse (stream)
  (iterate
    (while (peek-token stream))
    (collect (decl stream))))
