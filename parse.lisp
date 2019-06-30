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

(defun expect-token (expected)
  (let* ((token (advance-token))
         (relevant
           (case expected
             ((sym keyword num str) (type-of token))
             (t token))))
    (if (eq relevant expected)
      token
      (error "Expected ~S, but found ~S" expected token))))

(defun expect-ident ()
  (let ((token (expect-token 'sym)))
    (if (null (sym-path token))
      (sym-name token)
      (error "Expected an unqualified symbol, found ~S instead." token))))

(defun expect-keyword ()
  (let ((token (expect-token 'keyword)))
    (if (null (keyword-path token))
      (keyword-name token)
      (error "Expected an unqualified symbol, found ~S instead." token))))

(defun binary-op-p (token)
  (case token
    ((MUL DIV MOD ADD SUB LESS-THAN LESS-THAN-EQUALS GREATER-THAN
      GREATER-THAN-EQUALS AND OR EQUALS OF AS PERIOD) t)))

(defun parse-call-args ()
  (prog1
   (iterate
     (with rest-args = nil)
     (when (eq (peek-token) 'RIGHT-PAREN) (finish))
     (unless (first-time-p)
       (expect-token 'COMMA))
     (cond
       ((eq 'ELLIPSIS (peek-token))
        (if rest-args
            (error "You can only have one rest parameter.")
            (progn
              (advance-token)
              (setq rest-args (expression 0)))))
       ((eq 'KEYWORD (type-of (peek-token)))
        (let ((keyword (advance-token)))
          (if (or (eq 'comma (peek-token))
                  (eq 'right-paren (peek-token)))
            (collect keyword into args)
            (collect (cons (keyword-name keyword) (expression 0))
              into keywords))))
       (t
        (collect (expression 0) into args)))
     (finally
       (return (list :args args :keywords keywords :rest rest-args))))
   (expect-token 'RIGHT-PAREN)))

(defun parse-list-items (&aux first-keyword)
  (if (and (eq 'keyword (type-of (peek-token)))
           (setq first-keyword (advance-token))
           (not (eq 'comma (peek-token))))
    ;; property list
    (iterate
      (when (first-iteration-p)
        (collect (cons first-keyword (expression 0)) into props)
        (next-iteration))
      (when (eq (peek-token) 'RIGHT-BRACKET) (finish))
      (unless (first-iteration-p)
        (expect-token 'COMMA))
      (collect (cons (expect-token 'keyword) (expression 0))
        into props)
      (finally
        (return
          (prog1
            (make-property-list-literal :props props)
            (expect-token 'RIGHT-BRACKET)))))
    ;; regular list
    (iterate
      (when (and (first-iteration-p) first-keyword)
        (collect first-keyword into items)
        (next-iteration))
      (when (eq (peek-token) 'RIGHT-BRACKET) (finish))
      (unless (first-iteration-p)
        (expect-token 'COMMA))
      (collect (expression 0) into items)
      (finally
        (return
          (prog1 (make-list-literal :items items)
                 (expect-token 'RIGHT-BRACKET)))))))

(defun parse-cond-clause ()
  (let* ((pattern
          (unless (eq (peek-token) 'IF)
            (expression 0)))
         (condition
           (when (eq (peek-token) 'IF)
             (advance-token)
             (expression 0)))
         (block
           (progn
             (expect-token 'ARROW)
             (expect-token 'LEFT-BRACE)
             (parse-block))))
    (list :pattern pattern :condition condition :block block)))

(defun parse-cond-clauses ()
  (expect-token 'LEFT-BRACE)
  (iterate
    (until (eq (peek-token) 'RIGHT-BRACE))
    (collect (parse-cond-clause))
    (finally (advance-token))))

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

(defun nud (token)
  (typecase token
    ((or sym keyword num str) token)
    (symbol
      (case token
        ((LEFT-PAREN)
         (when (eq (peek-token) 'RIGHT-PAREN)
           (error "Can't have empty expression ()"))
         (prog1
           (expression 0)
           (expect-token 'RIGHT-PAREN)))
        ((LEFT-BRACKET) (parse-list-items))
        ((LEFT-BRACE) (parse-block))
        ((IF)
         (let ((condition (expression 0))
               (discard (expect-token 'LEFT-BRACE))
               (block (parse-block))
               (else-block (if (eq 'ELSE (peek-token))
                               (progn (advance-token)
                                      (expect-token 'LEFT-BRACE)
                                      (parse-block)))))
           (make-if-expr :condition condition :block block :else-block else-block)))
        ((WHILE)
         (let ((condition (expression 0))
               (discard (expect-token' LEFT-BRACE))
               (block (parse-block)))
           (make-while-expr :condition condition :block block)))
        ((LOOP)
         (make-loop-expr :expr (expression 0)))
        (t (error "Expected a unary operator but found ~S" token))))
    (t (error "Expected a unary operator but found ~S" token))))

(defun led (token left)
  (cond
    ((eq token 'PERIOD)
     (make-dot-call
       :left left
       :name (expect-token 'sym)
       :args (and (expect-token 'LEFT-PAREN) (parse-call-args))))
    ((eq token 'LEFT-PAREN)
     (make-call
       :left left
       :args (parse-call-args)))
    ((eq token 'MEMBER)
     (make-member-op
       :left left
       :property (expect-token 'sym)))
    ((eq token 'COND)
     (make-cond-expr
       :left left
       :clauses (parse-cond-clauses)))
    ((eq token 'LEFT-BRACKET)
     (make-index-op
       :left left
       :index (prog1 (expression 0) (expect-token 'RIGHT-BRACKET))))
    ((binary-op-p token)
     (make-binop :op token :left left :right (expression (lbp token))))

    (t
     (error "Expected a binary operator but found ~S" token))))

(defun expression (rbp)
  (iterate
    (with token = (advance-token))
    (with left = (nud token))

    (while (< rbp (lbp (peek-token))))
    (setq token (advance-token))
    (setq left (led token left))

    (finally (return left))))

(defun parse-block ()
  (iterate
    (until (eq 'right-brace (peek-token)))
    (collect (statement) into statements)
    (finally
      (expect-token'right-brace)
      (return (make-block :statements statements)))))

(defun parse-let ()
  (expect-token'LET)
  (let ((bindings
          (iterate
            (until (eq 'EQUALS (peek-token)))
            (unless (first-iteration-p)
              (expect-token 'COMMA))
            (collect (expect-ident))
            (finally (advance-token))))
        (expr (expression 0)))
    (make-let-stmt :bindings bindings :expr expr)))

;; TODO: IDEA: Control flow can only appear at the end of a block. Genius. Genius!
(defun statement ()
  (case (peek-token)
    ((LET) (parse-let))
    ((BREAK)
     (advance-token)
     (if (eq 'LEFT-PAREN (peek-token))
       (progn
         (advance-token)
         (if (eq 'RIGHT-PAREN (peek-token))
           (prog2 (advance-token) (make-break-stmt))
           (prog1 (make-break-stmt :expr (expression 0))
                  (expect-token 'RIGHT-PAREN))))
       (make-break-stmt)))
    ((RETURN)
     (advance-token)
     (if (eq 'LEFT-PAREN (peek-token))
       (progn
         (advance-token)
         (if (eq 'RIGHT-PAREN (peek-token))
           (prog2 (advance-token) (make-return-stmt))
           (prog1 (make-return-stmt :expr (expression 0))
                  (expect-token 'RIGHT-PAREN))))
       (make-return-stmt)))
    ((CONTINUE) (prog2 (advance-token) (make-continue-stmt)))
    (t (expression 0))))

(defun parse-type ()
  (expect-token 'sym))

(defun parse-fun-args ()
  (expect-token 'LEFT-PAREN)
  (iterate
    (with rest-arg = nil)
    (until (eq 'RIGHT-PAREN (peek-token)))
    (unless (first-iteration-p)
      (expect-token 'COMMA))
    (when (eq 'ELLIPSIS (peek-token))
      (when rest-arg (error "Cannot have more than one rest parameter."))
      (advance-token)
      (let ((name (expect-ident))
            (type (when (eq (peek-token) 'of)
                        (advance-token)
                        (parse-type))))
        (setq rest-arg (cons name type))
        (next-iteration)))
    (let* ((key (keyword-p (peek-token)))
           (name (if (keyword-p (peek-token))
                   (expect-keyword)
                   (expect-ident)))
           (type (when (eq (peek-token) 'of)
                   (advance-token)
                   (parse-type)))
           (arg (cons name type)))
      (if key (collect arg into keywords)
              (collect arg into args)))
    (finally
      (expect-token 'RIGHT-PAREN)
      (return (list :args args :keywords keywords :rest-arg rest-arg)))))

(defun parse-fun-decl ()
  (let ((name (expect-token 'sym))
        (args (parse-fun-args))
        (type (when (eq (peek-token) 'of)
                (advance-token)
                (parse-type)))
        (block (prog2 (expect-token 'LEFT-BRACE) (parse-block))))
    (make-fun-decl :name name :args args :type type :block block)))

(defun parse-fields ()
  (expect-token 'LEFT-PAREN)
  (iterate
    (until (eq 'RIGHT-PAREN (peek-token)))
    (unless (first-iteration-p)
      (expect-token 'COMMA))
    (let ((name (expect-token 'SYM))
          (type (when (eq 'OF (peek-token))
                  (advance-token)
                  (parse-type))))
      (collect (cons name type))
      (finally (expect-token 'RIGHT-PAREN)))))

(defun parse-type-decl ()
  (let ((name (expect-token 'sym))
        (fields (parse-fields)))
    (make-type-decl :name name :fields fields)))

(defun decl ()
  (case (advance-token)
    ((FUN)
     (parse-fun-decl))
    ((USE) (error "Unimplemented declaration 'use'"))
    ((TYPE)
     (parse-type-decl))
    (T (error "Unknown declaration."))))

(defun parse ()
  (iterate
    (while (peek-token))
    (collect (decl))))
