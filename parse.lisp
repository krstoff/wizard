(in-package :wizard)

(defstruct node attrs)

(defmacro defnode (name &rest fields)
  `(defstruct
     (,name
       (:include node)
       (:constructor ,name ,(cons '&OPTIONAL fields))
       (:constructor ,(intern (concatenate 'string "MAKE-" (symbol-name name)))))
     ,@fields))

;; Expressions
(defnode dot-call left name args keywords rest-arg)
(defnode call expr args keywords rest-arg)
(defnode index-op left index)
(defnode member-op left property)
(defnode list-literal items)
(defnode property-list-literal props)
(defnode closure-literal name args type body)
(defnode type-hint left type)
;; Control
(defnode block statements)
(defnode if-expr condition block else-block)
(defnode while-expr condition block)
(defnode loop-expr expr)
(defnode cond-expr left clauses)
(defnode try-expr block)
;; Statements
(defnode break-stmt expr)
(defnode return-stmt expr)
(defnode continue-stmt)
(defnode let-stmt bindings expr)
;; Declarations
(defnode fun-decl name args type block)
(defnode type-decl name fields)

;; If expected represents a type that is a terminal in the grammar, typechecks.
;; Otherwise, compares expected by EQL
(defun expect (expected)
  (let* ((token (advance-token))
         (relevant
           (case expected
             ((sym keyword num str) (type-of token))
             (t token))))
    (if (eql relevant expected)
      token
      (error "Expected ~S, but found ~S" expected token))))

(defun expect-ident ()
  (let ((token (expect 'sym)))
    (if (null (sym-path token))
      (sym-name token)
      (error "Expected an unqualified symbol, found ~S instead." token))))

(defun expect-keyword ()
  (let ((token (expect 'keyword)))
    (if (null (keyword-path token))
      (keyword-name token)
      (error "Expected an unqualified keyword, found ~S instead." token))))

(defun binary-op-p (token)
  (case token
    ((MUL DIV MOD ADD SUB LESS-THAN LESS-THAN-EQUALS GREATER-THAN
      GREATER-THAN-EQUALS AND OR EQUALS OF AS PERIOD) t)))

(defun lbp (token)
  (case token
    ((PERIOD MEMBER LEFT-PAREN LEFT-BRACKET OF) 80)

    ((MUL DIV MOD) 60)
    ((ADD SUB) 50)
    ((LESS-THAN LESS-THAN-EQUALS GREATER-THAN GREATER-THAN-EQUALS) 30)
    ((AND OR) 30)
    ((COND) 20)
    ((EQUALS RANGE) 10)
    (t 0)))

(defun expression (rbp)
  (iterate
    (with left = (nud (peek-token)))
    (while (< rbp (lbp (peek-token))))
    (setq left (led (peek-token) left))
    (finally (return left))))

(defun nud (token)
  (typecase token
    ((or sym keyword num str) (advance-token))
    (symbol
      (case token
        ((LEFT-PAREN)
         (advance-token)
         (when (eq (peek-token) 'RIGHT-PAREN)
           (error "Can't have empty expression ()"))
         (prog1
           (expression 0)
           (expect 'RIGHT-PAREN)))
        ((LEFT-BRACKET) (parse-list-items))
        ((LEFT-BRACE) (parse-block))
        ((IF)
         (advance-token)
         (let ((condition (expression 0))
               (block (parse-block))
               (else-block (if (eq 'ELSE (peek-token))
                               (progn (advance-token)
                                      (parse-block)))))
           (make-if-expr :condition condition :block block :else-block else-block)))
        ((WHILE)
         (advance-token)
         (let ((condition (expression 0))
               (block (parse-block)))
           (make-while-expr :condition condition :block block)))
        ((LOOP)
         (advance-token)
         (make-loop-expr :expr (expression 0)))
        ((FUN)
         (advance-token)
         (let* ((name (if (not (eq (peek-token) 'LEFT-PAREN))
                       (expect-ident)))
                (args (parse-fun-args))
                (type (when (eq (peek-token) 'OF)
                        (advance-token)
                        (parse-type)))
                (body (parse-block)))
           (closure-literal name args type body)))
        ((TRY)
         (parse-try))

        (t (error "Expected a unary operator but found ~S" token))))
    (t (error "Expected a unary operator but found ~S" token))))

(defun led (token left)
  (cond
    ((eq token 'PERIOD)
     (advance-token)
     (apply #'make-dot-call
       :left left
       :name (expect 'sym)
       (parse-call-args)))
    ((eq token 'LEFT-PAREN)
     (apply #'make-call
       :expr left
       (parse-call-args)))
    ((eq token 'MEMBER)
     (advance-token)
     (make-member-op
       :left left
       :property (expect 'sym)))
    ((eq token 'COND)
     (advance-token)
     (make-cond-expr
       :left left
       :clauses (parse-cond-clauses)))
    ((eq token 'LEFT-BRACKET)
     (advance-token)
     (make-index-op
       :left left
       :index (prog1 (expression 0) (expect 'RIGHT-BRACKET))))
    ((eq token 'OF)
     (advance-token)
     (type-hint left (parse-type)))
    ((binary-op-p token)
     (advance-token)
     (let ((name (make-sym :name token :path nil))
           (args `(,left ,(expression (lbp token)))))
       (call name args)))

    (t
     (error "Expected a binary operator but found ~S" token))))

(defun parse-list-items (&aux first-keyword)
  (expect 'LEFT-BRACKET)
  (if (and (typep (peek-token) 'keyword)
           (setq first-keyword (advance-token))
           (not (eq 'comma (peek-token))))
    ;; property list
    (iterate
      (when (first-iteration-p)
        (collect (cons first-keyword (expression 0)) into props)
        (next-iteration))
      (when (eq (peek-token) 'RIGHT-BRACKET) (finish))
      (unless (first-iteration-p)
        (expect 'COMMA))
      (collect (cons (expect 'keyword) (expression 0))
        into props)
      (finally
        (return
          (prog1
            (property-list-literal props)
            (expect 'RIGHT-BRACKET)))))
    ;; regular list
    (iterate
      (when (and (first-iteration-p) first-keyword)
        (collect first-keyword into items)
        (next-iteration))
      (when (eq (peek-token) 'RIGHT-BRACKET) (finish))
      (unless (first-iteration-p)
        (expect 'COMMA))
      (collect (expression 0) into items)
      (finally
        (return
          (prog1 (list-literal items)
                 (expect 'RIGHT-BRACKET)))))))

(defun parse-call-args ()
  (expect 'LEFT-PAREN)
  (prog1
   (iterate
     (with rest-args = nil)
     (when (eq (peek-token) 'RIGHT-PAREN) (finish))
     (unless (first-time-p)
       (expect 'COMMA))
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
       (return (list :args args :keywords keywords :rest-arg rest-args))))
   (expect 'RIGHT-PAREN)))

(defun parse-block ()
  (expect 'LEFT-BRACE)
  (iterate
    (until (eq 'right-brace (peek-token)))
    (collect (statement) into statements)
    (finally
      (expect 'right-brace)
      (return (make-block :statements statements)))))

(defun parse-let ()
  (expect 'LET)
  (let ((bindings
          (iterate
            (until (eq 'EQUALS (peek-token)))
            (unless (first-iteration-p)
              (expect 'COMMA))
            (collect (expect-ident))
            (finally (advance-token))))
        (expr (expression 0)))
    (make-let-stmt :bindings bindings :expr expr)))

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
                  (expect 'RIGHT-PAREN))))
       (make-break-stmt)))
    ((RETURN)
     (advance-token)
     (if (eq 'LEFT-PAREN (peek-token))
       (progn
         (advance-token)
         (if (eq 'RIGHT-PAREN (peek-token))
           (prog2 (advance-token) (make-return-stmt))
           (prog1 (make-return-stmt :expr (expression 0))
                  (expect 'RIGHT-PAREN))))
       (make-return-stmt)))
    ((CONTINUE) (prog2 (advance-token) (make-continue-stmt)))
    (t (expression 0))))

(defun parse-fun-args ()
  (expect 'LEFT-PAREN)
  (iterate
    (with rest-arg = nil)
    (until (eq 'RIGHT-PAREN (peek-token)))
    (unless (first-iteration-p)
      (expect 'COMMA))
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
      (expect 'RIGHT-PAREN)
      (return (list :args args :keywords keywords :rest-arg rest-arg)))))

(defun parse-type ()
  (expect 'sym))

(defun parse-try ()
  (expect 'try)
  (make-try-expr :block (parse-block)))

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
             (expect 'ARROW)
             (parse-block))))
    (labels ((check-clause (tree)
               (typecase tree
                 (null t)
                 ((or sym keyword num str) t)
                 (type-hint t)
                 (list-literal
                   (iterate (for value in (list-literal-items tree))
                     (check value)))
                 (property-list-literal
                   (iterate (for (prop . value) in (property-list-literal-props tree))
                     (check value)))
                 (t (error "Error: Expected a pattern, found the ~S ~S" (type-of tree) tree)))))
      (check-clause pattern))
    (list :pattern pattern :condition condition :block block)))

(defun parse-cond-clauses ()
  (expect 'LEFT-BRACE)
  (iterate
    (until (eq (peek-token) 'RIGHT-BRACE))
    (collect (parse-cond-clause))
    (finally (advance-token))))

(defun parse-fun-decl ()
  (expect 'fun)
  (let ((name (expect 'sym))
        (args (parse-fun-args))
        (type (when (eq (peek-token) 'of)
                (advance-token)
                (parse-type)))
        (block (parse-block)))
   (make-fun-decl :name name :args args :type type :block block)))

(defun parse-fields ()
  (expect 'LEFT-PAREN)
  (iterate
    (until (eq 'RIGHT-PAREN (peek-token)))
    (unless (first-iteration-p)
      (expect 'COMMA))
    (let ((name (expect-ident))
          (type (when (eq 'OF (peek-token))
                  (advance-token)
                  (parse-type))))
      (collect (cons name type) into fields)
      (finally (expect 'RIGHT-PAREN)
               (return fields)))))

(defun parse-type-decl ()
  (expect 'TYPE)
  (let ((name (expect-ident))
        (fields (parse-fields)))
    (make-type-decl :name name :fields fields)))

(defun decl ()
  (case (peek-token)
    ((FUN)
     (parse-fun-decl))
    ((USE) (error "Unimplemented declaration 'use'"))
    ((TYPE)
     (parse-type-decl))
    (T (error "Unknown declaration."))))
