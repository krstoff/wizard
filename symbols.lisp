(in-package :wizard)

(defun make-table (bindings parent)
  (cons bindings parent))

(defun lookup (s table)
  (if table
    (let* ((scope (car table))
           (entry (assoc s scope)))
      (if entry
        (cdr entry)
        (lookup s (cdr table))))
    nil))

(defun tablep (tree) t) ;; Todo make this into a struct.

(defun propagate (table tree)
  "Builds up the symbol table top down, resolving references,
  and propagates the types bottom up. Returns new symbols that are available
  to sibling nodes."
  (typecase tree
    ((OR NUM STR KEYWORD CONTINUE NULL) table)

    (SYM
      (let* ((table (if (sym-path tree)
                      (iterate
                        (with t = table)
                        (for sym in (sym-path tree))
                        (setq t (lookup s t))
                        (when (not t) (error "Could not find ~S in this scope." sym))
                        (finally (return t)))))
             (entry (lookup (sym-name tree) table)))
        (if entry
          (prog2 (setf (getf (node-attrs tree) :type) (getf (node-attrs entry) :type))
                 table)
          (error "Could not find ~S in this scope." (sym-name tree)))))

    (FUN-DECL
      (let* ((name (sym-name (fun-decl-name tree)))
             (fun-binding (cons name tree))
             (args (fun-decl-args tree))
             (arg-bindings (append (getf args :args) (getf args :keywords) (list (getf args :rest-arg))))
             (new-table (make-table (cons fun-binding arg-bindings) table)))

        (setf (getf (node-attrs tree) :type) (make-fun-type tree))
        (propagate new-table (fun-decl-body tree))
        ;; TODO: check declared type against propagated type of body
        (make-table (list fun-binding) table))) ;; TODO: This is not satisfactory for forward references.

    (TYPE-DECL
      (let* ((name (sym-name (type-decl-name tree)))
             (type-binding (cons name tree))
             (new-table (make-table (list type-binding) table))
             (fields (type-decl-fields)))
        (iterate
          (for (f . ty) in fields)
          (propagate new-table ty)
          (when (member f fields)
            (error "Field ~s was found twice in the definition of ~s" f tree))
          (collect f into fields2))))

    (LET-STMT
      (let* ((bindings (iterate (for b in let-stmt-bindings tree) (collect (cons b tree))))
             (new-table (make-table bindings table)))
        (propagate table (let-stmt-expr tree))
        new-table))

    (COND-EXPR
      (error "TODO: Propagate types and symbols in a COND-EXPR"))

    (CLOSURE-LITERAL
      (error "TODO: Propagate types and symbols in a CLOSURE-LITERAL."))

    (MEMBER-OP
      (propagate table (member-op-left tree))
      (let ((ty (getf (member-op-left tree) :type))
            (prop (member-op-property tree)))
        (if ty
          (if (get-type-field ty prop)
              table
              (error "Property ~S is not a field in the type ~S in the expression ~S"
                prop ty tree))
          (error "Could not determine the type of ~S" (member-op-left tree)))))

    (DOT-CALL-EXPR
      (let* ((left (dot-call-expr-left tree))
             (_ (propagate table left))
             (ty (or (getf left :type)
                     (error "Could not determine the type of ~S" left)))
             (name (dot-call-expr-name tree))
             (fn (or (get-type-function ty name)
                     (error "Could not find the associated function ~S for type ~S" name ty)))
             (args (dot-call-expr-args tree))
             (kwargs (dot-call-expr-keywords tree))
             (rest-arg (dot-call-expr-rest-arg tree)))
        (iterate (for arg in args) (propagate table arg))
        (iterate (for (kw . arg) in kwargs (propagate table arg)))
        (propagate table rest-arg)
        (check-call (fn-type fn) (cons left args) kwargs rest-arg)))

    (CALL-EXPR
      (let* ((expr (call-expr-expr tree))
             (_ (propagate table tree))
             (ty (getf expr :type))
             (args (call-expr-args tree))
             (kwargs (call-expr-keywords tree))
             (rest-arg (call-expr-rest-arg tree)))
        (iterate (for arg in args) (propagate table arg))
        (iterate (for (kw . arg) in kwargs (propagate table arg)))
        (propagate table rest-arg)
        (check-call ty args kwargs rest-arg)))

    (BLOCK
      (iterate
        (for stmt in (block-statements tree))
        (reducing stmt by #'propagate initial-value table))
      table)

    (RETURN-STMT
      (propagate table (return-stmt-expr tree))
      table)

    (BREAK-STMT
      (propagate table (break-stmt-expr tree))
      table)

    (TRY-EXPR
      (propagate table (try-expr-block tree))
      table)

    (LOOP
      (propagate table (loop-expr-expr tree))
      table)

    (IF-EXPR
      (propagate table (if-expr-condition tree))
      (propagate table (if-expr-block tree))
      (propagate table (if-expr-else-block tree))
      table)

    (TYPE-HINT
      (propagate table (type-hint-left tree))
      (propagate table (type-hint-type tree))
      table)

    (PROPERTY-LIST-LITERAL
      (iterate
        (for (prop . expr) in (property-list-literal-props tree))
        (propagate table expr))
      table)

    (LIST-LITERAL
      (iterate
        (for item in (list-literal-items tree))
        (propagate table item)))

    (INDEX-OP
      (propagate table (index-op-left tree))
      (propagate table (index-op-index tree)))))
