(in-package :wizard)

;; Types of symbol table entries
(defstruct var-entry type)
(defstruct fun-entry (type :type fun-type))
(defstruct type-entry fields fn-table) ;; alist of sym -> type

;; Compound type symbols
(defstruct fun-type
  args ;; list of types
  kwargs ;; alist of sym -> type
  rest-arg ;; type
  result) ;; list of types
(defstruct list-type item)
(defstruct values-type items)

(defun entry-type (entry)
  (typecase entry
    (VAR-ENTRY (var-entry-type entry))
    (FUN-ENTRY (fun-entry-type entry))
    (TYPE-ENTRY (error "TODO: need to associate type-entries with their constructor funs."))))

;; Simple types are: int list fun real any byte string nil error

(defun make-table (bindings parent)
  (cons bindings parent))

(defun lookup (s table)
  (when (not s)
    (return-from lookup nil))

  (if table
    (let* ((scope (car table))
           (entry (assoc s scope)))
      (if entry
        (cdr entry)
        (lookup s (cdr table))))
    nil))

(defun propagate (table tree)
  "Builds up the symbol table top down, resolving references,
  and propagates the types bottom up. Returns new symbols that are available
  to sibling nodes."
  (typecase tree
    ((OR NUM STR KEYWORD CONTINUE NULL) table)

    (SYM
      (let* ((table (if (sym-path tree)
                      (iterate
                        (with tb = table)
                        (for sym in (sym-path tree))
                        (setq tb (lookup s tb))
                        (when (not tb) (error "Could not find ~S in this scope." sym))
                        (finally (return tb)))))
             (entry (lookup (sym-name tree) table)))
        (if entry
          (prog2 (setf (expr-type tree) (entry-type entry))
                 table)
          (error "Could not find ~S in this scope." (sym-name tree)))))

    (FUN-DECL
      (let* ((name (sym-name (fun-decl-name tree)))
             (args (fun-decl-args tree))
             (kwargs (fun-decl-kwargs tree))
             (rest-arg (fun-decl-rest-arg tree))
             (arg-bindings (append args kwargs (list rest-arg))) ;; TODO: Change rest-arg type to "(list (cdr rest-arg))"
             (fun-type (make-fun-type :args args :kwargs kwargs :rest-arg rest-arg :result (fun-decl-result tree)))
             (fun-binding (cons name (make-fun-entry :type fun-type)))
             (new-table (make-table (cons fun-binding arg-bindings) table)))

        (propagate new-table (fun-decl-body tree))
        ;; TODO: check declared type against propagated type of body
        (make-table (list fun-binding) table))) ;; TODO: This is not satisfactory for forward references.

    (TYPE-DECL
      (let* ((name (sym-name (type-decl-name tree)))
             (fields (type-decl-fields tree))
             (new-type-entry
               (iterate
                 (for (f . ty) in fields)
                 (when (member f fields2)
                   (error "Field ~s was found twice in the definition of ~s" f tree))
                 (collect f into fields2)
                 (or (equalp name ty)
                     (lookup-types table ty)
                     (error "Could not find type ~s in this scope." ty))
                 (finally (return (make-type-entry :fields fields))))))

        (make-table (list (cons name new-type-entry)) table)))

    (LET-STMT
      (let* ((bindings (iterate (for b in let-stmt-bindings tree) (collect (cons (car b) (make-var-entry cdr b)))))
             (new-table (make-table bindings table)))
        (propagate table (let-stmt-expr tree))
        new-table))

    (COND-EXPR
      (error "TODO: Propagate types and symbols in a COND-EXPR"))

    (CLOSURE-LITERAL
      (error "TODO: Propagate types and symbols in a CLOSURE-LITERAL."))

    (MEMBER-OP
      (propagate table (member-op-left tree))
      (let* ((ty (or (expr-type (member-op-left tree))
                     (error "Could not determine the type of ~S" (member-op-left tree))))
             (type-entry (lookup table ty))
             (prop (member-op-property tree))
             (field (or (assoc prop (type-entry-fields type-entry) :test #'equalp)
                        (error "Property ~S is not a field in the type ~S in the expression ~S"
                          prop ty tree))))
        (setf (expr-type tree) (cdr field))))

    (DOT-CALL-EXPR
      (let* ((left (dot-call-expr-left tree))
             (_ (propagate table left))
             (ty (or (expr-type left)
                     (error "Could not determine the type of ~S" left)))
             (name (dot-call-expr-name tree))
             (ty-funs (type-entry-fn-table tree))
             (fn (or (lookup ty-funs name)
                     (error "Could not find the associated function ~S for type ~S" name ty)))
             (args (dot-call-expr-args tree))
             (kwargs (dot-call-expr-keywords tree))
             (rest-arg (dot-call-expr-rest-arg tree)))
        (iterate (for arg in args) (propagate table arg))
        (iterate (for (kw . arg) in kwargs (propagate table arg)))
        (propagate table rest-arg)
        (check-call fn (cons left args) kwargs rest-arg)
        (setf (expr-type tree) (car (fun-entry-result fn)))
        table))

    (CALL-EXPR
      (let* ((expr (call-expr-expr tree))
             (_ (propagate table tree))
             (ty (expr-type tree))
             (__ (when (not (fun-type-p ty))
                   (error "Tried to invoke a function call on the ~S ~S" ty expr)))
             (args (call-expr-args tree))
             (kwargs (call-expr-keywords tree))
             (rest-arg (call-expr-rest-arg tree)))
        (iterate (for arg in args) (propagate table arg))
        (iterate (for (kw . arg) in kwargs (propagate table arg)))
        (propagate table rest-arg)
        (setf (expr-type tree) (car (fun-entry-result fn)))
        (when ty
          (check-call ty args kwargs rest-arg))
        table))

    (BLOCK
      (iterate
        (for stmt in (block-statements tree))
        (reducing stmt by #'propagate initial-value table)
        (finally (setf (expr-type tree) (expr-type stmt))))
      table)

    (RETURN-STMT
      (propagate table (return-stmt-expr tree))
      (setf (expr-type tree) (expr-type (return-stmt-expr tree)))
      table)

    (BREAK-STMT
      (propagate table (break-stmt-expr tree))
      (setf (expr-type tree) (expr-type (break-stmt-expr tree)))
      table)

    (TRY-EXPR
      (propagate table (try-expr-block tree))
      (setf (expr-type tree) (expr-type (try-expr-block tree)))
      table)

    (LOOP
      (propagate table (loop-expr-expr tree))
      (setf (expr-type tree) (expr-type (loop-expr-expr tree)))
      table)

    (IF-EXPR
      (propagate table (if-expr-condition tree))
      (propagate table (if-expr-block tree))
      (propagate table (if-expr-else-block tree))
      ;; TODO: Join the types of the block and else-block
      table)

    (TYPE-HINT
      (propagate table (type-hint-left tree))
      (propagate table (type-hint-type tree))
      ;; TODO: Check inferred type against expected type. Insert dynamic check if necessary.
      (setf (expr-type tree) (type-hint-type tree))
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
