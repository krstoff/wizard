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

(defun resolve-symbols (tree &optional table)
  (typecase tree
    ((OR NULL CONTINUE NUM STR) nil)
    (SYM
      (if (null (sym-path tree))
        (if (lookup (sym-name tree) table)
          (values)
          (error "~S is undefined", (sym-name tree))))
      (error "TODO: symbol lookup with paths."))
    (FUN-DECl
      (let* ((name (sym-name (fun-decl-name tree)))
             (args-list (fun-decl-args tree))
             (positional (getf args-list :args))
             (keywords (getf args-list :keywords))
             (rest-arg (getf args-list :rest-arg))
             (bindings (append (list (cons name tree) rest-arg)
                         positional keywords rest-arg))
             (block (fun-decl-block tree))
             (new-table (make-table bindings table)))
        (resolve-symbols block new-table)))
    (BLOCK
      (let ((stmts (block-statements tree)))
        (iterate
          (for stmt in stmts)
          (reducing stmt by (lambda (table st) (resolve-symbols st table))))
        (values)))
    (LET-STMT
      (resolve-symbols (let-stmt-expr tree) table)
      (make-table (let-stmt-bindings tree) table))

    (COND-EXPR
      (error "TODO: Map patterns to new bindings."))

    (BINOP
      (resolve-symbols (binop-left tree) table)
      (resolve-symbols (binop-right tree) table)
      (values))

    (IF-EXPR
      (resolve-symbols (if-expr-condition tree) table)
      (resolve-symbols (if-expr-block tree) table)
      (resolve-symbols (if-expr-else tree) table)
      (values))

    (WHILE-EXPR
      (resolve-symbols (while-expr-condition tree) table)
      (resolve-symbols (while-expr-block tree) table)
      (values))

    (LOOP-EXPR
      (resolve-symbols (loop-expr-expr tree) table)
      (values))

    (BREAK-STMT (resolve-symbols (break-stmt-expr tree) table) (values))
    (RETURN-STMT (resolve-symbols (return-stmt-expr tree) table) (values))

    (CALL
      (resolve-symbols (call-left tree) table)
      (let* ((args (call-args tree))
             (positional (getf args :args))
             (keywords (getf args :keywords))
             (rest-arg (getf args :rest-arg)))
        (mapc #'(lambda (arg) (resolve-symbols arg table)) args)
        (mapc #'(lambda (kw) (resolve-symbols (cdr kw) table)) keywords)
        (resolve-symbols rest-arg table))
      (values))

    (DOT-CALL
      (error "TODO: Type resolution."))


    (INDEX-OP
      (resolve-symbols (index-op-left tree) table)
      (resolve-symbols (index-op-index tree) table)
      (values))

    (MEMBER-OP
      (resolve-symbols (member-op-left tree) table)
      (resolve-symbols (member-op-property tree) table)
      (values))

    (LIST-LITERAL
      (let ((items (list-literal-items tree)))
        (mapc #'(lambda (item) (resolve-symbols item table))) items
        (values)))
    (PROPERTY-LIST-LITERAL
      (let ((props (property-list-literal-props tree)))
        (iterate
          (for (p . e) in props)
          (resolve-symbols e table))
        (values)))))
