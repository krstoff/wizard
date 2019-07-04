(in-package :wizard)

(defun join (t1 t2)
  (if (and t1 t2)
      (or (eq t1 t2) (error "Could not join types ~s, ~s" t1 t2))
    (or t1 t2)))

(defun propagate-types (tree))
"Given an AST, propagate types throughtout the rest of the tree
and return the inferred type."
(typecase tree
  ((OR NULL CONTINUE-STMT) nil)

  (LIST-LITERAL
    (error "TODO: basic types"))
  (PROPERTY-LIST-LITERAL
    (error "TODO: basic types"))

  (IF-EXPR
    (let ((t1 (propagate-types (if-expr-block tree)))
          (else-block (if-expr-else-block tree)))
      (if else-block
        (type-union t1 (propagate-types else-block))
        t1)))


  (COND-EXPR
    (iterate
      (for clause in (cond-expr-clauses tree))
      (for block = (getf clause :block))
      (adjoining (propagate-types block))))


  (BREAK-STMT
    (propagate-types (break-stmt-expr tree))
    nil)
  (RETURN-STMT
    (propagate-types (return-stmt-expr tree))
    nil)

  (LET-STMT
    (if (< 1 (length let-stmt-bindings))
      (error "TODO: multi-value assignment")
      (let ((b (car (let-stmt-bindings tree)))
            (ty (propagate-types (let-stmt-expr tree))))
        (if (cdr b)
          (join b ty)
          (setf (cdr b) ty)))))

  ((BLOCK)
   (iterate
     (for statement in block-statements)
     (for ty = (propagate-types statement))
     (finally (setf (block-type tree) ty))))

  ((FUN-DECL)
   (let* ((declared (fun-decl-type tree))
          (body (fun-decl-block tree))
          (propagated (propagate-types body)))
     (if declared (join declared propagated)
       (setf (fun-decl-type tree) propagated))))

  (t (error "Do not know how to propagate types for ~S" (type-of tree))))
