(in-package :wizard)

(defun resolve-symbols (tree &optional table)
  (typecase tree
    (FUN-DECl
      (let* ((name (sym-name (fun-decl-name tree)))
             (args-list
               (mapcar #'(lambda (arg) (cons (sym-name (car arg)) (cdr arg)))
                 (fun-decl-args tree)))
             (positional (getf args-list :args))
             (keywords (getf args-list :keywords))
             (rest-arg (getf args-list :rest-arg)))))))
