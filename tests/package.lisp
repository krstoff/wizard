(defpackage #:wizard-tests
  (:shadowing-import-from wizard
    keyword str num sym
    add mul sub div mod of
    add-assign mul-assign sub-assign div-assign mod-assign
    less-than less-than-equals
    greater-than greater-than-equals
    call index-op member-op block
    if-expr while-expr loop-expr
    closure-literal try-expr type-hint
    dot-call cond-expr
    decl fun-decl type-decl)
  (:use cl fiveam wizard)

  (:export test-all))
