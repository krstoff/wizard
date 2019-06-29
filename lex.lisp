(in-package :wizard)

(defun peek (stream)
  "Equivalent to PEEK-CHAR NIL STREAM NIL NIL"
  (peek-char nil stream nil nil))

(defun advance (stream)
    "Equivalent to READ-CHAR STREAM NIL NIL"
    (read-char stream nil nil))

(defun whitespace (char)
  (case char
    ((#\SPACE #\NEWLINE #\TAB) t)))

(defun symbol-start-char-p (char)
  (or (alpha-char-p char) (char= char #\_) (char= char #\-)))
(defun symbol-char-p (char)
  (or (alphanumericp char) (char= char #\_) (char= char #\-)))

(defun trim-whitespace-and-comments (stream)
  (iterate
    (for char = (or (peek stream) (terminate)))
    (cond
      ;; comments
      ((char= #\# char)
       (iterate
         (initially (advance stream))
         (while (not (char= #\NEWLINE (or (peek stream) (finish)))))
         (advance stream)))
      ;; whitespace
      ((whitespace char)
       (iterate
         (initially (advance stream))
         (while (whitespace (or (peek stream) (finish))))
         (advance stream)))
      ;; we're done!
      (t (finish)))))


;; token types
(defstruct sym name path)
(defstruct keyword name path)
(defstruct num digits)
(defstruct str contents) ;; contents MAY be nil for empty strings

(defun lex-symbol (stream &aux (char (peek stream)))
  "Returns a buffer of characters for a single symbol."
  (assert (and (characterp char) (symbol-start-char-p char)))
  (iterate
    (with buf = (new-vec))
    (for char next (or (advance stream) (terminate)))
    (for next-char = (peek stream))
    (vector-push-extend char buf)
    (while (and next-char (symbol-char-p next-char)))
    (finally (return (intern buf)))))

(defun lex-digits (stream &aux (char (peek stream)))
  "Returns a buffer of characters for a sequence of digits."
  (assert (and (characterp char) (digit-char-p char)))
  (iterate
    (for char next (or (advance stream) (terminate)))
    (for next-char = (peek stream))
    (collect char into buf result-type 'vector)
    (while (and next-char (digit-char-p next-char)))
    (finally (return buf))))

(defun lex-string (stream)
  "Returns a buffer of characters for a string literal."
  (assert (char= #\" (advance stream)))
  (iterate
    (for char next (or (peek stream) (error "Unexpected end of file while reading string.")))
    (while (not (char= #\" char)))
    (when (char= #\\ char)
      (advance stream)
      ;; TODO: switch on the escape character
      (collect (or (advance stream (error "Unexpected end of file while reading string.")))
               into buf)
      (next-iteration))
    (collect (advance stream) into buf)
    (finally
      (advance stream)
      (return buf))))

;; CTOR :name NAME :path PATH for any qualified symbol
(defun lex-syms (ctor stream)
  (iterate
    (with path = nil)
    (with sym = (lex-symbol stream))
    (while (and (peek stream) (char= #\/ (peek stream))))
    (advance stream)
    (push sym path)
    (setq sym (lex-symbol stream))
    (finally
      (return (funcall ctor :name sym :path (nreverse path))))))

;; an a-list of reserved words ands
(defparameter +reserved-words+
  (mapcar #'(lambda (s) (cons (string-downcase (symbol-name s)) s))
    '(let try if else for while loop break return continue of as
      fun type pub case open
      and or not
      int list float any byte string nil error _)))
;; Takes a vector of chars and tries to convert it into a symbol representing a reserved word.
(defun make-reserved (chars &aux (string (symbol-name chars)))
  (cdr (assoc string +reserved-words+ :test #'equalp)))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun fsm-push (fsm chars value)
  (if (null chars)
    (if (assoc :VALUE fsm)
        (progn
          (setf (cdr (assoc :VALUE fsm)) value)
          fsm)
        (acons :value value fsm))
    (if (assoc (car chars) fsm)
      (progn
        (setf (cdr (assoc (car chars) fsm))
          (fsm-push (cdr (assoc (car chars) fsm)) (cdr chars) value))
        fsm)
      (acons (car chars) (fsm-push nil (cdr chars) value) fsm)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun make-fsm (rows) ;; rows has the form (("op" 'op) ...)
   (flet ((join (fsm op)
            (fsm-push fsm (coerce (car op) 'list) (cadr op))))
     (reduce #'join rows :initial-value nil))))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun make-cases (fsm s)
   (iterate
     (for case in fsm)
     (if (eq (car case) :value)
       (next-iteration))
     (collect `(,(car case)
                (progn (advance stream)
                  ,(make-state-transitions
                     (cdr (assoc (car case) fsm))
                     (cat s (car case)))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun make-state-transitions (fsm &optional (s ""))
   (cond
    ;; siblings, but no node
    ((null (assoc :value fsm))
     `(progn
        (case (peek stream)
          ,@(make-cases fsm s)
          ((nil) (error "Unknown operator ~a" ,s))
          (t (error "Unknown operator ~a" (cat ,s (peek stream)))))))
    ;; node, but no siblings
    ((null (set-difference fsm '((:value . nil)) :key #'car))
     `(progn
        (quote ,(cdr (assoc :value fsm)))))
    ;; node and siblings
    (t
      `(progn
         (case (peek stream)
           ,@(make-cases fsm s)
           (t (progn
                (advance stream)
                (quote ,(cdr (assoc :value fsm)))))))))))

(defmacro lex-operators (&rest table-rows)
  (sort table-rows #'string< :key #'car)
  (let ((op-chars (mapcar (lambda (op) (elt (car op) 0)) table-rows))
        (fsm (make-fsm table-rows)))
    `(case (peek stream)
       (,op-chars (return-from next-token ,(make-state-transitions fsm))))))

(defun next-token (stream &aux char)
  (trim-whitespace-and-comments stream)
  (setq char (peek stream))
  (when (not char) (return-from next-token nil))
  ;; This will return if any operators are matched.
  (lex-operators
    ("(" LEFT-PAREN) (")" RIGHT-PAREN) ("[" LEFT-BRACKET) ("]" RIGHT-BRACKET)
    ("{" LEFT-BRACE) ("}" RIGHT-BRACE) ("," COMMA) ("." PERIOD) (".." RANGE)
    ("..." ELLIPSIS) (";" SEMICOLON) ("@" AT) ("=" EQUALS) ("->" ARROW)
    ("+" ADD) ("*" MUL) ("-" SUB) ("/" DIV) ("%" MOD)
    ("+=" ADD-ASSIGN) ("*=" MUL-ASSIGN) ("-=" SUB-ASSIGN) ("/=" DIV-ASSIGN) ("%=" MOD-ASSIGN)
    ("<" LESS-THAN) (">" GREATER-THAN) ("<=" LESS-THAN-EQUALS) (">=" GREATER-THAN-EQUALS)
    ("?" COND))

  (cond
    ((symbol-start-char-p char)
     (let ((s (lex-syms #'make-sym stream)))
       ;; Try to convert this into a reserved word. Otherwise, it's a regular symbol.
       (or (and (null (sym-path s))
                (make-reserved (sym-name s))) ;; nil if fails
           s)))
    ((digit-char-p char)
     (make-num :digits (lex-digits stream)))

    ((char= #\" char)
     (make-str :contents (lex-string stream)))

    ((char= #\: char)
     (progn
       (advance stream)
       (if (and (peek stream) (char= (peek stream) #\:))
         (prog2 (advance stream) 'MEMBER))
       (lex-syms #'make-keyword stream)))

    (nil nil)
    ;; DEBUG
    (t (print (advance stream)))))

(defun tokenize (stream)
  (iterate
    (for token = (next-token stream))
    (while token)
    (collect token)))

(defvar *token* nil)

(defun peek-token (stream)
  (or *token* (setf *token* (next-token stream))))

(defun advance-token (stream)
  (if *token*
    (prog1 *token* (setf *token* nil))
    (next-token stream)))
