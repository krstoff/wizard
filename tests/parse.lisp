(in-package :wizard-tests)

(def-suite parser-tests
  :description "Parser test suite."
  :in wizard)

(in-suite parser-tests)

(defmacro with-src (string &body body)
  `(with-input-from-string (wizard::*stream* ,string)
     ,@body))

(test lexing
  "Test token lexing"
  (with-src "1"
    (finishes (wizard::expect 'WIZARD::NUM)))
  (with-src "wizard"
    (is (equalp (wizard::make-sym :name '|wizard| :path nil)
          (wizard::expect 'WIZARD::SYM))))
  (with-src "wizard/help"
    (is (equalp (wizard::make-sym :name '|help| :path '(|wizard|))
          (wizard::expect 'WIZARD::SYM))))
  (with-src ":wizard/docs/help"
    (is (equalp (wizard::make-keyword :name '|help| :path '(|wizard| |docs|))
          (wizard::expect 'WIZARD::KEYWORD))))

  (with-src "\"load sys ops\""
    (is (equalp (wizard::make-str :contents "load sys ops")
          (wizard::expect 'WIZARD::STR))))

  (with-src ":keyword name"
    (finishes (wizard::expect-keyword))
    (finishes (wizard::expect-ident))))

(test literals
  "Parsing complex literals"
  (with-src "[] [ ]"
    (let ((expected (wizard::Make-list-literal)))
      (is (equalp expected (wizard::parse-list-items)))
      (is (equalp expected (wizard::parse-list-items)))))
  (with-src "[1]"
    (let ((expected (wizard::make-list-literal :items (list (num "1")))))
      (is (equalp expected (wizard::parse-list-items)))))
  (with-src "[1, 2, 3, 4]"
    (let ((expected (wizard::make-list-literal :items (list (num "1") (num "2") (num "3") (num "4")))))
      (is (equalp expected (wizard::parse-list-items)))))
  (with-src "[:a, :b, :c]"
    (let ((expected (wizard::make-list-literal :items (list (keyword '|a|) (keyword '|b|) (keyword '|c|)))))
      (is (equalp expected (wizard::parse-list-items)))))
  (with-src "[:name \"Kris\", :age 24]"
    (is (equalp (wizard::make-property-list-literal :props
                  `((,(keyword '|name|) . ,(str "Kris"))
                    (,(keyword '|age|) . ,(num "24"))))
          (wizard::parse-list-items)))))

(eval-when (:compile-toplevel :load-toplevel)
  (defun tree- (form)
    (cond
      ((null form)
       nil)
      ((stringp form)
       `(str ,form))
      ((numberp form)
       `(num ,(write-to-string form)))
      ((keywordp form)
       `(keyword (intern (string-downcase (symbol-name ,form)))))
      ((symbolp form)
       `(sym (quote ,form)))
      ((and (listp form) (eq 'QUOTE (car form))
        `,form))
      ((listp form)
       `(,(car form) ,@(mapcar #'tree- (cdr form)))))))

(defmacro tree (form)
  (tree- form))

(defmacro parse (string expected form)
  `(with-src ,string
     (is (equalp (tree ,expected)
           ,form))))

(defmacro parse-expr (string expected)
  `(parse ,string ,expected (wizard::expression 0)))


(test expressions
  "Parse operators and other expressions"
  (parse-expr "a - b * c + d"
    (call add
      (list (call sub
              (list |a|
                    (call mul (list |b| |c|))))
            |d|)))

  (parse-expr "b + \"help\""
    (call add
      (list |b| "help")))

  (parse-expr "(a * (b + c))"
    (call mul (list |a| (call add (list |b| |c|)))))

  (parse-expr "f(1, 2)"
    (call |f| (list 1 2)))

  (parse-expr "f(1, :rank 2, :height 40)"
    (call |f| (list 1) (list (cons '|rank| 2) (cons '|height| 40))))

  (parse-expr "f(0, ...rest)"
    (call |f| (list 0) nil |rest|))

  (parse-expr "o[1]"
    (index-op |o| 1))

  (parse-expr "o[1 + 2]"
    (index-op |o| (call add (list 1 2))))

  (parse-expr "a < b"
    (call less-than (list |a| |b|)))

  (parse-expr "o[f(20)]"
    (index-op |o| (call |f| (list 20))))

  (parse-expr "f(20)[10]"
    (index-op (call |f| (list 20))
      10))

  (parse-expr "o[10](20)"
    (call (index-op |o| 10) (list 20)))

  (parse-expr "(o[10])(20)"
    (call (index-op |o| 10) (list 20)))

  (parse-expr "o::p"
    (member-op |o| |p|))

  (parse-expr "(1 + 2)::sign-bit"
    (member-op (call ADD (list 1 2)) |sign-bit|))

  (parse-expr "{
      run()
      call(:hello)
      2
    }"
    (block (list
             (call |run|)
             (call |call| (list :hello))
             2)))

  (parse-expr "if x::ready { process(x) }"
    (if-expr (member-op |x| |ready|)
             (block (list (call |process| (list |x|))))))

  (parse-expr "if x::ready { process(x) } else { wait() }"
    (if-expr (member-op |x| |ready|)
             (block (list (call |process| (list |x|))))
             (block (list (call |wait|)))))

  (parse-expr "while true { do-thing() }"
    (while-expr |true|
      (block (list (call |do-thing|)))))

  (parse-expr "loop recv()"
    (loop-expr (call |recv|)))

  (parse-expr "fun x () { }"
    (closure-literal '|x| (list ':args nil ':keywords nil ':rest-arg nil)
      nil (block nil)))

  (parse-expr "fun (x, y) { x + y }"
    (closure-literal nil (list ':args (list (cons '|x| nil) (cons '|y| nil)) ':keywords nil ':rest-arg nil)
      nil (block (list (call ADD (list |x| |y|))))))

  (parse-expr "fun (x of i, y of i) of i { x + y }"
    (closure-literal nil (list ':args (list (cons '|x| |i|) (cons '|y| |i|)) ':keywords nil ':rest-arg nil)
      |i| (block (list (call ADD (list |x| |y|))))))

  (parse-expr "fun (x of i, :y of i, ...rest of t) of i { process(rest) }"
    (closure-literal nil (list ':args (list (cons '|x| |i|)) ':keywords (list (cons '|y| |i|)) ':rest-arg (cons '|rest| |t|))
      |i| (block (list (call |process| (list |rest|))))))

  (parse-expr "try { a() b }"
    (try-expr (block (list (call |a|) |b|))))

  (parse-expr "a of i"
    (type-hint |a| |i|))

  (parse-expr "o::p of i"
    (type-hint (member-op |o| |p|) |i|))

  (parse-expr "o.method(arg)"
    (dot-call |o| |method| (list |arg|)))

  (parse-expr
    "2 ? {
      0 -> { :zero }
      1 -> { :one }
      2 -> { :two }
    }"
    (cond-expr 2
      (list (list ':pattern 0 ':condition nil ':block (block (list :zero)))
            (list ':pattern 1 ':condition nil ':block (block (list :one)))
            (list ':pattern 2 ':condition nil ':block (block (list :two))))))

  (parse-expr
    "0 ? {
      i if even(i) -> { :even }
      0 -> { :zero }
      i -> { :num }
    }"
    (cond-expr 0
      (list (list ':pattern |i| ':condition (call |even| (list |i|)) ':block (block (list :even)))
            (list ':pattern 0 ':condition nil ':block (block (list :zero)))
            (list ':pattern |i| ':condition nil ':block (block (list :num)))))))

(test declarations
  "Parse declarations"
  (parse "fun f(x of i, :y of i, ...rest of t) of i { process(rest) }"
    (fun-decl |f| (list ':args (list (cons '|x| |i|)) ':keywords (list (cons '|y| |i|)) ':rest-arg (cons '|rest| |t|))
      |i| (block (list (call |process| (list |rest|)))))
    (decl))

  (parse "type record(name of string, age of int)"
    (type-decl '|record| (list (cons |name| |string|) (cons |age| |int|)))
    (decl)))
