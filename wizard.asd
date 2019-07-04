(defsystem :wizard
  :description "Wizard is a notation for magic."
  :version "0.0.1"
  :author "Kristoffer Semelka <ksemelka@gmail.com>"
  :license "Mozilla Public License 2.0"
  :depends-on (:iterate)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "lex")
               (:file "parse")
               (:file "print"))
  :in-order-to ((test-op (test-op "wizard/tests"))))

(defsystem :wizard/tests
  :depends-on (:wizard :fiveam)
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "wizard")
                             (:file "parse"))))
  :perform (test-op (o s)
             (uiop:symbol-call :fiveam :run!
               (uiop:find-symbol* :wizard :wizard-tests))))
