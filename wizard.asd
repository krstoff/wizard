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
               (:file "print")))
