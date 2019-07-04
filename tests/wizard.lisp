(in-package :wizard-tests)

(def-suite wizard
    :description "The master suite of all wizard tests.")

(in-suite wizard)

(defun test-all ()
  (run! 'wizard))
