

(defpackage :clboy-test
  (:use common-lisp clboy-test.opcodes try))

(in-package clboy-test)

(deftest test-all ()
  (clboy-test.opcodes:test-cpu)
  (clboy-test.cart:test-carts))


(defun test (&key (debug nil) (print 'unexpected) (describe 'unexpected))
  (warn-on-tests-not-run ((find-package :clboy-test))
    (print (try 'test-all :debug debug :print print :describe describe))))


(test)
