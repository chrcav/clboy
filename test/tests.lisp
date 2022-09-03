



(in-package clboy-test)

(deftest test-all ()
  (test-cpu))


(defun test (&key (debug nil) (print 'unexpected) (describe 'unexpected))
  (warn-on-tests-not-run ((find-package :clboy-test))
    (print (try 'test-all :debug debug :print print :describe describe))))


(test)
