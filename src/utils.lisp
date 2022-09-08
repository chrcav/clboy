

(defpackage :utils
  (:use :common-lisp)
  (:export symb))
(in-package :utils)

(defun mkstr (&rest args)
  ;;; From On Lisp, page 58.
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))))

(defun symb (&rest args)
  ;;; From On Lisp, page 58.
  (values (intern (apply #'mkstr args))))
