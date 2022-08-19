

(in-package :clboy)

(defstruct gbspu)

(defun spu-read-memory-at-addr (gbspu addr)
  (case addr
    (otherwise #xff)))

(defun spu-write-memory-at-addr (gbspu addr val)
  (case addr
    (otherwise ())))
