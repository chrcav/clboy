
(defpackage :clboy-test
  (:use :common-lisp :clboy :try)
  (:import-from :clboy
                #:gbcpu-pc
                #:gbcpu-a
                #:gbcpu-b
                #:gbcpu-c
                #:gbcpu-d
                #:gbcpu-e
                #:gbcpu-h
                #:gbcpu-l
                #:make-gb
                #:gb-cpu
                #:emu-single-op
                #:write-memory-at-addr)
  (:export #:test-cpu))

(in-package :clboy-test)


(deftest test-cpu ()
  (test-8bit-load-ops))


(deftest test-8bit-load-ops ()
  (let* ((gb (make-gb))
        (cpu (gb-cpu gb)))
    (loop for op from #x40 to #x7f
          when (and (not (= (logand op #xf8) #x70))
                    (not (= (logand op #x7) #x6))
                    (not (= op #x76))) do
          (write-memory-at-addr gb #xc000 op)
          (setf (gbcpu-pc cpu) #xc000)
          (set-before cpu op 234)
          (set-after cpu op 120)
          (emu-single-op cpu gb)
          (is (= (gbcpu-pc cpu) #xc001))
          (is (= (get-after cpu op) 120))
        )))

(defun set-before (cpu op val)
  (case (logand op #xf8)
    (#x40 (setf (gbcpu-b cpu) val))
    (#x48 (setf (gbcpu-c cpu) val))
    (#x50 (setf (gbcpu-d cpu) val))
    (#x58 (setf (gbcpu-e cpu) val))
    (#x60 (setf (gbcpu-h cpu) val))
    (#x68 (setf (gbcpu-l cpu) val))
    (#x70 (setf (gbcpu-a cpu) val)) ; TODO needs to set memory at hl
    (#x78 (setf (gbcpu-a cpu) val))))
(defun get-after (cpu op)
  (case (logand op #xf8)
    (#x40 (gbcpu-b cpu))
    (#x48 (gbcpu-c cpu))
    (#x50 (gbcpu-d cpu))
    (#x58 (gbcpu-e cpu))
    (#x60 (gbcpu-h cpu))
    (#x68 (gbcpu-l cpu))
    (#x70 (gbcpu-a cpu)) ; TODO needs to get memory at hl
    (#x78 (gbcpu-a cpu))))
(defun set-after (cpu op val)
  (case (logand op #x7)
    (#x0 (setf (gbcpu-b cpu) val))
    (#x1 (setf (gbcpu-c cpu) val))
    (#x2 (setf (gbcpu-d cpu) val))
    (#x3 (setf (gbcpu-e cpu) val))
    (#x4 (setf (gbcpu-h cpu) val))
    (#x5 (setf (gbcpu-l cpu) val))
    (#x6 (setf (gbcpu-a cpu) val)) ; TODO needs to set memory at hl
    (#x7 (setf (gbcpu-a cpu) val))))

