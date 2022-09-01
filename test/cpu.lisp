
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
                #:gbcpu-flags
                #:make-gb
                #:gb-cpu
                #:emu-single-op
                #:write-memory-at-addr
                #:read-memory-at-addr
                #:flags-into-byte
                #:flags-from-byte
                #:set-reg-pair-hl-to-val
                #:get-byte-at-hl
                #:set-byte-at-hl)
  (:export #:test-cpu))

(in-package :clboy-test)


(deftest test-cpu ()
  (test-8bit-load-ops)
  (test-hl-8bit-load-ops)
  (test-set-bits)
  (test-reset-bits))


(deftest test-8bit-load-ops ()
  (let* ((gb (make-gb))
        (cpu (gb-cpu gb)))
    (loop for op from #x40 to #x7f
          when (and (not (= (logand op #xf8) #x70))
                    (not (= (logand op #x7) #x6))
                    (not (= op #x76))) do
          (let ((dest (get-dest-reg-from-op op))
                (src (get-src-reg-from-op op))
                (init-flags (flags-into-byte (gbcpu-flags cpu))))
          (write-memory-at-addr gb #xc000 op)
          (setf (gbcpu-pc cpu) #xc000)
          (set-reg gb dest 234)
          (set-reg gb src 120)
          (emu-single-op cpu gb)
          (is (= (gbcpu-pc cpu) #xc001))
          (is (= (flags-into-byte (gbcpu-flags cpu)) init-flags))
          (is (= (get-reg gb dest) 120) :ctx ("During opcode ~X the ~A register should match ~A" op dest (get-reg gb src))))
        )))

(deftest test-hl-8bit-load-ops ()
  (test-hl-8bit-load-ops-into-reg)
  (test-hl-8bit-load-ops-into-hl))

(deftest test-hl-8bit-load-ops-into-reg ()
  (let* ((gb (make-gb))
        (cpu (gb-cpu gb)))
    (loop for op from #x40 to #x7f
          when (and (= (logand op #x7) #x6)
                    (not (= op #x76))) do
          (set-reg-pair-hl-to-val cpu #xc100)
          (let ((res #x80)
                (dest (get-dest-reg-from-op op))
                (src (get-src-reg-from-op op))
                (init-flags (flags-into-byte (gbcpu-flags cpu))))
          (write-memory-at-addr gb #xc000 op)
          (setf (gbcpu-pc cpu) #xc000)
          (set-reg gb dest #xc1)
          (set-reg gb src res)
          (emu-single-op cpu gb)
          (is (= (gbcpu-pc cpu) #xc001))
          (is (= (flags-into-byte (gbcpu-flags cpu)) init-flags))
          (is (= (get-reg gb dest) res) :ctx ("During opcode ~X the ~A register should match ~A but found ~A" op dest res (get-reg gb dest))))
        )))

(deftest test-hl-8bit-load-ops-into-hl ()
  (let* ((gb (make-gb))
        (cpu (gb-cpu gb)))
    (loop for op from #x40 to #x7f
          when (and (= (logand op #xf8) #x70)
                    (not (= op #x76))) do
          (set-reg-pair-hl-to-val cpu #xc100)
          (let ((init #x10)
                (res #xc1)
                (dest (get-dest-reg-from-op op))
                (src (get-src-reg-from-op op))
                (init-flags (flags-into-byte (gbcpu-flags cpu))))
          (write-memory-at-addr gb #xc000 op)
          (setf (gbcpu-pc cpu) #xc000)
          (set-reg gb dest init)
          (set-reg gb src res)
          (emu-single-op cpu gb)
          (is (= (gbcpu-pc cpu) #xc001))
          (is (= (flags-into-byte (gbcpu-flags cpu)) init-flags))
          (is (= (get-reg gb dest) res) :ctx ("During opcode ~X the ~A register should match ~A but found ~A" op dest res (get-reg gb dest))))
        )))

;; TODO Arimithtic and Logic block 0x80 0xbf
;; TODO Bit, Rot, shift, swap
;; TODO set, reset

(deftest test-reset-bits ()
  (let* ((gb (make-gb))
        (cpu (gb-cpu gb)))
    (loop for op from #x80 to #xbf do
          (write-memory-at-addr gb #xc000 #xcb)
          (write-memory-at-addr gb #xc001 op)
          (set-reg-pair-hl-to-val cpu #xc100)
          (setf (gbcpu-pc cpu) #xc000)
          (let ((init-flags (flags-into-byte (gbcpu-flags cpu))))
            (set-reg gb (get-src-reg-from-op op) #xff)
            (emu-single-op cpu gb)
            (is (= (gbcpu-pc cpu) #xc002))
            (is (= (flags-into-byte (gbcpu-flags cpu)) init-flags))
            (is (< (get-reg gb (get-src-reg-from-op op)) #xff) :ctx ("During opcode ~X the ~A register should have bit ~A reset" op (get-src-reg-from-op op) (logand (ash op -3) #xf)))
        ))))

(deftest test-set-bits ()
  (let* ((gb (make-gb))
        (cpu (gb-cpu gb)))
    (loop for op from #xc0 to #xff do
          (write-memory-at-addr gb #xc000 #xcb)
          (write-memory-at-addr gb #xc001 op)
          (set-reg-pair-hl-to-val cpu #xc100)
          (setf (gbcpu-pc cpu) #xc000)
          (let ((init-flags (flags-into-byte (gbcpu-flags cpu))))
            (set-reg gb (get-src-reg-from-op op) #x00)
            (emu-single-op cpu gb)
            (is (= (gbcpu-pc cpu) #xc002))
            (is (= (flags-into-byte (gbcpu-flags cpu)) init-flags))
            (is (> (get-reg gb (get-src-reg-from-op op)) #x00) :ctx ("During opcode ~X the ~A register should have bit ~A set" op (get-src-reg-from-op op) (logand (ash op -3) #xf))))
        )))

(defun get-reg (gb reg)
  (let ((cpu (gb-cpu gb)))
    (case reg
      (:b (gbcpu-b cpu))
      (:c (gbcpu-c cpu))
      (:d (gbcpu-d cpu))
      (:e (gbcpu-e cpu))
      (:h (gbcpu-h cpu))
      (:l (gbcpu-l cpu))
      (:hl (get-byte-at-hl cpu gb))
      (:a (gbcpu-a cpu)))))
(defun set-reg (gb reg val)
  (let ((cpu (gb-cpu gb)))
    (case reg
      (:b (setf (gbcpu-b cpu) val))
      (:c (setf (gbcpu-c cpu) val))
      (:d (setf (gbcpu-d cpu) val))
      (:e (setf (gbcpu-e cpu) val))
      (:h (setf (gbcpu-h cpu) val))
      (:l (setf (gbcpu-l cpu) val))
      (:hl (set-byte-at-hl cpu gb val))
      (:a (setf (gbcpu-a cpu) val)))))

(defun get-dest-reg-from-op (op)
  (case (logand op #xf8)
    (#x40 :b)
    (#x48 :c)
    (#x50 :d)
    (#x58 :e)
    (#x60 :h)
    (#x68 :l)
    (#x70 :hl)
    (#x78 :a)))
(defun get-src-reg-from-op (op)
  (case (logand op #x7)
    (#x0 :b)
    (#x1 :c)
    (#x2 :d)
    (#x3 :e)
    (#x4 :h)
    (#x5 :l)
    (#x6 :hl)
    (#x7 :a)))
