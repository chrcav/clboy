

(in-package :clboy)
(defparameter *memory* (make-array #x10000 :element-type '(unsigned-byte 8)))
(defun write-memory-at-addr (gb addr val)
  (setf (aref *memory* addr) val))

(defun read-memory-at-addr (gb addr)
  (aref *memory* addr))

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
                #:gbcpu-sp
                #:gbcpu-flags
                #:copy-gbflags
                #:gbflags-z
                #:gbflags-n
                #:gbflags-h
                #:gbflags-c
                #:make-gb
                #:make-gbflags
                #:gb-cpu
                #:step-cpu
                #:flags-into-byte
                #:flags-from-byte
                #:read-memory-at-addr
                #:write-memory-at-addr
                #:get-address-from-reg-pair
                #:set-reg-pair-hl-to-val
                #:get-byte-at-hl
                #:push-addr-on-stack
                #:peek-addr-from-stack
                #:set-byte-at-hl)
  (:export #:test-cpu))

(in-package :clboy-test)


(deftest test-cpu ()
  (test-cpu-8bit-ops)
  (test-cpu-16bit-ops))

(deftest test-cpu-8bit-ops ()
  (test-cpu-8bit-load-ops)
  (test-cpu-8bit-arith-and-logic-ops)
  (test-cpu-8bit-bit-ops))

(deftest test-cpu-8bit-load-ops ()
  (loop for fin-val in (list 0 #xff #xf0 #x0f) do
    (test-8bit-load-ops fin-val)
    (test-8bit-load-imm-ops fin-val)))

(deftest test-cpu-8bit-arith-and-logic-ops ()
  (loop for init-val in (list 0 #xff #xf0 #x0f) do
    (loop for fin-val in (list 0 #xff #xf0 #x0f) do
      (test-8bit-load-ops fin-val)
      (test-8bit-load-imm-ops fin-val)
      (test-8bit-add-ops init-val fin-val)
      (test-adc-ops init-val fin-val)
      (test-sub-ops init-val fin-val)
      (test-sbc-ops init-val fin-val)
      (test-and-ops init-val fin-val)
      (test-xor-ops init-val fin-val)
      (test-or-ops init-val fin-val)
      (test-cp-ops init-val fin-val))
    (test-8bit-inc-ops init-val)
    (test-8bit-dec-ops init-val)))

(deftest test-cpu-8bit-bit-ops ()
  (loop for init-val in (list 0 #xff #xf0 #x0f) do
    (test-rlc-ops init-val)
    (test-rrc-ops init-val)
    (test-rl-ops init-val)
    (test-rr-ops init-val)
    (test-sla-ops init-val)
    (test-sra-ops init-val)
    (test-srl-ops init-val)
    (test-swap-ops init-val)
    (test-bit-ops init-val)
    (test-set-bit-ops init-val)
    (test-reset-bit-ops init-val)))

(deftest test-cpu-16bit-ops ()
  (loop for init-val in (list 0 #x00ff #xff00 #xffff) do
    (loop for fin-val in (list 0 #x00ff #xff00 #xffff) do
      (test-16bit-add-ops init-val fin-val))
    (test-16bit-load-imm-ops init-val)
    (test-16bit-inc-ops init-val)
    (test-16bit-dec-ops init-val)
    (test-push-ops init-val)
    (test-pop-ops init-val)))


(deftest test-load-op (op dest src fin-val)
  (let* ((gb (make-gb))
        (cpu (gb-cpu gb))
        (init-pc #xc000)
        (init-flags (flags-into-byte (gbcpu-flags cpu))))
    (when (= init-pc (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu)))
      (setf init-pc (+ init-pc 1)))
    (write-memory-at-addr gb init-pc op)
    (setf (gbcpu-pc cpu) init-pc)
    (set-reg gb src fin-val)
    (step-cpu cpu gb)
    (if (eq src :imm)
      (is (= (gbcpu-pc cpu) (+ init-pc 2)))
      (if (eq src :imm16)
        (is (= (gbcpu-pc cpu) (+ init-pc 3)))
        (is (= (gbcpu-pc cpu) (+ init-pc 1)))))
    (is (= (flags-into-byte (gbcpu-flags cpu)) init-flags))
    (is (= (get-reg gb dest) fin-val)
        :ctx ("During opcode ~X the ~A register should match ~A" op dest fin-val))))

(deftest test-8bit-load-ops (fin-val)
  (loop for op from #x40 to #x7f
        when (not (= op #x76)) do
        (test-load-op op (get-dest-reg-from-op op) (get-src-reg-from-op op) fin-val)))

(deftest test-8bit-load-imm-ops (fin-val)
  (loop for op in '((#x06 :b) (#x0e :c) (#x16 :d) (#x1e :e)
                    (#x26 :h) (#x2e :l) (#x36 :<hl) (#x3e :a)) do
        (test-load-op (car op) (cadr op) :imm fin-val)))

(deftest test-16bit-load-imm-ops (fin-val)
  (loop for op in '((#x01 :bc) (#x11 :de) (#x21 :hl) (#x31 :sp)) do
        (test-load-op (car op) (cadr op) :imm16 fin-val)))

(deftest test-8bit-inc-op (op reg init-val)
  (let* ((gb (make-gb))
         (cpu (gb-cpu gb))
         (init-pc #xc000)
         (init-flags (copy-gbflags (gbcpu-flags cpu)))
         (res (logand (+ init-val 1) #xff)))
    (when (eq reg :<hl)
      (set-reg-pair-hl-to-val cpu #xc100))
    (set-reg gb reg init-val)
    (write-memory-at-addr gb init-pc op)
    (setf (gbcpu-pc cpu) init-pc)
    (step-cpu cpu gb)
    (is (= (gbcpu-pc cpu) (+ init-pc 1)))
    (is (= (gbflags-z (gbcpu-flags cpu))
           (if (= res #x0) 1 0)))
    (is (= (gbflags-n (gbcpu-flags cpu)) 0))
    (is (= (gbflags-h (gbcpu-flags cpu)) (if (> (+ (logand init-val #xf) 1) #xf) 1 0)))
    (is (= (gbflags-c (gbcpu-flags cpu)) (gbflags-c init-flags)))
    (is (= (get-reg gb reg) res)
        :ctx ("During opcode ~X the ~A register should hold the result of ~A + ~A" op reg init-val 1))))

(deftest test-8bit-inc-ops (init-val)
  (loop for op in '((#x04 :b) (#x0c :c) (#x14 :d) (#x1c :e)
                    (#x24 :h) (#x2c :l) (#x34 :<hl) (#x3c :a)) do
        (test-8bit-inc-op (car op) (cadr op) init-val)))

(deftest test-8bit-dec-op (op reg init-val)
  (let* ((gb (make-gb))
         (cpu (gb-cpu gb))
         (init-pc #xc000)
         (init-flags (copy-gbflags (gbcpu-flags cpu)))
         (res (logand (- init-val 1) #xff)))
    (when (eq reg :<hl)
      (set-reg-pair-hl-to-val cpu #xc100))
    (set-reg gb reg init-val)
    (write-memory-at-addr gb init-pc op)
    (setf (gbcpu-pc cpu) init-pc)
    (step-cpu cpu gb)
    (is (= (gbcpu-pc cpu) (+ init-pc 1)))
    (is (= (gbflags-z (gbcpu-flags cpu))
           (if (= res #x0) 1 0)))
    (is (= (gbflags-n (gbcpu-flags cpu)) 1))
    (is (= (gbflags-h (gbcpu-flags cpu)) (if (= (logand init-val #xf) 0) 1 0)))
    (is (= (gbflags-c (gbcpu-flags cpu)) (gbflags-c init-flags)))
    (is (= (get-reg gb reg) res)
        :ctx ("During opcode ~X the ~A register should hold the result of ~A - ~A" op reg init-val 1))))

(deftest test-8bit-dec-ops (init-val)
  (loop for op in '((#x05 :b) (#x0d :c) (#x15 :d) (#x1d :e)
                    (#x25 :h) (#x2d :l) (#x35 :<hl) (#x3d :a)) do
        (test-8bit-dec-op (car op) (cadr op) init-val)))

(deftest test-16bit-inc-op (op reg init-val)
  (let* ((gb (make-gb))
         (cpu (gb-cpu gb))
         (init-pc #xc000)
         (init-flags (flags-into-byte (gbcpu-flags cpu)))
         (res (logand (+ init-val 1) #xffff)))
    (when (eq reg :<hl)
      (set-reg-pair-hl-to-val cpu #xc100))
    (set-reg gb reg init-val)
    (write-memory-at-addr gb init-pc op)
    (setf (gbcpu-pc cpu) init-pc)
    (step-cpu cpu gb)
    (is (= (gbcpu-pc cpu) (+ init-pc 1)))
    (is (= (flags-into-byte (gbcpu-flags cpu)) init-flags))
    (is (= (get-reg gb reg) res)
        :ctx ("During opcode ~X the ~A register should hold the result of ~A + ~A" op reg init-val 1))))

(deftest test-16bit-inc-ops (init-val)
  (loop for op in '((#x03 :bc) (#x13 :de) (#x23 :hl) (#x33 :sp)) do
        (test-16bit-inc-op (car op) (cadr op) init-val)))

(deftest test-16bit-dec-op (op reg init-val)
  (let* ((gb (make-gb))
         (cpu (gb-cpu gb))
         (init-pc #xc000)
         (init-flags (flags-into-byte (gbcpu-flags cpu)))
         (res (logand (- init-val 1) #xffff)))
    (when (eq reg :<hl)
      (set-reg-pair-hl-to-val cpu #xc100))
    (set-reg gb reg init-val)
    (write-memory-at-addr gb init-pc op)
    (setf (gbcpu-pc cpu) init-pc)
    (step-cpu cpu gb)
    (is (= (gbcpu-pc cpu) (+ init-pc 1)))
    (is (= (flags-into-byte (gbcpu-flags cpu)) init-flags))
    (is (= (get-reg gb reg) res)
        :ctx ("During opcode ~X the ~A register should hold the result of ~A - ~A" op reg init-val 1))))

(deftest test-16bit-dec-ops (init-val)
  (loop for op in '((#x0b :bc) (#x1b :de) (#x2b :hl) (#x3b :sp)) do
        (test-16bit-dec-op (car op) (cadr op) init-val)))

(deftest test-acc-op (op src init-val other-val res res-flags-byte)
  (let* ((gb (make-gb))
         (cpu (gb-cpu gb))
         (init-pc #xc000))
    (set-reg-pair-hl-to-val cpu #xc100)
    (set-reg gb :a init-val)
    (set-reg gb src other-val)
    (write-memory-at-addr gb init-pc op)
    (setf (gbcpu-pc cpu) init-pc)
    (step-cpu cpu gb)
    (if (eq src :imm)
      (is (= (gbcpu-pc cpu) (+ init-pc 2)))
      (is (= (gbcpu-pc cpu) (+ init-pc 1))))
    (is (= (flags-into-byte (gbcpu-flags cpu)) res-flags-byte))
    (is (= (gbcpu-a cpu) res)
        :ctx ("During opcode ~X the :A register should hold the result of ~A + ~A" op init-val other-val))))

(deftest test-8bit-add-ops (init-val other-val)
  (loop for op in '((#x80 :b) (#x81 :c) (#x82 :d) (#x83 :e)
                    (#x84 :h) (#x85 :l) (#x86 :<hl) (#x87 :a) (#xc6 :imm)) do
    (if (= (car op) #x87)
      (test-acc-op (car op) (cadr op) init-val init-val (logand (+ init-val init-val) #xff) (flags-into-byte (calc-8bit-add-flags init-val init-val)))
      (test-acc-op (car op) (cadr op) init-val other-val (logand (+ init-val other-val) #xff) (flags-into-byte (calc-8bit-add-flags init-val other-val))))))

(defun calc-8bit-add-flags (init-val other-val &optional (carry 0))
  (make-gbflags
    :z (if (= (logand (+ init-val other-val carry) #xff) #x0) 1 0)
    :n  0
    :h (if (> (+ (logand init-val #xf) (logand other-val #xf) carry) #xf) 1 0)
    :c (if (> (+ init-val other-val carry) #xff) 1 0)))

(defun calc-16bit-add-flags (init-val other-val)
  (make-gbflags
    :z (if (= (logand (+ init-val other-val) #xffff) #x0) 1 0)
    :n  0
    :h (if (> (+ (logand init-val #xfff) (logand other-val #xfff)) #xfff) 1 0)
    :c (if (> (+ init-val other-val) #xffff) 1 0)))

(deftest test-16bit-add-op (op src init-val other-val res re-flag-byte)
  (let* ((gb (make-gb))
         (cpu (gb-cpu gb))
         (init-pc #xc000)
         (init-flags (copy-gbflags (gbcpu-flags cpu))))
    (set-reg gb :hl init-val)
    (set-reg gb src other-val)
    (write-memory-at-addr gb init-pc op)
    (setf (gbcpu-pc cpu) init-pc)
    (step-cpu cpu gb)
    (is (= (gbcpu-pc cpu) (+ init-pc 1)))
    (is (= (gbflags-z (gbcpu-flags cpu)) (gbflags-z init-flags)))
    (is (= (gbflags-n (gbcpu-flags cpu)) 0))
    (is (= (gbflags-h (gbcpu-flags cpu))
           (if (> (+ (logand init-val #xfff) (logand other-val #xfff)) #xfff) 1 0)))
    (is (= (gbflags-c (gbcpu-flags cpu)) (if (> (+ init-val other-val) #xffff) 1 0)))
    (is (= (get-reg gb :hl) res)
        :ctx ("During opcode ~X the :HL register should hold the result of ~A + ~A" op init-val other-val))))

(deftest test-16bit-add-ops (init-val other-val)
  (loop for op in '((#x09 :bc) (#x19 :de) (#x29 :hl) (#x39 :sp)) do
    (if (eq (cadr op) :hl)
      (test-16bit-add-op (car op) (cadr op) init-val init-val (logand (+ init-val init-val) #xffff) (flags-into-byte (calc-16bit-add-flags init-val init-val)))
      (test-16bit-add-op (car op) (cadr op) init-val other-val (logand (+ init-val other-val) #xffff) (flags-into-byte (calc-16bit-add-flags init-val other-val))))))

(deftest test-pop-op (op reg init-val)
  (let* ((gb (make-gb))
         (cpu (gb-cpu gb))
         (init-pc #xc000)
         (init-sp #xd000)
         (init-flags (flags-into-byte (gbcpu-flags cpu))))
    (setf (gbcpu-sp cpu) init-sp)
    (push-addr-on-stack cpu gb init-val)
    (write-memory-at-addr gb init-pc op)
    (setf (gbcpu-pc cpu) init-pc)
    (step-cpu cpu gb)
    (is (= (gbcpu-pc cpu) (+ init-pc 1)))
    (is (= (gbcpu-sp cpu) init-sp))
    (if (eq reg :af)
    (is (= (flags-into-byte (gbcpu-flags cpu)) (logand init-val #xf0)))
    (is (= (flags-into-byte (gbcpu-flags cpu)) init-flags)))
    (is (= (get-reg gb reg) init-val)
        :ctx ("During opcode ~X the ~A register should equal ~A" op reg init-val))))

(deftest test-pop-ops (init-val)
  (loop for op in '((#xc1 :bc) (#xd1 :de) (#xe1 :hl) (#xf1 :af)) do
    (if (= (car op) #xf1)
      (test-pop-op (car op) (cadr op) (logand init-val #xfff0))
      (test-pop-op (car op) (cadr op) init-val))))

(deftest test-push-op (op reg init-val)
  (let* ((gb (make-gb))
         (cpu (gb-cpu gb))
         (init-pc #xc000)
         (init-sp #xd000))
    (set-reg gb reg init-val)
    (let ((init-flags (flags-into-byte (gbcpu-flags cpu))))
    (write-memory-at-addr gb init-pc op)
    (setf (gbcpu-pc cpu) init-pc)
    (setf (gbcpu-sp cpu) init-sp)
    (step-cpu cpu gb)
    (is (= (gbcpu-pc cpu) (+ init-pc 1)))
    (is (= (gbcpu-sp cpu) (- init-sp 2)))
    (is (= (flags-into-byte (gbcpu-flags cpu)) init-flags))
    (is (= (peek-addr-from-stack cpu gb) init-val)
        :ctx ("During opcode ~X the top of stack should equal ~A" op init-val)))))

(deftest test-push-ops (init-val)
  (loop for op in '((#xc5 :bc) (#xd5 :de) (#xe5 :hl) (#xf5 :af)) do
    (if (= (car op) #xf5)
      (test-push-op (car op) (cadr op) (logand init-val #xfff0))
      (test-push-op (car op) (cadr op) init-val))))

(deftest test-adc-ops (init-val other-val)
  (loop for op in '((#x88 :b) (#x89 :c) (#x8a :d) (#x8b :e)
                    (#x8c :h) (#x8d :l) (#x8e :<hl) (#x8f :a)
                    (#xce :imm)) do
    (if (= (car op) #x8f)
      (test-acc-op (car op) (cadr op) init-val init-val (logand (+ init-val init-val) #xff) (flags-into-byte (calc-8bit-add-flags init-val init-val)))
      (test-acc-op (car op) (cadr op) init-val other-val (logand (+ init-val other-val) #xff) (flags-into-byte (calc-8bit-add-flags init-val other-val))))))

(defun calc-sub-flags (init-val other-val &optional (carry 0))
  (make-gbflags
    :z (if (= (logand (- init-val other-val carry) #xff) #x0) 1 0)
    :n 1
    :h (if (< (logand init-val #xf) (logand (+ other-val carry) #xf)) 1 0)
    :c (if (< init-val (+ other-val carry)) 1 0)))

(deftest test-sub-ops (init-val other-val)
  (loop for op in '((#x90 :b) (#x91 :c) (#x92 :d) (#x93 :e)
                    (#x94 :h) (#x95 :l) (#x96 :<hl) (#x97 :a)
                    (#xd6 :imm)) do
    (if (= (car op) #x97)
      (test-acc-op (car op) (cadr op) init-val init-val (logand (- init-val init-val) #xff) (flags-into-byte (calc-sub-flags init-val init-val)))
      (test-acc-op (car op) (cadr op) init-val other-val (logand (- init-val other-val) #xff) (flags-into-byte (calc-sub-flags init-val other-val))))))

(deftest test-sbc-ops (init-val other-val)
  (loop for op in '((#x98 :b) (#x99 :c) (#x9a :d) (#x9b :e)
                    (#x9c :h) (#x9d :l) (#x9e :<hl) (#x9f :a)
                    (#xde :imm)) do
    (if (= (car op) #x9f)
      (test-acc-op (car op) (cadr op) init-val init-val (logand (- init-val init-val) #xff) (flags-into-byte (calc-sub-flags init-val init-val)))
      (test-acc-op (car op) (cadr op) init-val other-val (logand (- init-val other-val) #xff) (flags-into-byte (calc-sub-flags init-val other-val))))))

(defun calc-and-flags (init-val other-val)
  (make-gbflags
    :z (if (= (logand init-val other-val) #x0) 1 0)
    :n  0
    :h  1
    :c  0))

(deftest test-and-ops (init-val other-val)
  (loop for op in '((#xa0 :b) (#xa1 :c) (#xa2 :d) (#xa3 :e)
                    (#xa4 :h) (#xa5 :l) (#xa6 :<hl) (#xa7 :a)
                    (#xe6 :imm)) do
    (if (= (car op) #xa7)
      (test-acc-op (car op) (cadr op) init-val init-val (logand init-val init-val) (flags-into-byte (calc-and-flags init-val init-val)))
      (test-acc-op (car op) (cadr op) init-val other-val (logand init-val other-val) (flags-into-byte (calc-and-flags init-val other-val))))))

(defun calc-xor-flags (init-val other-val)
  (make-gbflags
    :z (if (= (logxor init-val other-val) #x0) 1 0)
    :n  0
    :h  0
    :c  0))

(deftest test-xor-ops (init-val other-val)
  (loop for op in '((#xa8 :b) (#xa9 :c) (#xaa :d) (#xab :e)
                    (#xac :h) (#xad :l) (#xae :<hl) (#xaf :a)
                    (#xee :imm)) do
    (if (= (car op) #xaf)
      (test-acc-op (car op) (cadr op) init-val init-val (logxor init-val init-val) (flags-into-byte (calc-xor-flags init-val init-val)))
      (test-acc-op (car op) (cadr op) init-val other-val (logxor init-val other-val) (flags-into-byte (calc-xor-flags init-val other-val))))))

(defun calc-or-flags (init-val other-val)
  (make-gbflags
    :z (if (= (logior init-val other-val) #x0) 1 0)
    :n  0
    :h  0
    :c  0))

(deftest test-or-ops (init-val other-val)
  (loop for op in '((#xb0 :b) (#xb1 :c) (#xb2 :d) (#xb3 :e)
                    (#xb4 :h) (#xb5 :l) (#xb6 :<hl) (#xb7 :a)
                    (#xf6 :imm)) do
    (if (= (car op) #xb7)
      (test-acc-op (car op) (cadr op) init-val init-val (logior init-val init-val) (flags-into-byte (calc-or-flags init-val init-val)))
      (test-acc-op (car op) (cadr op) init-val other-val (logior init-val other-val) (flags-into-byte (calc-or-flags init-val other-val))))))

(deftest test-cp-ops (init-val other-val)
  (loop for op in '((#xb8 :b) (#xb9 :c) (#xba :d) (#xbb :e)
                    (#xbc :h) (#xbd :l) (#xbe :<hl) (#xbf :a)
                    (#xfe :imm)) do
    (if (= (car op) #xbf)
      (test-acc-op (car op) (cadr op) init-val init-val init-val (flags-into-byte (calc-sub-flags init-val init-val)))
      (test-acc-op (car op) (cadr op) init-val other-val init-val (flags-into-byte (calc-sub-flags init-val other-val))))))

(deftest test-rlc-op (op init-val)
  (let* ((gb (make-gb))
         (cpu (gb-cpu gb))
         (init-pc #xc000)
         (res (logior (logand (ash init-val 1) #xff) (ash init-val -7))))
    (write-memory-at-addr gb init-pc (car op))
    (write-memory-at-addr gb (+ init-pc 1) (cadr op))
    (set-reg-pair-hl-to-val cpu #xc100)
    (setf (gbcpu-pc cpu) init-pc)
    (set-reg gb (get-src-reg-from-op (cadr op)) init-val)
    (step-cpu cpu gb)
    (is (= (gbcpu-pc cpu) (+ init-pc 2)))
    (is (= (get-reg gb (get-src-reg-from-op (cadr op))) res))
    (is (= (gbflags-n (gbcpu-flags cpu)) 0))
    (is (= (gbflags-h (gbcpu-flags cpu)) 0))
    (is (= (gbflags-c (gbcpu-flags cpu)) (if (= (logand init-val #x80) #x80) 1 0)))
    (is (= (gbflags-z (gbcpu-flags cpu))
           (if (= res #x0) 1 0)))))

(deftest test-rlc-ops (init-val)
  (loop for op from #x00 to #x07 do
        (test-rlc-op (list #xcb op) init-val)))

(deftest test-rrc-op (op init-val)
  (let* ((gb (make-gb))
         (cpu (gb-cpu gb))
         (init-pc #xc000)
         (res (logior (logand (ash init-val -1) #xff) (ash (logand init-val #x1) 7))))
    (write-memory-at-addr gb init-pc #xcb)
    (write-memory-at-addr gb (+ init-pc 1) op)
    (set-reg-pair-hl-to-val cpu #xc100)
    (setf (gbcpu-pc cpu) init-pc)
    (set-reg gb (get-src-reg-from-op op) init-val)
    (step-cpu cpu gb)
    (is (= (gbcpu-pc cpu) (+ init-pc 2)))
    (is (= (get-reg gb (get-src-reg-from-op op)) res))
    (is (= (gbflags-n (gbcpu-flags cpu)) 0))
    (is (= (gbflags-h (gbcpu-flags cpu)) 0))
    (is (= (gbflags-c (gbcpu-flags cpu)) (if (= (logand init-val #x01) #x01) 1 0)))
    (is (= (gbflags-z (gbcpu-flags cpu))
           (if (= res #x0) 1 0)))))

(deftest test-rrc-ops (init-val)
  (loop for op from #x08 to #x0f do
        (test-rrc-op op init-val)))

(deftest test-rl-op (op init-val)
  (let* ((gb (make-gb))
         (cpu (gb-cpu gb))
         (init-pc #xc000)
         (carry (gbflags-c (gbcpu-flags cpu)))
         (res (logior (logand (ash init-val 1) #xff) carry)))
    (write-memory-at-addr gb init-pc #xcb)
    (write-memory-at-addr gb (+ init-pc 1) op)
    (set-reg-pair-hl-to-val cpu #xc100)
    (setf (gbcpu-pc cpu) init-pc)
    (set-reg gb (get-src-reg-from-op op) init-val)
    (step-cpu cpu gb)
    (is (= (gbcpu-pc cpu) (+ init-pc 2)))
    (is (= (get-reg gb (get-src-reg-from-op op)) res))
    (is (= (gbflags-n (gbcpu-flags cpu)) 0))
    (is (= (gbflags-h (gbcpu-flags cpu)) 0))
    (is (= (gbflags-c (gbcpu-flags cpu)) (if (= (logand init-val #x80) #x80) 1 0)))
    (is (= (gbflags-z (gbcpu-flags cpu))
           (if (= res #x0) 1 0)))))

(deftest test-rl-ops (init-val)
  (loop for op from #x10 to #x17 do
        (test-rl-op op init-val)))

(deftest test-rr-op (op init-val)
  (let* ((gb (make-gb))
         (cpu (gb-cpu gb))
         (init-pc #xc000)
         (carry (gbflags-c (gbcpu-flags cpu)))
         (res (logior (logand (ash init-val -1) #xff) (ash carry 7))))
    (write-memory-at-addr gb init-pc #xcb)
    (write-memory-at-addr gb (+ init-pc 1) op)
    (set-reg-pair-hl-to-val cpu #xc100)
    (setf (gbcpu-pc cpu) init-pc)
    (set-reg gb (get-src-reg-from-op op) init-val)
    (step-cpu cpu gb)
    (is (= (gbcpu-pc cpu) (+ init-pc 2)))
    (is (= (get-reg gb (get-src-reg-from-op op)) res))
    (is (= (gbflags-n (gbcpu-flags cpu)) 0))
    (is (= (gbflags-h (gbcpu-flags cpu)) 0))
    (is (= (gbflags-c (gbcpu-flags cpu)) (if (= (logand init-val #x01) #x01) 1 0)))
    (is (= (gbflags-z (gbcpu-flags cpu))
           (if (= res #x0) 1 0)))))

(deftest test-rr-ops (init-val)
  (loop for op from #x18 to #x1f do
        (test-rr-op op init-val)))

(deftest test-sla-op (op init-val)
  (let* ((gb (make-gb))
         (cpu (gb-cpu gb))
         (init-pc #xc000)
         (res (logand (ash init-val 1) #xff)))
    (write-memory-at-addr gb init-pc #xcb)
    (write-memory-at-addr gb (+ init-pc 1) op)
    (set-reg-pair-hl-to-val cpu #xc100)
    (setf (gbcpu-pc cpu) init-pc)
    (set-reg gb (get-src-reg-from-op op) init-val)
    (step-cpu cpu gb)
    (is (= (gbcpu-pc cpu) (+ init-pc 2)))
    (is (= (get-reg gb (get-src-reg-from-op op)) res))
    (is (= (gbflags-n (gbcpu-flags cpu)) 0))
    (is (= (gbflags-h (gbcpu-flags cpu)) 0))
    (is (= (gbflags-c (gbcpu-flags cpu)) (if (= (logand init-val #x80) #x80) 1 0)))
    (is (= (gbflags-z (gbcpu-flags cpu))
           (if (= res #x0) 1 0)))))

(deftest test-sla-ops (init-val)
  (loop for op from #x20 to #x27 do
        (test-sla-op op init-val)))

(deftest test-sra-op (op init-val)
  (let* ((gb (make-gb))
         (cpu (gb-cpu gb))
         (init-pc #xc000)
         (res (logior (logand (ash init-val -1) #xff) (logand init-val #x80))))
    (write-memory-at-addr gb init-pc #xcb)
    (write-memory-at-addr gb (+ init-pc 1) op)
    (set-reg-pair-hl-to-val cpu #xc100)
    (setf (gbcpu-pc cpu) init-pc)
    (set-reg gb (get-src-reg-from-op op) init-val)
    (step-cpu cpu gb)
    (is (= (gbcpu-pc cpu) (+ init-pc 2)))
    (is (= (get-reg gb (get-src-reg-from-op op)) res))
    (is (= (gbflags-n (gbcpu-flags cpu)) 0))
    (is (= (gbflags-h (gbcpu-flags cpu)) 0))
    (is (= (gbflags-c (gbcpu-flags cpu)) (if (= (logand init-val #x01) #x01) 1 0)))
    (is (= (gbflags-z (gbcpu-flags cpu))
           (if (= res #x0) 1 0)))))

(deftest test-sra-ops (init-val)
  (loop for op from #x28 to #x2f do
        (test-sra-op op init-val)))

(deftest test-swap-op (op init-val)
  (let* ((gb (make-gb))
         (cpu (gb-cpu gb))
         (init-pc #xc000)
         (res (logior (logand (ash init-val -4) #xf) (ash (logand init-val #xf) 4))))
    (write-memory-at-addr gb init-pc #xcb)
    (write-memory-at-addr gb (+ init-pc 1) op)
    (set-reg-pair-hl-to-val cpu #xc100)
    (setf (gbcpu-pc cpu) init-pc)
    (set-reg gb (get-src-reg-from-op op) init-val)
    (step-cpu cpu gb)
    (is (= (gbcpu-pc cpu) (+ init-pc 2)))
    (is (= (get-reg gb (get-src-reg-from-op op)) res))
    (is (= (gbflags-n (gbcpu-flags cpu)) 0))
    (is (= (gbflags-h (gbcpu-flags cpu)) 0))
    (is (= (gbflags-c (gbcpu-flags cpu)) 0))
    (is (= (gbflags-z (gbcpu-flags cpu))
           (if (= init-val #x0) 1 0)))))

(deftest test-swap-ops (init-val)
  (loop for op from #x30 to #x37 do
        (test-swap-op op init-val)))

(deftest test-srl-op (op init-val)
  (let* ((gb (make-gb))
         (cpu (gb-cpu gb))
         (init-pc #xc000)
         (res (logand (ash init-val -1) #xff)))
    (write-memory-at-addr gb init-pc #xcb)
    (write-memory-at-addr gb (+ init-pc 1) op)
    (set-reg-pair-hl-to-val cpu #xc100)
    (setf (gbcpu-pc cpu) init-pc)
    (set-reg gb (get-src-reg-from-op op) init-val)
    (step-cpu cpu gb)
    (is (= (gbcpu-pc cpu) (+ init-pc 2)))
    (is (= (get-reg gb (get-src-reg-from-op op)) res))
    (is (= (gbflags-n (gbcpu-flags cpu)) 0))
    (is (= (gbflags-h (gbcpu-flags cpu)) 0))
    (is (= (gbflags-c (gbcpu-flags cpu)) (if (= (logand init-val #x01) #x01) 1 0)))
    (is (= (gbflags-z (gbcpu-flags cpu))
           (if (= res #x0) 1 0)))))

(deftest test-srl-ops (init-val)
  (loop for op from #x38 to #x3f do
        (test-srl-op op init-val)))

(deftest test-bit-op (op init-val)
  (let* ((gb (make-gb))
         (cpu (gb-cpu gb))
         (init-pc #xc000)
         (init-flags (copy-gbflags (gbcpu-flags cpu)))
         (bit-pos (logand (ash op -3) #x7)))
    (write-memory-at-addr gb init-pc #xcb)
    (write-memory-at-addr gb (+ init-pc 1) op)
    (set-reg-pair-hl-to-val cpu #xc100)
    (setf (gbcpu-pc cpu) init-pc)
      (set-reg gb (get-src-reg-from-op op) init-val)
      (step-cpu cpu gb)
      (is (= (gbcpu-pc cpu) (+ init-pc 2)))
      (is (= (gbflags-n (gbcpu-flags cpu)) 0))
      (is (= (gbflags-h (gbcpu-flags cpu)) 1))
      (is (= (gbflags-c (gbcpu-flags cpu)) (gbflags-c init-flags)))
      (is (= (gbflags-z (gbcpu-flags cpu))
             (if (= (logand (get-reg gb (get-src-reg-from-op op)) (ash 1 bit-pos)) #x0) 1 0)))))

(deftest test-bit-ops (init-val)
  (loop for op from #x40 to #x7f do
        (test-bit-op op init-val)))

(deftest test-reset-bit-op (op init-val)
  (let* ((gb (make-gb))
        (cpu (gb-cpu gb))
        (init-pc #xc000)
        (init-flags (flags-into-byte (gbcpu-flags cpu)))
        (bit-pos (logand (ash op -3) #x7)))
  (write-memory-at-addr gb init-pc #xcb)
  (write-memory-at-addr gb (+ init-pc 1) op)
  (set-reg-pair-hl-to-val cpu #xc100)
  (setf (gbcpu-pc cpu) init-pc)
    (set-reg gb (get-src-reg-from-op op) init-val)
    (step-cpu cpu gb)
    (is (= (gbcpu-pc cpu) (+ init-pc 2)))
    (is (= (flags-into-byte (gbcpu-flags cpu)) init-flags))
    (is (= (logand (get-reg gb (get-src-reg-from-op op)) (ash 1 bit-pos)) #x0)
        :ctx ("During opcode ~X the ~A register should have bit ~A reset" op (get-src-reg-from-op op) bit-pos))))

(deftest test-reset-bit-ops (init-val)
  (loop for op from #x80 to #xbf do
        (test-reset-bit-op op init-val)))

(deftest test-set-bit-op (op init-val)
  (let* ((gb (make-gb))
        (cpu (gb-cpu gb))
        (init-pc #xc000)
        (init-flags (flags-into-byte (gbcpu-flags cpu)))
          (bit-pos (logand (ash op -3) #x7)))
    (write-memory-at-addr gb init-pc #xcb)
    (write-memory-at-addr gb (+ init-pc 1) op)
    (set-reg-pair-hl-to-val cpu #xc100)
    (setf (gbcpu-pc cpu) init-pc)
    (set-reg gb (get-src-reg-from-op op) init-val)
    (step-cpu cpu gb)
    (is (= (gbcpu-pc cpu) (+ init-pc 2)))
    (is (= (flags-into-byte (gbcpu-flags cpu)) init-flags))
    (is (> (logand (get-reg gb (get-src-reg-from-op op)) (ash 1 bit-pos)) #x00)
        :ctx ("During opcode ~X the ~A register should have bit ~A set" op (get-src-reg-from-op op) bit-pos))))

(deftest test-set-bit-ops (init-val)
  (loop for op from #xc0 to #xff do
        (test-set-bit-op op init-val)))

(defun get-reg (gb reg)
  (let ((cpu (gb-cpu gb)))
    (case reg
      (:b (gbcpu-b cpu))
      (:c (gbcpu-c cpu))
      (:bc (logior (ash (gbcpu-b cpu) 8) (gbcpu-c cpu)))
      (:d (gbcpu-d cpu))
      (:e (gbcpu-e cpu))
      (:de (logior (ash (gbcpu-d cpu) 8) (gbcpu-e cpu)))
      (:h (gbcpu-h cpu))
      (:l (gbcpu-l cpu))
      (:hl (logior (ash (gbcpu-h cpu) 8) (gbcpu-l cpu)))
      (:<hl (get-byte-at-hl cpu gb))
      (:a (gbcpu-a cpu))
      (:af (logior (ash (gbcpu-a cpu) 8) (flags-into-byte (gbcpu-flags cpu))))
      (:sp (gbcpu-sp cpu))
      (:imm (read-memory-at-addr gb (+ (gbcpu-pc cpu) 1)))
      (:imm16 (logior (read-memory-at-addr gb (+ (gbcpu-pc cpu) 1))
                      (ash (read-memory-at-addr gb (+ (gbcpu-pc cpu) 2)) 8))))))

(defun set-reg (gb reg val)
  (let ((cpu (gb-cpu gb)))
    (case reg
      (:b (setf (gbcpu-b cpu) val))
      (:c (setf (gbcpu-c cpu) val))
      (:bc (setf (gbcpu-b cpu) (logand (ash val -8) #xff)
                 (gbcpu-c cpu) (logand val #xff)))
      (:d (setf (gbcpu-d cpu) val))
      (:e (setf (gbcpu-e cpu) val))
      (:de (setf (gbcpu-d cpu) (logand (ash val -8) #xff)
                 (gbcpu-e cpu) (logand val #xff)))
      (:h (setf (gbcpu-h cpu) val))
      (:l (setf (gbcpu-l cpu) val))
      (:hl (setf (gbcpu-h cpu) (logand (ash val -8) #xff)
                 (gbcpu-l cpu) (logand val #xff)))
      (:<hl (set-byte-at-hl cpu gb val))
      (:a (setf (gbcpu-a cpu) val))
      (:af (setf (gbcpu-a cpu) (logand (ash val -8) #xff)
                 (gbcpu-flags cpu) (flags-from-byte (logand val #xf0))))
      (:sp (setf (gbcpu-sp cpu) val))
      (:imm (write-memory-at-addr gb (+ (gbcpu-pc cpu) 1) val))
      (:imm16 (write-memory-at-addr gb (+ (gbcpu-pc cpu) 1) (logand val #xff))
              (write-memory-at-addr gb (+ (gbcpu-pc cpu) 2) (ash val -8))))))

(defun get-dest-reg-from-op (op)
  (case (logand op #xf8)
    (#x40 :b)
    (#x48 :c)
    (#x50 :d)
    (#x58 :e)
    (#x60 :h)
    (#x68 :l)
    (#x70 :<hl)
    (#x78 :a)))

(defun get-src-reg-from-op (op)
  (case (logand op #x7)
    (#x0 :b)
    (#x1 :c)
    (#x2 :d)
    (#x3 :e)
    (#x4 :h)
    (#x5 :l)
    (#x6 :<hl)
    (#x7 :a)))
