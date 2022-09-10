

(in-package :clboy)

(defstruct instruction
  (opcode #x00)
  (bytes 1)
  (cycles 1)
  (asm '(:nop))
  (fun '(or function null)))

(defmacro defincop (op reg)
  `(make-instruction
     :opcode ,op
     :bytes 1
     :cycles '(1 0)
     :asm '(:inc ,reg)
     :fun (lambda (cpu gb instr)
            (setf (,(utils:symb 'gbcpu- reg) cpu) (incr-reg cpu (,(utils:symb 'gbcpu- reg) cpu)))
            (incr-cpu-counters cpu instr))))
(defmacro defdecop (op reg)
  `(make-instruction
     :opcode ,op
     :bytes 1
     :cycles '(1 0)
     :asm '(:dec ,reg)
     :fun (lambda (cpu gb instr)
            (setf (,(utils:symb 'gbcpu- reg) cpu) (decr-reg cpu (,(utils:symb 'gbcpu- reg) cpu)))
            (incr-cpu-counters cpu instr))))

(defmacro definc16op (op reg1 reg2)
  `(make-instruction
     :opcode ,op
     :bytes 1
     :cycles '(2 0)
     :asm '(:inc ,(utils:symb reg2 reg2))
     :fun (lambda (cpu gb instr)
            (,(utils:symb 'set-reg-pair- reg1 reg2 '-to-val) cpu (incr-reg-pair (,(utils:symb 'gbcpu- reg1) cpu) (,(utils:symb 'gbcpu- reg2) cpu)))
            (incr-cpu-counters cpu instr))))
(defmacro defdec16op (op reg1 reg2)
  `(make-instruction
     :opcode ,op
     :bytes 1
     :cycles '(2 0)
     :asm '(:dec ,(utils:symb reg2 reg2))
     :fun (lambda (cpu gb instr)
            (,(utils:symb 'set-reg-pair- reg1 reg2 '-to-val) cpu (decr-reg-pair (,(utils:symb 'gbcpu- reg1) cpu) (,(utils:symb 'gbcpu- reg2) cpu)))
            (incr-cpu-counters cpu instr))))

(defun defldop (op dest src)
  (make-instruction
     :opcode op
     :bytes 1
     :cycles '(1 0)
     :asm '(:ld dest src)
     :fun (lambda (cpu gb instr)
            (setf (slot-value cpu dest) (slot-value cpu src))
            (incr-cpu-counters cpu instr))))

(defun defldfromhlop (op dest)
  (make-instruction
     :opcode op
     :bytes 1
     :cycles '(2 0)
     :asm '(:ld dest hl)
     :fun (lambda (cpu gb instr)
            (setf (slot-value cpu dest) (get-byte-at-hl cpu gb))
            (incr-cpu-counters cpu instr))))

(defun defarithop (op fun src)
  (make-instruction
     :opcode op
     :bytes 1
     :cycles '(1 0)
     :asm `(,fun a ,src)
     :fun (lambda (cpu gb instr)
            (setf (gbcpu-a cpu) (funcall fun cpu (gbcpu-a cpu) (slot-value cpu src)))
            (incr-cpu-counters cpu instr))))

(defmacro defcpop (op src)
  `(make-instruction
     :opcode ,op
     :bytes 1
     :cycles '(1 0)
     :asm '(:cp :a ,src)
     :fun (lambda (cpu gb instr)
            (cp-reg-with-val cpu (gbcpu-a cpu) (,(utils:symb 'gbcpu- src) cpu))
            (incr-cpu-counters cpu instr))))

(defun defcbprefop (op fun reg)
  (make-instruction
     :opcode op
     :bytes 2
     :cycles '(2 0)
     :asm '(fun reg)
     :fun (lambda (cpu gb instr)
            (setf (slot-value cpu reg) (funcall fun cpu (slot-value cpu reg)))
            (incr-cpu-counters cpu instr))))

(defun defbitop (op bit-pos reg)
  (make-instruction
     :opcode op
     :bytes 2
     :cycles '(2 0)
     :asm '(:bit bit-pos reg)
     :fun (lambda (cpu gb instr)
            (test-bit-reg cpu (slot-value cpu reg) bit-pos)
            (incr-cpu-counters cpu instr))))
(defun defbithlop (op bit-pos)
  (make-instruction
     :opcode op
     :bytes 2
     :cycles '(3 0)
     :asm '(:bit bit-pos :hl)
     :fun (lambda (cpu gb instr)
            (test-bit-reg cpu (get-byte-at-hl cpu gb) bit-pos)
            (incr-cpu-counters cpu instr))))

(defun defresop (op bit-pos reg)
  (make-instruction
     :opcode op
     :bytes 2
     :cycles '(2 0)
     :asm '(:res bit-pos reg)
     :fun (lambda (cpu gb instr)
            (setf (slot-value cpu reg)
                  (reset-bit-reg cpu (slot-value cpu reg) bit-pos))
            (incr-cpu-counters cpu instr))))
(defun defreshlop (op bit-pos)
  (make-instruction
     :opcode op
     :bytes 2
     :cycles '(4 0)
     :asm '(:res bit-pos :hl)
     :fun (lambda (cpu gb instr)
            (set-byte-at-hl cpu gb (reset-bit-reg cpu (get-byte-at-hl cpu gb) bit-pos))
            (incr-cpu-counters cpu instr))))

(defun defsetop (op bit-pos reg)
  (make-instruction
     :opcode op
     :bytes 2
     :cycles '(2 0)
     :asm '(:set bit-pos reg)
     :fun (lambda (cpu gb instr)
            (setf (slot-value cpu reg)
                  (set-bit-reg cpu (slot-value cpu reg) bit-pos))
            (incr-cpu-counters cpu instr))))
(defun defsethlop (op bit-pos)
  (make-instruction
     :opcode op
     :bytes 2
     :cycles '(4 0)
     :asm '(:set bit-pos hl)
     :fun (lambda (cpu gb instr)
            (set-byte-at-hl cpu gb (set-bit-reg cpu (get-byte-at-hl cpu gb) bit-pos))
            (incr-cpu-counters cpu instr))))


(defparameter ops (make-array #x100 :initial-element nil))
(defparameter cb-ops (make-array #x100 :initial-element nil))

(setf (aref ops #x00) (make-instruction
                        :opcode #x00 :bytes 1 :cycles '(1 0) :asm '(:nop)
                        :fun (lambda (cpu gb instr) (incr-cpu-counters cpu instr))))
(setf (aref ops #x01) (make-instruction
                        :opcode #x01 :bytes 3 :cycles '(3 0) :asm '(:ld "BC,u16")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-memory gb (+ (gbcpu-pc cpu) 1))))
                                 (set-reg-pair-bc-to-val cpu addr)
                                 (incr-cpu-counters cpu instr)))))

(setf (aref ops #x02) (make-instruction
                        :opcode #x02 :bytes 1 :cycles '(2 0) :asm '(:ld "(BC),A")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-reg-pair (gbcpu-b cpu) (gbcpu-c cpu))))
                                 (write-memory-at-addr gb addr (gbcpu-a cpu))
                                 (incr-cpu-counters cpu instr)))))

(setf (aref ops #x03) (definc16op #x03 :b :c))
(setf (aref ops #x04) (defincop #x04 :b))
(setf (aref ops #x05) (defdecop #x05 :b))
(setf (aref ops #x06) (make-instruction
                        :opcode #x06 :bytes 2 :cycles '(2 0) :asm '(:ld "B,u8")
                        :fun (lambda (cpu gb instr)
                               (let ((b (read-memory-at-addr gb  (+ (gbcpu-pc cpu) 1))))
                                 (setf (gbcpu-b cpu) b)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x07) (make-instruction
                        :opcode #x07 :bytes 1 :cycles '(1 0) :asm '(:rlca)
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-a cpu) (rot-left-c-reg cpu (gbcpu-a cpu))
                                     (gbflags-z (gbcpu-flags cpu)) 0)
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x08) (make-instruction
                        :opcode #x08 :bytes 3 :cycles '(5 0) :asm '(:ld "(u16),SP")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-memory gb (+ (gbcpu-pc cpu) 1))))
                                 (write-memory-at-addr gb addr (logand (gbcpu-sp cpu) #xff))
                                 (write-memory-at-addr gb (+ addr 1) (logand (ash (gbcpu-sp cpu) -8) #xff))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x09) (make-instruction
                        :opcode #x09 :bytes 1 :cycles '(2 0) :asm '(:add "HL,BC")
                        :fun (lambda (cpu gb instr)
                               (set-reg-pair-hl-to-val
                                 cpu (add16 cpu
                                            (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))
                                            (get-address-from-reg-pair (gbcpu-b cpu) (gbcpu-c cpu))))
                               (incr-cpu-counters cpu instr))))

(setf (aref ops #x0a)
      (make-instruction
        :opcode #x0a :bytes 1 :cycles '(2 0) :asm '(:ld "A,(BC)")
        :fun (lambda (cpu gb instr)
               (let* ((addr (get-address-from-reg-pair (gbcpu-b cpu) (gbcpu-c cpu)))
                      (val (read-memory-at-addr gb  addr)))
                 (setf (gbcpu-a cpu) val)
                 (incr-cpu-counters cpu instr)))))

(setf (aref ops #x0b) (defdec16op #x0b :b :c))
(setf (aref ops #x0c) (defincop #x0c :c))
(setf (aref ops #x0d) (defdecop #x0d :c))
(setf (aref ops #x0e) (make-instruction
                        :opcode #x0e :bytes 2 :cycles '(2 0) :asm '(:ld "C,u8")
                        :fun (lambda (cpu gb instr)
                               (let ((b (read-memory-at-addr gb  (+ (gbcpu-pc cpu) 1))))
                                 (setf (gbcpu-c cpu) b)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x0f) (make-instruction
                        :opcode #x0f :bytes 1 :cycles '(1 0) :asm '(:rrca)
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-a cpu) (rot-right-c-reg cpu (gbcpu-a cpu))
                                     (gbflags-z (gbcpu-flags cpu)) 0)
                               (incr-cpu-counters cpu instr))))


(setf (aref ops #x10) (make-instruction
                        :opcode #x10 :bytes 2 :cycles '(1 0) :asm '(:stop)
                        :fun (lambda (cpu gb instr)
                               (setf (gb-stopped? gb) t)
                               (write-memory-at-addr gb #xff04 0)
                               (incf (gbcpu-pc cpu) (instruction-bytes instr)))))

(setf (aref ops #x11) (make-instruction
                        :opcode #x11 :bytes 3 :cycles '(3 0) :asm '(:ld "DE,u16")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-memory gb (+ (gbcpu-pc cpu) 1))))
                                 (set-reg-pair-de-to-val cpu addr)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x12) (make-instruction
                        :opcode #x12 :bytes 1 :cycles '(2 0) :asm '(:ld "(DE),A")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-reg-pair (gbcpu-d cpu) (gbcpu-e cpu))))
                                 (write-memory-at-addr gb addr (gbcpu-a cpu))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x13) (definc16op #x13 :d :e))
(setf (aref ops #x14) (defincop #x14 :d))

(setf (aref ops #x15) (defdecop #x15 :d))

(setf (aref ops #x16) (make-instruction
                        :opcode #x16 :bytes 2 :cycles '(2 0) :asm '(:ld "D,u8")
                        :fun (lambda (cpu gb instr)
                               (let ((b (read-memory-at-addr gb  (+ (gbcpu-pc cpu) 1))))
                                 (setf (gbcpu-d cpu) b)
                                 (incr-cpu-counters cpu instr)))))

(setf (aref ops #x17) (make-instruction
                        :opcode #x17 :bytes 1 :cycles '(1 0) :asm '(:rla)
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-a cpu) (rot-left-reg cpu (gbcpu-a cpu))
                                     (gbflags-z (gbcpu-flags cpu)) 0)
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x18) (make-instruction
                        :opcode #x18 :bytes 2 :cycles '(3 0) :asm '(:jr "i8")
                        :fun (lambda (cpu gb instr)
                               (let* ((b (read-memory-at-addr gb  (+ (gbcpu-pc cpu) 1)))
                                      (addr (get-new-addr-from-relative (+ (gbcpu-pc cpu) 2) b)))
                                 (incr-cpu-counters cpu instr)
                                 (do-jump cpu addr)))))
(setf (aref ops #x19) (make-instruction
                        :opcode #x19 :bytes 1 :cycles '(2 0) :asm '(:add "HL,DE")
                        :fun (lambda (cpu gb instr)
                               (set-reg-pair-hl-to-val
                                 cpu (add16 cpu
                                            (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))
                                            (get-address-from-reg-pair (gbcpu-d cpu) (gbcpu-e cpu))))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x1a)
      (make-instruction
        :opcode #x1a :bytes 1 :cycles '(2 0) :asm '(:ld "A,(DE)")
        :fun (lambda (cpu gb instr)
               (let* ((addr (get-address-from-reg-pair (gbcpu-d cpu) (gbcpu-e cpu)))
                      (val (read-memory-at-addr gb  addr)))
                 (setf (gbcpu-a cpu) val)
                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x1b) (defdec16op #x1b :d :e))

(setf (aref ops #x1c) (defincop #x1c :e))
(setf (aref ops #x1d) (defdecop #x1d :e))

(setf (aref ops #x1e) (make-instruction
                        :opcode #x1e :bytes 2 :cycles '(2 0) :asm '(:ld "E,u8")
                        :fun (lambda (cpu gb instr)
                               (let ((b (read-memory-at-addr gb  (+ (gbcpu-pc cpu) 1))))
                                 (setf (gbcpu-e cpu) b)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x1f) (make-instruction
                        :opcode #x1f :bytes 1 :cycles '(1 0) :asm '(:rra)
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-a cpu) (rot-right-reg cpu (gbcpu-a cpu))
                                     (gbflags-z (gbcpu-flags cpu)) 0)
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x20) (make-instruction
                        :opcode #x20 :bytes 2 :cycles '(2 1) :asm '(:jr "NZ,i8")
                        :fun (lambda (cpu gb instr)
                               (let* ((b (read-memory-at-addr gb  (+ (gbcpu-pc cpu) 1)))
                                      (addr (get-new-addr-from-relative (+ (gbcpu-pc cpu) 2) b)))
                                 (incr-cpu-counters cpu instr)
                                 (if (= (gbflags-z (gbcpu-flags cpu)) #x00)
                                   (progn (incr-branched-clocks cpu instr)
                                          (do-jump cpu addr)))))))
(setf (aref ops #x21) (make-instruction
                        :opcode #x21 :bytes 3 :cycles '(3 0) :asm '(:ld "HL,u16")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-memory gb (+ (gbcpu-pc cpu) 1))))
                                 (set-reg-pair-hl-to-val cpu addr)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x22) (make-instruction
                        :opcode #x22 :bytes 1 :cycles '(2 0) :asm '(:ld "(HL+),A")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                 (write-memory-at-addr gb addr (gbcpu-a cpu))
                                 (set-reg-pair-hl-to-val cpu (incr-reg-pair (gbcpu-h cpu) (gbcpu-l cpu)))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x23) (definc16op #x23 :h :l))

(setf (aref ops #x24) (defincop #x24 :h))
(setf (aref ops #x25) (defdecop #x25 :h))
(setf (aref ops #x26) (make-instruction
                        :opcode #x26 :bytes 2 :cycles '(2 0) :asm '(:ld "H,u8")
                        :fun (lambda (cpu gb instr)
                               (let ((b (read-memory-at-addr gb  (+ (gbcpu-pc cpu) 1))))
                                 (setf (gbcpu-h cpu) b)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x27) (make-instruction
                        :opcode #x27 :bytes 1 :cycles '(1 0) :asm '(:daa)
                        :fun (lambda (cpu gb instr)
                               (daa cpu)
                               (incr-cpu-counters cpu instr))))

(setf (aref ops #x28) (make-instruction
                        :opcode #x28 :bytes 2 :cycles '(2 1) :asm '(:jr "Z,i8")
                        :fun (lambda (cpu gb instr)
                               (let* ((b (read-memory-at-addr gb  (+ (gbcpu-pc cpu) 1)))
                                      (addr (get-new-addr-from-relative (+ (gbcpu-pc cpu) 2) b)))
                                 (incr-cpu-counters cpu instr)
                                 (if (= (gbflags-z (gbcpu-flags cpu)) #x01)
                                   (progn (incr-branched-clocks cpu instr)
                                          (do-jump cpu addr)))))))
(setf (aref ops #x29) (make-instruction
                        :opcode #x29 :bytes 1 :cycles '(2 0) :asm '(:add "HL,HL")
                        :fun (lambda (cpu gb instr)
                               (set-reg-pair-hl-to-val
                                 cpu (add16 cpu
                                            (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))
                                            (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x2a) (make-instruction
                        :opcode #x2a :bytes 1 :cycles '(2 0) :asm '(:ld "A,(HL+)")
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-a cpu) (get-byte-at-hl cpu gb))
                               (set-reg-pair-hl-to-val cpu (incr-reg-pair (gbcpu-h cpu) (gbcpu-l cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x2b) (defdec16op #x2b :h :l))
(setf (aref ops #x2c) (defincop #x2c :l))
(setf (aref ops #x2d) (defdecop #x2d :l))
(setf (aref ops #x2e) (make-instruction
                        :opcode #x2e :bytes 2 :cycles '(2 0) :asm '(:ld "L,u8")
                        :fun (lambda (cpu gb instr)
                               (let ((b (read-memory-at-addr gb  (+ (gbcpu-pc cpu) 1))))
                                 (setf (gbcpu-l cpu) b)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x2f) (make-instruction
                        :opcode #x2f :bytes 1 :cycles '(1 0) :asm '(:cpl)
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-a cpu) (logxor (gbcpu-a cpu) #xff)
                                     (gbflags-n (gbcpu-flags cpu)) #x01
                                     (gbflags-h (gbcpu-flags cpu)) #x01)
                               (incr-cpu-counters cpu instr))))

(setf (aref ops #x30) (make-instruction
                        :opcode #x30 :bytes 2 :cycles '(2 1) :asm '(:jr "NC,i8")
                        :fun (lambda (cpu gb instr)
                               (let* ((b (read-memory-at-addr gb (+ (gbcpu-pc cpu) 1)))
                                      (addr (get-new-addr-from-relative (+ (gbcpu-pc cpu) 2) b)))
                                 (incr-cpu-counters cpu instr)
                                 (if (= (gbflags-c (gbcpu-flags cpu)) #x00)
                                   (progn (incr-branched-clocks cpu instr)
                                          (do-jump cpu addr)))))))
(setf (aref ops #x31) (make-instruction
                        :opcode #x31 :bytes 3 :cycles '(3 0) :asm '(:ld "SP,u16")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-memory gb (+ (gbcpu-pc cpu) 1))))
                                 (setf (gbcpu-sp cpu) addr)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x32) (make-instruction
                        :opcode #x32 :bytes 1 :cycles '(2 0) :asm '(:ld "(HL-),A")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                 (write-memory-at-addr gb addr (gbcpu-a cpu))
                                 (set-reg-pair-hl-to-val cpu (decr-reg-pair (gbcpu-h cpu) (gbcpu-l cpu)))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x33) (make-instruction
                        :opcode #x33 :bytes 1 :cycles '(2 0) :asm '(:inc "SP")
                        :fun (lambda (cpu gb instr)
                               (let ((res (logand (+ (gbcpu-sp cpu) 1) #xffff)))
                                 (setf (gbcpu-sp cpu) res)
                                 (incr-cpu-counters cpu instr)))))

(setf (aref ops #x34) (make-instruction
                        :opcode #x34 :bytes 1 :cycles '(3 0) :asm '(:inc "(HL)")
                        :fun (lambda (cpu gb instr)
                               (write-memory-at-addr
                                 gb
                                 (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))
                                 (incr-reg cpu (get-byte-at-hl cpu gb)))
                               (incr-cpu-counters cpu instr))))

(setf (aref ops #x35) (make-instruction
                        :opcode #x35 :bytes 1 :cycles '(3 0) :asm '(:dec "(HL)")
                        :fun (lambda (cpu gb instr)
                               (write-memory-at-addr
                                 gb
                                 (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))
                                 (decr-reg cpu (get-byte-at-hl cpu gb)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x36) (make-instruction
                        :opcode #x36 :bytes 2 :cycles '(3 0) :asm '(:ld "(HL),u8")
                        :fun (lambda (cpu gb instr)
                               (let ((b (read-memory-at-addr gb (+ (gbcpu-pc cpu) 1)))
                                     (addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                 (write-memory-at-addr gb addr b)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x37) (make-instruction
                        :opcode #x37 :bytes 1 :cycles '(1 0) :asm '(:scf)
                        :fun (lambda (cpu gb instr)
                               (setf (gbflags-n (gbcpu-flags cpu)) #x00
                                     (gbflags-h (gbcpu-flags cpu)) #x00
                                     (gbflags-c (gbcpu-flags cpu)) #x01)
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x38) (make-instruction
                        :opcode #x38 :bytes 2 :cycles '(2 1) :asm '(:jr "C,i8")
                        :fun (lambda (cpu gb instr)
                               (let* ((b (read-memory-at-addr gb (+ (gbcpu-pc cpu) 1)))
                                      (addr (get-new-addr-from-relative (+ (gbcpu-pc cpu) 2) b)))
                                 (incr-cpu-counters cpu instr)
                                 (if (= (gbflags-c (gbcpu-flags cpu)) #x01)
                                   (progn (incr-branched-clocks cpu instr)
                                          (do-jump cpu addr)))))))
(setf (aref ops #x39) (make-instruction
                        :opcode #x39 :bytes 1 :cycles '(2 0) :asm '(:add "HL,SP")
                        :fun (lambda (cpu gb instr)
                               (set-reg-pair-hl-to-val
                                 cpu (add16 cpu
                                            (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))
                                            (gbcpu-sp cpu)))
                               (incr-cpu-counters cpu instr))))

(setf (aref ops #x3a) (make-instruction
                        :opcode #x3a :bytes 1 :cycles '(2 0) :asm '(:ld "A,(HL-)")
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-a cpu) (get-byte-at-hl cpu gb))
                               (set-reg-pair-hl-to-val cpu (decr-reg-pair (gbcpu-h cpu) (gbcpu-l cpu)))
                               (incr-cpu-counters cpu instr))))

(setf (aref ops #x3b) (make-instruction
                        :opcode #x3b :bytes 1 :cycles '(2 0) :asm '(:dec "SP")
                        :fun (lambda (cpu gb instr)
                               (let ((res (if (= (gbcpu-sp cpu) 0) #xffff (- (gbcpu-sp cpu) 1))))
                                 (setf (gbcpu-sp cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x3c) (defincop #x3c :a))
(setf (aref ops #x3d) (defdecop #x3d :a))
(setf (aref ops #x3e) (make-instruction
                        :opcode #x3e :bytes 2 :cycles '(2 0) :asm '(:ld "A,u8")
                        :fun (lambda (cpu gb instr)
                               (let ((b (read-memory-at-addr gb (+ (gbcpu-pc cpu) 1))))
                                 (setf (gbcpu-a cpu) b)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x3f) (make-instruction
                        :opcode #x3f :bytes 1 :cycles '(1 0) :asm '(:ccf)
                        :fun (lambda (cpu gb instr)
                               (setf (gbflags-n (gbcpu-flags cpu)) #x00
                                     (gbflags-h (gbcpu-flags cpu)) #x00
                                     (gbflags-c (gbcpu-flags cpu)) (logxor (gbflags-c (gbcpu-flags cpu)) #x01))
                               (incr-cpu-counters cpu instr))))

;; Load Block
(loop for op from #x40 to #x7f
      when (not (= (logand op #x78) #x70)) do
      (if (= (logand op #x7) #x6)
        (setf (aref ops op) (defldfromhlop op
                (case (logand (ash op -3) #x7)
                  (#x0 'b) (#x1 'c) (#x2 'd) (#x3 'e) (#x4 'h) (#x5 'l) (#x7 'a))))
        (setf (aref ops op)
              (defldop op
                (case (logand (ash op -3) #x7)
                  (#x0 'b) (#x1 'c) (#x2 'd) (#x3 'e) (#x4 'h) (#x5 'l) (#x7 'a))
                (case (logand op #x7)
                  (#x0 'b) (#x1 'c) (#x2 'd) (#x3 'e) (#x4 'h) (#x5 'l) (#x7 'a))))))


;; Load Into (HL)
(defun defldintohlop (op reg)
  (make-instruction
    :opcode #x70 :bytes 1 :cycles '(2 0) :asm '(:ld "(HL)" reg)
    :fun (lambda (cpu gb instr)
           (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
             (write-memory-at-addr gb addr (slot-value cpu reg))
             (incr-cpu-counters cpu instr)))))

(loop for op in '(#x70 #x71 #x72 #x73 #x74 #x75 #x77)
      for reg in '(b c d e h l a) do
      (setf (aref ops op) (defldintohlop op reg)))

(setf (aref ops #x76) (make-instruction
                        :opcode #x76 :bytes 1 :cycles '(1 0) :asm '(:halt)
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-halted cpu) #x01)
                               (incr-cpu-counters cpu instr))))

;; Arithmentic

(loop for op-lsb in '(0 1 2 3 4 5 7)
      for reg in '(b c d e h l a) do
      (setf (aref ops (+ op-lsb #x80)) (defarithop (+ op-lsb #x80) #'add reg)
            (aref ops (+ op-lsb #x88)) (defarithop (+ op-lsb #x88) #'adc reg)
            (aref ops (+ op-lsb #x90)) (defarithop (+ op-lsb #x90) #'sub reg)
            (aref ops (+ op-lsb #x98)) (defarithop (+ op-lsb #x98) #'sbc reg)
            (aref ops (+ op-lsb #xa0)) (defarithop (+ op-lsb #xa0) #'and-op reg)
            (aref ops (+ op-lsb #xa8)) (defarithop (+ op-lsb #xa8) #'xor reg)
            (aref ops (+ op-lsb #xb0)) (defarithop (+ op-lsb #xb0) #'or-op reg)
            ))

(defun defarithhlop (op fun)
  (make-instruction
    :opcode #x86 :bytes 1 :cycles '(2 0) :asm '(:add "A,(HL)")
    :fun (lambda (cpu gb instr)
           (setf (gbcpu-a cpu) (funcall fun cpu (gbcpu-a cpu) (get-byte-at-hl cpu gb)))
           (incr-cpu-counters cpu instr))))

(setf (aref ops #x86) (defarithhlop #x86 #'add))

(setf (aref ops #x8e) (defarithhlop #x8e #'adc))

(setf (aref ops #x96) (defarithhlop #x96 #'sub))

(setf (aref ops #x9e) (defarithhlop #x9e #'sbc))

(setf (aref ops #xa6) (defarithhlop #xa6 #'and-op))

(setf (aref ops #xae) (defarithhlop #xae #'xor))

(setf (aref ops #xb6) (defarithhlop #xb6 #'or-op))

;; CPs
(setf (aref ops #xb8) (defcpop #xb8 :b))
(setf (aref ops #xb9) (defcpop #xb9 :c))
(setf (aref ops #xba) (defcpop #xba :d))
(setf (aref ops #xbb) (defcpop #xbb :e))
(setf (aref ops #xbc) (defcpop #xbc :h))
(setf (aref ops #xbd) (defcpop #xbd :l))
(setf (aref ops #xbe) (make-instruction
                        :opcode #xbe :bytes 1 :cycles '(2 0) :asm '(:cp "A,(HL)")
                        :fun (lambda (cpu gb instr)
                               (cp-reg-with-val cpu (gbcpu-a cpu) (get-byte-at-hl cpu gb))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xbf) (defcpop #xbf :a))

;; Misc jp call rst ret
(setf (aref ops #xc0) (make-instruction
                        :opcode #xc0 :bytes 1 :cycles '(2 3) :asm '(:ret "NZ")
                        :fun (lambda (cpu gb instr)
                               (incr-cpu-counters cpu instr)
                               (if (= (gbflags-z (gbcpu-flags cpu)) #x00)
                                 (progn (incr-branched-clocks cpu instr)
                                        (do-ret cpu gb))))))
(setf (aref ops #xc1) (make-instruction
                        :opcode #xc1 :bytes 1 :cycles '(3 0) :asm '(:pop "BC")
                        :fun (lambda (cpu gb instr)
                               (set-reg-pair-bc-to-val cpu (pop-addr-from-stack cpu gb))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xc2) (make-instruction
                        :opcode #xc2 :bytes 3 :cycles '(3 1) :asm '(:jp "NZ,u16")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-memory gb (+ (gbcpu-pc cpu) 1))))
                                 (incr-cpu-counters cpu instr)
                                 (if (= (gbflags-z (gbcpu-flags cpu)) #x00)
                                   (progn (incr-branched-clocks cpu instr)
                                          (do-jump cpu addr)))))))
(setf (aref ops #xc3) (make-instruction
                        :opcode #xc3 :bytes 3 :cycles '(4 0) :asm '(:jp "u16")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-memory gb (+ (gbcpu-pc cpu) 1))))
                                 (incr-cpu-counters cpu instr)
                                 (do-jump cpu addr)))))
(setf (aref ops #xc4) (make-instruction
                        :opcode #xc4 :bytes 3 :cycles '(3 3) :asm '(:call "NZ,u16")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-memory gb (+ (gbcpu-pc cpu) 1))))
                                 (incr-cpu-counters cpu instr)
                                 (if (= (gbflags-z (gbcpu-flags cpu)) #x00)
                                   (progn (incr-branched-clocks cpu instr)
                                          (do-call-at-addr cpu gb addr)))))))
(setf (aref ops #xc5) (make-instruction
                        :opcode #xc5 :bytes 1 :cycles '(4 0) :asm '(:push "BC")
                        :fun (lambda (cpu gb instr)
                               (push-reg-pair-on-stack cpu gb (gbcpu-b cpu) (gbcpu-c cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xc6) (make-instruction
                        :opcode #xc6 :bytes 2 :cycles '(2 0) :asm '(:add "A,u8")
                        :fun (lambda (cpu gb instr)
                               (let ((b (read-memory-at-addr gb (+ (gbcpu-pc cpu) 1))))
                                 (setf (gbcpu-a cpu) (add cpu (gbcpu-a cpu) b))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xc7) (make-instruction
                        :opcode #xc7 :bytes 1 :cycles '(4 0) :asm '(:rst "00h")
                        :fun (lambda (cpu gb instr)
                               (incr-cpu-counters cpu instr)
                               (do-rst cpu gb #x00))))
(setf (aref ops #xc8) (make-instruction
                        :opcode #xc8 :bytes 1 :cycles '(2 3) :asm '(:ret "Z")
                        :fun (lambda (cpu gb instr)
                               (incr-cpu-counters cpu instr)
                               (if (= (gbflags-z (gbcpu-flags cpu)) #x01)
                                 (progn (incr-branched-clocks cpu instr)
                                        (do-ret cpu gb))))))
(setf (aref ops #xc9) (make-instruction
                        :opcode #xc9 :bytes 1 :cycles '(4 0) :asm '(:ret)
                        :fun (lambda (cpu gb instr)
                               (incr-cpu-counters cpu instr)
                               (do-ret cpu gb))))
(setf (aref ops #xca) (make-instruction
                        :opcode #xca :bytes 3 :cycles '(3 1) :asm '(:jp "Z,u16")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-memory gb (+ (gbcpu-pc cpu) 1))))
                                 (incr-cpu-counters cpu instr)
                                 (if (= (gbflags-z (gbcpu-flags cpu)) #x01)
                                   (progn (incr-branched-clocks cpu instr)
                                          (do-jump cpu addr)))))))
(setf (aref ops #xcc) (make-instruction
                        :opcode #xcc :bytes 3 :cycles '(3 3) :asm '(:call "Z,u16")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-memory gb (+ (gbcpu-pc cpu) 1))))
                                 (incr-cpu-counters cpu instr)
                                 (if (= (gbflags-z (gbcpu-flags cpu)) #x01)
                                   (progn (incr-branched-clocks cpu instr)
                                          (do-call-at-addr cpu gb addr)))))))
(setf (aref ops #xcd) (make-instruction
                        :opcode #xcd :bytes 3 :cycles '(6 0) :asm '(:call "u16")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-memory gb (+ (gbcpu-pc cpu) 1))))
                                 (incr-cpu-counters cpu instr)
                                 (do-call-at-addr cpu gb addr)))))
(setf (aref ops #xce) (make-instruction
                        :opcode #xce :bytes 2 :cycles '(2 0) :asm '(:adc "A,u8")
                        :fun (lambda (cpu gb instr)
                               (let* ((b (read-memory-at-addr gb (+ (gbcpu-pc cpu) 1)))
                                      (res (adc cpu (gbcpu-a cpu) b)))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xcf) (make-instruction
                        :opcode #xcf :bytes 1 :cycles '(4 0) :asm '(:rst "08h")
                        :fun (lambda (cpu gb instr)
                               (incr-cpu-counters cpu instr)
                               (do-rst cpu gb #x08))))

(setf (aref ops #xd0) (make-instruction
                        :opcode #xd0 :bytes 1 :cycles '(2 3) :asm '(:ret "NC")
                        :fun (lambda (cpu gb instr)
                               (incr-cpu-counters cpu instr)
                               (if (= (gbflags-c (gbcpu-flags cpu)) #x00)
                                 (progn (incr-branched-clocks cpu instr)
                                        (do-ret cpu gb))))))
(setf (aref ops #xd1) (make-instruction
                        :opcode #xd1 :bytes 1 :cycles '(3 0) :asm '(:pop "DE")
                        :fun (lambda (cpu gb instr)
                               (set-reg-pair-de-to-val cpu (pop-addr-from-stack cpu gb))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xd2) (make-instruction
                        :opcode #xd2 :bytes 3 :cycles '(3 1) :asm '(:jp "NC,u16")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-memory gb (+ (gbcpu-pc cpu) 1))))
                                 (incr-cpu-counters cpu instr)
                                 (if (= (gbflags-c (gbcpu-flags cpu)) #x00)
                                   (progn (incr-branched-clocks cpu instr)
                                          (do-jump cpu addr)))))))
(setf (aref ops #xd4) (make-instruction
                        :opcode #xd4 :bytes 3 :cycles '(3 3) :asm '(:call "NC,u16")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-memory gb (+ (gbcpu-pc cpu) 1))))
                                 (incr-cpu-counters cpu instr)
                                 (if (= (gbflags-c (gbcpu-flags cpu)) #x00)
                                   (progn (incr-branched-clocks cpu instr)
                                          (do-call-at-addr cpu gb addr)))))))
(setf (aref ops #xd5) (make-instruction
                        :opcode #xd5 :bytes 1 :cycles '(4 0) :asm '(:push "DE")
                        :fun (lambda (cpu gb instr)
                               (push-reg-pair-on-stack cpu gb (gbcpu-d cpu) (gbcpu-e cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xd6) (make-instruction
                        :opcode #xd6 :bytes 2 :cycles '(2 0) :asm '(:sub "A,u8")
                        :fun (lambda (cpu gb instr)
                               (let ((b (read-memory-at-addr gb (+ (gbcpu-pc cpu) 1))))
                                 (setf (gbcpu-a cpu) (sub cpu (gbcpu-a cpu) b))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xd7) (make-instruction
                        :opcode #xd7 :bytes 1 :cycles '(4 0) :asm '(:rst "10h")
                        :fun (lambda (cpu gb instr)
                               (incr-cpu-counters cpu instr)
                               (do-rst cpu gb #x10))))
(setf (aref ops #xd8) (make-instruction
                        :opcode #xd8 :bytes 1 :cycles '(2 3) :asm '(:ret "C")
                        :fun (lambda (cpu gb instr)
                               (incr-cpu-counters cpu instr)
                               (if (= (gbflags-c (gbcpu-flags cpu)) #x01)
                                 (progn (incr-branched-clocks cpu instr)
                                        (do-ret cpu gb))))))
(setf (aref ops #xd9) (make-instruction
                        :opcode #xd9 :bytes 1 :cycles '(4 0) :asm '(:reti)
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-int-ena cpu) #x01)
                               (incr-cpu-counters cpu instr)
                               (do-ret cpu gb))))
(setf (aref ops #xda) (make-instruction
                        :opcode #xda :bytes 3 :cycles '(3 1) :asm '(:jp "C,u16")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-memory gb (+ (gbcpu-pc cpu) 1))))
                                 (incr-cpu-counters cpu instr)
                                 (if (= (gbflags-c (gbcpu-flags cpu)) #x01)
                                   (progn (incr-branched-clocks cpu instr)
                                          (do-jump cpu addr)))))))
(setf (aref ops #xdc) (make-instruction
                        :opcode #xdc :bytes 3 :cycles '(3 3) :asm '(:call "C,u16")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-memory gb (+ (gbcpu-pc cpu) 1))))
                                 (incr-cpu-counters cpu instr)
                                 (if (= (gbflags-c (gbcpu-flags cpu)) #x01)
                                   (progn (incr-branched-clocks cpu instr)
                                          (do-call-at-addr cpu gb addr)))))))
(setf (aref ops #xde) (make-instruction
                        :opcode #xde :bytes 2 :cycles '(2 0) :asm '(:sbc "A,u8")
                        :fun (lambda (cpu gb instr)
                               (let* ((b (read-memory-at-addr gb  (+ (gbcpu-pc cpu) 1)))
                                      (res (sbc cpu (gbcpu-a cpu) b)))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xdf) (make-instruction
                        :opcode #xdf :bytes 1 :cycles '(4 0) :asm '(:rst "18h")
                        :fun (lambda (cpu gb instr)
                               (incr-cpu-counters cpu instr)
                               (do-rst cpu gb #x18))))

(setf (aref ops #xe0) (make-instruction
                        :opcode #xe0 :bytes 2 :cycles '(3 0) :asm '(:ld "(FF00+u8),A")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (+ #xff00 (read-memory-at-addr gb (+ (gbcpu-pc cpu) 1)))))
                                 (write-memory-at-addr gb addr (gbcpu-a cpu))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xe1) (make-instruction
                        :opcode #xe1 :bytes 1 :cycles '(3 0) :asm '(:pop "HL")
                        :fun (lambda (cpu gb instr)
                               (set-reg-pair-hl-to-val cpu (pop-addr-from-stack cpu gb))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xe2) (make-instruction
                        :opcode #xe2 :bytes 1 :cycles '(2 0) :asm '(:ld "(FF00+C),A")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (+ #xff00 (gbcpu-c cpu))))
                                 (write-memory-at-addr gb addr (gbcpu-a cpu))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xe5) (make-instruction
                        :opcode #xe5 :bytes 1 :cycles '(4 0) :asm '(:push "HL")
                        :fun (lambda (cpu gb instr)
                               (push-reg-pair-on-stack cpu gb (gbcpu-h cpu) (gbcpu-l cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xe6) (make-instruction
                        :opcode #xe6 :bytes 2 :cycles '(2 0) :asm '(:and "A,u8")
                        :fun (lambda (cpu gb instr)
                               (let* ((b (read-memory-at-addr gb (+ (gbcpu-pc cpu) 1)))
                                      (res (and-op cpu (gbcpu-a cpu) b)))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xe7) (make-instruction
                        :opcode #xe7 :bytes 1 :cycles '(4 0) :asm '(:rst "20h")
                        :fun (lambda (cpu gb instr)
                               (incr-cpu-counters cpu instr)
                               (do-rst cpu gb #x20))))
(setf (aref ops #xe8) (make-instruction
                        :opcode #xe8 :bytes 2 :cycles '(4 0) :asm '(:add "SP,i8")
                        :fun (lambda (cpu gb instr)
                               (let ((b (read-memory-at-addr gb (+ (gbcpu-pc cpu) 1))))
                                 (setf (gbcpu-sp cpu) (add-signed-byte-to-sp cpu b))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xe9) (make-instruction
                        :opcode #xe9 :bytes 1 :cycles '(1 0) :asm '(:jp "HL")
                        :fun (lambda (cpu gb instr)
                               (incr-cpu-counters cpu instr)
                               (do-jump cpu (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))))
(setf (aref ops #xea) (make-instruction
                        :opcode #xea :bytes 3 :cycles '(4 0) :asm '(:ld "(u16),A")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-memory gb (+ (gbcpu-pc cpu) 1))))
                                 (write-memory-at-addr gb addr (gbcpu-a cpu))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xee) (make-instruction
                        :opcode #xee :bytes 2 :cycles '(2 0) :asm '(:xor "A,u8")
                        :fun (lambda (cpu gb instr)
                               (let* ((b (read-memory-at-addr gb (+ (gbcpu-pc cpu) 1)))
                                      (res (xor cpu (gbcpu-a cpu) b)))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xef) (make-instruction
                        :opcode #xef :bytes 1 :cycles '(4 0) :asm '(:rst "28h")
                        :fun (lambda (cpu gb instr)
                               (incr-cpu-counters cpu instr)
                               (do-rst cpu gb #x28))))

(setf (aref ops #xf0) (make-instruction
                        :opcode #xf0 :bytes 2 :cycles '(3 0) :asm '(:ld "A,(FF00+u8)")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (+ #xff00 (read-memory-at-addr gb (+ (gbcpu-pc cpu) 1)))))
                                 (setf (gbcpu-a cpu) (read-memory-at-addr gb addr))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xf1) (make-instruction
                        :opcode #xf1 :bytes 1 :cycles '(3 0) :asm '(:pop "AF")
                        :fun (lambda (cpu gb instr)
                               (set-reg-pair-af-to-val cpu (pop-addr-from-stack cpu gb))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xf2) (make-instruction
                        :opcode #xf2 :bytes 1 :cycles '(2 0) :asm '(:ld "A,(FF00+C)")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (+ #xff00 (gbcpu-c cpu))))
                                 (setf (gbcpu-a cpu) (read-memory-at-addr gb addr))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xf3) (make-instruction
                        :opcode #xf3 :bytes 1 :cycles '(1 0) :asm '(:di)
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-int-ena cpu) 0)
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xf5) (make-instruction
                        :opcode #xf5 :bytes 1 :cycles '(4 0) :asm '(:push "AF")
                        :fun (lambda (cpu gb instr)
                               (push-reg-pair-on-stack cpu gb (gbcpu-a cpu) (flags-into-byte (gbcpu-flags cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xf6) (make-instruction
                        :opcode #xf6 :bytes 2 :cycles '(2 0) :asm '(:or "A,u8")
                        :fun (lambda (cpu gb instr)
                               (let* ((b (read-memory-at-addr gb (+ (gbcpu-pc cpu) 1)))
                                      (res (or-op cpu (gbcpu-a cpu) b)))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xf7) (make-instruction
                        :opcode #xf7 :bytes 1 :cycles '(4 0) :asm '(:rst "30h")
                        :fun (lambda (cpu gb instr) (incr-cpu-counters cpu instr)
                               (do-rst cpu gb #x30))))
(setf (aref ops #xf8) (make-instruction
                        :opcode #xf8 :bytes 2 :cycles '(3 0) :asm '(:add "HL,SP+i8")
                        :fun (lambda (cpu gb instr)
                               (let ((b (read-memory-at-addr gb (+ (gbcpu-pc cpu) 1))))
                                 (set-reg-pair-hl-to-val cpu (add-signed-byte-to-sp cpu b))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xf9) (make-instruction
                        :opcode #xf9 :bytes 1 :cycles '(2 0) :asm '(:ld "SP,HL")
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-sp cpu) (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xfa) (make-instruction
                        :opcode #xfa :bytes 3 :cycles '(4 0) :asm '(:ld "A,(u16)")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-memory gb (+ (gbcpu-pc cpu) 1))))
                                 (setf (gbcpu-a cpu) (read-memory-at-addr gb addr))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xfb) (make-instruction
                        :opcode #xfb :bytes 1 :cycles '(1 0) :asm '(:ei)
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-int-ena cpu) 1)
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xfe) (make-instruction
                        :opcode #xfe :bytes 2 :cycles '(2 0) :asm '(:cp "A,u8")
                        :fun (lambda (cpu gb instr)
                               (let ((val (read-memory-at-addr gb (+ (gbcpu-pc cpu) 1))))
                                 (cp-reg-with-val cpu (gbcpu-a cpu) val)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xff) (make-instruction
                        :opcode #xff :bytes 1 :cycles '(4 0) :asm '(:rst "38h")
                        :fun (lambda (cpu gb instr)
                               (incr-cpu-counters cpu instr)
                               (do-rst cpu gb #x38))))

;; CB prefix op codes
(loop for reg in '(b c d e h l a)
      for op-lsb in '(#x0 #x1 #x2 #x3 #x4 #x5 #x7) do
      (setf (aref cb-ops op-lsb)          (defcbprefop op-lsb          #'rot-left-c-reg reg)
            (aref cb-ops (+ op-lsb #x08)) (defcbprefop (+ op-lsb #x08) #'rot-right-c-reg reg)
            (aref cb-ops (+ op-lsb #x10)) (defcbprefop (+ op-lsb #x10) #'rot-left-reg reg)
            (aref cb-ops (+ op-lsb #x18)) (defcbprefop (+ op-lsb #x18) #'rot-right-reg reg)
            (aref cb-ops (+ op-lsb #x20)) (defcbprefop (+ op-lsb #x20) #'sla reg)
            (aref cb-ops (+ op-lsb #x28)) (defcbprefop (+ op-lsb #x28) #'sra reg)
            (aref cb-ops (+ op-lsb #x30)) (defcbprefop (+ op-lsb #x30) #'swap-reg reg)
            (aref cb-ops (+ op-lsb #x38)) (defcbprefop (+ op-lsb #x38) #'srl reg)))

;; RLC
(setf (aref cb-ops #x06) (make-instruction
                           :opcode #x06 :bytes 2 :cycles '(4 0) :asm '(:rlc "(HL)")
                           :fun (lambda (cpu gb instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (write-memory-at-addr gb addr (rot-left-c-reg cpu (read-memory-at-addr gb addr)))
                                    (incr-cpu-counters cpu instr)))))

;; RRC
(setf (aref cb-ops #x0e) (make-instruction
                           :opcode #x0e :bytes 2 :cycles '(4 0) :asm '(:rrc "(HL)")
                           :fun (lambda (cpu gb instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (write-memory-at-addr gb addr (rot-right-c-reg cpu (read-memory-at-addr gb addr)))
                                    (incr-cpu-counters cpu instr)))))

;; RL
(setf (aref cb-ops #x16) (make-instruction
                           :opcode #x16 :bytes 2 :cycles '(4 0) :asm '(:rl "(HL)")
                           :fun (lambda (cpu gb instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (write-memory-at-addr gb addr (rot-left-reg cpu (read-memory-at-addr gb addr)))
                                    (incr-cpu-counters cpu instr)))))
;; RR
(setf (aref cb-ops #x1e) (make-instruction
                           :opcode #x1e :bytes 2 :cycles '(4 0) :asm '(:rr "(HL)")
                           :fun (lambda (cpu gb instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (write-memory-at-addr gb addr (rot-right-reg cpu (read-memory-at-addr gb addr)))
                                    (incr-cpu-counters cpu instr)))))

;; SLA
(setf (aref cb-ops #x26) (make-instruction
                           :opcode #x26 :bytes 2 :cycles '(4 0) :asm '(:sla "(HL)")
                           :fun (lambda (cpu gb instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (write-memory-at-addr gb addr (sla cpu (read-memory-at-addr gb addr)))
                                    (incr-cpu-counters cpu instr)))))

;; SRA
(setf (aref cb-ops #x2e) (make-instruction
                           :opcode #x2e :bytes 2 :cycles '(4 0) :asm '(:sra "(HL)")
                           :fun (lambda (cpu gb instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (write-memory-at-addr gb addr (sra cpu (read-memory-at-addr gb addr)))
                                    (incr-cpu-counters cpu instr)))))

;; SWAP
(setf (aref cb-ops #x36) (make-instruction
                           :opcode #x36 :bytes 2 :cycles '(4 0) :asm '(:swap "(HL)")
                           :fun (lambda (cpu gb instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (write-memory-at-addr gb addr (swap-reg cpu (read-memory-at-addr gb addr)))
                                    (incr-cpu-counters cpu instr)))))

;; SRL
(setf (aref cb-ops #x3e) (make-instruction
                           :opcode #x3e :bytes 2 :cycles '(4 0) :asm '(:srl "(HL)")
                           :fun (lambda (cpu gb instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (write-memory-at-addr gb addr (srl cpu (read-memory-at-addr gb addr)))
                                    (incr-cpu-counters cpu instr)))))

;; BIT
(loop for op from #x40 to #x7f do
      (if (= (logand op #x7) #x6)
        (setf (aref cb-ops op) (defbithlop op (logand (ash op -3) #x7)))
        (setf (aref cb-ops op)
              (defbitop op (logand (ash op -3) #x7)
                (case (logand op #x7)
                  (#x0 'b) (#x1 'c) (#x2 'd) (#x3 'e) (#x4 'h) (#x5 'l) (#x7 'a))))))

;; RES
(loop for op from #x80 to #xbf do
      (if (= (logand op #x7) #x6)
        (setf (aref cb-ops op) (defreshlop op (logand (ash op -3) #x7)))
        (setf (aref cb-ops op)
              (defresop op (logand (ash op -3) #x7)
                (case (logand op #x7)
                  (#x0 'b) (#x1 'c) (#x2 'd) (#x3 'e) (#x4 'h) (#x5 'l) (#x7 'a))))))

;; SET
(loop for op from #xc0 to #xff do
      (if (= (logand op #x7) #x6)
        (setf (aref cb-ops op) (defsethlop op (logand (ash op -3) #x7)))
        (setf (aref cb-ops op)
              (defsetop op (logand (ash op -3) #x7)
                (case (logand op #x7)
                  (#x0 'b) (#x1 'c) (#x2 'd) (#x3 'e) (#x4 'h) (#x5 'l) (#x7 'a))))))

(defun get-cb-instruction (cpu gb)
  (let* ((op (read-memory-at-addr gb (+ (gbcpu-pc cpu) 1)))
        (instr (aref cb-ops op)))
      (if (instruction-p instr)
        instr
        (format t "Unimplemented CB instruction ~X @ ~X~%" op (gbcpu-pc cpu)))))

