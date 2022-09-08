

(in-package :clboy)

(defstruct instruction
  (opcode #x00)
  (bytes 1)
  (cycles 1)
  (asm '(:nop))
  (fun '(or function null)))


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

(setf (aref ops #x03) (make-instruction
                        :opcode #x03 :bytes 1 :cycles '(2 0) :asm '(:inc "BC")
                        :fun (lambda (cpu gb instr)
                               (set-reg-pair-bc-to-val cpu (incr-reg-pair (gbcpu-b cpu) (gbcpu-c cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x04) (make-instruction
                        :opcode #x04 :bytes 1 :cycles '(1 0) :asm '(:inc "B")
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-b cpu) (incr-reg cpu (gbcpu-b cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x05) (make-instruction
                        :opcode #x05 :bytes 1 :cycles '(1 0) :asm '(:dec "B")
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-b cpu) (decr-reg cpu (gbcpu-b cpu)))
                               (incr-cpu-counters cpu instr))))
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

(setf (aref ops #x0b) (make-instruction
                        :opcode #x0b :bytes 1 :cycles '(2 0) :asm '(:dec "BC")
                        :fun (lambda (cpu gb instr)
                               (set-reg-pair-bc-to-val cpu (decr-reg-pair (gbcpu-b cpu) (gbcpu-c cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x0c) (make-instruction
                        :opcode #x0c :bytes 1 :cycles '(1 0) :asm '(:inc "C")
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-c cpu) (incr-reg cpu (gbcpu-c cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x0d) (make-instruction
                        :opcode #x0d :bytes 1 :cycles '(1 0) :asm '(:dec "C")
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-c cpu) (decr-reg cpu (gbcpu-c cpu)))
                               (incr-cpu-counters cpu instr))))
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
                        :opcode #x10 :bytes 1 :cycles '(1 0) :asm '(:stop)
                        :fun (lambda (cpu gb instr)
                               (setf (gb-stopped? *gb*) t)
                               (write-memory-at-addr gb #xff04 0)
                               (incr-cpu-counters cpu instr))))

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
(setf (aref ops #x13) (make-instruction
                        :opcode #x13 :bytes 1 :cycles '(2 0) :asm '(:inc "DE")
                        :fun (lambda (cpu gb instr)
                               (set-reg-pair-de-to-val cpu (incr-reg-pair (gbcpu-d cpu) (gbcpu-e cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x14) (make-instruction
                        :opcode #x14 :bytes 1 :cycles '(1 0) :asm '(:inc "D")
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-d cpu) (incr-reg cpu (gbcpu-d cpu)))
                               (incr-cpu-counters cpu instr))))

(setf (aref ops #x15) (make-instruction
                        :opcode #x15 :bytes 1 :cycles '(1 0) :asm '(:dec "D")
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-d cpu) (decr-reg cpu (gbcpu-d cpu)))
                               (incr-cpu-counters cpu instr))))

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
(setf (aref ops #x1b) (make-instruction
                        :opcode #x1b :bytes 1 :cycles '(2 0) :asm '(:dec "DE")
                        :fun (lambda (cpu gb instr)
                               (set-reg-pair-de-to-val cpu (decr-reg-pair (gbcpu-d cpu) (gbcpu-e cpu)))
                               (incr-cpu-counters cpu instr))))

(setf (aref ops #x1c) (make-instruction
                        :opcode #x1c :bytes 1 :cycles '(1 0) :asm '(:inc "E")
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-e cpu) (incr-reg cpu (gbcpu-e cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x1d) (make-instruction
                        :opcode #x1d :bytes 1 :cycles '(1 0) :asm '(:dec "E")
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-e cpu) (decr-reg cpu (gbcpu-e cpu)))
                               (incr-cpu-counters cpu instr))))

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
(setf (aref ops #x23) (make-instruction
                        :opcode #x23 :bytes 1 :cycles '(2 0) :asm '(:inc "HL")
                        :fun (lambda (cpu gb instr)
                               (set-reg-pair-hl-to-val cpu (incr-reg-pair (gbcpu-h cpu) (gbcpu-l cpu)))
                               (incr-cpu-counters cpu instr))))

(setf (aref ops #x24) (make-instruction
                        :opcode #x24 :bytes 1 :cycles '(1 0) :asm '(:inc "H")
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-h cpu) (incr-reg cpu (gbcpu-h cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x25) (make-instruction
                        :opcode #x25 :bytes 1 :cycles '(1 0) :asm '(:dec "H")
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-h cpu) (decr-reg cpu (gbcpu-h cpu)))
                               (incr-cpu-counters cpu instr))))
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
(setf (aref ops #x2b) (make-instruction
                        :opcode #x2b :bytes 1 :cycles '(2 0) :asm '(:dec "HL")
                        :fun (lambda (cpu gb instr)
                               (set-reg-pair-hl-to-val cpu (decr-reg-pair (gbcpu-h cpu) (gbcpu-l cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x2c) (make-instruction
                        :opcode #x2c :bytes 1 :cycles '(1 0) :asm '(:inc "L")
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-l cpu) (incr-reg cpu (gbcpu-l cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x2d) (make-instruction
                        :opcode #x2d :bytes 1 :cycles '(1 0) :asm '(:dec "L")
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-l cpu) (decr-reg cpu (gbcpu-l cpu)))
                               (incr-cpu-counters cpu instr))))
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
(setf (aref ops #x3c) (make-instruction
                        :opcode #x3c :bytes 1 :cycles '(1 0) :asm '(:inc "A")
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-a cpu) (incr-reg cpu (gbcpu-a cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x3d) (make-instruction
                        :opcode #x3d :bytes 1 :cycles '(1 0) :asm '(:dec "A")
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-a cpu) (decr-reg cpu (gbcpu-a cpu)))
                               (incr-cpu-counters cpu instr))))
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
(defmacro defldop (op dest src)
`(make-instruction
   :opcode ,op
   :bytes 1
   :cycles '(1 0)
   :asm '(:ld ,dest ,src)
   :fun (lambda (cpu gb instr)
          (setf (,(utils:symb 'gbcpu- dest) cpu) (,(utils:symb 'gbcpu- src) cpu))
          (incr-cpu-counters cpu instr))))

(defmacro defldfromhlop (op dest)
`(make-instruction
   :opcode ,op
   :bytes 1
   :cycles '(2 0)
   :asm '(:ld ,dest :hl)
   :fun (lambda (cpu gb instr)
          (setf (,(utils:symb 'gbcpu- dest) cpu) (get-byte-at-hl cpu gb))
          (incr-cpu-counters cpu instr))))

;; Load Into B
(setf (aref ops #x40) (defldop #x40 :b :b))
(setf (aref ops #x41) (defldop #x41 :b :c))
(setf (aref ops #x42) (defldop #x42 :b :d))
(setf (aref ops #x43) (defldop #x43 :b :e))
(setf (aref ops #x44) (defldop #x44 :b :h))
(setf (aref ops #x45) (defldop #x45 :b :l))
(setf (aref ops #x46) (defldfromhlop #x46 :b))
(setf (aref ops #x47) (defldop #x47 :b :a))

;; Load Into C
(setf (aref ops #x48) (defldop #x48 :c :b))
(setf (aref ops #x49) (defldop #x49 :c :c))
(setf (aref ops #x4a) (defldop #x4a :c :d))
(setf (aref ops #x4b) (defldop #x4b :c :e))
(setf (aref ops #x4c) (defldop #x4c :c :h))
(setf (aref ops #x4d) (defldop #x4d :c :l))
(setf (aref ops #x4e) (defldfromhlop #x4e :c))
(setf (aref ops #x4f) (defldop #x4f :c :a))

;; Load Into D
(setf (aref ops #x50) (defldop #x50 :d :b))
(setf (aref ops #x51) (defldop #x51 :d :c))
(setf (aref ops #x52) (defldop #x52 :d :d))
(setf (aref ops #x53) (defldop #x53 :d :e))
(setf (aref ops #x54) (defldop #x54 :d :h))
(setf (aref ops #x55) (defldop #x55 :d :l))
(setf (aref ops #x56) (defldfromhlop #x56 :d))
(setf (aref ops #x57) (defldop #x57 :d :a))

;; Load Into E
(setf (aref ops #x58) (defldop #x58 :e :b))
(setf (aref ops #x59) (defldop #x59 :e :c))
(setf (aref ops #x5a) (defldop #x5a :e :d))
(setf (aref ops #x5b) (defldop #x5b :e :e))
(setf (aref ops #x5c) (defldop #x5c :e :h))
(setf (aref ops #x5d) (defldop #x5d :e :l))
(setf (aref ops #x5e) (defldfromhlop #x5e :e))
(setf (aref ops #x5f) (defldop #x5f :e :a))

;; Load Into H
(setf (aref ops #x60) (defldop #x60 :h :b))
(setf (aref ops #x61) (defldop #x61 :h :c))
(setf (aref ops #x62) (defldop #x62 :h :d))
(setf (aref ops #x63) (defldop #x63 :h :e))
(setf (aref ops #x64) (defldop #x64 :h :h))
(setf (aref ops #x65) (defldop #x65 :h :l))
(setf (aref ops #x66) (defldfromhlop #x66 :h))
(setf (aref ops #x67) (defldop #x67 :h :a))

;; Load Into L
(setf (aref ops #x68) (defldop #x68 :l :b))
(setf (aref ops #x69) (defldop #x69 :l :c))
(setf (aref ops #x6a) (defldop #x6a :l :d))
(setf (aref ops #x6b) (defldop #x6b :l :e))
(setf (aref ops #x6c) (defldop #x6c :l :h))
(setf (aref ops #x6d) (defldop #x6d :l :l))
(setf (aref ops #x6e) (defldfromhlop #x6e :l))
(setf (aref ops #x6f) (defldop #x6f :l :a))

;; Load Into (HL)
(setf (aref ops #x70) (make-instruction
                        :opcode #x70 :bytes 1 :cycles '(2 0) :asm '(:ld "(HL),B")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                 (write-memory-at-addr gb addr (gbcpu-b cpu))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x71) (make-instruction
                        :opcode #x71 :bytes 1 :cycles '(2 0) :asm '(:ld "(HL),C")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                 (write-memory-at-addr gb addr (gbcpu-c cpu))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x72) (make-instruction
                        :opcode #x72 :bytes 1 :cycles '(2 0) :asm '(:ld "(HL),D")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                 (write-memory-at-addr gb addr (gbcpu-d cpu))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x73) (make-instruction
                        :opcode #x73 :bytes 1 :cycles '(2 0) :asm '(:ld "(HL),E")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                 (write-memory-at-addr gb addr (gbcpu-e cpu))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x74) (make-instruction
                        :opcode #x74 :bytes 1 :cycles '(2 0) :asm '(:ld "(HL),H")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                 (write-memory-at-addr gb addr (gbcpu-h cpu))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x75) (make-instruction
                        :opcode #x75 :bytes 1 :cycles '(2 0) :asm '(:ld "(HL),L")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                 (write-memory-at-addr gb addr (gbcpu-l cpu))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x76) (make-instruction
                        :opcode #x76 :bytes 1 :cycles '(1 0) :asm '(:halt)
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-halted cpu) #x01)
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x77) (make-instruction
                        :opcode #x77 :bytes 1 :cycles '(2 0) :asm '(:ld "(HL),A")
                        :fun (lambda (cpu gb instr)
                               (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                 (write-memory-at-addr gb addr (gbcpu-a cpu))
                                 (incr-cpu-counters cpu instr)))))

;; Load Into A
(setf (aref ops #x78) (defldop #x78 :a :b))
(setf (aref ops #x79) (defldop #x79 :a :c))
(setf (aref ops #x7a) (defldop #x7a :a :d))
(setf (aref ops #x7b) (defldop #x7b :a :e))
(setf (aref ops #x7c) (defldop #x7c :a :h))
(setf (aref ops #x7d) (defldop #x7d :a :l))
(setf (aref ops #x7e) (defldfromhlop #x7e :a))
(setf (aref ops #x7f) (defldop #x7f :a :a))

;; Arithmentic
;; ADDs
(defmacro defaddop (op src)
  `(make-instruction
     :opcode ,op
     :bytes 1
     :cycles '(1 0)
     :asm '(:add :a ,src)
     :fun (lambda (cpu gb instr)
            (let ((res (add cpu (gbcpu-a cpu) (,(utils:symb 'gbcpu- src) cpu))))
              (setf (gbcpu-a cpu) res)
              (incr-cpu-counters cpu instr)))))

(setf (aref ops #x80) (defaddop #x80 :b))
(setf (aref ops #x81) (defaddop #x81 :c))
(setf (aref ops #x82) (defaddop #x82 :d))
(setf (aref ops #x83) (defaddop #x83 :e))
(setf (aref ops #x84) (defaddop #x84 :h))
(setf (aref ops #x85) (defaddop #x85 :l))
(setf (aref ops #x86) (make-instruction
                        :opcode #x86 :bytes 1 :cycles '(2 0) :asm '(:add "A,(HL)")
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-a cpu) (add cpu (gbcpu-a cpu) (get-byte-at-hl cpu gb)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x87) (defaddop #x87 :a))

;; ADCs
(defmacro defadcop (op src)
  `(make-instruction
     :opcode ,op
     :bytes 1
     :cycles '(1 0)
     :asm '(:adc :a ,src)
     :fun (lambda (cpu gb instr)
            (let ((res (adc cpu (gbcpu-a cpu) (,(utils:symb 'gbcpu- src) cpu))))
              (setf (gbcpu-a cpu) res)
              (incr-cpu-counters cpu instr)))))
(setf (aref ops #x88) (defadcop #x88 :b))
(setf (aref ops #x89) (defadcop #x89 :c))
(setf (aref ops #x8a) (defadcop #x8a :d))
(setf (aref ops #x8b) (defadcop #x8b :e))
(setf (aref ops #x8c) (defadcop #x8c :h))
(setf (aref ops #x8d) (defadcop #x8d :l))
(setf (aref ops #x8e) (make-instruction
                        :opcode #x8e :bytes 1 :cycles '(2 0) :asm '(:adc "A,(HL)")
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-a cpu) (adc cpu (gbcpu-a cpu) (get-byte-at-hl cpu gb)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x8f) (defadcop #x8f :a))

;; SUBs
(defmacro defsubop (op src)
  `(make-instruction
     :opcode ,op
     :bytes 1
     :cycles '(1 0)
     :asm '(:sub :a ,src)
     :fun (lambda (cpu gb instr)
            (let ((res (sub cpu (gbcpu-a cpu) (,(utils:symb 'gbcpu- src) cpu))))
              (setf (gbcpu-a cpu) res)
              (incr-cpu-counters cpu instr)))))
(setf (aref ops #x90) (defsubop #x90 :b))
(setf (aref ops #x91) (defsubop #x91 :c))
(setf (aref ops #x92) (defsubop #x92 :d))
(setf (aref ops #x93) (defsubop #x93 :e))
(setf (aref ops #x94) (defsubop #x94 :h))
(setf (aref ops #x95) (defsubop #x95 :l))
(setf (aref ops #x96) (make-instruction
                        :opcode #x96 :bytes 1 :cycles '(2 0) :asm '(:sub "A,(HL)")
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-a cpu) (sub cpu (gbcpu-a cpu) (get-byte-at-hl cpu gb)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x97) (defsubop #x97 :a))

;; SBCs
(defmacro defsbcop (op src)
  `(make-instruction
     :opcode ,op
     :bytes 1
     :cycles '(1 0)
     :asm '(:sbc :a ,src)
     :fun (lambda (cpu gb instr)
            (let ((res (sbc cpu (gbcpu-a cpu) (,(utils:symb 'gbcpu- src) cpu))))
              (setf (gbcpu-a cpu) res)
              (incr-cpu-counters cpu instr)))))
(setf (aref ops #x98) (defsbcop #x98 :b))
(setf (aref ops #x99) (defsbcop #x99 :c))
(setf (aref ops #x9a) (defsbcop #x9a :d))
(setf (aref ops #x9b) (defsbcop #x9b :e))
(setf (aref ops #x9c) (defsbcop #x9c :h))
(setf (aref ops #x9d) (defsbcop #x9d :l))
(setf (aref ops #x9e) (make-instruction
                        :opcode #x9e :bytes 1 :cycles '(2 0) :asm '(:sbc "A,(HL)")
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-a cpu) (sbc cpu (gbcpu-a cpu) (get-byte-at-hl cpu gb)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x9f) (defsbcop #x9f :a))

;; ANDs
(defmacro defandop (op src)
  `(make-instruction
     :opcode ,op
     :bytes 1
     :cycles '(1 0)
     :asm '(:and :a ,src)
     :fun (lambda (cpu gb instr)
            (let ((res (and-op cpu (gbcpu-a cpu) (,(utils:symb 'gbcpu- src) cpu))))
              (setf (gbcpu-a cpu) res)
              (incr-cpu-counters cpu instr)))))
(setf (aref ops #xa0) (defandop #xa0 :b))
(setf (aref ops #xa1) (defandop #xa1 :c))
(setf (aref ops #xa2) (defandop #xa2 :d))
(setf (aref ops #xa3) (defandop #xa3 :e))
(setf (aref ops #xa4) (defandop #xa4 :h))
(setf (aref ops #xa5) (defandop #xa5 :l))
(setf (aref ops #xa6) (make-instruction
                        :opcode #xa6 :bytes 1 :cycles '(2 0) :asm '(:and "A,(HL)")
                        :fun (lambda (cpu gb instr)
                               (let ((res (and-op cpu (gbcpu-a cpu) (get-byte-at-hl cpu gb))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xa7) (defandop #xa7 :a))

;; XORs
(defmacro defxorop (op src)
  `(make-instruction
     :opcode ,op
     :bytes 1
     :cycles '(1 0)
     :asm '(:xor :a ,src)
     :fun (lambda (cpu gb instr)
            (let ((res (xor cpu (gbcpu-a cpu) (,(utils:symb 'gbcpu- src) cpu))))
              (setf (gbcpu-a cpu) res)
              (incr-cpu-counters cpu instr)))))
(setf (aref ops #xa8) (defxorop #xa8 :b))
(setf (aref ops #xa9) (defxorop #xa9 :c))
(setf (aref ops #xaa) (defxorop #xaa :d))
(setf (aref ops #xab) (defxorop #xab :e))
(setf (aref ops #xac) (defxorop #xac :h))
(setf (aref ops #xad) (defxorop #xad :l))
(setf (aref ops #xae) (make-instruction
                        :opcode #xae :bytes 1 :cycles '(2 0) :asm '(:xor "A,(HL)")
                        :fun (lambda (cpu gb instr)
                               (let ((res (xor cpu (gbcpu-a cpu) (get-byte-at-hl cpu gb))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xaf) (defxorop #xaf :a))


;; ORs
(defmacro deforop (op src)
  `(make-instruction
     :opcode ,op
     :bytes 1
     :cycles '(1 0)
     :asm '(:or :a ,src)
     :fun (lambda (cpu gb instr)
            (let ((res (or-op cpu (gbcpu-a cpu) (,(utils:symb 'gbcpu- src) cpu))))
              (setf (gbcpu-a cpu) res)
              (incr-cpu-counters cpu instr)))))
(setf (aref ops #xb0) (deforop #xb0 :b))
(setf (aref ops #xb1) (deforop #xb1 :c))
(setf (aref ops #xb2) (deforop #xb2 :d))
(setf (aref ops #xb3) (deforop #xb3 :e))
(setf (aref ops #xb4) (deforop #xb4 :h))
(setf (aref ops #xb5) (deforop #xb5 :l))
(setf (aref ops #xb6) (make-instruction
                        :opcode #xb6 :bytes 1 :cycles '(2 0) :asm '(:or "A,(HL)")
                        :fun (lambda (cpu gb instr)
                               (setf (gbcpu-a cpu) (or-op cpu (gbcpu-a cpu) (get-byte-at-hl cpu gb)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xb7) (deforop #xb7 :a))

;; CPs
(defmacro defcpop (op src)
  `(make-instruction
     :opcode ,op
     :bytes 1
     :cycles '(1 0)
     :asm '(:cp :a ,src)
     :fun (lambda (cpu gb instr)
            (cp-reg-with-val cpu (gbcpu-a cpu) (,(utils:symb 'gbcpu- src) cpu))
            (incr-cpu-counters cpu instr))))
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
                        :fun (lambda (cpu gb instr)
                               (incr-cpu-counters cpu instr)
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
;; RLC
(defmacro defrlcop (op reg)
  `(make-instruction
     :opcode ,op
     :bytes 2
     :cycles '(2 0)
     :asm '(:rlc ,reg)
     :fun (lambda (cpu gb instr)
            (setf (,(utils:symb 'gbcpu- reg) cpu) (rot-left-c-reg cpu (,(utils:symb 'gbcpu- reg) cpu)))
            (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x00) (defrlcop #x00 :b))
(setf (aref cb-ops #x01) (defrlcop #x01 :c))
(setf (aref cb-ops #x02) (defrlcop #x02 :d))
(setf (aref cb-ops #x03) (defrlcop #x03 :e))
(setf (aref cb-ops #x04) (defrlcop #x04 :h))
(setf (aref cb-ops #x05) (defrlcop #x05 :l))
(setf (aref cb-ops #x06) (make-instruction
                           :opcode #x06 :bytes 2 :cycles '(4 0) :asm '(:rlc "(HL)")
                           :fun (lambda (cpu gb instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (write-memory-at-addr gb addr (rot-left-c-reg cpu (read-memory-at-addr gb addr)))
                                    (incr-cpu-counters cpu instr)))))
(setf (aref cb-ops #x07) (defrlcop #x07 :a))

;; RRC
(defmacro defrrcop (op reg)
  `(make-instruction
     :opcode ,op
     :bytes 2
     :cycles '(2 0)
     :asm '(:rrc ,reg)
     :fun (lambda (cpu gb instr)
            (setf (,(utils:symb 'gbcpu- reg) cpu) (rot-right-c-reg cpu (,(utils:symb 'gbcpu- reg) cpu)))
            (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x08) (defrrcop #x08 :b))
(setf (aref cb-ops #x09) (defrrcop #x09 :c))
(setf (aref cb-ops #x0a) (defrrcop #x0a :d))
(setf (aref cb-ops #x0b) (defrrcop #x0b :e))
(setf (aref cb-ops #x0c) (defrrcop #x0c :h))
(setf (aref cb-ops #x0d) (defrrcop #x0d :l))
(setf (aref cb-ops #x0e) (make-instruction
                           :opcode #x0e :bytes 2 :cycles '(4 0) :asm '(:rrc "(HL)")
                           :fun (lambda (cpu gb instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (write-memory-at-addr gb addr (rot-right-c-reg cpu (read-memory-at-addr gb addr)))
                                    (incr-cpu-counters cpu instr)))))
(setf (aref cb-ops #x0f) (defrrcop #x0f :a))

;; RL
(defmacro defrlop (op reg)
  `(make-instruction
     :opcode ,op
     :bytes 2
     :cycles '(2 0)
     :asm '(:rl ,reg)
     :fun (lambda (cpu gb instr)
            (setf (,(utils:symb 'gbcpu- reg) cpu) (rot-left-reg cpu (,(utils:symb 'gbcpu- reg) cpu)))
            (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x10) (defrlop #x10 :b))
(setf (aref cb-ops #x11) (defrlop #x11 :c))
(setf (aref cb-ops #x12) (defrlop #x12 :d))
(setf (aref cb-ops #x13) (defrlop #x13 :e))
(setf (aref cb-ops #x14) (defrlop #x14 :h))
(setf (aref cb-ops #x15) (defrlop #x15 :l))
(setf (aref cb-ops #x16) (make-instruction
                           :opcode #x16 :bytes 2 :cycles '(4 0) :asm '(:rl "(HL)")
                           :fun (lambda (cpu gb instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (write-memory-at-addr gb addr (rot-left-reg cpu (read-memory-at-addr gb addr)))
                                    (incr-cpu-counters cpu instr)))))
(setf (aref cb-ops #x17) (defrlop #x17 :a))
;; RR
(defmacro defrrop (op reg)
  `(make-instruction
     :opcode ,op
     :bytes 2
     :cycles '(2 0)
     :asm '(:rr ,reg)
     :fun (lambda (cpu gb instr)
            (setf (,(utils:symb 'gbcpu- reg) cpu) (rot-right-reg cpu (,(utils:symb 'gbcpu- reg) cpu)))
            (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x18) (defrrop #x18 :b))
(setf (aref cb-ops #x19) (defrrop #x19 :c))
(setf (aref cb-ops #x1a) (defrrop #x1a :d))
(setf (aref cb-ops #x1b) (defrrop #x1b :e))
(setf (aref cb-ops #x1c) (defrrop #x1c :h))
(setf (aref cb-ops #x1d) (defrrop #x1d :l))
(setf (aref cb-ops #x1e) (make-instruction
                           :opcode #x1e :bytes 2 :cycles '(4 0) :asm '(:rr "(HL)")
                           :fun (lambda (cpu gb instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (write-memory-at-addr gb addr (rot-right-reg cpu (read-memory-at-addr gb addr)))
                                    (incr-cpu-counters cpu instr)))))
(setf (aref cb-ops #x1f) (defrrop #x1f :a))

;; SLA
(defmacro defslaop (op reg)
  `(make-instruction
     :opcode ,op
     :bytes 2
     :cycles '(2 0)
     :asm '(:sla ,reg)
     :fun (lambda (cpu gb instr)
            (setf (,(utils:symb 'gbcpu- reg) cpu) (sla cpu (,(utils:symb 'gbcpu- reg) cpu)))
            (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x20) (defslaop #x20 :b))
(setf (aref cb-ops #x21) (defslaop #x21 :c))
(setf (aref cb-ops #x22) (defslaop #x22 :d))
(setf (aref cb-ops #x23) (defslaop #x23 :e))
(setf (aref cb-ops #x24) (defslaop #x24 :h))
(setf (aref cb-ops #x25) (defslaop #x25 :l))
(setf (aref cb-ops #x26) (make-instruction
                           :opcode #x26 :bytes 2 :cycles '(4 0) :asm '(:sla "(HL)")
                           :fun (lambda (cpu gb instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (write-memory-at-addr gb addr (sla cpu (read-memory-at-addr gb addr)))
                                    (incr-cpu-counters cpu instr)))))
(setf (aref cb-ops #x27) (defslaop #x27 :a))

;; SRA
(defmacro defsraop (op reg)
  `(make-instruction
     :opcode ,op
     :bytes 2
     :cycles '(2 0)
     :asm '(:sra ,reg)
     :fun (lambda (cpu gb instr)
            (setf (,(utils:symb 'gbcpu- reg) cpu) (sra cpu (,(utils:symb 'gbcpu- reg) cpu)))
            (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x28) (defsraop #x28 :b))
(setf (aref cb-ops #x29) (defsraop #x29 :c))
(setf (aref cb-ops #x2a) (defsraop #x2a :d))
(setf (aref cb-ops #x2b) (defsraop #x2b :e))
(setf (aref cb-ops #x2c) (defsraop #x2c :h))
(setf (aref cb-ops #x2d) (defsraop #x2d :l))
(setf (aref cb-ops #x2e) (make-instruction
                           :opcode #x2e :bytes 2 :cycles '(4 0) :asm '(:sra "(HL)")
                           :fun (lambda (cpu gb instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (write-memory-at-addr gb addr (sra cpu (read-memory-at-addr gb addr)))
                                    (incr-cpu-counters cpu instr)))))
(setf (aref cb-ops #x2f) (defsraop #x2f :a))

;; SWAP
(defmacro defswapop (op reg)
  `(make-instruction
     :opcode ,op
     :bytes 2
     :cycles '(2 0)
     :asm '(:swap ,reg)
     :fun (lambda (cpu gb instr)
            (setf (,(utils:symb 'gbcpu- reg) cpu) (swap-reg cpu (,(utils:symb 'gbcpu- reg) cpu)))
            (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x30) (defswapop #x30 :b))
(setf (aref cb-ops #x31) (defswapop #x31 :c))
(setf (aref cb-ops #x32) (defswapop #x32 :d))
(setf (aref cb-ops #x33) (defswapop #x33 :e))
(setf (aref cb-ops #x34) (defswapop #x34 :h))
(setf (aref cb-ops #x35) (defswapop #x35 :l))
(setf (aref cb-ops #x36) (make-instruction
                           :opcode #x36 :bytes 2 :cycles '(4 0) :asm '(:swap "(HL)")
                           :fun (lambda (cpu gb instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (write-memory-at-addr gb addr (swap-reg cpu (read-memory-at-addr gb addr)))
                                    (incr-cpu-counters cpu instr)))))
(setf (aref cb-ops #x37) (defswapop #x37 :a))

;; SRL
(defmacro defsrlop (op reg)
  `(make-instruction
     :opcode ,op
     :bytes 2
     :cycles '(2 0)
     :asm '(:srl ,reg)
     :fun (lambda (cpu gb instr)
            (setf (,(utils:symb 'gbcpu- reg) cpu) (srl cpu (,(utils:symb 'gbcpu- reg) cpu)))
            (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x38) (defsrlop #x38 :b))
(setf (aref cb-ops #x39) (defsrlop #x39 :c))
(setf (aref cb-ops #x3a) (defsrlop #x3a :d))
(setf (aref cb-ops #x3b) (defsrlop #x3b :e))
(setf (aref cb-ops #x3c) (defsrlop #x3c :h))
(setf (aref cb-ops #x3d) (defsrlop #x3d :l))
(setf (aref cb-ops #x3e) (make-instruction
                           :opcode #x3e :bytes 2 :cycles '(4 0) :asm '(:srl "(HL)")
                           :fun (lambda (cpu gb instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (write-memory-at-addr gb addr (srl cpu (read-memory-at-addr gb addr)))
                                    (incr-cpu-counters cpu instr)))))
(setf (aref cb-ops #x3f) (defsrlop #x3f :a))

;; BIT
(defmacro defbitop (op bit-pos reg)
  `(make-instruction
     :opcode ,op
     :bytes 2
     :cycles '(2 0)
     :asm '(:bit ,bit-pos ,reg)
     :fun (lambda (cpu gb instr)
            (test-bit-reg cpu (,(utils:symb 'gbcpu- reg) cpu) ,bit-pos)
            (incr-cpu-counters cpu instr))))
(defmacro defbithlop (op bit-pos)
  `(make-instruction
     :opcode ,op
     :bytes 2
     :cycles '(3 0)
     :asm '(:bit ,bit-pos :hl)
     :fun (lambda (cpu gb instr)
            (test-bit-reg cpu (get-byte-at-hl cpu gb) ,bit-pos)
            (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x40) (defbitop #x40 0 :b))
(setf (aref cb-ops #x41) (defbitop #x41 0 :c))
(setf (aref cb-ops #x42) (defbitop #x42 0 :d))
(setf (aref cb-ops #x43) (defbitop #x43 0 :e))
(setf (aref cb-ops #x44) (defbitop #x44 0 :h))
(setf (aref cb-ops #x45) (defbitop #x45 0 :l))
(setf (aref cb-ops #x46) (defbithlop #x48 0))
(setf (aref cb-ops #x47) (defbitop #x47 0 :a))

(setf (aref cb-ops #x48) (defbitop #x48 1 :b))
(setf (aref cb-ops #x49) (defbitop #x49 1 :c))
(setf (aref cb-ops #x4a) (defbitop #x4a 1 :d))
(setf (aref cb-ops #x4b) (defbitop #x4b 1 :e))
(setf (aref cb-ops #x4c) (defbitop #x4c 1 :h))
(setf (aref cb-ops #x4d) (defbitop #x4d 1 :l))
(setf (aref cb-ops #x4e) (defbithlop #x4e 1))
(setf (aref cb-ops #x4f) (defbitop #x4f 1 :a))

(setf (aref cb-ops #x50) (defbitop #x50 2 :b))
(setf (aref cb-ops #x51) (defbitop #x51 2 :c))
(setf (aref cb-ops #x52) (defbitop #x52 2 :d))
(setf (aref cb-ops #x53) (defbitop #x53 2 :e))
(setf (aref cb-ops #x54) (defbitop #x54 2 :h))
(setf (aref cb-ops #x55) (defbitop #x55 2 :l))
(setf (aref cb-ops #x56) (defbithlop #x56 2))
(setf (aref cb-ops #x57) (defbitop #x57 2 :a))

(setf (aref cb-ops #x58) (defbitop #x58 3 :b))
(setf (aref cb-ops #x59) (defbitop #x59 3 :c))
(setf (aref cb-ops #x5a) (defbitop #x5a 3 :d))
(setf (aref cb-ops #x5b) (defbitop #x5b 3 :e))
(setf (aref cb-ops #x5c) (defbitop #x5c 3 :h))
(setf (aref cb-ops #x5d) (defbitop #x5d 3 :l))
(setf (aref cb-ops #x5e) (defbithlop #x5e 3))
(setf (aref cb-ops #x5f) (defbitop #x5f 3 :a))

(setf (aref cb-ops #x60) (defbitop #x60 4 :b))
(setf (aref cb-ops #x61) (defbitop #x61 4 :c))
(setf (aref cb-ops #x62) (defbitop #x62 4 :d))
(setf (aref cb-ops #x63) (defbitop #x63 4 :e))
(setf (aref cb-ops #x64) (defbitop #x64 4 :h))
(setf (aref cb-ops #x65) (defbitop #x65 4 :l))
(setf (aref cb-ops #x66) (defbithlop #x66 4))
(setf (aref cb-ops #x67) (defbitop #x67 4 :a))

(setf (aref cb-ops #x68) (defbitop #x68 5 :b))
(setf (aref cb-ops #x69) (defbitop #x69 5 :c))
(setf (aref cb-ops #x6a) (defbitop #x6a 5 :d))
(setf (aref cb-ops #x6b) (defbitop #x6b 5 :e))
(setf (aref cb-ops #x6c) (defbitop #x6c 5 :h))
(setf (aref cb-ops #x6d) (defbitop #x6d 5 :l))
(setf (aref cb-ops #x6e) (defbithlop #x6e 5))
(setf (aref cb-ops #x6f) (defbitop #x6f 5 :a))

(setf (aref cb-ops #x70) (defbitop #x70 6 :b))
(setf (aref cb-ops #x71) (defbitop #x71 6 :c))
(setf (aref cb-ops #x72) (defbitop #x72 6 :d))
(setf (aref cb-ops #x73) (defbitop #x73 6 :e))
(setf (aref cb-ops #x74) (defbitop #x74 6 :h))
(setf (aref cb-ops #x75) (defbitop #x75 6 :l))
(setf (aref cb-ops #x76) (defbithlop #x76 6))
(setf (aref cb-ops #x77) (defbitop #x77 6 :a))

(setf (aref cb-ops #x78) (defbitop #x78 7 :b))
(setf (aref cb-ops #x79) (defbitop #x79 7 :c))
(setf (aref cb-ops #x7a) (defbitop #x7a 7 :d))
(setf (aref cb-ops #x7b) (defbitop #x7b 7 :e))
(setf (aref cb-ops #x7c) (defbitop #x7c 7 :h))
(setf (aref cb-ops #x7d) (defbitop #x7d 7 :l))
(setf (aref cb-ops #x7e) (defbithlop #x7e 7))
(setf (aref cb-ops #x7f) (defbitop #x7f 7 :a))

;; RES
(defmacro defresop (op bit-pos reg)
  `(make-instruction
     :opcode ,op
     :bytes 2
     :cycles '(2 0)
     :asm '(:res ,bit-pos ,reg)
     :fun (lambda (cpu gb instr)
            (setf (,(utils:symb 'gbcpu- reg) cpu)
                  (reset-bit-reg cpu (,(utils:symb 'gbcpu- reg) cpu) ,bit-pos))
            (incr-cpu-counters cpu instr))))
(defmacro defreshlop (op bit-pos)
  `(make-instruction
     :opcode ,op
     :bytes 2
     :cycles '(3 0)
     :asm '(:res ,bit-pos :hl)
     :fun (lambda (cpu gb instr)
            (set-byte-at-hl cpu gb (reset-bit-reg cpu (get-byte-at-hl cpu gb) ,bit-pos))
            (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x80) (defresop #x80 0 :b))
(setf (aref cb-ops #x81) (defresop #x81 0 :c))
(setf (aref cb-ops #x82) (defresop #x82 0 :d))
(setf (aref cb-ops #x83) (defresop #x83 0 :e))
(setf (aref cb-ops #x84) (defresop #x84 0 :h))
(setf (aref cb-ops #x85) (defresop #x85 0 :l))
(setf (aref cb-ops #x86) (defreshlop #x88 0))
(setf (aref cb-ops #x87) (defresop #x87 0 :a))

(setf (aref cb-ops #x88) (defresop #x88 1 :b))
(setf (aref cb-ops #x89) (defresop #x89 1 :c))
(setf (aref cb-ops #x8a) (defresop #x8a 1 :d))
(setf (aref cb-ops #x8b) (defresop #x8b 1 :e))
(setf (aref cb-ops #x8c) (defresop #x8c 1 :h))
(setf (aref cb-ops #x8d) (defresop #x8d 1 :l))
(setf (aref cb-ops #x8e) (defreshlop #x8e 1))
(setf (aref cb-ops #x8f) (defresop #x8f 1 :a))

(setf (aref cb-ops #x90) (defresop #x90 2 :b))
(setf (aref cb-ops #x91) (defresop #x91 2 :c))
(setf (aref cb-ops #x92) (defresop #x92 2 :d))
(setf (aref cb-ops #x93) (defresop #x93 2 :e))
(setf (aref cb-ops #x94) (defresop #x94 2 :h))
(setf (aref cb-ops #x95) (defresop #x95 2 :l))
(setf (aref cb-ops #x96) (defreshlop #x96 2))
(setf (aref cb-ops #x97) (defresop #x97 2 :a))

(setf (aref cb-ops #x98) (defresop #x98 3 :b))
(setf (aref cb-ops #x99) (defresop #x99 3 :c))
(setf (aref cb-ops #x9a) (defresop #x9a 3 :d))
(setf (aref cb-ops #x9b) (defresop #x9b 3 :e))
(setf (aref cb-ops #x9c) (defresop #x9c 3 :h))
(setf (aref cb-ops #x9d) (defresop #x9d 3 :l))
(setf (aref cb-ops #x9e) (defreshlop #x9e 3))
(setf (aref cb-ops #x9f) (defresop #x9f 3 :a))

(setf (aref cb-ops #xa0) (defresop #xa0 4 :b))
(setf (aref cb-ops #xa1) (defresop #xa1 4 :c))
(setf (aref cb-ops #xa2) (defresop #xa2 4 :d))
(setf (aref cb-ops #xa3) (defresop #xa3 4 :e))
(setf (aref cb-ops #xa4) (defresop #xa4 4 :h))
(setf (aref cb-ops #xa5) (defresop #xa5 4 :l))
(setf (aref cb-ops #xa6) (defreshlop #xa6 4))
(setf (aref cb-ops #xa7) (defresop #xa7 4 :a))

(setf (aref cb-ops #xa8) (defresop #xa8 5 :b))
(setf (aref cb-ops #xa9) (defresop #xa9 5 :c))
(setf (aref cb-ops #xaa) (defresop #xaa 5 :d))
(setf (aref cb-ops #xab) (defresop #xab 5 :e))
(setf (aref cb-ops #xac) (defresop #xac 5 :h))
(setf (aref cb-ops #xad) (defresop #xad 5 :l))
(setf (aref cb-ops #xae) (defreshlop #xae 5))
(setf (aref cb-ops #xaf) (defresop #xaf 5 :a))

(setf (aref cb-ops #xb0) (defresop #xb0 6 :b))
(setf (aref cb-ops #xb1) (defresop #xb1 6 :c))
(setf (aref cb-ops #xb2) (defresop #xb2 6 :d))
(setf (aref cb-ops #xb3) (defresop #xb3 6 :e))
(setf (aref cb-ops #xb4) (defresop #xb4 6 :h))
(setf (aref cb-ops #xb5) (defresop #xb5 6 :l))
(setf (aref cb-ops #xb6) (defreshlop #xb6 6))
(setf (aref cb-ops #xb7) (defresop #xb7 6 :a))

(setf (aref cb-ops #xb8) (defresop #xb8 7 :b))
(setf (aref cb-ops #xb9) (defresop #xb9 7 :c))
(setf (aref cb-ops #xba) (defresop #xba 7 :d))
(setf (aref cb-ops #xbb) (defresop #xbb 7 :e))
(setf (aref cb-ops #xbc) (defresop #xbc 7 :h))
(setf (aref cb-ops #xbd) (defresop #xbd 7 :l))
(setf (aref cb-ops #xbe) (defreshlop #xbe 7))
(setf (aref cb-ops #xbf) (defresop #xbf 7 :a))

;; SET
(defmacro defsetop (op bit-pos reg)
  `(make-instruction
     :opcode ,op
     :bytes 2
     :cycles '(2 0)
     :asm '(:set ,bit-pos ,reg)
     :fun (lambda (cpu gb instr)
            (setf (,(utils:symb 'gbcpu- reg) cpu)
                  (set-bit-reg cpu (,(utils:symb 'gbcpu- reg) cpu) ,bit-pos))
            (incr-cpu-counters cpu instr))))
(defmacro defsethlop (op bit-pos)
  `(make-instruction
     :opcode ,op
     :bytes 2
     :cycles '(3 0)
     :asm '(:set ,bit-pos :hl)
     :fun (lambda (cpu gb instr)
            (set-byte-at-hl cpu gb (set-bit-reg cpu (get-byte-at-hl cpu gb) ,bit-pos))
            (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #xc0) (defsetop #xc0 0 :b))
(setf (aref cb-ops #xc1) (defsetop #xc1 0 :c))
(setf (aref cb-ops #xc2) (defsetop #xc2 0 :d))
(setf (aref cb-ops #xc3) (defsetop #xc3 0 :e))
(setf (aref cb-ops #xc4) (defsetop #xc4 0 :h))
(setf (aref cb-ops #xc5) (defsetop #xc5 0 :l))
(setf (aref cb-ops #xc6) (defsethlop #xc8 0))
(setf (aref cb-ops #xc7) (defsetop #xc7 0 :a))

(setf (aref cb-ops #xc8) (defsetop #xc8 1 :b))
(setf (aref cb-ops #xc9) (defsetop #xc9 1 :c))
(setf (aref cb-ops #xca) (defsetop #xca 1 :d))
(setf (aref cb-ops #xcb) (defsetop #xcb 1 :e))
(setf (aref cb-ops #xcc) (defsetop #xcc 1 :h))
(setf (aref cb-ops #xcd) (defsetop #xcd 1 :l))
(setf (aref cb-ops #xce) (defsethlop #xce 1))
(setf (aref cb-ops #xcf) (defsetop #xcf 1 :a))

(setf (aref cb-ops #xd0) (defsetop #xd0 2 :b))
(setf (aref cb-ops #xd1) (defsetop #xd1 2 :c))
(setf (aref cb-ops #xd2) (defsetop #xd2 2 :d))
(setf (aref cb-ops #xd3) (defsetop #xd3 2 :e))
(setf (aref cb-ops #xd4) (defsetop #xd4 2 :h))
(setf (aref cb-ops #xd5) (defsetop #xd5 2 :l))
(setf (aref cb-ops #xd6) (defsethlop #xd6 2))
(setf (aref cb-ops #xd7) (defsetop #xd7 2 :a))

(setf (aref cb-ops #xd8) (defsetop #xd8 3 :b))
(setf (aref cb-ops #xd9) (defsetop #xd9 3 :c))
(setf (aref cb-ops #xda) (defsetop #xda 3 :d))
(setf (aref cb-ops #xdb) (defsetop #xdb 3 :e))
(setf (aref cb-ops #xdc) (defsetop #xdc 3 :h))
(setf (aref cb-ops #xdd) (defsetop #xdd 3 :l))
(setf (aref cb-ops #xde) (defsethlop #xde 3))
(setf (aref cb-ops #xdf) (defsetop #xdf 3 :a))

(setf (aref cb-ops #xe0) (defsetop #xe0 4 :b))
(setf (aref cb-ops #xe1) (defsetop #xe1 4 :c))
(setf (aref cb-ops #xe2) (defsetop #xe2 4 :d))
(setf (aref cb-ops #xe3) (defsetop #xe3 4 :e))
(setf (aref cb-ops #xe4) (defsetop #xe4 4 :h))
(setf (aref cb-ops #xe5) (defsetop #xe5 4 :l))
(setf (aref cb-ops #xe6) (defsethlop #xe6 4))
(setf (aref cb-ops #xe7) (defsetop #xe7 4 :a))

(setf (aref cb-ops #xe8) (defsetop #xe8 5 :b))
(setf (aref cb-ops #xe9) (defsetop #xe9 5 :c))
(setf (aref cb-ops #xea) (defsetop #xea 5 :d))
(setf (aref cb-ops #xeb) (defsetop #xeb 5 :e))
(setf (aref cb-ops #xec) (defsetop #xec 5 :h))
(setf (aref cb-ops #xed) (defsetop #xed 5 :l))
(setf (aref cb-ops #xee) (defsethlop #xee 5))
(setf (aref cb-ops #xef) (defsetop #xef 5 :a))

(setf (aref cb-ops #xf0) (defsetop #xf0 6 :b))
(setf (aref cb-ops #xf1) (defsetop #xf1 6 :c))
(setf (aref cb-ops #xf2) (defsetop #xf2 6 :d))
(setf (aref cb-ops #xf3) (defsetop #xf3 6 :e))
(setf (aref cb-ops #xf4) (defsetop #xf4 6 :h))
(setf (aref cb-ops #xf5) (defsetop #xf5 6 :l))
(setf (aref cb-ops #xf6) (defsethlop #xf6 6))
(setf (aref cb-ops #xf7) (defsetop #xf7 6 :a))

(setf (aref cb-ops #xf8) (defsetop #xf8 7 :b))
(setf (aref cb-ops #xf9) (defsetop #xf9 7 :c))
(setf (aref cb-ops #xfa) (defsetop #xfa 7 :d))
(setf (aref cb-ops #xfb) (defsetop #xfb 7 :e))
(setf (aref cb-ops #xfc) (defsetop #xfc 7 :h))
(setf (aref cb-ops #xfd) (defsetop #xfd 7 :l))
(setf (aref cb-ops #xfe) (defsethlop #xfe 7))
(setf (aref cb-ops #xff) (defsetop #xff 7 :a))

(defun get-cb-instruction (cpu gb)
  (let* ((op (read-memory-at-addr gb (+ (gbcpu-pc cpu) 1)))
        (instr (aref cb-ops op)))
      (if (instruction-p instr)
        instr
        (format t "Unimplemented CB instruction ~X @ ~X~%" op (gbcpu-pc cpu)))))

