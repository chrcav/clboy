

;(defpackage :clboy
;  (:use :common-lisp))

;(in-package :clboy)

(defstruct gb
  (cpu (default-cpu) :type gbcpu)
  (mmu (make-gbmmu))
  (ppu (make-gbppu))
  (input (make-gbinput))
  (stopped? nil :type boolean))

(defstruct gbinput
  (down   nil :type boolean)
  (up     nil :type boolean)
  (left   nil :type boolean)
  (right  nil :type boolean)
  (a      nil :type boolean)
  (b      nil :type boolean)
  (start  nil :type boolean)
  (select nil :type boolean))

(defstruct gbcpu
  (a  0 :type (unsigned-byte 8))
  (b  0 :type (unsigned-byte 8))
  (c  0 :type (unsigned-byte 8))
  (d  0 :type (unsigned-byte 8))
  (e  0 :type (unsigned-byte 8))
  (h  0 :type (unsigned-byte 8))
  (l  0 :type (unsigned-byte 8))
  (pc 0 :type (unsigned-byte 16))
  (sp 0 :type (unsigned-byte 16))
  (clock 0)
  (div-clock 0 :type (unsigned-byte 16))
  (int-ena 0 :type (unsigned-byte 1))
  (halted 0 :type (unsigned-byte 1))
  flags (make-gbflags))

(defstruct gbflags
  (z 0 :type (unsigned-byte 1))
  (n 0 :type (unsigned-byte 1))
  (h 0 :type (unsigned-byte 1))
  (c 0 :type (unsigned-byte 1)))

(defstruct gbmmu
  (mem (make-array #x10000 :initial-element 0 :element-type '(unsigned-byte 8)))
  (bios (make-bios))
  (is-bios? t :type boolean))

(defstruct gbppu
  (framebuffer (static-vectors:make-static-vector (* 160 144 3)))
  (framebuffer-a (static-vectors:make-static-vector (* 160 144 4)))
  (cycles 0)
  (cur-line 0))

(defstruct instruction
  (opcode #x00)
  (bytes 1)
  (cycles 1)
  (asm '(:nop))
  (fun '(or function null)))

(defun read-rom-data-from-file (filename) (with-open-file (bin filename :element-type '(unsigned-byte 8))
                                            (loop for b = (read-byte bin nil)
                                                  while b collect b)))

(defun make-bios ()
  (make-array #x100 :initial-contents (read-rom-data-from-file "DMG_ROM.bin")))

(defun replace-memory-with-rom (mmu file) (replace (gbmmu-mem mmu) (read-rom-data-from-file file)))

;; TODO for half carry during additions we know that if the bottom nibble is less than either
;; argument then we had a half carry. It could ADC also if we added the carry to the lesser arg.
(defun set-flags-for-result (flags res)
  (setf (gbflags-z flags) (if (= res #x00) #x01 #x00)
        (gbflags-c flags) (if (> res #xff) #x01 #x00)))

(defun write-memory-at-addr (mmu addr val)
  (setf (aref (gbmmu-mem mmu) addr) val))
(defun read-memory-at-addr (mmu addr)
  (if (< addr #x100)
    (if (gbmmu-is-bios? mmu)
      (aref (gbmmu-bios mmu) addr)
      (aref (gbmmu-mem mmu) addr))
    (aref (gbmmu-mem mmu) addr)))

(defun get-address-from-memory (mmu pc)
  (let* ((lsb (read-memory-at-addr mmu pc))
         (msb (read-memory-at-addr mmu (+ pc 1))))
    (logior lsb (ash msb 8))))

(defun get-address-from-reg-pair (msb lsb)
  (logior lsb (ash msb 8)))


(defun get-byte-from-hl-address (cpu mmu)
  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
    (read-memory-at-addr mmu addr)))

(defun set-reg-pair-bc-to-val (cpu val)
  (setf (gbcpu-b cpu) (logand (ash val -8) #xff)
        (gbcpu-c cpu) (logand val #xff)))
(defun set-reg-pair-de-to-val (cpu val)
  (setf (gbcpu-d cpu) (logand (ash val -8) #xff)
        (gbcpu-e cpu) (logand val #xff)))
(defun set-reg-pair-hl-to-val (cpu val)
  (setf (gbcpu-h cpu) (logand (ash val -8) #xff)
        (gbcpu-l cpu) (logand val #xff)))
(defun set-reg-pair-af-to-val (cpu val)
  (setf (gbcpu-a cpu) (logand (ash val -8) #xff)
        (gbcpu-flags cpu) (flags-from-byte (logand val #xff))))

(defun incr-reg (cpu reg)
  (let ((res (logand (+ reg 1) #xff)))
    (setf (gbflags-z (gbcpu-flags cpu)) (if (= res #x00) #x01 #x00)
          (gbflags-n (gbcpu-flags cpu)) 0
          (gbflags-h (gbcpu-flags cpu)) (if (= (logand #x0f res) #x0) #x01 #x00))
    res))
(defun decr-reg (cpu reg)
  (let ((res (if (= reg 0) 255 (- reg 1))))
    (setf (gbflags-z (gbcpu-flags cpu)) (if (= res #x00) #x01 #x00)
          (gbflags-n (gbcpu-flags cpu)) 1
          (gbflags-h (gbcpu-flags cpu)) (if (= (logand #x0f res) #xf) #x01 #x00))
    res))

(defun incr-reg-pair (msb lsb)
  (let ((res (+ (logior lsb (ash msb 8)) 1)))
    res))
(defun decr-reg-pair (msb lsb)
  (let ((res (- (logior lsb (ash msb 8)) 1)))
    res))

(defun incr-cpu-counters (cpu instr)
  (incf (gbcpu-pc cpu) (instruction-bytes instr))
  (incr-clocks cpu instr))
(defun incr-clocks (cpu instr)
  (incf (gbcpu-clock cpu) (* (car (instruction-cycles instr)) 4))
  (incf (gbcpu-div-clock cpu) (car (instruction-cycles instr))))
(defun incr-branched-clocks (cpu instr)
  (incf (gbcpu-clock cpu) (* (cadr (instruction-cycles instr)) 4))
  (incf (gbcpu-div-clock cpu) (cadr (instruction-cycles instr))))

(defun get-new-addr-from-relative (addr b)
  (let* ((rel (make-signed-from-unsigned b))
        (res (+ addr rel)))
    (if (< res 0) (+ #x10000 res) res)))

(defun add-signed-byte-to-sp (cpu b)
  (let* ((sp (gbcpu-sp cpu))
         (res (get-new-addr-from-relative sp b)))
    (setf (gbflags-z (gbcpu-flags cpu)) #x00
          (gbflags-n (gbcpu-flags cpu)) #x00
          (gbflags-h (gbcpu-flags cpu)) (if (> (+ (logand sp #x0f) (logand b #x0f)) #xf) #x01 #x00)
          (gbflags-c (gbcpu-flags cpu)) (if (> (+ (logand sp #xff) (logand b #xff)) #xff) #x01 #x00))
    (logand res #xffff)))

(defun and-op (cpu val1 val2)
  (let ((res (logand val1 val2)))
    (setf (gbflags-z (gbcpu-flags cpu)) (if (= res #x00) #x01 #x00)
          (gbflags-n (gbcpu-flags cpu)) 0
          (gbflags-h (gbcpu-flags cpu)) 1
          (gbflags-c (gbcpu-flags cpu)) 0)
    res))
(defun or-op (cpu val1 val2)
  (let ((res (logior val1 val2)))
    (setf (gbflags-z (gbcpu-flags cpu)) (if (= res #x00) #x01 #x00)
          (gbflags-n (gbcpu-flags cpu)) 0
          (gbflags-h (gbcpu-flags cpu)) 0
          (gbflags-c (gbcpu-flags cpu)) 0)
    res))
(defun xor (cpu val1 val2)
  (let ((res (logxor val1 val2)))
    (setf (gbflags-z (gbcpu-flags cpu)) (if (= res #x00) #x01 #x00)
          (gbflags-n (gbcpu-flags cpu)) 0
          (gbflags-h (gbcpu-flags cpu)) 0
          (gbflags-c (gbcpu-flags cpu)) 0)
    res))

(defun add16 (cpu val1 val2)
  (let ((res (+ val1 val2)))
    (setf (gbflags-n (gbcpu-flags cpu)) #x00
          (gbflags-h (gbcpu-flags cpu)) (if (> (+ (logand #xfff val1) (logand #xfff val2)) #xfff) #x01 #x00)
          (gbflags-c (gbcpu-flags cpu)) (if (> res #xffff) #x01 #x00))
    (logand res #xffff)))

(defun add (cpu val1 val2 &optional (c 0))
  (let ((res (+ val1 val2 c)))
    (setf (gbflags-z (gbcpu-flags cpu)) (if (= (logand res #xff) #x00) #x01 #x00)
          (gbflags-n (gbcpu-flags cpu)) #x00
          (gbflags-h (gbcpu-flags cpu)) (if (> (+ (logand #x0f val1) (logand #x0f val2) c) #x0f) #x01 #x00)
          (gbflags-c (gbcpu-flags cpu)) (if (> res #xff) #x01 #x00))
    (logand res #xff)))
(defun adc (cpu val1 val2)
  (add cpu val1 val2 (gbflags-c (gbcpu-flags cpu))))

(defun sub (cpu val1 val2 &optional (c 0))
  (let* ((val2-with-c (+ val2 c))
        (res (if (< val1 val2-with-c) (- (+ val1 #x100) val2-with-c) (- val1 val2-with-c))))
    (setf (gbflags-z (gbcpu-flags cpu)) (if (= (logand res #xff) #x00) #x01 #x00)
          (gbflags-n (gbcpu-flags cpu)) 1
          (gbflags-h (gbcpu-flags cpu)) (if (< (logand #x0f val1) (+ (logand #x0f val2) c)) #x01 #x00)
          (gbflags-c (gbcpu-flags cpu)) (if (< val1 val2-with-c) #x01 #x00))
    (logand res #xff)))
(defun sbc (cpu val1 val2)
  (sub cpu val1 val2 (gbflags-c (gbcpu-flags cpu))))

(defun cp-reg-with-val (cpu reg val)
  (sub cpu reg val))


(defun do-call-at-addr (cpu mmu addr)
  (push-addr-on-stack cpu mmu (gbcpu-pc cpu))
  (setf (gbcpu-pc cpu) addr))

(defun do-rst (cpu mmu addr)
  (push-addr-on-stack cpu mmu (gbcpu-pc cpu))
  (setf (gbcpu-pc cpu) addr))

(defun do-jump (cpu addr)
  (setf (gbcpu-pc cpu) addr))

(defun do-ret (cpu mmu)
  (let ((addr (pop-addr-from-stack cpu mmu)))
    (setf (gbcpu-pc cpu) addr)))

(defun flags-into-byte (flags)
  (logior
    (ash (gbflags-c flags) 4)
    (ash (gbflags-h flags) 5)
    (ash (gbflags-n flags) 6)
    (ash (gbflags-z flags) 7)))

(defun flags-from-byte (val)
  (make-gbflags :c (logand (ash val -4) #x01)
                :h  (logand (ash val -5) #x01)
                :n  (logand (ash val -6) #x01)
                :z  (logand (ash val -7) #x01)))

(defun push-addr-on-stack (cpu mmu addr)
  (let ((sp (gbcpu-sp cpu))
        (lsb (logand addr #xff))
        (msb (logand (ash addr -8) #xff)))
    (decf (gbcpu-sp cpu) 2)
    (write-memory-at-addr mmu (- sp 1) msb)
    (write-memory-at-addr mmu (- sp 2) lsb)))
(defun pop-addr-from-stack (cpu mmu)
  (let* ((sp (gbcpu-sp cpu))
         (lsb (read-memory-at-addr mmu sp))
         (msb (read-memory-at-addr mmu (+ sp 1))))
    (incf (gbcpu-sp cpu) 2)
    (logior lsb (ash msb 8))))

(defun push-reg-pair-on-stack (cpu mmu reg1 reg2)
  (let ((sp (gbcpu-sp cpu)))
    (decf (gbcpu-sp cpu) 2)
    (write-memory-at-addr mmu (- sp 1) reg1)
    (write-memory-at-addr mmu (- sp 2) reg2)))


(defparameter ops (make-array #x100 :initial-element nil))
(defparameter cb-ops (make-array #x100 :initial-element nil))

(setf (aref ops #x00) (make-instruction
                        :opcode #x00 :bytes 1 :cycles '(1 0) :asm '(:nop)
                        :fun (lambda (cpu mmu instr) (incr-cpu-counters cpu instr))))
(setf (aref ops #x01) (make-instruction
                        :opcode #x01 :bytes 3 :cycles '(3 0) :asm '(:ld "BC,u16")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-memory mmu (+ (gbcpu-pc cpu) 1))))
                                 (set-reg-pair-bc-to-val cpu addr)
                                 (incr-cpu-counters cpu instr)))))

(setf (aref ops #x02) (make-instruction
                        :opcode #x02 :bytes 1 :cycles '(2 0) :asm '(:ld "(BC),A")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-reg-pair (gbcpu-b cpu) (gbcpu-c cpu))))
                                 (write-memory-at-addr mmu addr (gbcpu-a cpu))
                                 (incr-cpu-counters cpu instr)))))

(setf (aref ops #x03) (make-instruction
                        :opcode #x03 :bytes 1 :cycles '(2 0) :asm '(:inc "BC")
                        :fun (lambda (cpu mmu instr)
                               (set-reg-pair-bc-to-val cpu (incr-reg-pair (gbcpu-b cpu) (gbcpu-c cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x04) (make-instruction
                        :opcode #x04 :bytes 1 :cycles '(1 0) :asm '(:inc "B")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-b cpu) (incr-reg cpu (gbcpu-b cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x05) (make-instruction
                        :opcode #x05 :bytes 1 :cycles '(1 0) :asm '(:dec "B")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-b cpu) (decr-reg cpu (gbcpu-b cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x06) (make-instruction
                        :opcode #x06 :bytes 2 :cycles '(2 0) :asm '(:ld "B,u8")
                        :fun (lambda (cpu mmu instr)
                               (let ((b (read-memory-at-addr mmu  (+ (gbcpu-pc cpu) 1))))
                                 (setf (gbcpu-b cpu) b)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x07) (make-instruction
                        :opcode #x07 :bytes 1 :cycles '(1 0) :asm '(:rlca)
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-a cpu ) (rot-left-c-reg cpu (gbcpu-a cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x08) (make-instruction
                        :opcode #x08 :bytes 3 :cycles '(5 0) :asm '(:ld "(u16),SP")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-memory mmu (+ (gbcpu-pc cpu) 1))))
                                 (write-memory-at-addr mmu addr (logand (gbcpu-sp cpu) #xff))
                                 (write-memory-at-addr mmu (+ addr 1) (logand (ash (gbcpu-sp cpu) -8) #xff))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x09) (make-instruction
                        :opcode #x09 :bytes 1 :cycles '(2 0) :asm '(:add "HL,BC")
                        :fun (lambda (cpu mmu instr)
                               (set-reg-pair-hl-to-val
                                 cpu (add16 cpu
                                            (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))
                                            (get-address-from-reg-pair (gbcpu-b cpu) (gbcpu-c cpu))))
                               (incr-cpu-counters cpu instr))))

(setf (aref ops #x0a)
      (make-instruction
        :opcode #x0a :bytes 1 :cycles '(2 0) :asm '(:ld "A,(BC)")
        :fun (lambda (cpu mmu instr)
               (let* ((addr (get-address-from-reg-pair (gbcpu-b cpu) (gbcpu-c cpu)))
                      (val (read-memory-at-addr mmu  addr)))
                 (setf (gbcpu-a cpu) val)
                 (incr-cpu-counters cpu instr)))))

(setf (aref ops #x0b) (make-instruction
                        :opcode #x0b :bytes 1 :cycles '(2 0) :asm '(:dec "BC")
                        :fun (lambda (cpu mmu instr)
                               (set-reg-pair-bc-to-val cpu (decr-reg-pair (gbcpu-b cpu) (gbcpu-c cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x0c) (make-instruction
                        :opcode #x0c :bytes 1 :cycles '(1 0) :asm '(:inc "C")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-c cpu) (incr-reg cpu (gbcpu-c cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x0d) (make-instruction
                        :opcode #x0d :bytes 1 :cycles '(1 0) :asm '(:dec "C")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-c cpu) (decr-reg cpu (gbcpu-c cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x0e) (make-instruction
                        :opcode #x0e :bytes 2 :cycles '(2 0) :asm '(:ld "C,u8")
                        :fun (lambda (cpu mmu instr)
                               (let ((b (read-memory-at-addr mmu  (+ (gbcpu-pc cpu) 1))))
                                 (setf (gbcpu-c cpu) b)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x0f) (make-instruction
                        :opcode #x0f :bytes 1 :cycles '(1 0) :asm '(:rrca)
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-a cpu ) (rot-right-c-reg cpu (gbcpu-a cpu)))
                               (incr-cpu-counters cpu instr))))


(setf (aref ops #x10) (make-instruction
                        :opcode #x10 :bytes 1 :cycles '(1 0) :asm '(:stop)
                        :fun (lambda (cpu mmu instr)
                               (if (= (gbcpu-int-ena cpu) 1)
                                 (setf (gb-stopped? *gb*) t))
                               (incr-cpu-counters cpu instr))))

(setf (aref ops #x11) (make-instruction
                        :opcode #x11 :bytes 3 :cycles '(3 0) :asm '(:ld "DE,u16")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-memory mmu (+ (gbcpu-pc cpu) 1))))
                                 (set-reg-pair-de-to-val cpu addr)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x12) (make-instruction
                        :opcode #x12 :bytes 1 :cycles '(2 0) :asm '(:ld "(DE),A")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-reg-pair (gbcpu-d cpu) (gbcpu-e cpu))))
                                 (write-memory-at-addr mmu addr (gbcpu-a cpu))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x13) (make-instruction
                        :opcode #x13 :bytes 1 :cycles '(2 0) :asm '(:inc "DE")
                        :fun (lambda (cpu mmu instr)
                               (set-reg-pair-de-to-val cpu (incr-reg-pair (gbcpu-d cpu) (gbcpu-e cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x14) (make-instruction
                        :opcode #x14 :bytes 1 :cycles '(1 0) :asm '(:inc "D")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-d cpu) (incr-reg cpu (gbcpu-d cpu)))
                               (incr-cpu-counters cpu instr))))

(setf (aref ops #x15) (make-instruction
                        :opcode #x15 :bytes 1 :cycles '(1 0) :asm '(:dec "D")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-d cpu) (decr-reg cpu (gbcpu-d cpu)))
                               (incr-cpu-counters cpu instr))))

(setf (aref ops #x16) (make-instruction
                        :opcode #x16 :bytes 2 :cycles '(2 0) :asm '(:ld "D,u8")
                        :fun (lambda (cpu mmu instr)
                               (let ((b (read-memory-at-addr mmu  (+ (gbcpu-pc cpu) 1))))
                                 (setf (gbcpu-d cpu) b)
                                 (incr-cpu-counters cpu instr)))))

(setf (aref ops #x17) (make-instruction
                        :opcode #x17 :bytes 1 :cycles '(1 0) :asm '(:rla)
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-a cpu ) (rot-left-reg cpu (gbcpu-a cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x18) (make-instruction
                        :opcode #x18 :bytes 2 :cycles '(3 0) :asm '(:jr "i8")
                        :fun (lambda (cpu mmu instr)
                               (let* ((b (read-memory-at-addr mmu  (+ (gbcpu-pc cpu) 1)))
                                      (addr (get-new-addr-from-relative (+ (gbcpu-pc cpu) 2) b)))
                                 (incr-cpu-counters cpu instr)
                                 (do-jump cpu addr)))))
(setf (aref ops #x19) (make-instruction
                        :opcode #x19 :bytes 1 :cycles '(2 0) :asm '(:add "HL,DE")
                        :fun (lambda (cpu mmu instr)
                               (set-reg-pair-hl-to-val
                                 cpu (add16 cpu
                                            (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))
                                            (get-address-from-reg-pair (gbcpu-d cpu) (gbcpu-e cpu))))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x1a)
      (make-instruction
        :opcode #x1a :bytes 1 :cycles '(2 0) :asm '(:ld "A,(DE)")
        :fun (lambda (cpu mmu instr)
               (let* ((addr (get-address-from-reg-pair (gbcpu-d cpu) (gbcpu-e cpu)))
                      (val (read-memory-at-addr mmu  addr)))
                 (setf (gbcpu-a cpu) val)
                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x1b) (make-instruction
                        :opcode #x1b :bytes 1 :cycles '(2 0) :asm '(:dec "DE")
                        :fun (lambda (cpu mmu instr)
                               (set-reg-pair-de-to-val cpu (decr-reg-pair (gbcpu-d cpu) (gbcpu-e cpu)))
                               (incr-cpu-counters cpu instr))))

(setf (aref ops #x1c) (make-instruction
                        :opcode #x1c :bytes 1 :cycles '(1 0) :asm '(:inc "E")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-e cpu) (incr-reg cpu (gbcpu-e cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x1d) (make-instruction
                        :opcode #x1d :bytes 1 :cycles '(1 0) :asm '(:dec "E")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-e cpu) (decr-reg cpu (gbcpu-e cpu)))
                               (incr-cpu-counters cpu instr))))

(setf (aref ops #x1e) (make-instruction
                        :opcode #x1e :bytes 2 :cycles '(2 0) :asm '(:ld "E,u8")
                        :fun (lambda (cpu mmu instr)
                               (let ((b (read-memory-at-addr mmu  (+ (gbcpu-pc cpu) 1))))
                                 (setf (gbcpu-e cpu) b)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x1f) (make-instruction
                        :opcode #x1f :bytes 1 :cycles '(1 0) :asm '(:rra)
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-a cpu ) (rot-right-reg cpu (gbcpu-a cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x20) (make-instruction
                        :opcode #x20 :bytes 2 :cycles '(2 1) :asm '(:jr "NZ,i8")
                        :fun (lambda (cpu mmu instr)
                               (let* ((b (read-memory-at-addr mmu  (+ (gbcpu-pc cpu) 1)))
                                      (addr (get-new-addr-from-relative (+ (gbcpu-pc cpu) 2) b)))
                                 (incr-cpu-counters cpu instr)
                                 (if (= (gbflags-z (gbcpu-flags cpu)) #x00)
                                   (progn (incr-branched-clocks cpu instr)
                                          (do-jump cpu addr)))))))
(setf (aref ops #x21) (make-instruction
                        :opcode #x21 :bytes 3 :cycles '(3 0) :asm '(:ld "HL,u16")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-memory mmu (+ (gbcpu-pc cpu) 1))))
                                 (set-reg-pair-hl-to-val cpu addr)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x22) (make-instruction
                        :opcode #x22 :bytes 1 :cycles '(2 0) :asm '(:ld "(HL+),A")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                 (write-memory-at-addr mmu addr (gbcpu-a cpu))
                                 (set-reg-pair-hl-to-val cpu (incr-reg-pair (gbcpu-h cpu) (gbcpu-l cpu)))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x23) (make-instruction
                        :opcode #x23 :bytes 1 :cycles '(2 0) :asm '(:inc "HL")
                        :fun (lambda (cpu mmu instr)
                               (set-reg-pair-hl-to-val cpu (incr-reg-pair (gbcpu-h cpu) (gbcpu-l cpu)))
                               (incr-cpu-counters cpu instr))))

(setf (aref ops #x24) (make-instruction
                        :opcode #x24 :bytes 1 :cycles '(1 0) :asm '(:inc "H")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-h cpu) (incr-reg cpu (gbcpu-h cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x25) (make-instruction
                        :opcode #x25 :bytes 1 :cycles '(1 0) :asm '(:dec "H")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-h cpu) (decr-reg cpu (gbcpu-h cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x26) (make-instruction
                        :opcode #x26 :bytes 2 :cycles '(2 0) :asm '(:ld "H,u8")
                        :fun (lambda (cpu mmu instr)
                               (let ((b (read-memory-at-addr mmu  (+ (gbcpu-pc cpu) 1))))
                                 (setf (gbcpu-h cpu) b)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x27) (make-instruction
                        :opcode #x27 :bytes 1 :cycles '(1 0) :asm '(:daa)
                        :fun (lambda (cpu mmu instr)
                               (let* ((a (gbcpu-a cpu))
                                      (cor-lsb (if (or
                                                     (= (gbflags-h (gbcpu-flags cpu)) #x01)
                                                     (> (logand a #x0f) #x09))
                                                 #x06
                                                 #x00))
                                      (correction (logior cor-lsb (if (or
                                                                        (= (gbflags-c (gbcpu-flags cpu)) #x01)
                                                                        (> a #x99))
                                                                    #x60
                                                                    #x00)))
                                      (res (if (= (gbflags-n (gbcpu-flags cpu)) #x01)
                                             (sub cpu a correction)
                                             (add cpu a correction))))
                                 (setf (gbcpu-a cpu) (logand res #xff)
                                       (gbflags-h (gbcpu-flags cpu)) #x00
                                       (gbflags-c (gbcpu-flags cpu)) (if (> res #x99) #x01 #x00))
                                 (incr-cpu-counters cpu instr)))))

(setf (aref ops #x28) (make-instruction
                        :opcode #x28 :bytes 2 :cycles '(2 1) :asm '(:jr "Z,i8")
                        :fun (lambda (cpu mmu instr)
                               (let* ((b (read-memory-at-addr mmu  (+ (gbcpu-pc cpu) 1)))
                                      (addr (get-new-addr-from-relative (+ (gbcpu-pc cpu) 2) b)))
                                 (incr-cpu-counters cpu instr)
                                 (if (= (gbflags-z (gbcpu-flags cpu)) #x01)
                                   (progn (incr-branched-clocks cpu instr)
                                          (do-jump cpu addr)))))))
(setf (aref ops #x29) (make-instruction
                        :opcode #x29 :bytes 1 :cycles '(2 0) :asm '(:add "HL,HL")
                        :fun (lambda (cpu mmu instr)
                               (set-reg-pair-hl-to-val
                                 cpu (add16 cpu
                                            (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))
                                            (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x2a) (make-instruction
                        :opcode #x2a :bytes 1 :cycles '(2 0) :asm '(:ld "A,(HL+)")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-a cpu) (get-byte-from-hl-address cpu mmu))
                               (set-reg-pair-hl-to-val cpu (incr-reg-pair (gbcpu-h cpu) (gbcpu-l cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x2b) (make-instruction
                        :opcode #x2b :bytes 1 :cycles '(2 0) :asm '(:dec "HL")
                        :fun (lambda (cpu mmu instr)
                               (set-reg-pair-hl-to-val cpu (decr-reg-pair (gbcpu-h cpu) (gbcpu-l cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x2c) (make-instruction
                        :opcode #x2c :bytes 1 :cycles '(1 0) :asm '(:inc "L")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-l cpu) (incr-reg cpu (gbcpu-l cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x2d) (make-instruction
                        :opcode #x2d :bytes 1 :cycles '(1 0) :asm '(:dec "L")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-l cpu) (decr-reg cpu (gbcpu-l cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x2e) (make-instruction
                        :opcode #x2e :bytes 2 :cycles '(2 0) :asm '(:ld "L,u8")
                        :fun (lambda (cpu mmu instr)
                               (let ((b (read-memory-at-addr mmu  (+ (gbcpu-pc cpu) 1))))
                                 (setf (gbcpu-l cpu) b)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x2f) (make-instruction
                        :opcode #x2f :bytes 1 :cycles '(1 0) :asm '(:cpl)
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-a cpu) (logxor (gbcpu-a cpu) #xff)
                                     (gbflags-n (gbcpu-flags cpu)) #x01
                                     (gbflags-h (gbcpu-flags cpu)) #x01)
                               (incr-cpu-counters cpu instr))))

(setf (aref ops #x30) (make-instruction
                        :opcode #x30 :bytes 2 :cycles '(2 1) :asm '(:jr "NC,i8")
                        :fun (lambda (cpu mmu instr)
                               (let* ((b (read-memory-at-addr mmu (+ (gbcpu-pc cpu) 1)))
                                      (addr (get-new-addr-from-relative (+ (gbcpu-pc cpu) 2) b)))
                                 (incr-cpu-counters cpu instr)
                                 (if (= (gbflags-c (gbcpu-flags cpu)) #x00)
                                   (progn (incr-branched-clocks cpu instr)
                                          (do-jump cpu addr)))))))
(setf (aref ops #x31) (make-instruction
                        :opcode #x31 :bytes 3 :cycles '(3 0) :asm '(:ld "SP,u16")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-memory mmu (+ (gbcpu-pc cpu) 1))))
                                 (setf (gbcpu-sp cpu) addr)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x32) (make-instruction
                        :opcode #x32 :bytes 1 :cycles '(2 0) :asm '(:ld "(HL-),A")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                 (write-memory-at-addr mmu addr (gbcpu-a cpu))
                                 (set-reg-pair-hl-to-val cpu (decr-reg-pair (gbcpu-h cpu) (gbcpu-l cpu)))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x33) (make-instruction
                        :opcode #x33 :bytes 1 :cycles '(2 0) :asm '(:inc "SP")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (logand (+ (gbcpu-sp cpu) 1) #xffff)))
                                 (setf (gbcpu-sp cpu) res)
                                 (incr-cpu-counters cpu instr)))))

(setf (aref ops #x34) (make-instruction
                        :opcode #x34 :bytes 1 :cycles '(3 0) :asm '(:inc "(HL)")
                        :fun (lambda (cpu mmu instr)
                               (write-memory-at-addr
                                 mmu
                                 (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))
                                 (incr-reg cpu (get-byte-from-hl-address cpu mmu)))
                               (incr-cpu-counters cpu instr))))

(setf (aref ops #x35) (make-instruction
                        :opcode #x35 :bytes 1 :cycles '(3 0) :asm '(:dec "(HL)")
                        :fun (lambda (cpu mmu instr)
                               (write-memory-at-addr
                                 mmu
                                 (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))
                                 (decr-reg cpu (get-byte-from-hl-address cpu mmu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x36) (make-instruction
                        :opcode #x36 :bytes 2 :cycles '(3 0) :asm '(:ld "(HL),u8")
                        :fun (lambda (cpu mmu instr)
                               (let ((b (read-memory-at-addr mmu (+ (gbcpu-pc cpu) 1)))
                                     (addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                 (write-memory-at-addr mmu addr b)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x37) (make-instruction
                        :opcode #x37 :bytes 1 :cycles '(1 0) :asm '(:scf)
                        :fun (lambda (cpu mmu instr)
                               (setf (gbflags-n (gbcpu-flags cpu)) #x00
                                     (gbflags-h (gbcpu-flags cpu)) #x00
                                     (gbflags-c (gbcpu-flags cpu)) #x01)
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x38) (make-instruction
                        :opcode #x38 :bytes 2 :cycles '(2 1) :asm '(:jr "C,i8")
                        :fun (lambda (cpu mmu instr)
                               (let* ((b (read-memory-at-addr mmu (+ (gbcpu-pc cpu) 1)))
                                      (addr (get-new-addr-from-relative (+ (gbcpu-pc cpu) 2) b)))
                                 (incr-cpu-counters cpu instr)
                                 (if (= (gbflags-c (gbcpu-flags cpu)) #x01)
                                   (progn (incr-branched-clocks cpu instr)
                                          (do-jump cpu addr)))))))
(setf (aref ops #x39) (make-instruction
                        :opcode #x39 :bytes 1 :cycles '(2 0) :asm '(:add "HL,SP")
                        :fun (lambda (cpu mmu instr)
                               (set-reg-pair-hl-to-val
                                 cpu (add16 cpu
                                            (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))
                                            (gbcpu-sp cpu)))
                               (incr-cpu-counters cpu instr))))

(setf (aref ops #x3a) (make-instruction
                        :opcode #x3a :bytes 1 :cycles '(2 0) :asm '(:ld "A,(HL-)")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-a cpu) (get-byte-from-hl-address cpu mmu))
                               (set-reg-pair-hl-to-val cpu (decr-reg-pair (gbcpu-h cpu) (gbcpu-l cpu)))
                               (incr-cpu-counters cpu instr))))

(setf (aref ops #x3b) (make-instruction
                        :opcode #x3b :bytes 1 :cycles '(2 0) :asm '(:dec "SP")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (if (= (gbcpu-sp cpu) 0) #xffff (- (gbcpu-sp cpu) 1))))
                                 (setf (gbcpu-sp cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x3c) (make-instruction
                        :opcode #x3c :bytes 1 :cycles '(1 0) :asm '(:inc "A")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-a cpu) (incr-reg cpu (gbcpu-a cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x3d) (make-instruction
                        :opcode #x3d :bytes 1 :cycles '(1 0) :asm '(:dec "A")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-a cpu) (decr-reg cpu (gbcpu-a cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x3e) (make-instruction
                        :opcode #x3e :bytes 2 :cycles '(2 0) :asm '(:ld "A,u8")
                        :fun (lambda (cpu mmu instr)
                               (let ((b (read-memory-at-addr mmu (+ (gbcpu-pc cpu) 1))))
                                 (setf (gbcpu-a cpu) b)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x3f) (make-instruction
                        :opcode #x3f :bytes 1 :cycles '(1 0) :asm '(:ccf)
                        :fun (lambda (cpu mmu instr)
                               (setf (gbflags-n (gbcpu-flags cpu)) #x00
                                     (gbflags-h (gbcpu-flags cpu)) #x00
                                     (gbflags-c (gbcpu-flags cpu)) (logxor (gbflags-c (gbcpu-flags cpu)) #x01))
                               (incr-cpu-counters cpu instr))))

;; Load Block

;; Load Into B
(setf (aref ops #x40) (make-instruction
                        :opcode #x40 :bytes 1 :cycles '(1 0) :asm '(:ld "B,B")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-b cpu) (gbcpu-b cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x41) (make-instruction
                        :opcode #x41 :bytes 1 :cycles '(1 0) :asm '(:ld "B,C")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-b cpu) (gbcpu-c cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x42) (make-instruction
                        :opcode #x42 :bytes 1 :cycles '(1 0) :asm '(:ld "B,D")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-b cpu) (gbcpu-d cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x43) (make-instruction
                        :opcode #x43 :bytes 1 :cycles '(1 0) :asm '(:ld "B,E")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-b cpu) (gbcpu-e cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x44) (make-instruction
                        :opcode #x44 :bytes 1 :cycles '(1 0) :asm '(:ld "B,H")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-b cpu) (gbcpu-h cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x45) (make-instruction
                        :opcode #x45 :bytes 1 :cycles '(1 0) :asm '(:ld "B,L")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-b cpu) (gbcpu-l cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x46) (make-instruction
                        :opcode #x46 :bytes 1 :cycles '(2 0) :asm '(:ld "B,(HL)")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-b cpu) (get-byte-from-hl-address cpu mmu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x47) (make-instruction
                        :opcode #x47 :bytes 1 :cycles '(1 0) :asm '(:ld "B,A")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-b cpu) (gbcpu-a cpu))
                               (incr-cpu-counters cpu instr))))

;; Load Into C
(setf (aref ops #x48) (make-instruction
                        :opcode #x48 :bytes 1 :cycles '(1 0) :asm '(:ld "C,B")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-c cpu) (gbcpu-b cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x49) (make-instruction
                        :opcode #x49 :bytes 1 :cycles '(1 0) :asm '(:ld "C,C")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-c cpu) (gbcpu-c cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x4a) (make-instruction
                        :opcode #x4a :bytes 1 :cycles '(1 0) :asm '(:ld "C,D")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-c cpu) (gbcpu-d cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x4b) (make-instruction
                        :opcode #x4b :bytes 1 :cycles '(1 0) :asm '(:ld "C,E")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-c cpu) (gbcpu-e cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x4c) (make-instruction
                        :opcode #x4c :bytes 1 :cycles '(1 0) :asm '(:ld "C,H")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-c cpu) (gbcpu-h cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x4d) (make-instruction
                        :opcode #x4d :bytes 1 :cycles '(1 0) :asm '(:ld "C,L")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-c cpu) (gbcpu-l cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x4e) (make-instruction
                        :opcode #x4e :bytes 1 :cycles '(2 0) :asm '(:ld "C,(HL)")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-c cpu) (get-byte-from-hl-address cpu mmu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x4f) (make-instruction
                        :opcode #x4f :bytes 1 :cycles '(1 0) :asm '(:ld "C,A")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-c cpu) (gbcpu-a cpu))
                               (incr-cpu-counters cpu instr))))

;; Load Into D
(setf (aref ops #x50) (make-instruction
                        :opcode #x50 :bytes 1 :cycles '(1 0) :asm '(:ld "D,B")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-d cpu) (gbcpu-b cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x51) (make-instruction
                        :opcode #x51 :bytes 1 :cycles '(1 0) :asm '(:ld "D,C")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-d cpu) (gbcpu-c cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x52) (make-instruction
                        :opcode #x52 :bytes 1 :cycles '(1 0) :asm '(:ld "D,D")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-d cpu) (gbcpu-d cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x53) (make-instruction
                        :opcode #x53 :bytes 1 :cycles '(1 0) :asm '(:ld "D,E")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-d cpu) (gbcpu-e cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x54) (make-instruction
                        :opcode #x54 :bytes 1 :cycles '(1 0) :asm '(:ld "D,H")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-d cpu) (gbcpu-h cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x55) (make-instruction
                        :opcode #x55 :bytes 1 :cycles '(1 0) :asm '(:ld "D,L")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-d cpu) (gbcpu-l cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x56) (make-instruction
                        :opcode #x56 :bytes 1 :cycles '(2 0) :asm '(:ld "D,(HL)")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-d cpu) (get-byte-from-hl-address cpu mmu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x57) (make-instruction
                        :opcode #x57 :bytes 1 :cycles '(1 0) :asm '(:ld "D,A")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-d cpu) (gbcpu-a cpu))
                               (incr-cpu-counters cpu instr))))

;; Load Into E
(setf (aref ops #x58) (make-instruction
                        :opcode #x58 :bytes 1 :cycles '(1 0) :asm '(:ld "E,B")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-e cpu) (gbcpu-b cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x59) (make-instruction
                        :opcode #x59 :bytes 1 :cycles '(1 0) :asm '(:ld "E,C")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-e cpu) (gbcpu-c cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x5a) (make-instruction
                        :opcode #x5a :bytes 1 :cycles '(1 0) :asm '(:ld "E,D")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-e cpu) (gbcpu-d cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x5b) (make-instruction
                        :opcode #x5b :bytes 1 :cycles '(1 0) :asm '(:ld "E,E")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-e cpu) (gbcpu-e cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x5c) (make-instruction
                        :opcode #x5c :bytes 1 :cycles '(1 0) :asm '(:ld "E,H")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-e cpu) (gbcpu-h cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x5d) (make-instruction
                        :opcode #x5d :bytes 1 :cycles '(1 0) :asm '(:ld "E,L")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-e cpu) (gbcpu-l cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x5e) (make-instruction
                        :opcode #x5e :bytes 1 :cycles '(2 0) :asm '(:ld "E,(HL)")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-e cpu) (get-byte-from-hl-address cpu mmu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x5f) (make-instruction
                        :opcode #x5f :bytes 1 :cycles '(1 0) :asm '(:ld "E,A")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-e cpu) (gbcpu-a cpu))
                               (incr-cpu-counters cpu instr))))

;; Load Into H
(setf (aref ops #x60) (make-instruction
                        :opcode #x60 :bytes 1 :cycles '(1 0) :asm '(:ld "H,B")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-h cpu) (gbcpu-b cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x61) (make-instruction
                        :opcode #x61 :bytes 1 :cycles '(1 0) :asm '(:ld "H,C")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-h cpu) (gbcpu-c cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x62) (make-instruction
                        :opcode #x62 :bytes 1 :cycles '(1 0) :asm '(:ld "H,D")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-h cpu) (gbcpu-d cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x63) (make-instruction
                        :opcode #x63 :bytes 1 :cycles '(1 0) :asm '(:ld "H,E")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-h cpu) (gbcpu-e cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x64) (make-instruction
                        :opcode #x64 :bytes 1 :cycles '(1 0) :asm '(:ld "H,H")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-h cpu) (gbcpu-h cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x65) (make-instruction
                        :opcode #x65 :bytes 1 :cycles '(1 0) :asm '(:ld "H,L")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-h cpu) (gbcpu-l cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x66) (make-instruction
                        :opcode #x66 :bytes 1 :cycles '(2 0) :asm '(:ld "H,(HL)")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-h cpu) (get-byte-from-hl-address cpu mmu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x67) (make-instruction
                        :opcode #x67 :bytes 1 :cycles '(1 0) :asm '(:ld "H,A")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-h cpu) (gbcpu-a cpu))
                               (incr-cpu-counters cpu instr))))

;; Load Into L
(setf (aref ops #x68) (make-instruction
                        :opcode #x68 :bytes 1 :cycles '(1 0) :asm '(:ld "L,B")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-l cpu) (gbcpu-b cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x69) (make-instruction
                        :opcode #x69 :bytes 1 :cycles '(1 0) :asm '(:ld "L,C")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-l cpu) (gbcpu-c cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x6a) (make-instruction
                        :opcode #x6a :bytes 1 :cycles '(1 0) :asm '(:ld "L,D")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-l cpu) (gbcpu-d cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x6b) (make-instruction
                        :opcode #x6b :bytes 1 :cycles '(1 0) :asm '(:ld "L,E")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-l cpu) (gbcpu-e cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x6c) (make-instruction
                        :opcode #x6c :bytes 1 :cycles '(1 0) :asm '(:ld "L,H")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-l cpu) (gbcpu-h cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x6d) (make-instruction
                        :opcode #x6d :bytes 1 :cycles '(1 0) :asm '(:ld "L,L")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-l cpu) (gbcpu-l cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x6e) (make-instruction
                        :opcode #x6e :bytes 1 :cycles '(2 0) :asm '(:ld "L,(HL)")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-l cpu) (get-byte-from-hl-address cpu mmu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x6f) (make-instruction
                        :opcode #x6f :bytes 1 :cycles '(1 0) :asm '(:ld "L,A")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-l cpu) (gbcpu-a cpu))
                               (incr-cpu-counters cpu instr))))

;; Load Into (HL)
(setf (aref ops #x70) (make-instruction
                        :opcode #x70 :bytes 1 :cycles '(2 0) :asm '(:ld "(HL),B")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                 (write-memory-at-addr mmu addr (gbcpu-b cpu))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x71) (make-instruction
                        :opcode #x71 :bytes 1 :cycles '(2 0) :asm '(:ld "(HL),C")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                 (write-memory-at-addr mmu addr (gbcpu-c cpu))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x72) (make-instruction
                        :opcode #x72 :bytes 1 :cycles '(2 0) :asm '(:ld "(HL),D")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                 (write-memory-at-addr mmu addr (gbcpu-d cpu))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x73) (make-instruction
                        :opcode #x73 :bytes 1 :cycles '(2 0) :asm '(:ld "(HL),E")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                 (write-memory-at-addr mmu addr (gbcpu-e cpu))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x74) (make-instruction
                        :opcode #x74 :bytes 1 :cycles '(2 0) :asm '(:ld "(HL),H")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                 (write-memory-at-addr mmu addr (gbcpu-h cpu))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x75) (make-instruction
                        :opcode #x75 :bytes 1 :cycles '(2 0) :asm '(:ld "(HL),L")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                 (write-memory-at-addr mmu addr (gbcpu-l cpu))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x76) (make-instruction
                        :opcode #x76 :bytes 1 :cycles '(1 0) :asm '(:halt)
                        :fun (lambda (cpu mmu instr)
                               (if (= (gbcpu-int-ena cpu) 1)
                                 (setf (gbcpu-halted cpu) #x01))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x77) (make-instruction
                        :opcode #x77 :bytes 1 :cycles '(2 0) :asm '(:ld "(HL),A")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                 (write-memory-at-addr mmu addr (gbcpu-a cpu))
                                 (incr-cpu-counters cpu instr)))))

;; Load Into A
(setf (aref ops #x78) (make-instruction
                        :opcode #x78 :bytes 1 :cycles '(1 0) :asm '(:ld "A,B")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-a cpu) (gbcpu-b cpu))
                               (incr-cpu-counters cpu instr))))

(setf (aref ops #x79) (make-instruction
                        :opcode #x79 :bytes 1 :cycles '(1 0) :asm '(:ld "A,C")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-a cpu) (gbcpu-c cpu))
                               (incr-cpu-counters cpu instr))))

(setf (aref ops #x7a) (make-instruction
                        :opcode #x7a :bytes 1 :cycles '(1 0) :asm '(:ld "A,D")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-a cpu) (gbcpu-d cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x7b) (make-instruction
                        :opcode #x7b :bytes 1 :cycles '(1 0) :asm '(:ld "A,E")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-a cpu) (gbcpu-e cpu))
                               (incr-cpu-counters cpu instr))))

(setf (aref ops #x7c) (make-instruction
                        :opcode #x7c :bytes 1 :cycles '(1 0) :asm '(:ld "A,H")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-a cpu) (gbcpu-h cpu))
                               (incr-cpu-counters cpu instr))))

(setf (aref ops #x7d) (make-instruction
                        :opcode #x7d :bytes 1 :cycles '(1 0) :asm '(:ld "A,L")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-a cpu) (gbcpu-l cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x7e) (make-instruction
                        :opcode #x7e :bytes 1 :cycles '(2 0) :asm '(:ld "A,(HL)")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-a cpu) (get-byte-from-hl-address cpu mmu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x7f) (make-instruction
                        :opcode #x7f :bytes 1 :cycles '(1 0) :asm '(:ld "A,A")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-a cpu) (gbcpu-a cpu))
                               (incr-cpu-counters cpu instr))))

;; Arithmentic
;; ADDs
(setf (aref ops #x80) (make-instruction
                        :opcode #x80 :bytes 1 :cycles '(1 0) :asm '(:add "A,B")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (add cpu (gbcpu-a cpu) (gbcpu-b cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x81) (make-instruction
                        :opcode #x81 :bytes 1 :cycles '(1 0) :asm '(:add "A,C")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (add cpu (gbcpu-a cpu) (gbcpu-c cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x82) (make-instruction
                        :opcode #x82 :bytes 1 :cycles '(1 0) :asm '(:add "A,D")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (add cpu (gbcpu-a cpu) (gbcpu-d cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x83) (make-instruction
                        :opcode #x83 :bytes 1 :cycles '(1 0) :asm '(:add "A,E")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (add cpu (gbcpu-a cpu) (gbcpu-e cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x84) (make-instruction
                        :opcode #x84 :bytes 1 :cycles '(1 0) :asm '(:add "A,H")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (add cpu (gbcpu-a cpu) (gbcpu-h cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x85) (make-instruction
                        :opcode #x85 :bytes 1 :cycles '(1 0) :asm '(:add "A,L")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (add cpu (gbcpu-a cpu) (gbcpu-l cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x86) (make-instruction
                        :opcode #x86 :bytes 1 :cycles '(2 0) :asm '(:add "A,(HL)")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-a cpu) (add cpu (gbcpu-a cpu) (get-byte-from-hl-address cpu mmu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x87) (make-instruction
                        :opcode #x87 :bytes 1 :cycles '(1 0) :asm '(:add "A,A")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (add cpu (gbcpu-a cpu) (gbcpu-a cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))

;; ADCs
(setf (aref ops #x88) (make-instruction
                        :opcode #x88 :bytes 1 :cycles '(1 0) :asm '(:adc "A,B")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (adc cpu (gbcpu-a cpu) (gbcpu-b cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x89) (make-instruction
                        :opcode #x89 :bytes 1 :cycles '(1 0) :asm '(:adc "A,C")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (adc cpu (gbcpu-a cpu) (gbcpu-c cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x8a) (make-instruction
                        :opcode #x8a :bytes 1 :cycles '(1 0) :asm '(:adc "A,D")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (adc cpu (gbcpu-a cpu) (gbcpu-d cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x8b) (make-instruction
                        :opcode #x8b :bytes 1 :cycles '(1 0) :asm '(:adc "A,E")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (adc cpu (gbcpu-a cpu) (gbcpu-e cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x8c) (make-instruction
                        :opcode #x8c :bytes 1 :cycles '(1 0) :asm '(:adc "A,H")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (adc cpu (gbcpu-a cpu) (gbcpu-h cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x8d) (make-instruction
                        :opcode #x8d :bytes 1 :cycles '(1 0) :asm '(:adc "A,L")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (adc cpu (gbcpu-a cpu) (gbcpu-l cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x8e) (make-instruction
                        :opcode #x8e :bytes 1 :cycles '(2 0) :asm '(:adc "A,(HL)")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-a cpu) (adc cpu (gbcpu-a cpu) (get-byte-from-hl-address cpu mmu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x8f) (make-instruction
                        :opcode #x8f :bytes 1 :cycles '(1 0) :asm '(:adc "A,A")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (adc cpu (gbcpu-a cpu) (gbcpu-a cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))

;; SUBs
(setf (aref ops #x90) (make-instruction
                        :opcode #x90 :bytes 1 :cycles '(1 0) :asm '(:sub "A,B")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (sub cpu (gbcpu-a cpu) (gbcpu-b cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x91) (make-instruction
                        :opcode #x91 :bytes 1 :cycles '(1 0) :asm '(:sub "A,C")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (sub cpu (gbcpu-a cpu) (gbcpu-c cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x92) (make-instruction
                        :opcode #x92 :bytes 1 :cycles '(1 0) :asm '(:sub "A,D")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (sub cpu (gbcpu-a cpu) (gbcpu-d cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x93) (make-instruction
                        :opcode #x93 :bytes 1 :cycles '(1 0) :asm '(:sub "A,E")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (sub cpu (gbcpu-a cpu) (gbcpu-e cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x94) (make-instruction
                        :opcode #x94 :bytes 1 :cycles '(1 0) :asm '(:sub "A,H")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (sub cpu (gbcpu-a cpu) (gbcpu-h cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x95) (make-instruction
                        :opcode #x95 :bytes 1 :cycles '(1 0) :asm '(:sub "A,L")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (sub cpu (gbcpu-a cpu) (gbcpu-l cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x96) (make-instruction
                        :opcode #x96 :bytes 1 :cycles '(2 0) :asm '(:sub "A,(HL)")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-a cpu) (sub cpu (gbcpu-a cpu) (get-byte-from-hl-address cpu mmu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x97) (make-instruction
                        :opcode #x97 :bytes 1 :cycles '(1 0) :asm '(:sub "A,A")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (sub cpu (gbcpu-a cpu) (gbcpu-a cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))

;; SBCs
(setf (aref ops #x98) (make-instruction
                        :opcode #x98 :bytes 1 :cycles '(1 0) :asm '(:sbc "A,B")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (sbc cpu (gbcpu-a cpu) (gbcpu-b cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x99) (make-instruction
                        :opcode #x99 :bytes 1 :cycles '(1 0) :asm '(:sbc "A,C")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (sbc cpu (gbcpu-a cpu) (gbcpu-c cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x9a) (make-instruction
                        :opcode #x9a :bytes 1 :cycles '(1 0) :asm '(:sbc "A,D")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (sbc cpu (gbcpu-a cpu) (gbcpu-d cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x9b) (make-instruction
                        :opcode #x9b :bytes 1 :cycles '(1 0) :asm '(:sbc "A,E")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (sbc cpu (gbcpu-a cpu) (gbcpu-e cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x9c) (make-instruction
                        :opcode #x9c :bytes 1 :cycles '(1 0) :asm '(:sbc "A,H")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (sbc cpu (gbcpu-a cpu) (gbcpu-h cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x9d) (make-instruction
                        :opcode #x9d :bytes 1 :cycles '(1 0) :asm '(:sbc "A,L")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (sbc cpu (gbcpu-a cpu) (gbcpu-l cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #x9e) (make-instruction
                        :opcode #x9e :bytes 1 :cycles '(2 0) :asm '(:sbc "A,(HL)")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-a cpu) (sbc cpu (gbcpu-a cpu) (get-byte-from-hl-address cpu mmu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #x9f) (make-instruction
                        :opcode #x9f :bytes 1 :cycles '(1 0) :asm '(:sbc "A,A")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (sbc cpu (gbcpu-a cpu) (gbcpu-a cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))

;; ANDs
(setf (aref ops #xa0) (make-instruction
                        :opcode #xa0 :bytes 1 :cycles '(1 0) :asm '(:and "A,B")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (and-op cpu (gbcpu-a cpu) (gbcpu-b cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xa1) (make-instruction
                        :opcode #xa1 :bytes 1 :cycles '(1 0) :asm '(:and "A,C")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (and-op cpu (gbcpu-a cpu) (gbcpu-c cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xa2) (make-instruction
                        :opcode #xa2 :bytes 1 :cycles '(1 0) :asm '(:and "A,D")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (and-op cpu (gbcpu-a cpu) (gbcpu-d cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xa3) (make-instruction
                        :opcode #xa3 :bytes 1 :cycles '(1 0) :asm '(:and "A,E")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (and-op cpu (gbcpu-a cpu) (gbcpu-e cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xa4) (make-instruction
                        :opcode #xa4 :bytes 1 :cycles '(1 0) :asm '(:and "A,H")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (and-op cpu (gbcpu-a cpu) (gbcpu-h cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xa5) (make-instruction
                        :opcode #xa5 :bytes 1 :cycles '(1 0) :asm '(:and "A,L")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (and-op cpu (gbcpu-a cpu) (gbcpu-l cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xa6) (make-instruction
                        :opcode #xa6 :bytes 1 :cycles '(2 0) :asm '(:and "A,(HL)")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (and-op cpu (gbcpu-a cpu) (get-byte-from-hl-address cpu mmu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xa7) (make-instruction
                        :opcode #xa7 :bytes 1 :cycles '(1 0) :asm '(:and "A,A")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (and-op cpu (gbcpu-a cpu) (gbcpu-a cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))

;; XORs
(setf (aref ops #xa8) (make-instruction
                        :opcode #xa8 :bytes 1 :cycles '(1 0) :asm '(:xor "A,B")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (xor cpu (gbcpu-a cpu) (gbcpu-b cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xa9) (make-instruction
                        :opcode #xa9 :bytes 1 :cycles '(1 0) :asm '(:xor "A,C")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (xor cpu (gbcpu-a cpu) (gbcpu-c cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xaa) (make-instruction
                        :opcode #xaa :bytes 1 :cycles '(1 0) :asm '(:xor "A,D")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (xor cpu (gbcpu-a cpu) (gbcpu-d cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xab) (make-instruction
                        :opcode #xab :bytes 1 :cycles '(1 0) :asm '(:xor "A,E")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (xor cpu (gbcpu-a cpu) (gbcpu-e cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xac) (make-instruction
                        :opcode #xac :bytes 1 :cycles '(1 0) :asm '(:xor "A,H")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (xor cpu (gbcpu-a cpu) (gbcpu-h cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xad) (make-instruction
                        :opcode #xad :bytes 1 :cycles '(1 0) :asm '(:xor "A,L")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (xor cpu (gbcpu-a cpu) (gbcpu-l cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xae) (make-instruction
                        :opcode #xae :bytes 1 :cycles '(2 0) :asm '(:xor "A,(HL)")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (xor cpu (gbcpu-a cpu) (get-byte-from-hl-address cpu mmu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xaf) (make-instruction
                        :opcode #xaf :bytes 1 :cycles '(1 0) :asm '(:xor "A,A")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (xor cpu (gbcpu-a cpu) (gbcpu-a cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))


;; ORs
(setf (aref ops #xb0) (make-instruction
                        :opcode #xb0 :bytes 1 :cycles '(1 0) :asm '(:or "A,B")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (or-op cpu (gbcpu-a cpu) (gbcpu-b cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xb1) (make-instruction
                        :opcode #xb1 :bytes 1 :cycles '(1 0) :asm '(:or "A,C")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (or-op cpu (gbcpu-a cpu) (gbcpu-c cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xb2) (make-instruction
                        :opcode #xb2 :bytes 1 :cycles '(1 0) :asm '(:or "A,D")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (or-op cpu (gbcpu-a cpu) (gbcpu-d cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xb3) (make-instruction
                        :opcode #xb3 :bytes 1 :cycles '(1 0) :asm '(:or "A,E")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (or-op cpu (gbcpu-a cpu) (gbcpu-e cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xb4) (make-instruction
                        :opcode #xb4 :bytes 1 :cycles '(1 0) :asm '(:or "A,H")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (or-op cpu (gbcpu-a cpu) (gbcpu-h cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xb5) (make-instruction
                        :opcode #xb5 :bytes 1 :cycles '(1 0) :asm '(:or "A,L")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (or-op cpu (gbcpu-a cpu) (gbcpu-l cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xb6) (make-instruction
                        :opcode #xb6 :bytes 1 :cycles '(2 0) :asm '(:or "A,(HL)")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-a cpu) (or-op cpu (gbcpu-a cpu) (get-byte-from-hl-address cpu mmu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xb7) (make-instruction
                        :opcode #xb7 :bytes 1 :cycles '(1 0) :asm '(:or "A,A")
                        :fun (lambda (cpu mmu instr)
                               (let ((res (or-op cpu (gbcpu-a cpu) (gbcpu-a cpu))))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))

;; CPs
(setf (aref ops #xb8) (make-instruction
                        :opcode #xb8 :bytes 1 :cycles '(1 0) :asm '(:cp "A,B")
                        :fun (lambda (cpu mmu instr)
                               (cp-reg-with-val cpu (gbcpu-a cpu) (gbcpu-b cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xb9) (make-instruction
                        :opcode #xb9 :bytes 1 :cycles '(1 0) :asm '(:cp "A,C")
                        :fun (lambda (cpu mmu instr)
                               (cp-reg-with-val cpu (gbcpu-a cpu) (gbcpu-c cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xba) (make-instruction
                        :opcode #xba :bytes 1 :cycles '(1 0) :asm '(:cp "A,D")
                        :fun (lambda (cpu mmu instr)
                               (cp-reg-with-val cpu (gbcpu-a cpu) (gbcpu-d cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xbb) (make-instruction
                        :opcode #xbb :bytes 1 :cycles '(1 0) :asm '(:cp "A,E")
                        :fun (lambda (cpu mmu instr)
                               (cp-reg-with-val cpu (gbcpu-a cpu) (gbcpu-e cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xbc) (make-instruction
                        :opcode #xbc :bytes 1 :cycles '(1 0) :asm '(:cp "A,H")
                        :fun (lambda (cpu mmu instr)
                               (cp-reg-with-val cpu (gbcpu-a cpu) (gbcpu-h cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xbd) (make-instruction
                        :opcode #xbd :bytes 1 :cycles '(1 0) :asm '(:cp "A,L")
                        :fun (lambda (cpu mmu instr)
                               (cp-reg-with-val cpu (gbcpu-a cpu) (gbcpu-l cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xbe) (make-instruction
                        :opcode #xbe :bytes 1 :cycles '(2 0) :asm '(:cp "A,(HL)")
                        :fun (lambda (cpu mmu instr)
                               (cp-reg-with-val cpu (gbcpu-a cpu) (get-byte-from-hl-address cpu mmu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xbf) (make-instruction
                        :opcode #xbf :bytes 1 :cycles '(1 0) :asm '(:cp "A,A")
                        :fun (lambda (cpu mmu instr)
                               (cp-reg-with-val cpu (gbcpu-a cpu) (gbcpu-a cpu))
                               (incr-cpu-counters cpu instr))))

;; Misc jp call rst ret
(setf (aref ops #xc0) (make-instruction
                        :opcode #xc0 :bytes 1 :cycles '(2 3) :asm '(:ret "NZ")
                        :fun (lambda (cpu mmu instr)
                               (incr-cpu-counters cpu instr)
                               (if (= (gbflags-z (gbcpu-flags cpu)) #x00)
                                 (progn (incr-branched-clocks cpu instr)
                                        (do-ret cpu mmu))))))
(setf (aref ops #xc1) (make-instruction
                        :opcode #xc1 :bytes 1 :cycles '(3 0) :asm '(:pop "BC")
                        :fun (lambda (cpu mmu instr)
                               (set-reg-pair-bc-to-val cpu (pop-addr-from-stack cpu mmu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xc2) (make-instruction
                        :opcode #xc2 :bytes 3 :cycles '(3 1) :asm '(:jp "NZ,u16")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-memory mmu (+ (gbcpu-pc cpu) 1))))
                                 (incr-cpu-counters cpu instr)
                                 (if (= (gbflags-z (gbcpu-flags cpu)) #x00)
                                   (progn (incr-branched-clocks cpu instr)
                                          (do-jump cpu addr)))))))
(setf (aref ops #xc3) (make-instruction
                        :opcode #xc3 :bytes 3 :cycles '(4 0) :asm '(:jp "u16")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-memory mmu (+ (gbcpu-pc cpu) 1))))
                                 (incr-cpu-counters cpu instr)
                                 (do-jump cpu addr)))))
(setf (aref ops #xc4) (make-instruction
                        :opcode #xc4 :bytes 3 :cycles '(3 3) :asm '(:call "NZ,u16")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-memory mmu (+ (gbcpu-pc cpu) 1))))
                                 (incr-cpu-counters cpu instr)
                                 (if (= (gbflags-z (gbcpu-flags cpu)) #x00)
                                   (progn (incr-branched-clocks cpu instr)
                                          (do-call-at-addr cpu mmu addr)))))))
(setf (aref ops #xc5) (make-instruction
                        :opcode #xc5 :bytes 1 :cycles '(4 0) :asm '(:push "BC")
                        :fun (lambda (cpu mmu instr)
                               (push-reg-pair-on-stack cpu mmu (gbcpu-b cpu) (gbcpu-c cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xc6) (make-instruction
                        :opcode #xc6 :bytes 2 :cycles '(2 0) :asm '(:add "A,u8")
                        :fun (lambda (cpu mmu instr)
                               (let ((b (read-memory-at-addr mmu (+ (gbcpu-pc cpu) 1))))
                                 (setf (gbcpu-a cpu) (add cpu (gbcpu-a cpu) b))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xc7) (make-instruction
                        :opcode #xc7 :bytes 1 :cycles '(4 0) :asm '(:rst "00h")
                        :fun (lambda (cpu mmu instr)
                               (incr-cpu-counters cpu instr)
                               (do-rst cpu mmu #x00))))
(setf (aref ops #xc8) (make-instruction
                        :opcode #xc8 :bytes 1 :cycles '(2 3) :asm '(:ret "Z")
                        :fun (lambda (cpu mmu instr)
                               (incr-cpu-counters cpu instr)
                               (if (= (gbflags-z (gbcpu-flags cpu)) #x01)
                                 (progn (incr-branched-clocks cpu instr)
                                        (do-ret cpu mmu))))))
(setf (aref ops #xc9) (make-instruction
                        :opcode #xc9 :bytes 1 :cycles '(4 0) :asm '(:ret)
                        :fun (lambda (cpu mmu instr)
                               (incr-cpu-counters cpu instr)
                               (do-ret cpu mmu))))
(setf (aref ops #xca) (make-instruction
                        :opcode #xca :bytes 3 :cycles '(3 1) :asm '(:jp "Z,u16")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-memory mmu (+ (gbcpu-pc cpu) 1))))
                                 (incr-cpu-counters cpu instr)
                                 (if (= (gbflags-z (gbcpu-flags cpu)) #x01)
                                   (progn (incr-branched-clocks cpu instr)
                                          (do-jump cpu addr)))))))
(setf (aref ops #xcc) (make-instruction
                        :opcode #xcc :bytes 3 :cycles '(3 3) :asm '(:call "Z,u16")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-memory mmu (+ (gbcpu-pc cpu) 1))))
                                 (incr-cpu-counters cpu instr)
                                 (if (= (gbflags-z (gbcpu-flags cpu)) #x01)
                                   (progn (incr-branched-clocks cpu instr)
                                          (do-call-at-addr cpu mmu addr)))))))
(setf (aref ops #xcd) (make-instruction
                        :opcode #xcd :bytes 3 :cycles '(6 0) :asm '(:call "u16")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-memory mmu (+ (gbcpu-pc cpu) 1))))
                                 (incr-cpu-counters cpu instr)
                                 (do-call-at-addr cpu mmu addr)))))
(setf (aref ops #xce) (make-instruction
                        :opcode #xce :bytes 2 :cycles '(2 0) :asm '(:adc "A,u8")
                        :fun (lambda (cpu mmu instr)
                               (let* ((b (read-memory-at-addr mmu (+ (gbcpu-pc cpu) 1)))
                                      (res (adc cpu (gbcpu-a cpu) b)))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xcf) (make-instruction
                        :opcode #xcf :bytes 1 :cycles '(4 0) :asm '(:rst "08h")
                        :fun (lambda (cpu mmu instr)
                               (incr-cpu-counters cpu instr)
                               (do-rst cpu mmu #x08))))

(setf (aref ops #xd0) (make-instruction
                        :opcode #xd0 :bytes 1 :cycles '(2 3) :asm '(:ret "NC")
                        :fun (lambda (cpu mmu instr)
                               (incr-cpu-counters cpu instr)
                               (if (= (gbflags-c (gbcpu-flags cpu)) #x00)
                                 (progn (incr-branched-clocks cpu instr)
                                        (do-ret cpu mmu))))))
(setf (aref ops #xd1) (make-instruction
                        :opcode #xd1 :bytes 1 :cycles '(3 0) :asm '(:pop "DE")
                        :fun (lambda (cpu mmu instr)
                               (set-reg-pair-de-to-val cpu (pop-addr-from-stack cpu mmu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xd2) (make-instruction
                        :opcode #xd2 :bytes 3 :cycles '(3 1) :asm '(:jp "NC,u16")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-memory mmu (+ (gbcpu-pc cpu) 1))))
                                 (incr-cpu-counters cpu instr)
                                 (if (= (gbflags-c (gbcpu-flags cpu)) #x00)
                                   (progn (incr-branched-clocks cpu instr)
                                          (do-jump cpu addr)))))))
(setf (aref ops #xd4) (make-instruction
                        :opcode #xd4 :bytes 3 :cycles '(3 3) :asm '(:call "NC,u16")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-memory mmu (+ (gbcpu-pc cpu) 1))))
                                 (incr-cpu-counters cpu instr)
                                 (if (= (gbflags-c (gbcpu-flags cpu)) #x00)
                                   (progn (incr-branched-clocks cpu instr)
                                          (do-call-at-addr cpu mmu addr)))))))
(setf (aref ops #xd5) (make-instruction
                        :opcode #xd5 :bytes 1 :cycles '(4 0) :asm '(:push "DE")
                        :fun (lambda (cpu mmu instr)
                               (push-reg-pair-on-stack cpu mmu (gbcpu-d cpu) (gbcpu-e cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xd6) (make-instruction
                        :opcode #xd6 :bytes 2 :cycles '(2 0) :asm '(:sub "A,u8")
                        :fun (lambda (cpu mmu instr)
                               (let ((b (read-memory-at-addr mmu (+ (gbcpu-pc cpu) 1))))
                                 (setf (gbcpu-a cpu) (sub cpu (gbcpu-a cpu) b))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xd7) (make-instruction
                        :opcode #xd7 :bytes 1 :cycles '(4 0) :asm '(:rst "10h")
                        :fun (lambda (cpu mmu instr)
                               (incr-cpu-counters cpu instr)
                               (do-rst cpu mmu #x10))))
(setf (aref ops #xd8) (make-instruction
                        :opcode #xd8 :bytes 1 :cycles '(2 3) :asm '(:ret "C")
                        :fun (lambda (cpu mmu instr)
                               (incr-cpu-counters cpu instr)
                               (if (= (gbflags-c (gbcpu-flags cpu)) #x01)
                                 (progn (incr-branched-clocks cpu instr)
                                        (do-ret cpu mmu))))))
(setf (aref ops #xd9) (make-instruction
                        :opcode #xd9 :bytes 1 :cycles '(4 0) :asm '(:reti)
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-int-ena cpu) #x01)
                               (incr-cpu-counters cpu instr)
                               (do-ret cpu mmu))))
(setf (aref ops #xda) (make-instruction
                        :opcode #xda :bytes 3 :cycles '(3 1) :asm '(:jp "C,u16")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-memory mmu (+ (gbcpu-pc cpu) 1))))
                                 (incr-cpu-counters cpu instr)
                                 (if (= (gbflags-c (gbcpu-flags cpu)) #x01)
                                   (progn (incr-branched-clocks cpu instr)
                                          (do-jump cpu addr)))))))
(setf (aref ops #xdc) (make-instruction
                        :opcode #xdc :bytes 3 :cycles '(3 3) :asm '(:call "C,u16")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-memory mmu (+ (gbcpu-pc cpu) 1))))
                                 (incr-cpu-counters cpu instr)
                                 (if (= (gbflags-c (gbcpu-flags cpu)) #x01)
                                   (progn (incr-branched-clocks cpu instr)
                                          (do-call-at-addr cpu mmu addr)))))))
(setf (aref ops #xde) (make-instruction
                        :opcode #xde :bytes 2 :cycles '(2 0) :asm '(:sbc "A,u8")
                        :fun (lambda (cpu mmu instr)
                               (let* ((b (read-memory-at-addr mmu  (+ (gbcpu-pc cpu) 1)))
                                      (res (sbc cpu (gbcpu-a cpu) b)))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xdf) (make-instruction
                        :opcode #xdf :bytes 1 :cycles '(4 0) :asm '(:rst "18h")
                        :fun (lambda (cpu mmu instr)
                               (incr-cpu-counters cpu instr)
                               (do-rst cpu mmu #x18))))

(setf (aref ops #xe0) (make-instruction
                        :opcode #xe0 :bytes 2 :cycles '(3 0) :asm '(:ld "(FF00+u8),A")
                        :fun (lambda (cpu mmu instr)
                               (let* ((b (read-memory-at-addr mmu (+ (gbcpu-pc cpu) 1)))
                                      (addr (+ #xff00 b)))
                                 (write-memory-at-addr mmu addr (gbcpu-a cpu))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xe1) (make-instruction
                        :opcode #xe1 :bytes 1 :cycles '(3 0) :asm '(:pop "HL")
                        :fun (lambda (cpu mmu instr)
                               (set-reg-pair-hl-to-val cpu (pop-addr-from-stack cpu mmu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xe2) (make-instruction
                        :opcode #xe2 :bytes 1 :cycles '(2 0) :asm '(:ld "(FF00+C),A")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (+ #xff00 (gbcpu-c cpu))))
                                 (write-memory-at-addr mmu addr (gbcpu-a cpu))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xe5) (make-instruction
                        :opcode #xe5 :bytes 1 :cycles '(4 0) :asm '(:push "HL")
                        :fun (lambda (cpu mmu instr)
                               (push-reg-pair-on-stack cpu mmu (gbcpu-h cpu) (gbcpu-l cpu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xe6) (make-instruction
                        :opcode #xe6 :bytes 2 :cycles '(2 0) :asm '(:and "A,u8")
                        :fun (lambda (cpu mmu instr)
                               (let* ((b (read-memory-at-addr mmu (+ (gbcpu-pc cpu) 1)))
                                      (res (and-op cpu (gbcpu-a cpu) b)))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xe7) (make-instruction
                        :opcode #xe7 :bytes 1 :cycles '(4 0) :asm '(:rst "20h")
                        :fun (lambda (cpu mmu instr)
                               (incr-cpu-counters cpu instr)
                               (do-rst cpu mmu #x20))))
(setf (aref ops #xe8) (make-instruction
                        :opcode #xe8 :bytes 2 :cycles '(4 0) :asm '(:add "SP,i8")
                        :fun (lambda (cpu mmu instr)
                               (let ((b (read-memory-at-addr mmu (+ (gbcpu-pc cpu) 1))))
                                 (setf (gbcpu-sp cpu) (add-signed-byte-to-sp cpu b))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xe9) (make-instruction
                        :opcode #xe9 :bytes 1 :cycles '(1 0) :asm '(:jp "HL")
                        :fun (lambda (cpu mmu instr)
                               (incr-cpu-counters cpu instr)
                               (do-jump cpu (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))))
(setf (aref ops #xea) (make-instruction
                        :opcode #xea :bytes 3 :cycles '(4 0) :asm '(:ld "(u16),A")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-memory mmu (+ (gbcpu-pc cpu) 1))))
                                 (write-memory-at-addr mmu addr (gbcpu-a cpu))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xee) (make-instruction
                        :opcode #xee :bytes 2 :cycles '(2 0) :asm '(:xor "A,u8")
                        :fun (lambda (cpu mmu instr)
                               (let* ((b (read-memory-at-addr mmu (+ (gbcpu-pc cpu) 1)))
                                      (res (xor cpu (gbcpu-a cpu) b)))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xef) (make-instruction
                        :opcode #xef :bytes 1 :cycles '(4 0) :asm '(:rst "28h")
                        :fun (lambda (cpu mmu instr)
                               (incr-cpu-counters cpu instr)
                               (do-rst cpu mmu #x28))))

(setf (aref ops #xf0) (make-instruction
                        :opcode #xf0 :bytes 2 :cycles '(3 0) :asm '(:ld "A,(FF00+u8)")
                        :fun (lambda (cpu mmu instr)
                               (let* ((b (read-memory-at-addr mmu (+ (gbcpu-pc cpu) 1)))
                                      (addr (+ #xff00 b)))
                                 (setf (gbcpu-a cpu) (read-memory-at-addr mmu addr))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xf1) (make-instruction
                        :opcode #xf1 :bytes 1 :cycles '(3 0) :asm '(:pop "AF")
                        :fun (lambda (cpu mmu instr)
                               (set-reg-pair-af-to-val cpu (pop-addr-from-stack cpu mmu))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xf2) (make-instruction
                        :opcode #xf2 :bytes 1 :cycles '(2 0) :asm '(:ld "A,(FF00+C)")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (+ #xff00 (gbcpu-c cpu))))
                                 (setf (gbcpu-a cpu) (read-memory-at-addr mmu addr))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xf3) (make-instruction
                        :opcode #xf3 :bytes 1 :cycles '(1 0) :asm '(:di)
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-int-ena cpu) 0)
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xf5) (make-instruction
                        :opcode #xf5 :bytes 1 :cycles '(4 0) :asm '(:push "AF")
                        :fun (lambda (cpu mmu instr)
                               (push-reg-pair-on-stack cpu mmu (gbcpu-a cpu) (flags-into-byte (gbcpu-flags cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xf6) (make-instruction
                        :opcode #xf6 :bytes 2 :cycles '(2 0) :asm '(:or "A,u8")
                        :fun (lambda (cpu mmu instr)
                               (let* ((b (read-memory-at-addr mmu (+ (gbcpu-pc cpu) 1)))
                                      (res (or-op cpu (gbcpu-a cpu) b)))
                                 (setf (gbcpu-a cpu) res)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xf7) (make-instruction
                        :opcode #xf7 :bytes 1 :cycles '(4 0) :asm '(:rst "30h")
                        :fun (lambda (cpu mmu instr)
                               (incr-cpu-counters cpu instr)
                               (do-rst cpu mmu #x30))))
(setf (aref ops #xf8) (make-instruction
                        :opcode #xf8 :bytes 2 :cycles '(3 0) :asm '(:add "HL,SP+i8")
                        :fun (lambda (cpu mmu instr)
                               (let ((b (read-memory-at-addr mmu (+ (gbcpu-pc cpu) 1))))
                                 (set-reg-pair-hl-to-val cpu (add-signed-byte-to-sp cpu b))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xf9) (make-instruction
                        :opcode #xf9 :bytes 1 :cycles '(2 0) :asm '(:ld "SP,HL")
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-sp cpu) (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu)))
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xfa) (make-instruction
                        :opcode #xfa :bytes 3 :cycles '(4 0) :asm '(:ld "A,(u16)")
                        :fun (lambda (cpu mmu instr)
                               (let ((addr (get-address-from-memory mmu (+ (gbcpu-pc cpu) 1))))
                                 (setf (gbcpu-a cpu) (read-memory-at-addr mmu addr))
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xfb) (make-instruction
                        :opcode #xfb :bytes 1 :cycles '(1 0) :asm '(:ei)
                        :fun (lambda (cpu mmu instr)
                               (setf (gbcpu-int-ena cpu) 1)
                               (incr-cpu-counters cpu instr))))
(setf (aref ops #xfe) (make-instruction
                        :opcode #xfe :bytes 2 :cycles '(2 0) :asm '(:cp "A,u8")
                        :fun (lambda (cpu mmu instr)
                               (let ((val (read-memory-at-addr mmu (+ (gbcpu-pc cpu) 1))))
                                 (cp-reg-with-val cpu (gbcpu-a cpu) val)
                                 (incr-cpu-counters cpu instr)))))
(setf (aref ops #xff) (make-instruction
                        :opcode #xff :bytes 1 :cycles '(4 0) :asm '(:rst "38h")
                        :fun (lambda (cpu mmu instr)
                               (incr-cpu-counters cpu instr)
                               (do-rst cpu mmu #x38))))

(defun test-bit-reg (cpu val bit-pos)
  (let ((res (logand val (ash #x01 bit-pos))))
    (setf (gbflags-z (gbcpu-flags cpu)) (if (= res #x00) #x01 #x00)
          (gbflags-n (gbcpu-flags cpu)) 0
          (gbflags-h (gbcpu-flags cpu)) 1)
    res))
(defun reset-bit-reg (cpu val bit-pos)
  (let ((res (logand val (logxor (ash #x01 bit-pos) #xff))))
    res))
(defun set-bit-reg (cpu val bit-pos)
  (let ((res (logior val (ash #x01 bit-pos))))
    res))

(defun rot-left-reg (cpu val)
  (let ((b7 (logand (ash val -7) #x01))
        (res (logand (logior (ash val 1) (gbflags-c (gbcpu-flags cpu))) #xff)))
    (setf (gbflags-z (gbcpu-flags cpu)) (if (= res #x00) #x01 #x00)
          (gbflags-n (gbcpu-flags cpu)) 0
          (gbflags-h (gbcpu-flags cpu)) 0
          (gbflags-c (gbcpu-flags cpu)) b7)
    res))
(defun rot-left-c-reg (cpu val)
  (let* ((b7 (logand (ash val -7) #x01))
         (res (logand (logior (ash val 1) b7) #xff)))
    (setf (gbflags-z (gbcpu-flags cpu)) (if (= res #x00) #x01 #x00)
          (gbflags-n (gbcpu-flags cpu)) 0
          (gbflags-h (gbcpu-flags cpu)) 0
          (gbflags-c (gbcpu-flags cpu)) b7)
    res))

(defun rot-right-reg (cpu val)
  (let ((b0 (logand val #x01))
        (res (logand (logior (ash val -1) (ash (gbflags-c (gbcpu-flags cpu)) 7)) #xff)))
    (setf (gbflags-z (gbcpu-flags cpu)) (if (= res #x00) #x01 #x00)
          (gbflags-n (gbcpu-flags cpu)) 0
          (gbflags-h (gbcpu-flags cpu)) 0
          (gbflags-c (gbcpu-flags cpu)) b0)
    res))
(defun rot-right-c-reg (cpu val)
  (let* ((b0 (logand val #x01))
         (res (logand (logior (ash val -1) (ash b0 7)) #xff)))
    (setf (gbflags-z (gbcpu-flags cpu)) (if (= res #x00) #x01 #x00)
          (gbflags-n (gbcpu-flags cpu)) 0
          (gbflags-h (gbcpu-flags cpu)) 0
          (gbflags-c (gbcpu-flags cpu)) b0)
    res))

(defun sla (cpu val)
  (let ((b7 (logand (ash val -7) #x01))
        (res (logand (ash val 1) #xfe)))
    (setf (gbflags-z (gbcpu-flags cpu)) (if (= res #x00) #x01 #x00)
          (gbflags-n (gbcpu-flags cpu)) 0
          (gbflags-h (gbcpu-flags cpu)) 0
          (gbflags-c (gbcpu-flags cpu)) b7)
    res))
(defun sra (cpu val)
  (let* ((b0 (logand val #x01))
        (b7 (logand val #x80))
        (res (logior (logand (ash val -1) #xff) b7)))
    (setf (gbflags-z (gbcpu-flags cpu)) (if (= res #x00) #x01 #x00)
          (gbflags-n (gbcpu-flags cpu)) 0
          (gbflags-h (gbcpu-flags cpu)) 0
          (gbflags-c (gbcpu-flags cpu)) b0)
    res))
(defun srl (cpu val)
  (let ((b0 (logand val #x01))
        (res (logand (ash val -1) #x7f)))
    (setf (gbflags-z (gbcpu-flags cpu)) (if (= res #x00) #x01 #x00)
          (gbflags-n (gbcpu-flags cpu)) 0
          (gbflags-h (gbcpu-flags cpu)) 0
          (gbflags-c (gbcpu-flags cpu)) b0)
    res))

(defun swap-reg (cpu val)
  (let* ((lsb (logand val #xf))
         (msb (logand (ash val -4) #xf))
         (res (logior (ash lsb 4) msb)))
    (setf (gbflags-z (gbcpu-flags cpu)) (if (= res #x00) #x01 #x00)
          (gbflags-n (gbcpu-flags cpu)) 0
          (gbflags-h (gbcpu-flags cpu)) 0
          (gbflags-c (gbcpu-flags cpu)) 0)
    res))

;; RLC
(setf (aref cb-ops #x00) (make-instruction
                           :opcode #x00 :bytes 2 :cycles '(2 0) :asm '(:rlc "B")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-b cpu ) (rot-left-c-reg cpu (gbcpu-b cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x01) (make-instruction
                           :opcode #x01 :bytes 2 :cycles '(2 0) :asm '(:rlc "C")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-c cpu ) (rot-left-c-reg cpu (gbcpu-c cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x02) (make-instruction
                           :opcode #x02 :bytes 2 :cycles '(2 0) :asm '(:rlc "D")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-d cpu ) (rot-left-c-reg cpu (gbcpu-d cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x03) (make-instruction
                           :opcode #x03 :bytes 2 :cycles '(2 0) :asm '(:rlc "E")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-e cpu ) (rot-left-c-reg cpu (gbcpu-e cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x04) (make-instruction
                           :opcode #x04 :bytes 2 :cycles '(2 0) :asm '(:rlc "H")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-h cpu ) (rot-left-c-reg cpu (gbcpu-h cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x05) (make-instruction
                           :opcode #x05 :bytes 2 :cycles '(2 0) :asm '(:rlc "L")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-l cpu ) (rot-left-c-reg cpu (gbcpu-l cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x06) (make-instruction
                           :opcode #x06 :bytes 2 :cycles '(4 0) :asm '(:rlc "(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (write-memory-at-addr mmu addr (rot-left-c-reg cpu (read-memory-at-addr mmu addr)))
                                    (incr-cpu-counters cpu instr)))))
(setf (aref cb-ops #x07) (make-instruction
                           :opcode #x07 :bytes 2 :cycles '(2 0) :asm '(:rlc "A")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-a cpu ) (rot-left-c-reg cpu (gbcpu-a cpu)))
                                  (incr-cpu-counters cpu instr))))

;; RRC
(setf (aref cb-ops #x08) (make-instruction
                           :opcode #x08 :bytes 2 :cycles '(2 0) :asm '(:rrc "B")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-b cpu ) (rot-right-c-reg cpu (gbcpu-b cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x09) (make-instruction
                           :opcode #x09 :bytes 2 :cycles '(2 0) :asm '(:rrc "C")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-c cpu ) (rot-right-c-reg cpu (gbcpu-c cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x0a) (make-instruction
                           :opcode #x0a :bytes 2 :cycles '(2 0) :asm '(:rrc "D")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-d cpu ) (rot-right-c-reg cpu (gbcpu-d cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x0b) (make-instruction
                           :opcode #x0b :bytes 2 :cycles '(2 0) :asm '(:rrc "E")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-e cpu ) (rot-right-c-reg cpu (gbcpu-e cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x0c) (make-instruction
                           :opcode #x0c :bytes 2 :cycles '(2 0) :asm '(:rrc "H")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-h cpu ) (rot-right-c-reg cpu (gbcpu-h cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x0d) (make-instruction
                           :opcode #x0d :bytes 2 :cycles '(2 0) :asm '(:rrc "L")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-l cpu ) (rot-right-c-reg cpu (gbcpu-l cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x0e) (make-instruction
                           :opcode #x0e :bytes 2 :cycles '(4 0) :asm '(:rrc "(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (write-memory-at-addr mmu addr (rot-right-c-reg cpu (read-memory-at-addr mmu addr)))
                                    (incr-cpu-counters cpu instr)))))
(setf (aref cb-ops #x0f) (make-instruction
                           :opcode #x0f :bytes 2 :cycles '(2 0) :asm '(:rrc "A")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-a cpu ) (rot-right-c-reg cpu (gbcpu-a cpu)))
                                  (incr-cpu-counters cpu instr))))

;; RL
(setf (aref cb-ops #x10) (make-instruction
                           :opcode #x10 :bytes 2 :cycles '(2 0) :asm '(:rl "B")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-b cpu ) (rot-left-reg cpu (gbcpu-b cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x11) (make-instruction
                           :opcode #x11 :bytes 2 :cycles '(2 0) :asm '(:rl "C")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-c cpu ) (rot-left-reg cpu (gbcpu-c cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x12) (make-instruction
                           :opcode #x12 :bytes 2 :cycles '(2 0) :asm '(:rl "D")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-d cpu ) (rot-left-reg cpu (gbcpu-d cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x13) (make-instruction
                           :opcode #x13 :bytes 2 :cycles '(2 0) :asm '(:rl "E")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-e cpu ) (rot-left-reg cpu (gbcpu-e cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x14) (make-instruction
                           :opcode #x14 :bytes 2 :cycles '(2 0) :asm '(:rl "H")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-h cpu ) (rot-left-reg cpu (gbcpu-h cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x15) (make-instruction
                           :opcode #x15 :bytes 2 :cycles '(2 0) :asm '(:rl "L")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-l cpu ) (rot-left-reg cpu (gbcpu-l cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x16) (make-instruction
                           :opcode #x16 :bytes 2 :cycles '(4 0) :asm '(:rl "(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (write-memory-at-addr mmu addr (rot-left-reg cpu (read-memory-at-addr mmu addr)))
                                    (incr-cpu-counters cpu instr)))))
(setf (aref cb-ops #x17) (make-instruction
                           :opcode #x17 :bytes 2 :cycles '(2 0) :asm '(:rl "A")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-a cpu ) (rot-left-reg cpu (gbcpu-a cpu)))
                                  (incr-cpu-counters cpu instr))))
;; RR
(setf (aref cb-ops #x18) (make-instruction
                           :opcode #x18 :bytes 2 :cycles '(2 0) :asm '(:rr "B")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-b cpu ) (rot-right-reg cpu (gbcpu-b cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x19) (make-instruction
                           :opcode #x19 :bytes 2 :cycles '(2 0) :asm '(:rr "C")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-c cpu ) (rot-right-reg cpu (gbcpu-c cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x1a) (make-instruction
                           :opcode #x1a :bytes 2 :cycles '(2 0) :asm '(:rr "D")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-d cpu ) (rot-right-reg cpu (gbcpu-d cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x1b) (make-instruction
                           :opcode #x1b :bytes 2 :cycles '(2 0) :asm '(:rr "E")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-e cpu ) (rot-right-reg cpu (gbcpu-e cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x1c) (make-instruction
                           :opcode #x1c :bytes 2 :cycles '(2 0) :asm '(:rr "H")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-h cpu ) (rot-right-reg cpu (gbcpu-h cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x1d) (make-instruction
                           :opcode #x1d :bytes 2 :cycles '(2 0) :asm '(:rr "L")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-l cpu ) (rot-right-reg cpu (gbcpu-l cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x1e) (make-instruction
                           :opcode #x1e :bytes 2 :cycles '(4 0) :asm '(:rr "(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (write-memory-at-addr mmu addr (rot-right-reg cpu (read-memory-at-addr mmu addr)))
                                    (incr-cpu-counters cpu instr)))))
(setf (aref cb-ops #x1f) (make-instruction
                           :opcode #x1f :bytes 2 :cycles '(2 0) :asm '(:rr "A")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-a cpu ) (rot-right-reg cpu (gbcpu-a cpu)))
                                  (incr-cpu-counters cpu instr))))

;; SLA
(setf (aref cb-ops #x20) (make-instruction
                           :opcode #x20 :bytes 2 :cycles '(2 0) :asm '(:sla "B")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-b cpu ) (sla cpu (gbcpu-b cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x21) (make-instruction
                           :opcode #x21 :bytes 2 :cycles '(2 0) :asm '(:sla "C")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-c cpu ) (sla cpu (gbcpu-c cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x22) (make-instruction
                           :opcode #x22 :bytes 2 :cycles '(2 0) :asm '(:sla "D")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-d cpu ) (sla cpu (gbcpu-d cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x23) (make-instruction
                           :opcode #x23 :bytes 2 :cycles '(2 0) :asm '(:sla "E")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-e cpu ) (sla cpu (gbcpu-e cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x24) (make-instruction
                           :opcode #x24 :bytes 2 :cycles '(2 0) :asm '(:sla "H")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-h cpu ) (sla cpu (gbcpu-h cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x25) (make-instruction
                           :opcode #x25 :bytes 2 :cycles '(2 0) :asm '(:sla "L")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-l cpu ) (sla cpu (gbcpu-l cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x26) (make-instruction
                           :opcode #x26 :bytes 2 :cycles '(4 0) :asm '(:sla "(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (write-memory-at-addr mmu addr (sla cpu (read-memory-at-addr mmu addr)))
                                    (incr-cpu-counters cpu instr)))))
(setf (aref cb-ops #x27) (make-instruction
                           :opcode #x27 :bytes 2 :cycles '(2 0) :asm '(:sla "A")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-a cpu ) (sla cpu (gbcpu-a cpu)))
                                  (incr-cpu-counters cpu instr))))

;; SRA
(setf (aref cb-ops #x28) (make-instruction
                           :opcode #x28 :bytes 2 :cycles '(2 0) :asm '(:sra "B")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-b cpu ) (sra cpu (gbcpu-b cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x29) (make-instruction
                           :opcode #x29 :bytes 2 :cycles '(2 0) :asm '(:sra "C")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-c cpu ) (sra cpu (gbcpu-c cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x2a) (make-instruction
                           :opcode #x2a :bytes 2 :cycles '(2 0) :asm '(:sra "D")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-d cpu ) (sra cpu (gbcpu-d cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x2b) (make-instruction
                           :opcode #x2b :bytes 2 :cycles '(2 0) :asm '(:sra "E")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-e cpu ) (sra cpu (gbcpu-e cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x2c) (make-instruction
                           :opcode #x2c :bytes 2 :cycles '(2 0) :asm '(:sra "H")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-h cpu ) (sra cpu (gbcpu-h cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x2d) (make-instruction
                           :opcode #x2d :bytes 2 :cycles '(2 0) :asm '(:sra "L")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-l cpu ) (sra cpu (gbcpu-l cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x2e) (make-instruction
                           :opcode #x2e :bytes 2 :cycles '(4 0) :asm '(:sra "(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (write-memory-at-addr mmu addr (sra cpu (read-memory-at-addr mmu addr)))
                                    (incr-cpu-counters cpu instr)))))
(setf (aref cb-ops #x2f) (make-instruction
                           :opcode #x2f :bytes 2 :cycles '(2 0) :asm '(:sra "A")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-a cpu ) (sra cpu (gbcpu-a cpu)))
                                  (incr-cpu-counters cpu instr))))

;; SWAP
(setf (aref cb-ops #x30) (make-instruction
                           :opcode #x30 :bytes 2 :cycles '(2 0) :asm '(:swap "B")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-b cpu ) (swap-reg cpu (gbcpu-b cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x31) (make-instruction
                           :opcode #x31 :bytes 2 :cycles '(2 0) :asm '(:swap "C")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-c cpu ) (swap-reg cpu (gbcpu-c cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x32) (make-instruction
                           :opcode #x32 :bytes 2 :cycles '(2 0) :asm '(:swap "D")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-d cpu ) (swap-reg cpu (gbcpu-d cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x33) (make-instruction
                           :opcode #x33 :bytes 2 :cycles '(2 0) :asm '(:swap "E")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-e cpu ) (swap-reg cpu (gbcpu-e cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x34) (make-instruction
                           :opcode #x34 :bytes 2 :cycles '(2 0) :asm '(:swap "H")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-h cpu ) (swap-reg cpu (gbcpu-h cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x35) (make-instruction
                           :opcode #x35 :bytes 2 :cycles '(2 0) :asm '(:swap "L")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-l cpu ) (swap-reg cpu (gbcpu-l cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x36) (make-instruction
                           :opcode #x36 :bytes 2 :cycles '(4 0) :asm '(:swap "(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (write-memory-at-addr mmu addr (swap-reg cpu (read-memory-at-addr mmu addr)))
                                    (incr-cpu-counters cpu instr)))))
(setf (aref cb-ops #x37) (make-instruction
                           :opcode #x37 :bytes 2 :cycles '(2 0) :asm '(:swap "(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (set-reg-pair-hl-to-val cpu (swap-reg cpu (get-byte-from-hl-address cpu mmu)))
                                  (incr-cpu-counters cpu instr))))

;; SRL
(setf (aref cb-ops #x38) (make-instruction
                           :opcode #x38 :bytes 2 :cycles '(2 0) :asm '(:srl "B")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-b cpu ) (srl cpu (gbcpu-b cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x39) (make-instruction
                           :opcode #x39 :bytes 2 :cycles '(2 0) :asm '(:srl "C")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-c cpu ) (srl cpu (gbcpu-c cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x3a) (make-instruction
                           :opcode #x3a :bytes 2 :cycles '(2 0) :asm '(:srl "D")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-d cpu ) (srl cpu (gbcpu-d cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x3b) (make-instruction
                           :opcode #x3b :bytes 2 :cycles '(2 0) :asm '(:srl "E")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-e cpu ) (srl cpu (gbcpu-e cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x3c) (make-instruction
                           :opcode #x3c :bytes 2 :cycles '(2 0) :asm '(:srl "H")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-h cpu ) (srl cpu (gbcpu-h cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x3d) (make-instruction
                           :opcode #x3d :bytes 2 :cycles '(2 0) :asm '(:srl "L")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-l cpu ) (srl cpu (gbcpu-l cpu)))
                                  (incr-cpu-counters cpu instr))))
(setf (aref cb-ops #x3e) (make-instruction
                           :opcode #x3e :bytes 2 :cycles '(4 0) :asm '(:srl "(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (write-memory-at-addr mmu addr (srl cpu (read-memory-at-addr mmu addr)))
                                    (incr-cpu-counters cpu instr)))))
(setf (aref cb-ops #x3f) (make-instruction
                           :opcode #x3f :bytes 2 :cycles '(2 0) :asm '(:srl "A")
                           :fun (lambda (cpu mmu instr)
                                  (setf (gbcpu-a cpu ) (srl cpu (gbcpu-a cpu)))
                                  (incr-cpu-counters cpu instr))))

;; BIT
(setf (aref cb-ops #x40) (make-instruction
                           :opcode #x40 :bytes 2 :cycles '(2 0) :asm '(:bit "0,B")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-b cpu) 0))))
(setf (aref cb-ops #x41) (make-instruction
                           :opcode #x41 :bytes 2 :cycles '(2 0) :asm '(:bit "0,C")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-c cpu) 0))))
(setf (aref cb-ops #x42) (make-instruction
                           :opcode #x42 :bytes 2 :cycles '(2 0) :asm '(:bit "0,D")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-d cpu) 0))))
(setf (aref cb-ops #x43) (make-instruction
                           :opcode #x43 :bytes 2 :cycles '(2 0) :asm '(:bit "0,E")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-e cpu) 0))))
(setf (aref cb-ops #x44) (make-instruction
                           :opcode #x44 :bytes 2 :cycles '(2 0) :asm '(:bit "0,H")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-h cpu) 0))))
(setf (aref cb-ops #x45) (make-instruction
                           :opcode #x45 :bytes 2 :cycles '(2 0) :asm '(:bit "0,L")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-l cpu) 0))))
(setf (aref cb-ops #x46) (make-instruction
                           :opcode #x46 :bytes 2 :cycles '(3 0) :asm '(:bit "0,(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (incr-cpu-counters cpu instr)
                                    (test-bit-reg cpu (read-memory-at-addr mmu addr) 0)))))
(setf (aref cb-ops #x47) (make-instruction
                           :opcode #x47 :bytes 2 :cycles '(2 0) :asm '(:bit "0,A")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-a cpu) 0))))

(setf (aref cb-ops #x48) (make-instruction
                           :opcode #x40 :bytes 2 :cycles '(2 0) :asm '(:bit "1,B")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-b cpu) 1))))
(setf (aref cb-ops #x49) (make-instruction
                           :opcode #x41 :bytes 2 :cycles '(2 0) :asm '(:bit "1,C")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-c cpu) 1))))
(setf (aref cb-ops #x4a) (make-instruction
                           :opcode #x42 :bytes 2 :cycles '(2 0) :asm '(:bit "1,D")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-d cpu) 1))))
(setf (aref cb-ops #x4b) (make-instruction
                           :opcode #x43 :bytes 2 :cycles '(2 0) :asm '(:bit "1,E")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-e cpu) 1))))
(setf (aref cb-ops #x4c) (make-instruction
                           :opcode #x44 :bytes 2 :cycles '(2 0) :asm '(:bit "1,H")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-h cpu) 1))))
(setf (aref cb-ops #x4d) (make-instruction
                           :opcode #x45 :bytes 2 :cycles '(2 0) :asm '(:bit "1,L")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-l cpu) 1))))
(setf (aref cb-ops #x4e) (make-instruction
                           :opcode #x46 :bytes 2 :cycles '(3 0) :asm '(:bit "1,(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (incr-cpu-counters cpu instr)
                                    (test-bit-reg cpu (read-memory-at-addr mmu addr) 1)))))
(setf (aref cb-ops #x4f) (make-instruction
                           :opcode #x4f :bytes 2 :cycles '(2 0) :asm '(:bit "1,A")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-a cpu) 1))))
(setf (aref cb-ops #x50) (make-instruction
                           :opcode #x40 :bytes 2 :cycles '(2 0) :asm '(:bit "2,B")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-b cpu) 2))))
(setf (aref cb-ops #x51) (make-instruction
                           :opcode #x41 :bytes 2 :cycles '(2 0) :asm '(:bit "2,C")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-c cpu) 2))))
(setf (aref cb-ops #x52) (make-instruction
                           :opcode #x42 :bytes 2 :cycles '(2 0) :asm '(:bit "2,D")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-d cpu) 2))))
(setf (aref cb-ops #x53) (make-instruction
                           :opcode #x43 :bytes 2 :cycles '(2 0) :asm '(:bit "2,E")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-e cpu) 2))))
(setf (aref cb-ops #x54) (make-instruction
                           :opcode #x44 :bytes 2 :cycles '(2 0) :asm '(:bit "2,H")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-h cpu) 2))))
(setf (aref cb-ops #x55) (make-instruction
                           :opcode #x45 :bytes 2 :cycles '(2 0) :asm '(:bit "2,L")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-l cpu) 2))))
(setf (aref cb-ops #x56) (make-instruction
                           :opcode #x46 :bytes 2 :cycles '(3 0) :asm '(:bit "2,(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (incr-cpu-counters cpu instr)
                                    (test-bit-reg cpu (read-memory-at-addr mmu addr) 2)))))
(setf (aref cb-ops #x57) (make-instruction
                           :opcode #x47 :bytes 2 :cycles '(2 0) :asm '(:bit "2,A")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-a cpu) 2))))

(setf (aref cb-ops #x58) (make-instruction
                           :opcode #x40 :bytes 2 :cycles '(2 0) :asm '(:bit "3,B")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-b cpu) 3))))
(setf (aref cb-ops #x59) (make-instruction
                           :opcode #x41 :bytes 2 :cycles '(2 0) :asm '(:bit "3,C")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-c cpu) 3))))
(setf (aref cb-ops #x5a) (make-instruction
                           :opcode #x42 :bytes 2 :cycles '(2 0) :asm '(:bit "3,D")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-d cpu) 3))))
(setf (aref cb-ops #x5b) (make-instruction
                           :opcode #x43 :bytes 2 :cycles '(2 0) :asm '(:bit "3,E")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-e cpu) 3))))
(setf (aref cb-ops #x5c) (make-instruction
                           :opcode #x44 :bytes 2 :cycles '(2 0) :asm '(:bit "3,H")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-h cpu) 3))))
(setf (aref cb-ops #x5d) (make-instruction
                           :opcode #x45 :bytes 2 :cycles '(2 0) :asm '(:bit "3,L")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-l cpu) 3))))
(setf (aref cb-ops #x5e) (make-instruction
                           :opcode #x46 :bytes 2 :cycles '(3 0) :asm '(:bit "3,(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (incr-cpu-counters cpu instr)
                                    (test-bit-reg cpu (read-memory-at-addr mmu addr) 3)))))
(setf (aref cb-ops #x5f) (make-instruction
                           :opcode #x47 :bytes 2 :cycles '(2 0) :asm '(:bit "3,A")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-a cpu) 3))))
(setf (aref cb-ops #x60) (make-instruction
                           :opcode #x40 :bytes 2 :cycles '(2 0) :asm '(:bit "4,B")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-b cpu) 4))))
(setf (aref cb-ops #x61) (make-instruction
                           :opcode #x41 :bytes 2 :cycles '(2 0) :asm '(:bit "4,C")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-c cpu) 4))))
(setf (aref cb-ops #x62) (make-instruction
                           :opcode #x42 :bytes 2 :cycles '(2 0) :asm '(:bit "4,D")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-d cpu) 4))))
(setf (aref cb-ops #x63) (make-instruction
                           :opcode #x43 :bytes 2 :cycles '(2 0) :asm '(:bit "4,E")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-e cpu) 4))))
(setf (aref cb-ops #x64) (make-instruction
                           :opcode #x44 :bytes 2 :cycles '(2 0) :asm '(:bit "4,H")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-h cpu) 4))))
(setf (aref cb-ops #x65) (make-instruction
                           :opcode #x45 :bytes 2 :cycles '(2 0) :asm '(:bit "4,L")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-l cpu) 4))))
(setf (aref cb-ops #x66) (make-instruction
                           :opcode #x46 :bytes 2 :cycles '(3 0) :asm '(:bit "4,(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (incr-cpu-counters cpu instr)
                                    (test-bit-reg cpu (read-memory-at-addr mmu addr) 4)))))
(setf (aref cb-ops #x67) (make-instruction
                           :opcode #x47 :bytes 2 :cycles '(2 0) :asm '(:bit "4,A")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-a cpu) 4))))
(setf (aref cb-ops #x68) (make-instruction
                           :opcode #x40 :bytes 2 :cycles '(2 0) :asm '(:bit "5,B")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-b cpu) 5))))
(setf (aref cb-ops #x69) (make-instruction
                           :opcode #x41 :bytes 2 :cycles '(2 0) :asm '(:bit "5,C")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-c cpu) 5))))
(setf (aref cb-ops #x6a) (make-instruction
                           :opcode #x42 :bytes 2 :cycles '(2 0) :asm '(:bit "5,D")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-d cpu) 5))))
(setf (aref cb-ops #x6b) (make-instruction
                           :opcode #x43 :bytes 2 :cycles '(2 0) :asm '(:bit "5,E")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-e cpu) 5))))
(setf (aref cb-ops #x6c) (make-instruction
                           :opcode #x44 :bytes 2 :cycles '(2 0) :asm '(:bit "5,H")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-h cpu) 5))))
(setf (aref cb-ops #x6d) (make-instruction
                           :opcode #x45 :bytes 2 :cycles '(2 0) :asm '(:bit "5,L")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-l cpu) 5))))
(setf (aref cb-ops #x6e) (make-instruction
                           :opcode #x46 :bytes 2 :cycles '(3 0) :asm '(:bit "5,(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (incr-cpu-counters cpu instr)
                                    (test-bit-reg cpu (read-memory-at-addr mmu addr) 5)))))
(setf (aref cb-ops #x6f) (make-instruction
                           :opcode #x47 :bytes 2 :cycles '(2 0) :asm '(:bit "5,A")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-a cpu) 5))))
(setf (aref cb-ops #x70) (make-instruction
                           :opcode #x40 :bytes 2 :cycles '(2 0) :asm '(:bit "6,B")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-b cpu) 6))))
(setf (aref cb-ops #x71) (make-instruction
                           :opcode #x41 :bytes 2 :cycles '(2 0) :asm '(:bit "6,C")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-c cpu) 6))))
(setf (aref cb-ops #x72) (make-instruction
                           :opcode #x42 :bytes 2 :cycles '(2 0) :asm '(:bit "6,D")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-d cpu) 6))))
(setf (aref cb-ops #x73) (make-instruction
                           :opcode #x43 :bytes 2 :cycles '(2 0) :asm '(:bit "6,E")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-e cpu) 6))))
(setf (aref cb-ops #x74) (make-instruction
                           :opcode #x44 :bytes 2 :cycles '(2 0) :asm '(:bit "6,H")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-h cpu) 6))))
(setf (aref cb-ops #x75) (make-instruction
                           :opcode #x45 :bytes 2 :cycles '(2 0) :asm '(:bit "6,L")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-l cpu) 6))))
(setf (aref cb-ops #x76) (make-instruction
                           :opcode #x46 :bytes 2 :cycles '(3 0) :asm '(:bit "6,(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (incr-cpu-counters cpu instr)
                                    (test-bit-reg cpu (read-memory-at-addr mmu addr) 6)))))
(setf (aref cb-ops #x77) (make-instruction
                           :opcode #x47 :bytes 2 :cycles '(2 0) :asm '(:bit "6,A")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-a cpu) 6))))
(setf (aref cb-ops #x78) (make-instruction
                           :opcode #x40 :bytes 2 :cycles '(2 0) :asm '(:bit "7,B")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-b cpu) 7))))
(setf (aref cb-ops #x79) (make-instruction
                           :opcode #x41 :bytes 2 :cycles '(2 0) :asm '(:bit "7,C")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-c cpu) 7))))
(setf (aref cb-ops #x7a) (make-instruction
                           :opcode #x42 :bytes 2 :cycles '(2 0) :asm '(:bit "7,D")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-d cpu) 7))))
(setf (aref cb-ops #x7b) (make-instruction
                           :opcode #x43 :bytes 2 :cycles '(2 0) :asm '(:bit "7,E")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-e cpu) 7))))
(setf (aref cb-ops #x7c) (make-instruction
                           :opcode #x44 :bytes 2 :cycles '(2 0) :asm '(:bit "7,H")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-h cpu) 7))))
(setf (aref cb-ops #x7d) (make-instruction
                           :opcode #x45 :bytes 2 :cycles '(2 0) :asm '(:bit "7,L")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-l cpu) 7))))
(setf (aref cb-ops #x7e) (make-instruction
                           :opcode #x46 :bytes 2 :cycles '(3 0) :asm '(:bit "7,(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (incr-cpu-counters cpu instr)
                                    (test-bit-reg cpu (read-memory-at-addr mmu addr) 7)))))
(setf (aref cb-ops #x7f) (make-instruction
                           :opcode #x47 :bytes 2 :cycles '(2 0) :asm '(:bit "7,A")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (test-bit-reg cpu (gbcpu-a cpu) 7))))

;; RES
(setf (aref cb-ops #x80) (make-instruction
                           :opcode #x80 :bytes 2 :cycles '(2 0) :asm '(:res "0,B")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-b cpu) (reset-bit-reg cpu (gbcpu-b cpu) 0)))))
(setf (aref cb-ops #x81) (make-instruction
                           :opcode #x81 :bytes 2 :cycles '(2 0) :asm '(:res "0,C")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-c cpu) (reset-bit-reg cpu (gbcpu-c cpu) 0)))))
(setf (aref cb-ops #x82) (make-instruction
                           :opcode #x82 :bytes 2 :cycles '(2 0) :asm '(:res "0,D")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-d cpu) (reset-bit-reg cpu (gbcpu-d cpu) 0)))))
(setf (aref cb-ops #x83) (make-instruction
                           :opcode #x83 :bytes 2 :cycles '(2 0) :asm '(:res "0,E")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-e cpu) (reset-bit-reg cpu (gbcpu-e cpu) 0)))))
(setf (aref cb-ops #x84) (make-instruction
                           :opcode #x84 :bytes 2 :cycles '(2 0) :asm '(:res "0,H")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-h cpu) (reset-bit-reg cpu (gbcpu-h cpu) 0)))))
(setf (aref cb-ops #x85) (make-instruction
                           :opcode #x85 :bytes 2 :cycles '(2 0) :asm '(:res "0,L")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-l cpu) (reset-bit-reg cpu (gbcpu-l cpu) 0)))))
(setf (aref cb-ops #x86) (make-instruction
                           :opcode #x86 :bytes 2 :cycles '(4 0) :asm '(:res "0,(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (incr-cpu-counters cpu instr)
                                    (write-memory-at-addr mmu addr (reset-bit-reg cpu (read-memory-at-addr mmu addr) 0))))))
(setf (aref cb-ops #x87) (make-instruction
                           :opcode #x87 :bytes 2 :cycles '(2 0) :asm '(:res "0,A")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-a cpu) (reset-bit-reg cpu (gbcpu-a cpu) 0)))))

(setf (aref cb-ops #x88) (make-instruction
                           :opcode #x88 :bytes 2 :cycles '(2 0) :asm '(:res "1,B")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-b cpu) (reset-bit-reg cpu (gbcpu-b cpu) 1)))))
(setf (aref cb-ops #x89) (make-instruction
                           :opcode #x89 :bytes 2 :cycles '(2 0) :asm '(:res "1,C")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-c cpu) (reset-bit-reg cpu (gbcpu-c cpu) 1)))))
(setf (aref cb-ops #x8a) (make-instruction
                           :opcode #x8a :bytes 2 :cycles '(2 0) :asm '(:res "1,D")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-d cpu) (reset-bit-reg cpu (gbcpu-d cpu) 1)))))
(setf (aref cb-ops #x8b) (make-instruction
                           :opcode #x8b :bytes 2 :cycles '(2 0) :asm '(:res "1,E")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-e cpu) (reset-bit-reg cpu (gbcpu-e cpu) 1)))))
(setf (aref cb-ops #x8c) (make-instruction
                           :opcode #x8c :bytes 2 :cycles '(2 0) :asm '(:res "1,H")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-h cpu) (reset-bit-reg cpu (gbcpu-h cpu) 1)))))
(setf (aref cb-ops #x8d) (make-instruction
                           :opcode #x8d :bytes 2 :cycles '(2 0) :asm '(:res "1,L")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-l cpu) (reset-bit-reg cpu (gbcpu-l cpu) 1)))))
(setf (aref cb-ops #x8e) (make-instruction
                           :opcode #x8e :bytes 2 :cycles '(4 0) :asm '(:res "1,(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (incr-cpu-counters cpu instr)
                                    (write-memory-at-addr mmu addr (reset-bit-reg cpu (read-memory-at-addr mmu addr) 1))))))
(setf (aref cb-ops #x8f) (make-instruction
                           :opcode #x8f :bytes 2 :cycles '(2 0) :asm '(:res "1,A")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-a cpu) (reset-bit-reg cpu (gbcpu-a cpu) 1)))))

(setf (aref cb-ops #x90) (make-instruction
                           :opcode #x90 :bytes 2 :cycles '(2 0) :asm '(:res "2,B")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-b cpu) (reset-bit-reg cpu (gbcpu-b cpu) 2)))))
(setf (aref cb-ops #x91) (make-instruction
                           :opcode #x91 :bytes 2 :cycles '(2 0) :asm '(:res "2,C")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-c cpu) (reset-bit-reg cpu (gbcpu-c cpu) 2)))))
(setf (aref cb-ops #x92) (make-instruction
                           :opcode #x92 :bytes 2 :cycles '(2 0) :asm '(:res "2,D")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-d cpu) (reset-bit-reg cpu (gbcpu-d cpu) 2)))))
(setf (aref cb-ops #x93) (make-instruction
                           :opcode #x93 :bytes 2 :cycles '(2 0) :asm '(:res "2,E")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-e cpu) (reset-bit-reg cpu (gbcpu-e cpu) 2)))))
(setf (aref cb-ops #x94) (make-instruction
                           :opcode #x94 :bytes 2 :cycles '(2 0) :asm '(:res "2,H")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-h cpu) (reset-bit-reg cpu (gbcpu-h cpu) 2)))))
(setf (aref cb-ops #x95) (make-instruction
                           :opcode #x95 :bytes 2 :cycles '(2 0) :asm '(:res "2,L")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-l cpu) (reset-bit-reg cpu (gbcpu-l cpu) 2)))))
(setf (aref cb-ops #x96) (make-instruction
                           :opcode #x96 :bytes 2 :cycles '(4 0) :asm '(:res "2,(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (incr-cpu-counters cpu instr)
                                    (write-memory-at-addr mmu addr (reset-bit-reg cpu (read-memory-at-addr mmu addr) 2))))))
(setf (aref cb-ops #x97) (make-instruction
                           :opcode #x97 :bytes 2 :cycles '(2 0) :asm '(:res "2,A")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-a cpu) (reset-bit-reg cpu (gbcpu-a cpu) 2)))))

(setf (aref cb-ops #x98) (make-instruction
                           :opcode #x98 :bytes 2 :cycles '(2 0) :asm '(:res "3,B")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-b cpu) (reset-bit-reg cpu (gbcpu-b cpu) 3)))))
(setf (aref cb-ops #x99) (make-instruction
                           :opcode #x99 :bytes 2 :cycles '(2 0) :asm '(:res "3,C")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-c cpu) (reset-bit-reg cpu (gbcpu-c cpu) 3)))))
(setf (aref cb-ops #x9a) (make-instruction
                           :opcode #x9a :bytes 2 :cycles '(2 0) :asm '(:res "3,D")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-d cpu) (reset-bit-reg cpu (gbcpu-d cpu) 3)))))
(setf (aref cb-ops #x9b) (make-instruction
                           :opcode #x9b :bytes 2 :cycles '(2 0) :asm '(:res "3,E")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-e cpu) (reset-bit-reg cpu (gbcpu-e cpu) 3)))))
(setf (aref cb-ops #x9c) (make-instruction
                           :opcode #x9c :bytes 2 :cycles '(2 0) :asm '(:res "3,H")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-h cpu) (reset-bit-reg cpu (gbcpu-h cpu) 3)))))
(setf (aref cb-ops #x9d) (make-instruction
                           :opcode #x9d :bytes 2 :cycles '(2 0) :asm '(:res "3,L")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-l cpu) (reset-bit-reg cpu (gbcpu-l cpu) 3)))))
(setf (aref cb-ops #x9e) (make-instruction
                           :opcode #x9e :bytes 2 :cycles '(4 0) :asm '(:res "3,(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (incr-cpu-counters cpu instr)
                                    (write-memory-at-addr mmu addr (reset-bit-reg cpu (read-memory-at-addr mmu addr) 3))))))
(setf (aref cb-ops #x9f) (make-instruction
                           :opcode #x9f :bytes 2 :cycles '(2 0) :asm '(:res "3,A")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-a cpu) (reset-bit-reg cpu (gbcpu-a cpu) 3)))))

(setf (aref cb-ops #xa0) (make-instruction
                           :opcode #xa0 :bytes 2 :cycles '(2 0) :asm '(:res "4,B")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-b cpu) (reset-bit-reg cpu (gbcpu-b cpu) 4)))))
(setf (aref cb-ops #xa1) (make-instruction
                           :opcode #xa1 :bytes 2 :cycles '(2 0) :asm '(:res "4,C")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-c cpu) (reset-bit-reg cpu (gbcpu-c cpu) 4)))))
(setf (aref cb-ops #xa2) (make-instruction
                           :opcode #xa2 :bytes 2 :cycles '(2 0) :asm '(:res "4,D")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-d cpu) (reset-bit-reg cpu (gbcpu-d cpu) 4)))))
(setf (aref cb-ops #xa3) (make-instruction
                           :opcode #xa3 :bytes 2 :cycles '(2 0) :asm '(:res "4,E")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-e cpu) (reset-bit-reg cpu (gbcpu-e cpu) 4)))))
(setf (aref cb-ops #xa4) (make-instruction
                           :opcode #xa4 :bytes 2 :cycles '(2 0) :asm '(:res "4,H")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-h cpu) (reset-bit-reg cpu (gbcpu-h cpu) 4)))))
(setf (aref cb-ops #xa5) (make-instruction
                           :opcode #xa5 :bytes 2 :cycles '(2 0) :asm '(:res "4,L")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-l cpu) (reset-bit-reg cpu (gbcpu-l cpu) 4)))))
(setf (aref cb-ops #xa6) (make-instruction
                           :opcode #xa6 :bytes 2 :cycles '(4 0) :asm '(:res "4,(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (incr-cpu-counters cpu instr)
                                    (write-memory-at-addr mmu addr (reset-bit-reg cpu (read-memory-at-addr mmu addr) 4))))))
(setf (aref cb-ops #xa7) (make-instruction
                           :opcode #xa7 :bytes 2 :cycles '(2 0) :asm '(:res "4,A")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-a cpu) (reset-bit-reg cpu (gbcpu-a cpu) 4)))))

(setf (aref cb-ops #xa8) (make-instruction
                           :opcode #xa8 :bytes 2 :cycles '(2 0) :asm '(:res "5,B")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-b cpu) (reset-bit-reg cpu (gbcpu-b cpu) 5)))))
(setf (aref cb-ops #xa9) (make-instruction
                           :opcode #xa9 :bytes 2 :cycles '(2 0) :asm '(:res "5,C")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-c cpu) (reset-bit-reg cpu (gbcpu-c cpu) 5)))))
(setf (aref cb-ops #xaa) (make-instruction
                           :opcode #xaa :bytes 2 :cycles '(2 0) :asm '(:res "5,D")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-d cpu) (reset-bit-reg cpu (gbcpu-d cpu) 5)))))
(setf (aref cb-ops #xab) (make-instruction
                           :opcode #xab :bytes 2 :cycles '(2 0) :asm '(:res "5,E")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-e cpu) (reset-bit-reg cpu (gbcpu-e cpu) 5)))))
(setf (aref cb-ops #xac) (make-instruction
                           :opcode #xac :bytes 2 :cycles '(2 0) :asm '(:res "5,H")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-h cpu) (reset-bit-reg cpu (gbcpu-h cpu) 5)))))
(setf (aref cb-ops #xad) (make-instruction
                           :opcode #xad :bytes 2 :cycles '(2 0) :asm '(:res "5,L")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-l cpu) (reset-bit-reg cpu (gbcpu-l cpu) 5)))))
(setf (aref cb-ops #xae) (make-instruction
                           :opcode #xae :bytes 2 :cycles '(4 0) :asm '(:res "5,(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (incr-cpu-counters cpu instr)
                                    (write-memory-at-addr mmu addr (reset-bit-reg cpu (read-memory-at-addr mmu addr) 5))))))
(setf (aref cb-ops #xaf) (make-instruction
                           :opcode #xaf :bytes 2 :cycles '(2 0) :asm '(:res "5,A")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-a cpu) (reset-bit-reg cpu (gbcpu-a cpu) 5)))))

(setf (aref cb-ops #xb0) (make-instruction
                           :opcode #xb0 :bytes 2 :cycles '(2 0) :asm '(:res "6,B")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-b cpu) (reset-bit-reg cpu (gbcpu-b cpu) 6)))))
(setf (aref cb-ops #xb1) (make-instruction
                           :opcode #xb1 :bytes 2 :cycles '(2 0) :asm '(:res "6,C")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-c cpu) (reset-bit-reg cpu (gbcpu-c cpu) 6)))))
(setf (aref cb-ops #xb2) (make-instruction
                           :opcode #xb2 :bytes 2 :cycles '(2 0) :asm '(:res "6,D")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-d cpu) (reset-bit-reg cpu (gbcpu-d cpu) 6)))))
(setf (aref cb-ops #xb3) (make-instruction
                           :opcode #xb3 :bytes 2 :cycles '(2 0) :asm '(:res "6,E")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-e cpu) (reset-bit-reg cpu (gbcpu-e cpu) 6)))))
(setf (aref cb-ops #xb4) (make-instruction
                           :opcode #xb4 :bytes 2 :cycles '(2 0) :asm '(:res "6,H")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-h cpu) (reset-bit-reg cpu (gbcpu-h cpu) 6)))))
(setf (aref cb-ops #xb5) (make-instruction
                           :opcode #xb5 :bytes 2 :cycles '(2 0) :asm '(:res "6,L")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-l cpu) (reset-bit-reg cpu (gbcpu-l cpu) 6)))))
(setf (aref cb-ops #xb6) (make-instruction
                           :opcode #xb6 :bytes 2 :cycles '(4 0) :asm '(:res "6,(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (incr-cpu-counters cpu instr)
                                    (write-memory-at-addr mmu addr (reset-bit-reg cpu (read-memory-at-addr mmu addr) 6))))))
(setf (aref cb-ops #xb7) (make-instruction
                           :opcode #xb7 :bytes 2 :cycles '(2 0) :asm '(:res "6,A")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-a cpu) (reset-bit-reg cpu (gbcpu-a cpu) 6)))))

(setf (aref cb-ops #xb8) (make-instruction
                           :opcode #xb8 :bytes 2 :cycles '(2 0) :asm '(:res "7,B")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-b cpu) (reset-bit-reg cpu (gbcpu-b cpu) 7)))))
(setf (aref cb-ops #xb9) (make-instruction
                           :opcode #xb9 :bytes 2 :cycles '(2 0) :asm '(:res "7,C")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-c cpu) (reset-bit-reg cpu (gbcpu-c cpu) 7)))))
(setf (aref cb-ops #xba) (make-instruction
                           :opcode #xba :bytes 2 :cycles '(2 0) :asm '(:res "7,D")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-d cpu) (reset-bit-reg cpu (gbcpu-d cpu) 7)))))
(setf (aref cb-ops #xbb) (make-instruction
                           :opcode #xbb :bytes 2 :cycles '(2 0) :asm '(:res "7,E")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-e cpu) (reset-bit-reg cpu (gbcpu-e cpu) 7)))))
(setf (aref cb-ops #xbc) (make-instruction
                           :opcode #xbc :bytes 2 :cycles '(2 0) :asm '(:res "7,H")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-h cpu) (reset-bit-reg cpu (gbcpu-h cpu) 7)))))
(setf (aref cb-ops #xbd) (make-instruction
                           :opcode #xbd :bytes 2 :cycles '(2 0) :asm '(:res "7,L")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-l cpu) (reset-bit-reg cpu (gbcpu-l cpu) 7)))))
(setf (aref cb-ops #xbe) (make-instruction
                           :opcode #xbe :bytes 2 :cycles '(4 0) :asm '(:res "7,(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (incr-cpu-counters cpu instr)
                                    (write-memory-at-addr mmu addr (reset-bit-reg cpu (read-memory-at-addr mmu addr) 7))))))
(setf (aref cb-ops #xbf) (make-instruction
                           :opcode #xbf :bytes 2 :cycles '(2 0) :asm '(:res "7,A")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-a cpu) (reset-bit-reg cpu (gbcpu-a cpu) 7)))))

;; SET
(setf (aref cb-ops #xc0) (make-instruction
                           :opcode #xc0 :bytes 2 :cycles '(2 0) :asm '(:bit "0,B")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-b cpu) (set-bit-reg cpu (gbcpu-b cpu) 0)))))
(setf (aref cb-ops #xc1) (make-instruction
                           :opcode #xc1 :bytes 2 :cycles '(2 0) :asm '(:bit "0,C")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-c cpu) (set-bit-reg cpu (gbcpu-c cpu) 0)))))
(setf (aref cb-ops #xc2) (make-instruction
                           :opcode #xc2 :bytes 2 :cycles '(2 0) :asm '(:bit "0,D")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-d cpu) (set-bit-reg cpu (gbcpu-d cpu) 0)))))
(setf (aref cb-ops #xc3) (make-instruction
                           :opcode #xc3 :bytes 2 :cycles '(2 0) :asm '(:bit "0,E")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-e cpu) (set-bit-reg cpu (gbcpu-e cpu) 0)))))
(setf (aref cb-ops #xc4) (make-instruction
                           :opcode #xc4 :bytes 2 :cycles '(2 0) :asm '(:bit "0,H")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-h cpu) (set-bit-reg cpu (gbcpu-h cpu) 0)))))
(setf (aref cb-ops #xc5) (make-instruction
                           :opcode #xc5 :bytes 2 :cycles '(2 0) :asm '(:bit "0,L")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-l cpu) (set-bit-reg cpu (gbcpu-l cpu) 0)))))
(setf (aref cb-ops #xc6) (make-instruction
                           :opcode #xc6 :bytes 2 :cycles '(4 0) :asm '(:bit "0,(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (incr-cpu-counters cpu instr)
                                    (write-memory-at-addr mmu addr (set-bit-reg cpu (read-memory-at-addr mmu addr) 0))))))
(setf (aref cb-ops #xc7) (make-instruction
                           :opcode #xc7 :bytes 2 :cycles '(2 0) :asm '(:bit "0,A")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-a cpu) (set-bit-reg cpu (gbcpu-a cpu) 0)))))

(setf (aref cb-ops #xc8) (make-instruction
                           :opcode #xc8 :bytes 2 :cycles '(2 0) :asm '(:bit "1,B")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-b cpu) (set-bit-reg cpu (gbcpu-b cpu) 1)))))
(setf (aref cb-ops #xc9) (make-instruction
                           :opcode #xc9 :bytes 2 :cycles '(2 0) :asm '(:bit "1,C")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-c cpu) (set-bit-reg cpu (gbcpu-c cpu) 1)))))
(setf (aref cb-ops #xca) (make-instruction
                           :opcode #xca :bytes 2 :cycles '(2 0) :asm '(:bit "1,D")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-d cpu) (set-bit-reg cpu (gbcpu-d cpu) 1)))))
(setf (aref cb-ops #xcb) (make-instruction
                           :opcode #xcb :bytes 2 :cycles '(2 0) :asm '(:bit "1,E")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-e cpu) (set-bit-reg cpu (gbcpu-e cpu) 1)))))
(setf (aref cb-ops #xcc) (make-instruction
                           :opcode #xcc :bytes 2 :cycles '(2 0) :asm '(:bit "1,H")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-h cpu) (set-bit-reg cpu (gbcpu-h cpu) 1)))))
(setf (aref cb-ops #xcd) (make-instruction
                           :opcode #xcd :bytes 2 :cycles '(2 0) :asm '(:bit "1,L")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-l cpu) (set-bit-reg cpu (gbcpu-l cpu) 1)))))
(setf (aref cb-ops #xce) (make-instruction
                           :opcode #xce :bytes 2 :cycles '(4 0) :asm '(:bit "1,(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (incr-cpu-counters cpu instr)
                                    (write-memory-at-addr mmu addr (set-bit-reg cpu (read-memory-at-addr mmu addr) 1))))))
(setf (aref cb-ops #xcf) (make-instruction
                           :opcode #xcf :bytes 2 :cycles '(2 0) :asm '(:bit "1,A")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-a cpu) (set-bit-reg cpu (gbcpu-a cpu) 1)))))

(setf (aref cb-ops #xd0) (make-instruction
                           :opcode #xd0 :bytes 2 :cycles '(2 0) :asm '(:bit "2,B")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-b cpu) (set-bit-reg cpu (gbcpu-b cpu) 2)))))
(setf (aref cb-ops #xd1) (make-instruction
                           :opcode #xd1 :bytes 2 :cycles '(2 0) :asm '(:bit "2,C")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-c cpu) (set-bit-reg cpu (gbcpu-c cpu) 2)))))
(setf (aref cb-ops #xd2) (make-instruction
                           :opcode #xd2 :bytes 2 :cycles '(2 0) :asm '(:bit "2,D")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-d cpu) (set-bit-reg cpu (gbcpu-d cpu) 2)))))
(setf (aref cb-ops #xd3) (make-instruction
                           :opcode #xd3 :bytes 2 :cycles '(2 0) :asm '(:bit "2,E")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-e cpu) (set-bit-reg cpu (gbcpu-e cpu) 2)))))
(setf (aref cb-ops #xd4) (make-instruction
                           :opcode #xd4 :bytes 2 :cycles '(2 0) :asm '(:bit "2,H")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-h cpu) (set-bit-reg cpu (gbcpu-h cpu) 2)))))
(setf (aref cb-ops #xd5) (make-instruction
                           :opcode #xd5 :bytes 2 :cycles '(2 0) :asm '(:bit "2,L")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-l cpu) (set-bit-reg cpu (gbcpu-l cpu) 2)))))
(setf (aref cb-ops #xd6) (make-instruction
                           :opcode #xd6 :bytes 2 :cycles '(4 0) :asm '(:bit "2,(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (incr-cpu-counters cpu instr)
                                    (write-memory-at-addr mmu addr (set-bit-reg cpu (read-memory-at-addr mmu addr) 2))))))
(setf (aref cb-ops #xd7) (make-instruction
                           :opcode #xd7 :bytes 2 :cycles '(2 0) :asm '(:bit "2,A")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-a cpu) (set-bit-reg cpu (gbcpu-a cpu) 2)))))

(setf (aref cb-ops #xd8) (make-instruction
                           :opcode #xd8 :bytes 2 :cycles '(2 0) :asm '(:bit "3,B")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-b cpu) (set-bit-reg cpu (gbcpu-b cpu) 3)))))
(setf (aref cb-ops #xd9) (make-instruction
                           :opcode #xd9 :bytes 2 :cycles '(2 0) :asm '(:bit "3,C")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-c cpu) (set-bit-reg cpu (gbcpu-c cpu) 3)))))
(setf (aref cb-ops #xda) (make-instruction
                           :opcode #xda :bytes 2 :cycles '(2 0) :asm '(:bit "3,D")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-d cpu) (set-bit-reg cpu (gbcpu-d cpu) 3)))))
(setf (aref cb-ops #xdb) (make-instruction
                           :opcode #xdb :bytes 2 :cycles '(2 0) :asm '(:bit "3,E")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-e cpu) (set-bit-reg cpu (gbcpu-e cpu) 3)))))
(setf (aref cb-ops #xdc) (make-instruction
                           :opcode #xdc :bytes 2 :cycles '(2 0) :asm '(:bit "3,H")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-h cpu) (set-bit-reg cpu (gbcpu-h cpu) 3)))))
(setf (aref cb-ops #xdd) (make-instruction
                           :opcode #xdd :bytes 2 :cycles '(2 0) :asm '(:bit "3,L")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-l cpu) (set-bit-reg cpu (gbcpu-l cpu) 3)))))
(setf (aref cb-ops #xde) (make-instruction
                           :opcode #xde :bytes 2 :cycles '(4 0) :asm '(:bit "3,(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (incr-cpu-counters cpu instr)
                                    (write-memory-at-addr mmu addr (set-bit-reg cpu (read-memory-at-addr mmu addr) 3))))))
(setf (aref cb-ops #xdf) (make-instruction
                           :opcode #xdf :bytes 2 :cycles '(2 0) :asm '(:bit "3,A")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-a cpu) (set-bit-reg cpu (gbcpu-a cpu) 3)))))

(setf (aref cb-ops #xe0) (make-instruction
                           :opcode #xe0 :bytes 2 :cycles '(2 0) :asm '(:bit "4,B")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-b cpu) (set-bit-reg cpu (gbcpu-b cpu) 4)))))
(setf (aref cb-ops #xe1) (make-instruction
                           :opcode #xe1 :bytes 2 :cycles '(2 0) :asm '(:bit "4,C")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-c cpu) (set-bit-reg cpu (gbcpu-c cpu) 4)))))
(setf (aref cb-ops #xe2) (make-instruction
                           :opcode #xe2 :bytes 2 :cycles '(2 0) :asm '(:bit "4,D")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-d cpu) (set-bit-reg cpu (gbcpu-d cpu) 4)))))
(setf (aref cb-ops #xe3) (make-instruction
                           :opcode #xe3 :bytes 2 :cycles '(2 0) :asm '(:bit "4,E")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-e cpu) (set-bit-reg cpu (gbcpu-e cpu) 4)))))
(setf (aref cb-ops #xe4) (make-instruction
                           :opcode #xe4 :bytes 2 :cycles '(2 0) :asm '(:bit "4,H")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-h cpu) (set-bit-reg cpu (gbcpu-h cpu) 4)))))
(setf (aref cb-ops #xe5) (make-instruction
                           :opcode #xe5 :bytes 2 :cycles '(2 0) :asm '(:bit "4,L")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-l cpu) (set-bit-reg cpu (gbcpu-l cpu) 4)))))
(setf (aref cb-ops #xe6) (make-instruction
                           :opcode #xe6 :bytes 2 :cycles '(4 0) :asm '(:bit "4,(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (incr-cpu-counters cpu instr)
                                    (write-memory-at-addr mmu addr (set-bit-reg cpu (read-memory-at-addr mmu addr) 4))))))
(setf (aref cb-ops #xe7) (make-instruction
                           :opcode #xe7 :bytes 2 :cycles '(2 0) :asm '(:bit "4,A")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-a cpu) (set-bit-reg cpu (gbcpu-a cpu) 4)))))

(setf (aref cb-ops #xe8) (make-instruction
                           :opcode #xe8 :bytes 2 :cycles '(2 0) :asm '(:bit "5,B")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-b cpu) (set-bit-reg cpu (gbcpu-b cpu) 5)))))
(setf (aref cb-ops #xe9) (make-instruction
                           :opcode #xe9 :bytes 2 :cycles '(2 0) :asm '(:bit "5,C")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-c cpu) (set-bit-reg cpu (gbcpu-c cpu) 5)))))
(setf (aref cb-ops #xea) (make-instruction
                           :opcode #xea :bytes 2 :cycles '(2 0) :asm '(:bit "5,D")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-d cpu) (set-bit-reg cpu (gbcpu-d cpu) 5)))))
(setf (aref cb-ops #xeb) (make-instruction
                           :opcode #xeb :bytes 2 :cycles '(2 0) :asm '(:bit "5,E")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-e cpu) (set-bit-reg cpu (gbcpu-e cpu) 5)))))
(setf (aref cb-ops #xec) (make-instruction
                           :opcode #xec :bytes 2 :cycles '(2 0) :asm '(:bit "5,H")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-h cpu) (set-bit-reg cpu (gbcpu-h cpu) 5)))))
(setf (aref cb-ops #xed) (make-instruction
                           :opcode #xed :bytes 2 :cycles '(2 0) :asm '(:bit "5,L")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-l cpu) (set-bit-reg cpu (gbcpu-l cpu) 5)))))
(setf (aref cb-ops #xee) (make-instruction
                           :opcode #xee :bytes 2 :cycles '(4 0) :asm '(:bit "5,(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (incr-cpu-counters cpu instr)
                                    (write-memory-at-addr mmu addr (set-bit-reg cpu (read-memory-at-addr mmu addr) 5))))))
(setf (aref cb-ops #xef) (make-instruction
                           :opcode #xef :bytes 2 :cycles '(2 0) :asm '(:bit "5,A")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-a cpu) (set-bit-reg cpu (gbcpu-a cpu) 5)))))

(setf (aref cb-ops #xf0) (make-instruction
                           :opcode #xf0 :bytes 2 :cycles '(2 0) :asm '(:bit "6,B")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-b cpu) (set-bit-reg cpu (gbcpu-b cpu) 6)))))
(setf (aref cb-ops #xf1) (make-instruction
                           :opcode #xf1 :bytes 2 :cycles '(2 0) :asm '(:bit "6,C")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-c cpu) (set-bit-reg cpu (gbcpu-c cpu) 6)))))
(setf (aref cb-ops #xf2) (make-instruction
                           :opcode #xf2 :bytes 2 :cycles '(2 0) :asm '(:bit "6,D")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-d cpu) (set-bit-reg cpu (gbcpu-d cpu) 6)))))
(setf (aref cb-ops #xf3) (make-instruction
                           :opcode #xf3 :bytes 2 :cycles '(2 0) :asm '(:bit "6,E")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-e cpu) (set-bit-reg cpu (gbcpu-e cpu) 6)))))
(setf (aref cb-ops #xf4) (make-instruction
                           :opcode #xf4 :bytes 2 :cycles '(2 0) :asm '(:bit "6,H")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-h cpu) (set-bit-reg cpu (gbcpu-h cpu) 6)))))
(setf (aref cb-ops #xf5) (make-instruction
                           :opcode #xf5 :bytes 2 :cycles '(2 0) :asm '(:bit "6,L")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-l cpu) (set-bit-reg cpu (gbcpu-l cpu) 6)))))
(setf (aref cb-ops #xf6) (make-instruction
                           :opcode #xf6 :bytes 2 :cycles '(4 0) :asm '(:bit "6,(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (incr-cpu-counters cpu instr)
                                    (write-memory-at-addr mmu addr (set-bit-reg cpu (read-memory-at-addr mmu addr) 6))))))
(setf (aref cb-ops #xf7) (make-instruction
                           :opcode #xf7 :bytes 2 :cycles '(2 0) :asm '(:bit "6,A")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-a cpu) (set-bit-reg cpu (gbcpu-a cpu) 6)))))

(setf (aref cb-ops #xf8) (make-instruction
                           :opcode #xf8 :bytes 2 :cycles '(2 0) :asm '(:bit "7,B")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-b cpu) (set-bit-reg cpu (gbcpu-b cpu) 7)))))
(setf (aref cb-ops #xf9) (make-instruction
                           :opcode #xf9 :bytes 2 :cycles '(2 0) :asm '(:bit "7,C")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-c cpu) (set-bit-reg cpu (gbcpu-c cpu) 7)))))
(setf (aref cb-ops #xfa) (make-instruction
                           :opcode #xfa :bytes 2 :cycles '(2 0) :asm '(:bit "7,D")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-d cpu) (set-bit-reg cpu (gbcpu-d cpu) 7)))))
(setf (aref cb-ops #xfb) (make-instruction
                           :opcode #xfb :bytes 2 :cycles '(2 0) :asm '(:bit "7,E")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-e cpu) (set-bit-reg cpu (gbcpu-e cpu) 7)))))
(setf (aref cb-ops #xfc) (make-instruction
                           :opcode #xfc :bytes 2 :cycles '(2 0) :asm '(:bit "7,H")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-h cpu) (set-bit-reg cpu (gbcpu-h cpu) 7)))))
(setf (aref cb-ops #xfd) (make-instruction
                           :opcode #xfd :bytes 2 :cycles '(2 0) :asm '(:bit "7,L")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-l cpu) (set-bit-reg cpu (gbcpu-l cpu) 7)))))
(setf (aref cb-ops #xfe) (make-instruction
                           :opcode #xfe :bytes 2 :cycles '(4 0) :asm '(:bit "7,(HL)")
                           :fun (lambda (cpu mmu instr)
                                  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
                                    (incr-cpu-counters cpu instr)
                                    (write-memory-at-addr mmu addr (set-bit-reg cpu (read-memory-at-addr mmu addr) 7))))))
(setf (aref cb-ops #xff) (make-instruction
                           :opcode #xff :bytes 2 :cycles '(2 0) :asm '(:bit "7,A")
                           :fun (lambda (cpu mmu instr)
                                  (incr-cpu-counters cpu instr)
                                  (setf (gbcpu-a cpu) (set-bit-reg cpu (gbcpu-a cpu) 7)))))



(defun get-cb-instruction (cpu mmu)
  (let* ((op (read-memory-at-addr mmu (+ (gbcpu-pc cpu) 1)))
        (instr (aref cb-ops op)))
      (if (instruction-p instr)
        instr
        (format t "Unimplemented CB instruction ~X @ ~X~%" op (gbcpu-pc cpu)))))
(defparameter halted-instr (make-instruction :opcode 0 :bytes 0 :cycles '(1 0) :asm '(:cpu "halted")
                                             :fun (lambda (cpu mmu instr) (incr-clocks cpu instr))))
(defun emu-single-op (cpu mmu)
    (let* ((op (read-memory-at-addr mmu (gbcpu-pc cpu)))
          (instr (if (= (gbcpu-halted cpu) #x00) (if (= op #xcb) (get-cb-instruction cpu mmu) (aref ops op)) halted-instr)))
    (if (instruction-p instr)
      (progn (if (null (instruction-fun instr))
               (format t "Unable to run function for instruction ~X @ ~X~%" op (gbcpu-pc cpu))
               (funcall (instruction-fun instr) cpu mmu instr))
             instr)
      (format t "Unimplemented instruction ~X @ ~X~%" op (gbcpu-pc cpu)))))

(defun do-interrupt (cpu mmu interrupt-id)
  (setf (gbcpu-halted cpu) #x00)
  (write-memory-at-addr mmu #xff0f (logand (read-memory-at-addr mmu #xff0f) (logxor (ash #x01 interrupt-id) #xff)))
  (do-call-at-addr cpu mmu (+ (* interrupt-id 8) #x40)))

(defun handle-interrupts (cpu mmu )
  (if (= (gbcpu-int-ena cpu) #x01)
      ; VBLANK interupt
      (if (= (logand (logand (read-memory-at-addr mmu #xffff) #x01) (logand (read-memory-at-addr mmu #xff0f) #x01)) #x01)
        (do-interrupt cpu mmu 0)
      ; LCDC Status interupt
      (if (= (logand (logand (read-memory-at-addr mmu #xffff) #x02) (logand (read-memory-at-addr mmu #xff0f) #x02)) #x02)
        (do-interrupt cpu mmu 1)
      ; Timer interupt
      (if (= (logand (logand (read-memory-at-addr mmu #xffff) #x04) (logand (read-memory-at-addr mmu #xff0f) #x04)) #x04)
        (do-interrupt cpu mmu 2)
      ; Serial Transfer interupt
      (if (= (logand (logand (read-memory-at-addr mmu #xffff) #x08) (logand (read-memory-at-addr mmu #xff0f) #x08)) #x08)
        (do-interrupt cpu mmu 3)
      ; Hi-Lo of P10-P13 interupt
      (if (= (logand (logand (read-memory-at-addr mmu #xffff) #x10) (logand (read-memory-at-addr mmu #xff0f) #x10)) #x10)
        (do-interrupt cpu mmu 4))))))))

(defun set-interrupt-flag (mmu bit-pos)
  (write-memory-at-addr mmu #xff05 (logior (read-memory-at-addr mmu #xff05) (ash #x01 (- 0 bit-pos)))))

(defun handle-timers (cpu mmu)
  (if (> (gbcpu-div-clock cpu) #xff)
    (progn
      (setf (gbcpu-div-clock cpu) (- (gbcpu-div-clock cpu) #x100))
      (write-memory-at-addr mmu #xff04 (logand (+ (read-memory-at-addr mmu #xff04) #x01) #xff))))
  (if (= (logand (read-memory-at-addr mmu #xff07) #x04) #x04)
    (incr-timer-by-cycles cpu mmu (get-cycles-per-timer-tick (get-timer-frequency (read-memory-at-addr mmu #xff07))))
    (setf (gbcpu-clock cpu) 0)))

(defun incr-timer-by-cycles (cpu mmu cycles-per-tick)
  (let* ((cycles (gbcpu-clock cpu))
         (ticks (floor cycles cycles-per-tick))
         (remainder (mod cycles cycles-per-tick))
         (cur-ticks (read-memory-at-addr mmu #xff05))
         (new-ticks (+ cur-ticks ticks)))
    (setf (gbcpu-clock cpu) remainder)
    (if (> new-ticks #xff)
      (progn (write-memory-at-addr mmu #xff0f (logior (read-memory-at-addr mmu #xff0f) #x04))
             (write-memory-at-addr mmu #xff05 (+ (read-memory-at-addr mmu #xff06) (logand new-ticks #xff))))
      (write-memory-at-addr mmu #xff05 (logand new-ticks #xff)))))

(defun get-cycles-per-timer-tick (freq)
  (/ CPU_SPEED freq))

(defun get-timer-frequency (tac)
  (let ((tac-two-lsb (logand tac #x3)))
    (if (= tac-two-lsb #x01) 262144
      (if (= tac-two-lsb #x02) 65536
        (if (= tac-two-lsb #x03) 16384
          4096)))))

(defconstant CPU_SPEED 4194304)

(defconstant COLORS #(255 255 255
                      192 192 192
                      96 96 96
                       0  0  0))

(defun make-signed-from-unsigned (unsign-byte)
  (if (< unsign-byte 128)
      unsign-byte
      (logior unsign-byte (- (mask-field (byte 1 7) #xff)))))

(defun update-ppu-framebuffer (ppu mmu)
  (let* ((row (gbppu-cur-line ppu))
         (scroll-y (read-memory-at-addr mmu #xff42))
         (scroll-x (read-memory-at-addr mmu #xff43))
         (lcdc (read-memory-at-addr mmu #xff40))
         (tilemap-loc (if (= (logand lcdc #x08) #x08) #x9c00 #x9800))
         (tiledata-loc (if (= (logand lcdc #x10) #x10) #x8000 #x9000)))
    ;(format t "tilmap @~X, tildata @~X~%" tilemap-loc tiledata-loc)
    (when (< row 144)
    (loop for col from 0 to 159
      do
      ;(format t "~A ~A~%" row col)
      (let* ((yoffset (+ row scroll-y))
             (xoffset (+ col scroll-x))
             (addr (+ tilemap-loc (* (floor yoffset 8) 32) (floor xoffset 8)))
            (tile-no (if (= tiledata-loc #x8000)
                       (read-memory-at-addr mmu addr)
                       (make-signed-from-unsigned (read-memory-at-addr mmu addr))))
            (colorbitpos (- 7 (mod xoffset 8)))
            (color-addr (+ tiledata-loc (* tile-no #x10) (* (mod yoffset 8) 2)))
            (colorbyte1 (read-memory-at-addr mmu color-addr))
            (colorbyte2 (read-memory-at-addr mmu (+ color-addr 1)))
            (colorval (+
                         (logand (ash colorbyte1 (- 0 colorbitpos)) #x01)
                         (* (logand (ash colorbyte2 (- 0 colorbitpos)) #x01) 2))))
        ;(format t "tilemap addr ~X, tile-no ~X, color-addr ~X, colorval ~X~%" addr tile-no color-addr colorval)
        (let ((palette-col (logand (ash (read-memory-at-addr mmu #xff47) (* colorval -2)) 3)))
        (setf (aref (gbppu-framebuffer ppu) (+ (* row 160 3) (* col 3))) (aref COLORS (* palette-col 3))
              (aref (gbppu-framebuffer ppu) (+ (* row 160 3) (* col 3) 1)) (aref COLORS (+ (* palette-col 3) 1))
              (aref (gbppu-framebuffer ppu) (+ (* row 160 3) (* col 3) 2)) (aref COLORS (+ (* palette-col 3) 2))
              ;(aref (gbppu-framebuffer-a ppu) (+ (* row 144 4) (* col 4) 3)) #xff
              )))))
    (if (> row 153) (setf (gbppu-cur-line ppu) 0) (incf (gbppu-cur-line ppu)))
    (write-memory-at-addr mmu #xff44 (gbppu-cur-line ppu))))

(defun step-ppu (ppu cpu mmu renderer texture)
  (incf (gbppu-cycles ppu) (gbcpu-clock cpu))
  (when (> (gbppu-cycles ppu) 2)
      (update-ppu-framebuffer ppu mmu)
      (setf (gbppu-cycles ppu) 0))
  (when (>= (gbppu-cur-line ppu) 144)
    (sdl2:update-texture
      texture
      (cffi:null-pointer)
      (static-vectors:static-vector-pointer
        (gbppu-framebuffer ppu)) (* 160 3))
    (sdl2:render-copy renderer texture)
    (sdl2:render-present renderer)
    (set-interrupt-flag mmu 0)))

(defparameter *out* ())
(defun emu-main (gb)
  (let ((cpu (gb-cpu gb))
        (mmu (gb-mmu gb))
        (ppu (gb-ppu gb))
        (input (gb-input gb)))
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "SDL2 Renderer API Demo" :flags '(:shown))
      (sdl2:with-renderer (renderer win)
				(let ((texture (sdl2:create-texture renderer :rgb24 :streaming 160 144)))
          (sdl2:with-event-loop (:method :poll)
            (:keydown (:keysym keysym)
              (setf (gb-stopped? gb) nil)
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-w)
                (set-interrupt-flag mmu 4)
                (setf (gbinput-up input) t))
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-a)
                (set-interrupt-flag mmu 4)
                (setf (gbinput-left input) t))
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-s)
                (set-interrupt-flag mmu 4)
                (setf (gbinput-down input) t))
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-d)
                (set-interrupt-flag mmu 4)
                (setf (gbinput-right input) t))
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-space)
                (set-interrupt-flag mmu 4)
                (setf (gbinput-start input) t)))
            (:keyup (:keysym keysym)
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
               (sdl2:push-event :quit))
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-w)
                (set-interrupt-flag mmu 4)
                (setf (gbinput-up input) nil))
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-a)
                (set-interrupt-flag mmu 4)
                (setf (gbinput-left input) nil))
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-s)
                (set-interrupt-flag mmu 4)
                (setf (gbinput-down input) nil))
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-d)
                (set-interrupt-flag mmu 4)
                (setf (gbinput-right input) nil))
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-space)
                (setf (gbinput-start input) nil)))
            (:idle ()
              (when (not (gb-stopped? gb))
                (let (( instr (emu-single-op cpu mmu)))
                  (if (not instr) (sdl2:push-event :quit))
                  ;(when (instruction-p instr)
                     ;(format t "~X: ~A --> PC=~X~%" (instruction-opcode instr) (instruction-asm instr) (gbcpu-pc cpu)))
                  (if (= (read-memory-at-addr mmu #xff02) #x81)
                    (progn
                      (setf *out* (cons (code-char (read-memory-at-addr mmu #xff01)) *out*))
                      (write-memory-at-addr mmu #xff02 0)))
                  (if (= (gbcpu-pc cpu) #x100) (setf (gbmmu-is-bios? mmu) nil))
                  (step-ppu ppu cpu mmu renderer texture)
                  (handle-timers cpu mmu)
                  (handle-interrupts cpu mmu))))
            (:quit () t))))))))


(defparameter *gb* (make-gb))

(defun reset-gb ()
  (setf *gb* (make-gb))
  (replace-memory-with-rom (gb-mmu *gb*) loaded-rom)
  ;(write-memory-at-addr mmu #xff44 #x90)
  )

(defun dump-mem-region (start end)
  (loop for a from start to end
        collect (read-memory-at-addr (gb-mmu *gb*) a)))

(defun dump-blargg-output ()
  (dump-mem-region #x9800 #x9BFF))

(defun dump-out ()
  (format t "~{~A~}" (reverse *out*)))

;; test rom memory replace calls
(defparameter loaded-rom "./opus1.gb")
;(defparameter loaded-rom "~/repos/github/retrio/gb-test-roms/instr_timing/instr_timing.gb") ; PASSED
;(defparameter loaded-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/cpu_instrs.gb")
;(defparameter loaded-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/01-special.gb")
;(defparameter loaded-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/02-interrupts.gb")
;(defparameter loaded-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/03-op sp,hl.gb") ; PASSED
;(defparameter loaded-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/04-op r,imm.gb") ; PASSED
;(defparameter loaded-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/05-op rp.gb") ; PASSED
;(defparameter loaded-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/06-ld r,r.gb") ; PASSED
;(defparameter loaded-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/07-jr,jp,call,ret,rst.gb") ; PASSED
;(defparameter loaded-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/08-misc instrs.gb") ; PASSED
;(defparameter loaded-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/09-op r,r.gb")
;(defparameter loaded-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/10-bit ops.gb") ; PASSED
;(defparameter loaded-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/11-op a,(hl).gb")

(replace-memory-with-rom (gb-mmu *gb*) loaded-rom)

;(write-memory-at-addr mmu #xff44 #x90)

(defun run () (emu-main *gb*))

