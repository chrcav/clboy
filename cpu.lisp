


(in-package :clboy)

(defconstant +cpu-speed+ 4194304)

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
  (clock-remainder 0)
  (div-clock 0 :type (unsigned-byte 16))
  (int-ena 0 :type (unsigned-byte 1))
  (halted 0 :type (unsigned-byte 1))
  (flags (make-gbflags)))

(defstruct gbflags
  (z 0 :type (unsigned-byte 1))
  (n 0 :type (unsigned-byte 1))
  (h 0 :type (unsigned-byte 1))
  (c 0 :type (unsigned-byte 1)))

(defun get-address-from-reg-pair (msb lsb)
  (logior lsb (ash msb 8)))


(defun get-byte-from-hl-address (cpu gb)
  (let ((addr (get-address-from-reg-pair (gbcpu-h cpu) (gbcpu-l cpu))))
    (read-memory-at-addr gb addr)))

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

(defun daa (cpu)
  (let* ((a (gbcpu-a cpu))
         (cor-lsb
           (if (or (= (gbflags-h (gbcpu-flags cpu)) #x01)
                   (and (= (gbflags-n (gbcpu-flags cpu)) #x00) (> (logand a #xf) #x09)))
             #x06
             #x00))
         (cor-msb
           (if (or (= (gbflags-c (gbcpu-flags cpu)) #x01)
                   (and (= (gbflags-n (gbcpu-flags cpu)) #x00) (> a #x99)))
             #x60
             #x00))
         (res
           (if (= (gbflags-n (gbcpu-flags cpu)) #x01)
             (- a (+ cor-lsb cor-msb))
             (+ a (+ cor-lsb cor-msb)))))
    (setf (gbcpu-a cpu) (logand res #xff)
          (gbflags-z (gbcpu-flags cpu)) (if (= (logand res #xff) #x00) #x01 #x00)
          (gbflags-h (gbcpu-flags cpu)) #x00
          (gbflags-c (gbcpu-flags cpu)) (if (= cor-msb #x60) #x01 #x00))))

(defun do-call-at-addr (cpu gb addr)
  (push-addr-on-stack cpu gb (gbcpu-pc cpu))
  (setf (gbcpu-pc cpu) addr))

(defun do-rst (cpu gb addr)
  (push-addr-on-stack cpu gb (gbcpu-pc cpu))
  (setf (gbcpu-pc cpu) addr))

(defun do-jump (cpu addr)
  (setf (gbcpu-pc cpu) addr))

(defun do-ret (cpu gb)
  (let ((addr (pop-addr-from-stack cpu gb)))
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

(defun push-addr-on-stack (cpu gb addr)
  (let ((sp (gbcpu-sp cpu))
        (lsb (logand addr #xff))
        (msb (logand (ash addr -8) #xff)))
    (decf (gbcpu-sp cpu) 2)
    (write-memory-at-addr gb (- sp 1) msb)
    (write-memory-at-addr gb (- sp 2) lsb)))
(defun pop-addr-from-stack (cpu gb)
  (let* ((sp (gbcpu-sp cpu))
         (lsb (read-memory-at-addr gb sp))
         (msb (read-memory-at-addr gb (+ sp 1))))
    (incf (gbcpu-sp cpu) 2)
    (logior lsb (ash msb 8))))

(defun push-reg-pair-on-stack (cpu gb reg1 reg2)
  (let ((sp (gbcpu-sp cpu)))
    (decf (gbcpu-sp cpu) 2)
    (write-memory-at-addr gb (- sp 1) reg1)
    (write-memory-at-addr gb (- sp 2) reg2)))

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


(defun emu-single-op (cpu gb)
  (if (= (gbcpu-halted cpu) #x00)
    (let* ((op (read-memory-at-addr gb (gbcpu-pc cpu)))
          (instr (if (= op #xcb) (get-cb-instruction cpu gb) (aref ops op))))
      (if (instruction-p instr)
        (progn (if (null (instruction-fun instr))
               (format t "Unable to run function for instruction ~X @ ~X~%" op (gbcpu-pc cpu))
               (funcall (instruction-fun instr) cpu gb instr))
             instr)
      (format t "Unimplemented instruction ~X @ ~X~%" op (gbcpu-pc cpu))))
    (progn (incf (gbcpu-clock cpu) 4)
           (incf (gbcpu-div-clock cpu)))))

(defun do-interrupt (cpu gb interrupt-id)
  (setf (gbcpu-int-ena cpu) #x00)
  (write-memory-at-addr gb #xff0f (logand (read-memory-at-addr gb #xff0f) (logxor (ash #x01 interrupt-id) #xff)))
  (do-call-at-addr cpu gb (+ (* interrupt-id 8) #x40)))

(defun step-cpu (cpu gb)
  (let ((instr (emu-single-op cpu gb)))
    (when (not instr) (sdl2:push-event :quit) nil)
    (when (and *debug* (instruction-p instr))
       (format t "~X: ~A --> PC=~X~%" (instruction-opcode instr) (instruction-asm instr) (gbcpu-pc cpu)))
    ;(when (= (gbcpu-pc cpu) #xc33d) (sdl2:push-event :quit) nil)
    ;(when (= (instruction-opcode instr) #x27) (sdl2:push-event :quit) nil)
    (if (= (gbcpu-pc cpu) #x100) (setf (gb-is-bios? gb) nil))
    t))
