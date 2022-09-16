

(in-package :clboy)

(defstruct gbcart
  "represents a gameboy cartridge with variable size ROM and RAM space.
  RAM and ROM are bankable based on the different CARTTYPEs"
  (rom (make-array #x8000 :element-type '(unsigned-byte 8)))
  (ram (make-array #x2000 :initial-element 0 :element-type '(unsigned-byte 8)))
  (filename nil)
  (carttype 0)
  (ramon nil :type boolean)
  (rombank 1)
  (rommask 1)
  (rammask 1)
  (rambank 0)
  (timerbank 0)
  (timers (make-array 5 :initial-element 0 :element-type '(unsigned-byte 8)))
  (mode 0))


(defun make-gbcart-from-rom (filename)
  "Makes a new GBCART based on the rom data found in FILENAME"
  (let ((cart (make-gbcart :filename filename)))
    (replace-memory-with-rom cart filename)
    (get-carttype-from-rom cart)
    (get-rommask-from-rom cart)
    (get-ramsize-from-rom cart)
    cart))

(defun replace-memory-with-rom (cart file)
  "given a GBCART CART replace rom with FILE contents"
  (let ((rom (read-rom-data-from-file file)))
    (setf (gbcart-rom cart) (make-array (length rom) :element-type '(unsigned-byte 8)))
    (replace (gbcart-rom cart) rom)))

(defun get-carttype-from-rom (cart)
  "read carttype value from CART memory"
  (setf (gbcart-carttype cart) (cart-read-memory-at-addr cart #x147)))

(defun get-rommask-from-rom (cart)
  "Read rommask value based on CART rom size memory location"
  (setf (gbcart-rommask cart) (- (ash #x02 (cart-read-memory-at-addr cart #x148)) 1)))

(defun get-ramsize-from-rom (cart)
  "Read ramsize from CART header memory location"
  (let ((rambanks (case (aref (gbcart-rom cart) #x0149)
                   (#x00 0)
                   (#x02 1)
                   (#x03 4)
                   (#x04 16)
                   (#x05 8))))
    (if (> rambanks 0) (setf (gbcart-ram cart) (make-array (* rambanks #x2000) :element-type '(unsigned-byte 8))))
    (setf (gbcart-rammask cart) (- rambanks 1))))

(defun cart-read-memory-at-addr (cart addr)
  "Route memory reads from mmu to CART memory at ADDR. Memory can be ROM, RAM, or other cart data
  based on address and mbc flags"
  (case (gbcart-carttype cart)
       ((#x1 #x2 #x3) (cart-mbc1-read cart addr))
       ((#x5 #x6) (cart-mbc2-read cart addr))
       ((#xf #x10 #x11 #x12 #x13) (cart-mbc3-read cart addr))
       (otherwise
         (case (logand addr #xf000)
           ((#x0000 #x1000 #x2000 #x3000 #x4000 #x5000 #x6000 #x7000)
            (aref (gbcart-rom cart) addr))
           ((#xa000 #xb000)
            (aref (gbcart-ram cart) addr))))))

(defun cart-write-memory-at-addr (cart addr val)
  "Route memory writes from mmu to CART memory at ADDR. Memory can be ROM, RAM, or other cart data
  based on address and mbc flags"
  (case (gbcart-carttype cart)
    ((#x1 #x2 #x3) (cart-mbc1-write cart addr val))
    ((#x5 #x6) (cart-mbc2-write cart addr val))
    ((#xf #x10 #x11 #x12 #x13) (cart-mbc3-write cart addr val))
    (otherwise
      (case (logand addr #xf000)
        ((#xa000 #xb000)
         (setf (aref (gbcart-ram cart)
                     (+ (* (gbcart-rambank cart) #x2000) (logand addr #x1fff)))
               val))))))

(defun cart-mbc1-read (cart addr)
  "CART memory reads for mbc1 type cartridges. reads the memory at ADDR based on which banks are selected."
  (case (logand addr #xf000)
    ((#x0000 #x1000 #x2000 #x3000)
     (aref (gbcart-rom cart) (+ (* (logand (if (= (gbcart-mode cart) 1) (gbcart-rombank cart) 0) #x60) #x4000) (logand addr #x3fff))))
    ((#x4000 #x5000 #x6000 #x7000)
     (aref (gbcart-rom cart) (+ (* (gbcart-rombank cart) #x4000) (logand addr #x3fff))))
    ((#xa000 #xb000)
     (if (gbcart-ramon cart)
       (aref (gbcart-ram cart) (+ (* (if (= (gbcart-mode cart) 1) (gbcart-rambank cart) #x2000) (logand addr #x1fff))))
       #xff))))

(defun cart-mbc1-write (cart addr val)
  "CART memory writes for mbc1 cartridges. writes at ADDR with VAL will modify ram values or update
  various mbc flags"
  (case (logand addr #xf000)
    ((#x0000 #x1000)
     (setf (gbcart-ramon cart) (= (logand val #xf) #x0a)))
    ((#x2000 #x3000)
     (let* ((lower5bits (logand val #x1f))
            (rombank
              (logand
                (+ (logand (gbcart-rombank cart) #x60) (if (> lower5bits 1) lower5bits 1))
                (gbcart-rommask cart))))
       (setf (gbcart-rombank cart) rombank)))
    ((#x4000 #x5000)
     (let ((rombank
             (logand
               (+ (logand (gbcart-rombank cart) #x1f) (ash (logand val #x03) 5))
               (gbcart-rommask cart))))
       (setf (gbcart-rombank cart) rombank))
     (when (= (gbcart-mode cart) #x01)
       (let ((rambank (logand (logand val #x03) (gbcart-rammask cart))))
         (setf (gbcart-rambank cart) rambank))))
    ((#x6000 #x7000)
     (setf (gbcart-mode cart) (logand val #x01)))
    ((#xa000 #xb000)
     (if (gbcart-ramon cart)
       (setf (aref (gbcart-ram cart) (+ (* (gbcart-rambank cart) #x2000) (logand addr #x1fff))) val)))))

(defun cart-mbc2-read (cart addr)
  "CART memory reads for mbc2 type cartridges. reads the memory at ADDR based on which banks are selected."
  (case (logand addr #xf000)
    ((#x0000 #x1000 #x2000 #x3000)
      (aref (gbcart-rom cart) (+ (* (logand (if (= (gbcart-mode cart) 1) (gbcart-rombank cart) 0) #x60) #x4000) (logand addr #x3fff))))
    ((#x4000 #x5000 #x6000 #x7000)
      (aref (gbcart-rom cart) (+ (* (gbcart-rombank cart) #x4000) (logand addr #x3fff))))
    ((#xa000 #xb000)
      (if (gbcart-ramon cart)
        (aref (gbcart-ram cart) (logand addr #x1ff))
        #xff))))

(defun cart-mbc2-write (cart addr val)
  "CART memory writes for mbc2 cartridges. writes at ADDR with VAL will modify ram values or update
  various mbc flags"
  (case (logand addr #xf000)
    ((#x0000 #x1000 #x2000 #x3000)
     (if (= (logand addr #x100) 0)
       (setf (gbcart-ramon cart) (= (logand val #xf) #x0a))
       (let* ((lsb (logand val #xf))
              (rombank (logand (if (> lsb 1) lsb 1) (gbcart-rommask cart))))
         (setf (gbcart-rombank cart) rombank))))
    ((#xa000 #xb000)
     (if (gbcart-ramon cart)
       (setf (aref (gbcart-ram cart) (logand addr #x1ff)) val)))))

(defun cart-mbc3-read (cart addr)
  "CART memory reads for mbc3 type cartridges. reads the memory at ADDR based on which banks are selected."
  (case (logand addr #xf000)
    ((#x0000 #x1000 #x2000 #x3000)
      (aref (gbcart-rom cart) (logand addr #x3fff)))
    ((#x4000 #x5000 #x6000 #x7000)
      (aref (gbcart-rom cart) (+ (* (gbcart-rombank cart) #x4000) (logand addr #x3fff))))
    ((#xa000 #xb000)
     (if (gbcart-ramon cart)
         (if (>= (gbcart-rambank cart) 0)
           (aref (gbcart-ram cart) (+ (* (gbcart-rambank cart) #x2000) (logand addr #x1fff)))
         (if (>= (gbcart-timerbank cart) 0)
           (aref (gbcart-timers cart) (logand (gbcart-timerbank cart) #x7))
           #xff))
       #xff))))

(defun cart-mbc3-write (cart addr val)
  "CART memory writes for mbc3 cartridges. writes at ADDR with VAL will modify ram values or update
  various mbc flags"
  (case (logand addr #xf000)
    ((#x0000 #x1000)
     (setf (gbcart-ramon cart) (= (logand val #xf) #x0a)))
    ((#x2000 #x3000)
     (let* ((rombank (logand (if (> val 1) val 1) (gbcart-rommask cart))))
       (setf (gbcart-rombank cart) rombank)))
    ((#x4000 #x5000)
     (let ((rambank (logand (logand (ash val -5) #x03) (gbcart-rammask cart))))
       (if (= (logand rambank #x8) #x0)
       (setf (gbcart-rambank cart) rambank
             (gbcart-timerbank cart) -1)
       (setf (gbcart-timerbank cart) rambank
             (gbcart-rambank cart) -1))))
    ((#x6000 #x7000) ; TODO latch clock data
     ())
    ((#xa000 #xb000)
     (when (gbcart-ramon cart)
       (when (>= (gbcart-rambank cart) 0)
         (setf (aref (gbcart-ram cart) (+ (* (gbcart-rambank cart) #x2000) (logand addr #x1fff))) val))
       (when (>= (gbcart-timerbank cart) 0) ; TODO need to understand how the timers function
         (setf (aref (gbcart-timers cart) (logand (gbcart-timerbank cart) #x7)) val))))))

