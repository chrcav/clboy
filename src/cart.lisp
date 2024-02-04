

(in-package :clboy)

(defstruct gbcart
  "represents a gameboy cartridge with variable size ROM and RAM space.
  RAM and ROM are bankable based on the different CARTTYPEs"
  (rom (make-array #x8000 :element-type '(unsigned-byte 8)))
  (ram (make-array #x2000 :initial-element 0 :element-type '(unsigned-byte 8)))
  (filename nil)
  (carttype 0)
  (cgb 0)
  (ramon nil :type boolean)
  (rombank 1)
  (rommask 1)
  (rammask 1)
  (rambank 0)
  (timer (make-rtc))
  (timers (make-array 5 :initial-element 0 :element-type '(unsigned-byte 8)))
  (mode 0))

(defstruct rtc
  (latched? nil :type boolean)
  (latch-reg #xff :type (unsigned-byte 8))
  (secs 0)
  (mins 0)
  (hours 0)
  (days 0)
  (halt? nil :type boolean)
  (carry 0)
  (last-tick (get-internal-real-time)))


(defun make-gbcart-from-rom (filename)
  "Makes a new GBCART based on the rom data found in FILENAME"
  (let ((cart (make-gbcart :filename filename)))
    (replace-memory-with-rom cart filename)
    (get-carttype-from-rom cart)
    (get-rommask-from-rom cart)
    (get-ramsize-from-rom cart)
    (initialize-ram-from-file cart (concatenate 'string filename ".ram"))
    (get-cgb cart)
    cart))

(defun cart-write-ram-to-file (cart filename)
  "writes the contents of CART ram to file referenced by FILENAME only if carttype has battery"
  (case (gbcart-carttype cart)
    ((#x03 #x06 #x09 #x0d #x0f #x10
      #x13 #x1b #x1e #x22 #xff)
     (with-open-file (bin filename
                       :element-type '(unsigned-byte 8)
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
     (write-sequence (gbcart-ram cart) bin)))))

(defun cart-ram-filename (cart)
  (concatenate 'string (gbcart-filename cart) ".ram"))

(defun initialize-ram-from-file (cart file)
  (let ((ram (read-rom-data-from-file file)))
    (replace (gbcart-ram cart) ram)))

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

(defun get-cgb (cart)
  (setf (gbcart-cgb cart) (cart-read-memory-at-addr cart #x143)))

(defun cart-read-memory-at-addr (cart addr)
  "Route memory reads from mmu to CART memory at ADDR. Memory can be ROM, RAM, or other cart data
  based on address and mbc flags"
  (case (gbcart-carttype cart)
    ((#x1 #x2 #x3) (cart-mbc1-read cart addr))
    ((#x5 #x6) (cart-mbc2-read cart addr))
    ((#xf #x10 #x11 #x12 #x13) (cart-mbc3-read cart addr))
    ((#x19 #x1a #x1b #x1c #x1d #x1e) (cart-mbc5-read cart addr))
    (otherwise
      (case (logand addr #xf000)
        ((#x0000 #x1000 #x2000 #x3000 #x4000 #x5000 #x6000 #x7000)
         (aref (gbcart-rom cart) addr))
        ((#xa000 #xb000)
         (aref (gbcart-ram cart) (logand addr #x1fff)))))))

(defun cart-write-memory-at-addr (cart addr val)
  "Route memory writes from mmu to CART memory at ADDR. Memory can be ROM, RAM, or other cart data
  based on address and mbc flags"
  (case (gbcart-carttype cart)
    ((#x1 #x2 #x3) (cart-mbc1-write cart addr val))
    ((#x5 #x6) (cart-mbc2-write cart addr val))
    ((#xf #x10 #x11 #x12 #x13) (cart-mbc3-write cart addr val))
    ((#x19 #x1a #x1b #x1c #x1d #x1e) (cart-mbc5-write cart addr val))
    (otherwise
      (case (logand addr #xf000)
        ((#xa000 #xb000)
         (setf (aref (gbcart-ram cart) (logand addr #x1fff)) val))))))

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
     (let ((lower5bits (logand val #x1f)))
       (setf (gbcart-rombank cart) 
             (logand
                (+ (logand (gbcart-rombank cart) #x60) (if (> lower5bits 1) lower5bits 1))
                (gbcart-rommask cart)))))
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
       (let ((lsb (logand val #xf)))
         (setf (gbcart-rombank cart) (logand (if (> lsb 1) lsb 1) (gbcart-rommask cart))))))
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
           (if (>= (gbcart-rambank cart) 8)
             (rtc-read (gbcart-timer cart) (logand (gbcart-rambank cart) #x7))
             (aref (gbcart-ram cart) (+ (* (gbcart-rambank cart) #x2000) (logand addr #x1fff))))
           #xff)
       #xff))))

(defun cart-mbc3-write (cart addr val)
  "CART memory writes for mbc3 cartridges. writes at ADDR with VAL will modify ram values or update
  various mbc flags"
  (case (logand addr #xf000)
    ((#x0000 #x1000)
     (setf (gbcart-ramon cart) (= (logand val #xf) #x0a)))
    ((#x2000 #x3000)
     (setf (gbcart-rombank cart) (logand (if (> val 1) val 1) (gbcart-rommask cart))))
    ((#x4000 #x5000)
     (if (= (logand val #x8) #x0)
       (setf (gbcart-rambank cart) (logand val (gbcart-rammask cart)))
       (setf (gbcart-rambank cart) val)))
    ((#x6000 #x7000)
     (write-timer-latch (gbcart-timer cart) val))
    ((#xa000 #xb000)
     (when (gbcart-ramon cart)
       (when (>= (gbcart-rambank cart) 0)
         (if (>= (gbcart-rambank cart) 8)
           (rtc-write (gbcart-timer cart) (logand (gbcart-rambank cart) #x7) val)
           (setf (aref (gbcart-ram cart) (+ (* (gbcart-rambank cart) #x2000) (logand addr #x1fff))) val)))))))

(defun cart-mbc5-read (cart addr)
  "CART memory reads for mbc5 type cartridges. reads the memory at ADDR based on which banks are selected."
  (case (logand addr #xf000)
    ((#x0000 #x1000 #x2000 #x3000)
      (aref (gbcart-rom cart) (logand addr #x3fff)))
    ((#x4000 #x5000 #x6000 #x7000)
      (aref (gbcart-rom cart) (+ (* (gbcart-rombank cart) #x4000) (logand addr #x3fff))))
    ((#xa000 #xb000)
     (if (gbcart-ramon cart)
         (if (>= (gbcart-rambank cart) 0)
           (aref (gbcart-ram cart) (+ (* (gbcart-rambank cart) #x2000) (logand addr #x1fff)))
           #xff)
       #xff))))

(defun cart-mbc5-write (cart addr val)
  "CART memory writes for mbc5 cartridges. writes at ADDR with VAL will modify ram values or update
  various mbc flags"
  (case (logand addr #xf000)
    ((#x0000 #x1000)
     (setf (gbcart-ramon cart) (= (logand val #xf) #x0a)))
    (#x2000
     (let ((rombank (logand (logior (logand (gbcart-rommask cart) #x100) val) (gbcart-rommask cart))))
       (setf (gbcart-rombank cart) rombank)))
    (#x3000
     (let ((rombank (logand (logior (logand (gbcart-rommask cart) #xff) (logand val #x1)) (gbcart-rommask cart))))
       (setf (gbcart-rombank cart) rombank)))
    ((#x4000 #x5000)
     (setf (gbcart-rambank cart) (logand (logand (ash val -5) #x03) (gbcart-rammask cart))))
    ((#xa000 #xb000)
     (when (gbcart-ramon cart)
       (when (>= (gbcart-rambank cart) 0)
         (setf (aref (gbcart-ram cart) (+ (* (gbcart-rambank cart) #x2000) (logand addr #x1fff))) val))))))

(defun rtc-write (rtc timerbank val)
  (case timerbank
    (#x0 (setf (rtc-secs rtc) (mod val 60)))
    (#x1 (setf (rtc-mins rtc) (mod val 60)))
    (#x2 (setf (rtc-hours rtc) (mod val 24)))
    (#x3 (setf (rtc-days rtc) (logior (logand (rtc-days rtc) #x100) (logand val #xff))))
    (#x4
      (if (= (logand val #x40) #x40)
        (setf (rtc-halt? rtc) t)
        (setf (rtc-halt? rtc) nil
              (rtc-last-tick rtc) (get-internal-real-time)))
      (setf (rtc-days rtc) (logior (logand (rtc-days rtc) #xff) (ash (logand val #x1) 8)))
      (setf (rtc-carry rtc) (ash val -7)))
    (otherwise ())))

(defun rtc-read (rtc timerbank)
  (logand #xff
          (case timerbank
            (#x0 (rtc-secs rtc))
            (#x1 (rtc-mins rtc))
            (#x2 (rtc-hours rtc))
            (#x3 (rtc-days rtc))
            (#x4
              (logior (ash (rtc-days rtc) -8)
                      (ash (clboy-utils:bool-as-bit (rtc-halt? rtc)) 6)
                      (ash (rtc-carry rtc) 7)))
            (otherwise #xff))))

(defun write-timer-latch (rtc val)
   (if (and (= (rtc-latch-reg rtc) 0) (= val 1))
     (setf (rtc-latched? rtc) (not (rtc-latched? rtc))))
   (setf (rtc-latch-reg rtc) val))

(defun reset-timer (rtc)
  (make-rtc :halt? (rtc-halt? rtc)))

(defun step-rtc (rtc)
  (when (and (not (rtc-halt? rtc)) (not (rtc-latched? rtc)))
    (when (>= (- (get-internal-real-time) (rtc-last-tick rtc)) internal-time-units-per-second)
      (loop for i from 1 to (floor (- (get-internal-real-time) (rtc-last-tick rtc)) internal-time-units-per-second)
            do (tick-rtc rtc))
      (setf (rtc-last-tick rtc) (get-internal-real-time)))))

(defun tick-rtc (rtc)
  (when (>= (incf (rtc-secs rtc)) 60)
    (decf (rtc-secs rtc) 60)
    (when (>= (incf (rtc-mins rtc)) 60)
      (decf (rtc-mins rtc) 60)
      (when (>= (incf (rtc-hours rtc)) 24)
        (decf (rtc-hours rtc) 24)
        (when (>= (incf (rtc-days rtc)) #x1ff)
          (decf (rtc-days rtc) #x1ff)
          (setf (rtc-carry rtc) 1))))))
