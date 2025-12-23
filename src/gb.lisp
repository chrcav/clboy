


(in-package :clboy)


(defstruct gbinput
  (down   nil :type boolean)
  (up     nil :type boolean)
  (left   nil :type boolean)
  (right  nil :type boolean)
  (a      nil :type boolean)
  (b      nil :type boolean)
  (start  nil :type boolean)
  (select nil :type boolean)
  (reg #xff :type (unsigned-byte 8)))


;; MMU

(defun write-memory-at-addr (gb addr val)
  "Routes various writes for GB to various other components (i.e. ppu, cart, etc.) based on ADDR.
  Location is set to or modified based on VAL"
  (case (logand addr #xf000)
    ((#x0000 #x1000 #x2000 #x3000 #x4000
      #x5000 #x6000 #x7000 #xa000 #xb000)
     (if (gbcart-p (gb-cart gb)) (cart-write-memory-at-addr (gb-cart gb) addr val)))
    ((#x8000 #x9000)
     (ppu-write-memory-at-addr (gb-ppu gb) addr val))
    ((#xc000 #xe000)
     (setf (aref (gb-int-ram gb) (logand addr #x0fff)) val))
    (#xd000
     (setf (aref (gb-int-ram gb) (+ (logand addr #x0fff) (* (gb-int-ram-bank gb) #x1000))) val))
    (#xf000
     (case (logand addr #x0f00)
       ((#x100 #x200 #x300 #x400 #x500 #x600 #x700
         #x800 #x900 #xa00 #xb00 #xc00 #xd00)
        (setf (aref (gb-int-ram gb) (+ (logand addr #x0fff) (* (gb-int-ram-bank gb) #x1000))) val))
       (#xe00
        (ppu-write-memory-at-addr (gb-ppu gb) addr val))
       (#xf00
        (case (logand addr #x00f0)
          (#x00
           (case (logand addr #x000f)
             (#x0 (setf (gbinput-reg (gb-input gb)) val))
             (#x4 (setf (gb-div-clock gb) 0
                        (gb-tima gb) (gb-tma gb)))
             (#x5 (setf (gb-tima gb) val))
             (#x6 (setf (gb-tma gb) val))
             (#x7 (setf (gb-tac gb) val))
             (otherwise (setf (aref (gb-zero-page gb) (logand addr #xff)) val))))
          ((#x10 #x20 #x30) (spu-write-memory-at-addr (gb-spu gb) addr val))
          (#x40
           (case (logand addr #x000f)
             ((#x0 #x1 #x2 #x3 #x4 #x5 #x6 #x7 #x8 #x9 #xa #xb #xf) (ppu-write-memory-at-addr (gb-ppu gb) addr val))
             (#xd (if (cgb-p gb) (setf (cgb-is-speed-armed? gb) (= (logand val #x01) #x01))))))
          (#x50
           (case (logand addr #x000f)
             (#x0 (setf (gb-is-bios? gb) (= val 0)))
             ((#x1 #x2 #x3 #x4 #x5) (ppu-write-memory-at-addr (gb-ppu gb) addr val))
             ;(#x6 #xff) TODO IR port handling CGB mode
          ))
          (#x60
           (case (logand addr #x000f)
             ((#x8 #x9 #xa #xb) (ppu-write-memory-at-addr (gb-ppu gb) addr val)) ; CGB PPU palletes
             (otherwise (setf (aref (gb-zero-page gb) (logand addr #xff)) val))))
          (#x70
           (case (logand addr #x000f)
             (#x0 (if (cgb-p gb) (setf (gb-int-ram-bank gb) (if (> (logand val #x7) 0) (logand val #x7) 1))))
             ((#x2 #x3 #x4 #x5) (setf (aref (gb-zero-page gb) (logand addr #xff)) val)) ;CGB undocumented registers
             ((#x6 #x7) (spu-write-memory-at-addr (gb-spu gb) addr val)))) ;CGB SPU PCM
          (otherwise (setf (aref (gb-zero-page gb) (logand addr #xff)) val))))))))

(defun read-memory-at-addr (gb addr)
  "Routes various reads for GB to various other components (i.e. ppu, cart, etc.) based on ADDR."
  (case (logand addr #xf000)
    ((#x0000 #x1000 #x2000 #x3000 #x4000
      #x5000 #x6000 #x7000 #xa000 #xb000)
      (if (and (or (< addr #x100) (and (> addr #x1ff) (< addr #x900))) (gb-is-bios? gb))
          (aref (gb-bios gb) addr)
          (if (gbcart-p (gb-cart gb)) (cart-read-memory-at-addr (gb-cart gb) addr) #xff)))
    ((#x8000 #x9000)
      (ppu-read-memory-at-addr (gb-ppu gb) addr))
    ((#xc000 #xe000)
      (aref (gb-int-ram gb) (logand addr #x0fff)))
    (#xd000
      (aref (gb-int-ram gb) (+ (logand addr #x0fff) (* (gb-int-ram-bank gb) #x1000))))
    (#xf000
      (case (logand addr #x0f00)
        ((#x000 #x100 #x200 #x300 #x400 #x500 #x600
          #x700 #x800 #x900 #xa00 #xb00 #xc00 #xd00)
          (aref (gb-int-ram gb) (+ (logand addr #x0fff) (* (gb-int-ram-bank gb) #x1000))))
        (#xe00
          (ppu-read-memory-at-addr (gb-ppu gb) addr))
        (#xf00
          (case (logand addr #x00f0)
            (#x00
             (case (logand addr #x000f)
               (#x0 (input-read-memory (gb-input gb)))
               (#x4 (gb-div-clock gb))
               (#x5 (gb-tima gb))
               (#x6 (gb-tma gb))
               (#x7 (gb-tac gb))
               (otherwise (aref (gb-zero-page gb) (logand addr #xff)))))
            ((#x10 #x20 #x30) (spu-read-memory-at-addr (gb-spu gb) addr))
            (#x40
              (case (logand addr #x000f)
                ((#x0 #x1 #x2 #x3 #x4 #x5 #x6 #x7 #x8 #x9 #xa #xb #xf) (ppu-read-memory-at-addr (gb-ppu gb) addr))
                (#xd (if (cgb-p gb) (logior (if (cgb-is-speed-armed? gb) #x01 #x00) (if (cgb-is-double-speed? gb) #x80 #x00)) #x00))
                (otherwise #xff)))
            (#x50
              (case (logand addr #x000f)
                (#x0 (if (gb-is-bios? gb) #xff (if (cgb-p gb) #x11 #xff)))
                ((#x1 #x2 #x3 #x4 #x5) (ppu-read-memory-at-addr (gb-ppu gb) addr))
                ;(#x6 #xff) TODO IR port handling CGB mode
                (otherwise #xff)))
            (#x60
             (case (logand addr #x000f)
               ((#x8 #x9 #xa #xb) (ppu-read-memory-at-addr (gb-ppu gb) addr))
               (otherwise (aref (gb-zero-page gb) (logand addr #xff)))))
            (#x70
             (case (logand addr #x000f)
               (#x0 (if (cgb-p gb) (gb-int-ram-bank gb)))
               ((#x1 #x2 #x3 #x4 #x5) (aref (gb-zero-page gb) (logand addr #xff))) ;CGB mode undocumented registers
               ((#x6 #x7) (spu-read-memory-at-addr (gb-spu gb) addr)))) ;CGB mode SPU PCM
            (otherwise (aref (gb-zero-page gb) (logand addr #xff)))))))))

(defun maybe-do-speed-switch (gb)
  (when (cgb-is-speed-armed? gb)
    (setf (gb-stopped? gb) nil
          (cgb-is-double-speed? gb) (if (cgb-is-double-speed? gb) nil t)
          (cgb-is-speed-armed? gb) nil)
    (format t "is speed doubled ~A~%"
            (cgb-is-double-speed? gb))
    (if (cgb-is-double-speed? gb)
        (setf (gbcpu-cpu-speed (gb-cpu gb)) (* +default-cpu-speed+ 2))
        (setf (gbcpu-cpu-speed (gb-cpu gb)) +default-cpu-speed+))))

(defun get-address-from-memory (gb addr)
  "Reads a 16-bit address from memory of GB at ADDR in little endian"
  (let ((lsb (read-memory-at-addr gb addr))
        (msb (read-memory-at-addr gb (+ addr 1))))
    (logior lsb (ash msb 8))))

;; INTERRUPTS
(defun handle-interrupts (cpu gb)
  "handles interrupts that are enabled and active based on registers at #xff0f and #xffff. Interrupts
  are passed to the CPU to execute a call like instruction"
  ; VBLANK interupt
  (if (= (logand (read-memory-at-addr gb #xffff) (read-memory-at-addr gb #xff0f) #x01) #x01)
    (do-interrupt cpu gb 0)
  ; LCDC Status interupt
  (if (= (logand (read-memory-at-addr gb #xffff) (read-memory-at-addr gb #xff0f) #x02) #x02)
    (do-interrupt cpu gb 1)
  ; Timer interupt
  (if (= (logand (read-memory-at-addr gb #xffff) (read-memory-at-addr gb #xff0f) #x04) #x04)
    (do-interrupt cpu gb 2)
  ; Serial Transfer interupt
  (if (= (logand (read-memory-at-addr gb #xffff) (read-memory-at-addr gb #xff0f) #x08) #x08)
    (do-interrupt cpu gb 3)
  ; Hi-Lo of P10-P13 interupt
  (if (= (logand (read-memory-at-addr gb #xffff) (read-memory-at-addr gb #xff0f) #x10) #x10)
    (do-interrupt cpu gb 4)))))))

(defun set-interrupt-flag (gb bit-pos)
  (write-memory-at-addr gb #xff0f (logior (read-memory-at-addr gb #xff0f) (ash #x01 bit-pos))))

;; TIMER
(defun handle-timers (cpu gb)
  "updates timer registers based on the number of cycles from the last instruction."
  (let ((cycles (gbcpu-clock cpu))
        (remainder (gbcpu-clock-remainder cpu)))
  (if (= (logand (gb-tac gb) #x04) #x04)
    (incr-timer-by-cycles cpu gb (+ cycles remainder) (get-cycles-per-timer-tick (gb-tac gb))))
  (incf (gbcpu-div-cycles cpu) (/ cycles 4))
  (when (> (gbcpu-div-cycles cpu) #xff)
      (setf (gbcpu-div-cycles cpu) (- (gbcpu-div-cycles cpu) #x100)
            (gb-div-clock gb) (logand (+ (gb-div-clock gb) #x01) #xff)))
  (setf (gbcpu-clock cpu) 0)))

(defun incr-timer-by-cycles (cpu gb cycles cycles-per-tick)
  "incements the timer by the number of cycles of the last instuction and triggers interrupt when
  CYCLES-PER-TICK cycles have elapsed since last interrupt"
  (let ((new-ticks (+ (gb-tima gb) (floor cycles cycles-per-tick))))
    (setf (gbcpu-clock-remainder cpu) (mod cycles cycles-per-tick))
    (if (> new-ticks #xff)
      (progn (set-interrupt-flag gb 2)
             (setf (gb-tima gb) (+ (gb-tma gb) (logand new-ticks #xff))))
      (setf (gb-tima gb) (logand new-ticks #xff)))))

(defun get-cycles-per-timer-tick (tac)
  "number of cycles per timer tick based on the TAC register of the timer."
  (let ((tac-two-lsb (logand tac #x3)))
    (if (= tac-two-lsb #x01) 16
      (if (= tac-two-lsb #x02) 64
        (if (= tac-two-lsb #x03) 256
          1024)))))

;; utils

(defparameter *dmg-bios*
  '(#x31 #xFE #xFF #xAF #x21 #xFF #x9F #x32 #xCB #x7C #x20 #xFB #x21 #x26 #xFF #x0E
    #x11 #x3E #x80 #x32 #xE2 #x0C #x3E #xF3 #xE2 #x32 #x3E #x77 #x77 #x3E #xFC #xE0
    #x47 #x11 #x04 #x01 #x21 #x10 #x80 #x1A #xCD #x95 #x00 #xCD #x96 #x00 #x13 #x7B
    #xFE #x34 #x20 #xF3 #x11 #xD8 #x00 #x06 #x08 #x1A #x13 #x22 #x23 #x05 #x20 #xF9
    #x3E #x19 #xEA #x10 #x99 #x21 #x2F #x99 #x0E #x0C #x3D #x28 #x08 #x32 #x0D #x20
    #xF9 #x2E #x0F #x18 #xF3 #x67 #x3E #x64 #x57 #xE0 #x42 #x3E #x91 #xE0 #x40 #x04
    #x1E #x02 #x0E #x0C #xF0 #x44 #xFE #x90 #x20 #xFA #x0D #x20 #xF7 #x1D #x20 #xF2
    #x0E #x13 #x24 #x7C #x1E #x83 #xFE #x62 #x28 #x06 #x1E #xC1 #xFE #x64 #x20 #x06
    #x7B #xE2 #x0C #x3E #x87 #xE2 #xF0 #x42 #x90 #xE0 #x42 #x15 #x20 #xD2 #x05 #x20
    #x4F #x16 #x20 #x18 #xCB #x4F #x06 #x04 #xC5 #xCB #x11 #x17 #xC1 #xCB #x11 #x17
    #x05 #x20 #xF5 #x22 #x23 #x22 #x23 #xC9 #xCE #xED #x66 #x66 #xCC #x0D #x00 #x0B
    #x03 #x73 #x00 #x83 #x00 #x0C #x00 #x0D #x00 #x08 #x11 #x1F #x88 #x89 #x00 #x0E
    #xDC #xCC #x6E #xE6 #xDD #xDD #xD9 #x99 #xBB #xBB #x67 #x63 #x6E #x0E #xEC #xCC
    #xDD #xDC #x99 #x9F #xBB #xB9 #x33 #x3E #x3C #x42 #xB9 #xA5 #xB9 #xA5 #x42 #x3C
    #x21 #x04 #x01 #x11 #xA8 #x00 #x1A #x13 #xBE #x20 #xFE #x23 #x7D #xFE #x34 #x20
    #xF5 #x06 #x19 #x78 #x86 #x23 #x05 #x20 #xFB #x86 #x20 #xFE #x3E #x01 #xE0 #x50)
  "DMG bios as a list of bytes")

(defparameter *cgb-bios*
  '(#x31 #xFE #xFF #x3E #x02 #xC3 #x7C #x00 #xD3 #x00 #x98 #xA0 #x12 #xD3 #x00
 #x80 #x00 #x40 #x1E #x53 #xD0 #x00 #x1F #x42 #x1C #x00 #x14 #x2A #x4D #x19
 #x8C #x7E #x00 #x7C #x31 #x6E #x4A #x45 #x52 #x4A #x00 #x00 #xFF #x53 #x1F
 #x7C #xFF #x03 #x1F #x00 #xFF #x1F #xA7 #x00 #xEF #x1B #x1F #x00 #xEF #x1B
 #x00 #x7C #x00 #x00 #xFF #x03 #xCE #xED #x66 #x66 #xCC #x0D #x00 #x0B #x03
 #x73 #x00 #x83 #x00 #x0C #x00 #x0D #x00 #x08 #x11 #x1F #x88 #x89 #x00 #x0E
 #xDC #xCC #x6E #xE6 #xDD #xDD #xD9 #x99 #xBB #xBB #x67 #x63 #x6E #x0E #xEC
 #xCC #xDD #xDC #x99 #x9F #xBB #xB9 #x33 #x3E #x3C #x42 #xB9 #xA5 #xB9 #xA5
 #x42 #x3C #x58 #x43 #xE0 #x70 #x3E #xFC #xE0 #x47 #xCD #x75 #x02 #xCD #x00
 #x02 #x26 #xD0 #xCD #x03 #x02 #x21 #x00 #xFE #x0E #xA0 #xAF #x22 #x0D #x20
 #xFC #x11 #x04 #x01 #x21 #x10 #x80 #x4C #x1A #xE2 #x0C #xCD #xC6 #x03 #xCD
 #xC7 #x03 #x13 #x7B #xFE #x34 #x20 #xF1 #x11 #x72 #x00 #x06 #x08 #x1A #x13
 #x22 #x23 #x05 #x20 #xF9 #xCD #xF0 #x03 #x3E #x01 #xE0 #x4F #x3E #x91 #xE0
 #x40 #x21 #xB2 #x98 #x06 #x4E #x0E #x44 #xCD #x91 #x02 #xAF #xE0 #x4F #x0E
 #x80 #x21 #x42 #x00 #x06 #x18 #xF2 #x0C #xBE #x20 #xFE #x23 #x05 #x20 #xF7
 #x21 #x34 #x01 #x06 #x19 #x78 #x86 #x2C #x05 #x20 #xFB #x86 #x20 #xFE #xCD
 #x1C #x03 #x18 #x02 #x00 #x00 #xCD #xD0 #x05 #xAF #xE0 #x70 #x3E #x11 #xE0
 #x50 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
 #x00 #x00 #x21 #x00 #x80 #xAF #x22 #xCB #x6C #x28 #xFB #xC9 #x2A #x12 #x13
 #x0D #x20 #xFA #xC9 #xE5 #x21 #x0F #xFF #xCB #x86 #xCB #x46 #x28 #xFC #xE1
 #xC9 #x11 #x00 #xFF #x21 #x03 #xD0 #x0E #x0F #x3E #x30 #x12 #x3E #x20 #x12
 #x1A #x2F #xA1 #xCB #x37 #x47 #x3E #x10 #x12 #x1A #x2F #xA1 #xB0 #x4F #x7E
 #xA9 #xE6 #xF0 #x47 #x2A #xA9 #xA1 #xB0 #x32 #x47 #x79 #x77 #x3E #x30 #x12
 #xC9 #x3E #x80 #xE0 #x68 #xE0 #x6A #x0E #x6B #x2A #xE2 #x05 #x20 #xFB #x4A
 #x09 #x43 #x0E #x69 #x2A #xE2 #x05 #x20 #xFB #xC9 #xC5 #xD5 #xE5 #x21 #x00
 #xD8 #x06 #x01 #x16 #x3F #x1E #x40 #xCD #x4A #x02 #xE1 #xD1 #xC1 #xC9 #x3E
 #x80 #xE0 #x26 #xE0 #x11 #x3E #xF3 #xE0 #x12 #xE0 #x25 #x3E #x77 #xE0 #x24
 #x21 #x30 #xFF #xAF #x0E #x10 #x22 #x2F #x0D #x20 #xFB #xC9 #xCD #x11 #x02
 #xCD #x62 #x02 #x79 #xFE #x38 #x20 #x14 #xE5 #xAF #xE0 #x4F #x21 #xA7 #x99
 #x3E #x38 #x22 #x3C #xFE #x3F #x20 #xFA #x3E #x01 #xE0 #x4F #xE1 #xC5 #xE5
 #x21 #x43 #x01 #xCB #x7E #xCC #x89 #x05 #xE1 #xC1 #xCD #x11 #x02 #x79 #xD6
 #x30 #xD2 #x06 #x03 #x79 #xFE #x01 #xCA #x06 #x03 #x7D #xFE #xD1 #x28 #x21
 #xC5 #x06 #x03 #x0E #x01 #x16 #x03 #x7E #xE6 #xF8 #xB1 #x22 #x15 #x20 #xF8
 #x0C #x79 #xFE #x06 #x20 #xF0 #x11 #x11 #x00 #x19 #x05 #x20 #xE7 #x11 #xA1
 #xFF #x19 #xC1 #x04 #x78 #x1E #x83 #xFE #x62 #x28 #x06 #x1E #xC1 #xFE #x64
 #x20 #x07 #x7B #xE0 #x13 #x3E #x87 #xE0 #x14 #xFA #x02 #xD0 #xFE #x00 #x28
 #x0A #x3D #xEA #x02 #xD0 #x79 #xFE #x01 #xCA #x91 #x02 #x0D #xC2 #x91 #x02
 #xC9 #x0E #x26 #xCD #x4A #x03 #xCD #x11 #x02 #xCD #x62 #x02 #x0D #x20 #xF4
 #xCD #x11 #x02 #x3E #x01 #xE0 #x4F #xCD #x3E #x03 #xCD #x41 #x03 #xAF #xE0
 #x4F #xCD #x3E #x03 #xC9 #x21 #x08 #x00 #x11 #x51 #xFF #x0E #x05 #xCD #x0A
 #x02 #xC9 #xC5 #xD5 #xE5 #x21 #x40 #xD8 #x0E #x20 #x7E #xE6 #x1F #xFE #x1F
 #x28 #x01 #x3C #x57 #x2A #x07 #x07 #x07 #xE6 #x07 #x47 #x3A #x07 #x07 #x07
 #xE6 #x18 #xB0 #xFE #x1F #x28 #x01 #x3C #x0F #x0F #x0F #x47 #xE6 #xE0 #xB2
 #x22 #x78 #xE6 #x03 #x5F #x7E #x0F #x0F #xE6 #x1F #xFE #x1F #x28 #x01 #x3C
 #x07 #x07 #xB3 #x22 #x0D #x20 #xC7 #xE1 #xD1 #xC1 #xC9 #x0E #x00 #x1A #xE6
 #xF0 #xCB #x49 #x28 #x02 #xCB #x37 #x47 #x23 #x7E #xB0 #x22 #x1A #xE6 #x0F
 #xCB #x49 #x20 #x02 #xCB #x37 #x47 #x23 #x7E #xB0 #x22 #x13 #xCB #x41 #x28
 #x0D #xD5 #x11 #xF8 #xFF #xCB #x49 #x28 #x03 #x11 #x08 #x00 #x19 #xD1 #x0C
 #x79 #xFE #x18 #x20 #xCC #xC9 #x47 #xD5 #x16 #x04 #x58 #xCB #x10 #x17 #xCB
 #x13 #x17 #x15 #x20 #xF6 #xD1 #x22 #x23 #x22 #x23 #xC9 #x3E #x19 #xEA #x10
 #x99 #x21 #x2F #x99 #x0E #x0C #x3D #x28 #x08 #x32 #x0D #x20 #xF9 #x2E #x0F
 #x18 #xF3 #xC9 #x3E #x01 #xE0 #x4F #xCD #x00 #x02 #x11 #x07 #x06 #x21 #x80
 #x80 #x0E #xC0 #x1A #x22 #x23 #x22 #x23 #x13 #x0D #x20 #xF7 #x11 #x04 #x01
 #xCD #x8F #x03 #x01 #xA8 #xFF #x09 #xCD #x8F #x03 #x01 #xF8 #xFF #x09 #x11
 #x72 #x00 #x0E #x08 #x23 #x1A #x22 #x13 #x0D #x20 #xF9 #x21 #xC2 #x98 #x06
 #x08 #x3E #x08 #x0E #x10 #x22 #x0D #x20 #xFC #x11 #x10 #x00 #x19 #x05 #x20
 #xF3 #xAF #xE0 #x4F #x21 #xC2 #x98 #x3E #x08 #x22 #x3C #xFE #x18 #x20 #x02
 #x2E #xE2 #xFE #x28 #x20 #x03 #x21 #x02 #x99 #xFE #x38 #x20 #xED #x21 #xD8
 #x08 #x11 #x40 #xD8 #x06 #x08 #x3E #xFF #x12 #x13 #x12 #x13 #x0E #x02 #xCD
 #x0A #x02 #x3E #x00 #x12 #x13 #x12 #x13 #x13 #x13 #x05 #x20 #xEA #xCD #x62
 #x02 #x21 #x4B #x01 #x7E #xFE #x33 #x20 #x0B #x2E #x44 #x1E #x30 #x2A #xBB
 #x20 #x49 #x1C #x18 #x04 #x2E #x4B #x1E #x01 #x2A #xBB #x20 #x3E #x2E #x34
 #x01 #x10 #x00 #x2A #x80 #x47 #x0D #x20 #xFA #xEA #x00 #xD0 #x21 #xC7 #x06
 #x0E #x00 #x2A #xB8 #x28 #x08 #x0C #x79 #xFE #x4F #x20 #xF6 #x18 #x1F #x79
 #xD6 #x41 #x38 #x1C #x21 #x16 #x07 #x16 #x00 #x5F #x19 #xFA #x37 #x01 #x57
 #x7E #xBA #x28 #x0D #x11 #x0E #x00 #x19 #x79 #x83 #x4F #xD6 #x5E #x38 #xED
 #x0E #x00 #x21 #x33 #x07 #x06 #x00 #x09 #x7E #xE6 #x1F #xEA #x08 #xD0 #x7E
 #xE6 #xE0 #x07 #x07 #x07 #xEA #x0B #xD0 #xCD #xE9 #x04 #xC9 #x11 #x91 #x07
 #x21 #x00 #xD9 #xFA #x0B #xD0 #x47 #x0E #x1E #xCB #x40 #x20 #x02 #x13 #x13
 #x1A #x22 #x20 #x02 #x1B #x1B #xCB #x48 #x20 #x02 #x13 #x13 #x1A #x22 #x13
 #x13 #x20 #x02 #x1B #x1B #xCB #x50 #x28 #x05 #x1B #x2B #x1A #x22 #x13 #x1A
 #x22 #x13 #x0D #x20 #xD7 #x21 #x00 #xD9 #x11 #x00 #xDA #xCD #x64 #x05 #xC9
 #x21 #x12 #x00 #xFA #x05 #xD0 #x07 #x07 #x06 #x00 #x4F #x09 #x11 #x40 #xD8
 #x06 #x08 #xE5 #x0E #x02 #xCD #x0A #x02 #x13 #x13 #x13 #x13 #x13 #x13 #xE1
 #x05 #x20 #xF0 #x11 #x42 #xD8 #x0E #x02 #xCD #x0A #x02 #x11 #x4A #xD8 #x0E
 #x02 #xCD #x0A #x02 #x2B #x2B #x11 #x44 #xD8 #x0E #x02 #xCD #x0A #x02 #xC9
 #x0E #x60 #x2A #xE5 #xC5 #x21 #xE8 #x07 #x06 #x00 #x4F #x09 #x0E #x08 #xCD
 #x0A #x02 #xC1 #xE1 #x0D #x20 #xEC #xC9 #xFA #x08 #xD0 #x11 #x18 #x00 #x3C
 #x3D #x28 #x03 #x19 #x20 #xFA #xC9 #xCD #x1D #x02 #x78 #xE6 #xFF #x28 #x0F
 #x21 #xE4 #x08 #x06 #x00 #x2A #xB9 #x28 #x08 #x04 #x78 #xFE #x0C #x20 #xF6
 #x18 #x2D #x78 #xEA #x05 #xD0 #x3E #x1E #xEA #x02 #xD0 #x11 #x0B #x00 #x19
 #x56 #x7A #xE6 #x1F #x5F #x21 #x08 #xD0 #x3A #x22 #x7B #x77 #x7A #xE6 #xE0
 #x07 #x07 #x07 #x5F #x21 #x0B #xD0 #x3A #x22 #x7B #x77 #xCD #xE9 #x04 #xCD
 #x28 #x05 #xC9 #xCD #x11 #x02 #xFA #x43 #x01 #xCB #x7F #x28 #x04 #xE0 #x4C
 #x18 #x28 #x3E #x04 #xE0 #x4C #x3E #x01 #xE0 #x6C #x21 #x00 #xDA #xCD #x7B
 #x05 #x06 #x10 #x16 #x00 #x1E #x08 #xCD #x4A #x02 #x21 #x7A #x00 #xFA #x00
 #xD0 #x47 #x0E #x02 #x2A #xB8 #xCC #xDA #x03 #x0D #x20 #xF8 #xC9 #x01 #x0F
 #x3F #x7E #xFF #xFF #xC0 #x00 #xC0 #xF0 #xF1 #x03 #x7C #xFC #xFE #xFE #x03
 #x07 #x07 #x0F #xE0 #xE0 #xF0 #xF0 #x1E #x3E #x7E #xFE #x0F #x0F #x1F #x1F
 #xFF #xFF #x00 #x00 #x01 #x01 #x01 #x03 #xFF #xFF #xE1 #xE0 #xC0 #xF0 #xF9
 #xFB #x1F #x7F #xF8 #xE0 #xF3 #xFD #x3E #x1E #xE0 #xF0 #xF9 #x7F #x3E #x7C
 #xF8 #xE0 #xF8 #xF0 #xF0 #xF8 #x00 #x00 #x7F #x7F #x07 #x0F #x9F #xBF #x9E
 #x1F #xFF #xFF #x0F #x1E #x3E #x3C #xF1 #xFB #x7F #x7F #xFE #xDE #xDF #x9F
 #x1F #x3F #x3E #x3C #xF8 #xF8 #x00 #x00 #x03 #x03 #x07 #x07 #xFF #xFF #xC1
 #xC0 #xF3 #xE7 #xF7 #xF3 #xC0 #xC0 #xC0 #xC0 #x1F #x1F #x1E #x3E #x3F #x1F
 #x3E #x3E #x80 #x00 #x00 #x00 #x7C #x1F #x07 #x00 #x0F #xFF #xFE #x00 #x7C
 #xF8 #xF0 #x00 #x1F #x0F #x0F #x00 #x7C #xF8 #xF8 #x00 #x3F #x3E #x1C #x00
 #x0F #x0F #x0F #x00 #x7C #xFF #xFF #x00 #x00 #xF8 #xF8 #x00 #x07 #x0F #x0F
 #x00 #x81 #xFF #xFF #x00 #xF3 #xE1 #x80 #x00 #xE0 #xFF #x7F #x00 #xFC #xF0
 #xC0 #x00 #x3E #x7C #x7C #x00 #x00 #x00 #x00 #x00 #x00 #x88 #x16 #x36 #xD1
 #xDB #xF2 #x3C #x8C #x92 #x3D #x5C #x58 #xC9 #x3E #x70 #x1D #x59 #x69 #x19
 #x35 #xA8 #x14 #xAA #x75 #x95 #x99 #x34 #x6F #x15 #xFF #x97 #x4B #x90 #x17
 #x10 #x39 #xF7 #xF6 #xA2 #x49 #x4E #x43 #x68 #xE0 #x8B #xF0 #xCE #x0C #x29
 #xE8 #xB7 #x86 #x9A #x52 #x01 #x9D #x71 #x9C #xBD #x5D #x6D #x67 #x3F #x6B
 #xB3 #x46 #x28 #xA5 #xC6 #xD3 #x27 #x61 #x18 #x66 #x6A #xBF #x0D #xF4 #x42
 #x45 #x46 #x41 #x41 #x52 #x42 #x45 #x4B #x45 #x4B #x20 #x52 #x2D #x55 #x52
 #x41 #x52 #x20 #x49 #x4E #x41 #x49 #x4C #x49 #x43 #x45 #x20 #x52 #x7C #x08
 #x12 #xA3 #xA2 #x07 #x87 #x4B #x20 #x12 #x65 #xA8 #x16 #xA9 #x86 #xB1 #x68
 #xA0 #x87 #x66 #x12 #xA1 #x30 #x3C #x12 #x85 #x12 #x64 #x1B #x07 #x06 #x6F
 #x6E #x6E #xAE #xAF #x6F #xB2 #xAF #xB2 #xA8 #xAB #x6F #xAF #x86 #xAE #xA2
 #xA2 #x12 #xAF #x13 #x12 #xA1 #x6E #xAF #xAF #xAD #x06 #x4C #x6E #xAF #xAF
 #x12 #x7C #xAC #xA8 #x6A #x6E #x13 #xA0 #x2D #xA8 #x2B #xAC #x64 #xAC #x6D
 #x87 #xBC #x60 #xB4 #x13 #x72 #x7C #xB5 #xAE #xAE #x7C #x7C #x65 #xA2 #x6C
 #x64 #x85 #x80 #xB0 #x40 #x88 #x20 #x68 #xDE #x00 #x70 #xDE #x20 #x78 #x20
 #x20 #x38 #x20 #xB0 #x90 #x20 #xB0 #xA0 #xE0 #xB0 #xC0 #x98 #xB6 #x48 #x80
 #xE0 #x50 #x1E #x1E #x58 #x20 #xB8 #xE0 #x88 #xB0 #x10 #x20 #x00 #x10 #x20
 #xE0 #x18 #xE0 #x18 #x00 #x18 #xE0 #x20 #xA8 #xE0 #x20 #x18 #xE0 #x00 #x20
 #x18 #xD8 #xC8 #x18 #xE0 #x00 #xE0 #x40 #x28 #x28 #x28 #x18 #xE0 #x60 #x20
 #x18 #xE0 #x00 #x00 #x08 #xE0 #x18 #x30 #xD0 #xD0 #xD0 #x20 #xE0 #xE8 #xFF
 #x7F #xBF #x32 #xD0 #x00 #x00 #x00 #x9F #x63 #x79 #x42 #xB0 #x15 #xCB #x04
 #xFF #x7F #x31 #x6E #x4A #x45 #x00 #x00 #xFF #x7F #xEF #x1B #x00 #x02 #x00
 #x00 #xFF #x7F #x1F #x42 #xF2 #x1C #x00 #x00 #xFF #x7F #x94 #x52 #x4A #x29
 #x00 #x00 #xFF #x7F #xFF #x03 #x2F #x01 #x00 #x00 #xFF #x7F #xEF #x03 #xD6
 #x01 #x00 #x00 #xFF #x7F #xB5 #x42 #xC8 #x3D #x00 #x00 #x74 #x7E #xFF #x03
 #x80 #x01 #x00 #x00 #xFF #x67 #xAC #x77 #x13 #x1A #x6B #x2D #xD6 #x7E #xFF
 #x4B #x75 #x21 #x00 #x00 #xFF #x53 #x5F #x4A #x52 #x7E #x00 #x00 #xFF #x4F
 #xD2 #x7E #x4C #x3A #xE0 #x1C #xED #x03 #xFF #x7F #x5F #x25 #x00 #x00 #x6A
 #x03 #x1F #x02 #xFF #x03 #xFF #x7F #xFF #x7F #xDF #x01 #x12 #x01 #x00 #x00
 #x1F #x23 #x5F #x03 #xF2 #x00 #x09 #x00 #xFF #x7F #xEA #x03 #x1F #x01 #x00
 #x00 #x9F #x29 #x1A #x00 #x0C #x00 #x00 #x00 #xFF #x7F #x7F #x02 #x1F #x00
 #x00 #x00 #xFF #x7F #xE0 #x03 #x06 #x02 #x20 #x01 #xFF #x7F #xEB #x7E #x1F
 #x00 #x00 #x7C #xFF #x7F #xFF #x3F #x00 #x7E #x1F #x00 #xFF #x7F #xFF #x03
 #x1F #x00 #x00 #x00 #xFF #x03 #x1F #x00 #x0C #x00 #x00 #x00 #xFF #x7F #x3F
 #x03 #x93 #x01 #x00 #x00 #x00 #x00 #x00 #x42 #x7F #x03 #xFF #x7F #xFF #x7F
 #x8C #x7E #x00 #x7C #x00 #x00 #xFF #x7F #xEF #x1B #x80 #x61 #x00 #x00 #xFF
 #x7F #x00 #x7C #xE0 #x03 #x1F #x7C #x1F #x00 #xFF #x03 #x40 #x41 #x42 #x20
 #x21 #x22 #x80 #x81 #x82 #x10 #x11 #x12 #x12 #xB0 #x79 #xB8 #xAD #x16 #x17
 #x07 #xBA #x05 #x7C #x13 #x00 #x00 #x00 #x00)
  "cgb bios as a list of bytes")

(defun make-bios ()
  "creates the bios array of memory for the gb based on *CGB-BIOS* parameter."
  (make-array #x900 :initial-contents *cgb-bios*))
(defun make-dmg-bios ()
  "creates the bios array of memory for the gb based on *DMG-BIOS* parameter."
  (make-array #x100 :initial-contents *dmg-bios*))
(defun make-cgb-bios ()
  "creates the bios array of memory for the gb based on *CGB-BIOS* parameter."
  (make-array #x900 :initial-contents *cgb-bios*))

(defun read-bin-data-from-file (filename)
  "reads rom data from a file referenced by FILENAME as a list of bytes"
  (with-open-file (bin filename :element-type '(unsigned-byte 8) :if-does-not-exist nil)
    (if bin (loop for b = (read-byte bin nil) while b collect b))))

(defun make-signed-from-unsigned (unsign-byte)
  "convert a unsigned byte to a signed byte"
  (if (< unsign-byte 128)
      unsign-byte
      (logior unsign-byte (- (mask-field (byte 1 7) #xff)))))

;; I/O

(defun get-p14-byte (input)
  "calculates a byte that represents INPUT when p14 is active based on the state of buttons:
  down, up, left, and right."
  (+ (if (gbinput-down input) #x0 #x8)
     (if (gbinput-up input) #x0 #x4)
     (if (gbinput-left input) #x0 #x2)
     (if (gbinput-right input) #x0 #x1)))

(defun get-p15-byte (input)
  "calculates a byte that represents INPUT when p14 is active based on the state of buttons:
  start, select, b, and a."
  (+ (if (gbinput-start input) #x0 #x8)
     (if (gbinput-select input) #x0 #x4)
     (if (gbinput-b input) #x0 #x2)
     (if (gbinput-a input) #x0 #x1)))

(defun input-read-memory (input)
  "read the INPUT data based on whether p14 or p15 is active for reading"
  (let ((input-val (logand (gbinput-reg input) #x30)))
    (if (= input-val #x30) ; both p14 and p15 inactive
      (logior input-val #x0f)
    (if (= input-val #x20)
      (+ input-val (get-p14-byte input))
    (if (= input-val #x10)
      (+ input-val (get-p15-byte input))
      #x0f)))))

(defun handle-keyup (input gb keysym)
  "handles keyup events from SDL2 by updating the state of each button in INPUT and special keys
  like quit and reset. Each key is identified by KEYSYM."
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
    (sdl2:push-event :quit))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-p)
    (setf (gb-paused? gb) (not (gb-paused? gb))))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-r)
    (gb-reset gb))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-1)
    (gbspu-toggle-audio-channel (gb-spu gb) 1))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-2)
    (gbspu-toggle-audio-channel (gb-spu gb) 2))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-3)
    (gbspu-toggle-audio-channel (gb-spu gb) 3))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-4)
    (gbspu-toggle-audio-channel (gb-spu gb) 4))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-b)
    (render-full-background (gb-ppu gb)))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-t)
    (render-full-tiledata (gb-ppu gb)))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-w)
    (setf (gbinput-up input) nil))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-a)
    (setf (gbinput-left input) nil))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-s)
    (setf (gbinput-down input) nil))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-d)
    (setf (gbinput-right input) nil))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-space)
    (setf (gbinput-start input) nil))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-backspace)
    (setf (gbinput-select input) nil))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-k)
    (setf (gbinput-b input) nil))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-j)
    (setf (gbinput-a input) nil)))

(defun handle-keydown (input gb keysym)
  "handles keydown events from SDL2 by updating the state of each button in INPUT and special keys
  like quit and reset. Each key is identified by KEYSYM."
  (setf (gb-stopped? gb) nil)
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-w)
    (set-interrupt-flag gb 4)
    (setf (gbinput-up input) t))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-a)
    (set-interrupt-flag gb 4)
    (setf (gbinput-left input) t))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-s)
    (set-interrupt-flag gb 4)
    (setf (gbinput-down input) t))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-d)
    (set-interrupt-flag gb 4)
    (setf (gbinput-right input) t))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-space)
    (set-interrupt-flag gb 4)
    (setf (gbinput-start input) t))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-backspace)
    (set-interrupt-flag gb 4)
    (setf (gbinput-select input) t))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-k)
    (set-interrupt-flag gb 4)
    (setf (gbinput-b input) t))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-j)
    (set-interrupt-flag gb 4)
    (setf (gbinput-a input) t)))

(defun handle-controlleraxismotion (input axis value)
  "handles controlleraxismotion events from SDL2 by updating the state of each button in INPUT and special keys
  like quit and reset. Each key is identified by KEYSYM."
  (when (= axis sdl2-ffi:+sdl-controller-axis-leftx+)
    (if (< value -10000)
      (setf (gbinput-left input) t)
      (setf (gbinput-left input) nil))
    (if (> value 10000)
      (setf (gbinput-right input) t)
      (setf (gbinput-right input) nil)))
  (when (= axis sdl2-ffi:+sdl-controller-axis-lefty+)
    (if (< value -10000)
      (setf (gbinput-up input) t)
      (setf (gbinput-up input) nil))
    (if (> value 10000)
      (setf (gbinput-down input) t)
      (setf (gbinput-down input) nil))))

(defun handle-controllerbuttondown (input gb button)
  "handles controllerbuttondown events from SDL2 by updating the state of each button in INPUT and special keys
  like quit and reset. Each key is identified by KEYSYM."
  (when (= button sdl2-ffi:+sdl-controller-button-dpad-up+)
    (set-interrupt-flag gb 4)
    (setf (gbinput-up input) t))
  (when (= button sdl2-ffi:+sdl-controller-button-dpad-left+)
    (set-interrupt-flag gb 4)
    (setf (gbinput-left input) t))
  (when (= button sdl2-ffi:+sdl-controller-button-dpad-down+)
    (set-interrupt-flag gb 4)
    (setf (gbinput-down input) t))
  (when (= button sdl2-ffi:+sdl-controller-button-dpad-right+)
    (set-interrupt-flag gb 4)
    (setf (gbinput-right input) t))
  (when (= button sdl2-ffi:+sdl-controller-button-start+)
    (set-interrupt-flag gb 4)
    (setf (gbinput-start input) t))
  (when (= button sdl2-ffi:+sdl-controller-button-back+)
    (set-interrupt-flag gb 4)
    (setf (gbinput-select input) t))
  (when (= button sdl2-ffi:+sdl-controller-button-b+)
    (set-interrupt-flag gb 4)
    (setf (gbinput-a input) t))
  (when (= button sdl2-ffi:+sdl-controller-button-a+)
    (set-interrupt-flag gb 4)
    (setf (gbinput-b input) t)))

(defun handle-controllerbuttonup (input button)
  "handles controllerbuttonup events from SDL2 by updating the state of each button in INPUT and special keys
  like quit and reset. Each key is identified by KEYSYM."
  (when (= button sdl2-ffi:+sdl-controller-button-dpad-up+)
    (setf (gbinput-up input) nil))
  (when (= button sdl2-ffi:+sdl-controller-button-dpad-left+)
    (setf (gbinput-left input) nil))
  (when (= button sdl2-ffi:+sdl-controller-button-dpad-down+)
    (setf (gbinput-down input) nil))
  (when (= button sdl2-ffi:+sdl-controller-button-dpad-right+)
    (setf (gbinput-right input) nil))
  (when (= button sdl2-ffi:+sdl-controller-button-start+)
    (setf (gbinput-start input) nil))
  (when (= button sdl2-ffi:+sdl-controller-button-back+)
    (setf (gbinput-select input) nil))
  (when (= button sdl2-ffi:+sdl-controller-button-b+)
    (setf (gbinput-a input) nil))
  (when (= button sdl2-ffi:+sdl-controller-button-a+)
    (setf (gbinput-b input) nil)))

;; CPU

;; SPU

(defparameter *width* 256)
(defparameter *height* 256)
(defparameter *scale* 3)

(defparameter *debug* nil
  "when T debug info is collected/printed")

;(defconstant +cycles-per-internal-time-units+ (floor +cpu-speed+ internal-time-units-per-second))
;(defconstant +cycles-per-frame+ (floor +cpu-speed+ 60))
(defconstant +time-units-per-frame+ (truncate (* (/ 60) internal-time-units-per-second)))

(defun cycles-per-internal-time-units (cpu-speed) (floor cpu-speed internal-time-units-per-second))
(defun cycles-per-frame (cpu-speed) (floor cpu-speed 60))

(defparameter *ppu-time* (make-profiler))
(defparameter *spu-time* (make-profiler))

(defun emu-main (gb)
  "Main loop for CL-Boy Game Boy emulator. handles setting up the window, controllers, and audio
  device. Loops through instructions and delegates to each component based on the clock cycles of
  each instruction."
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "CL-Boy" :flags '(:shown :opengl) :w (* *width* *scale*) :h (* *height* *scale*))
      (sdl2:with-renderer (renderer win)
        (let ((texture
                (sdl2:create-texture renderer :rgb24 :streaming +screen-pixel-width+ +screen-pixel-height+))
              (rect (sdl2:make-rect
                      (* (/ (- *width* +screen-pixel-width+) 2) *scale*)
                      (* (/ (- *height* +screen-pixel-height+) 2) *scale*)
                      (* +screen-pixel-width+ *scale*) (* +screen-pixel-height+ *scale*)))
              (audio-device (sdl2::open-audio-device +sample-rate+ :f32 2 (floor +audio-buffer-size+ 2)))
              (last-frame-time (get-internal-real-time)))
          (loop for c from 0 to (- (sdl2:joystick-count) 1) do
                (when (sdl2:game-controller-p c)
                  (format t "Found controller: ~a~%"
                          (sdl2:game-controller-name-for-index c))
                  (sdl2:game-controller-open c)))
          (sdl2::unpause-audio-device audio-device)
          (setf (gbppu-renderer (gb-ppu gb)) renderer
                (gbppu-render-rect (gb-ppu gb)) rect
                (gbppu-texture (gb-ppu gb)) texture
                (gbspu-device (gb-spu gb)) audio-device)
          (sdl2:with-event-loop (:method :poll)
            (:keydown (:keysym keysym)
                      (handle-keydown (gb-input gb) gb keysym))
            (:keyup (:keysym keysym)
                    (handle-keyup (gb-input gb) gb keysym))
            (:controlleraxismotion
                           (:axis axis :value value)
                           (handle-controlleraxismotion (gb-input gb) axis value))
            (:controllerbuttondown (:button button)
                                   (handle-controllerbuttondown (gb-input gb) gb button))
            (:controllerbuttonup (:button button)
                                 (handle-controllerbuttonup (gb-input gb) button))
            (:idle ()
              (let ((cpu (gb-cpu gb))
                    (ppu (gb-ppu gb))
                    (spu (gb-spu gb)))
                (when (not (gb-paused? gb))
                  (when (not (gb-stopped? gb))
                    (loop while (step-cpu cpu gb)
                          for cyc = 0 then (+ cyc (gbcpu-clock cpu))
                          while (< cyc (cycles-per-frame (gbcpu-cpu-speed cpu))) do
                          (step-ppu ppu gb (/ (gbcpu-clock cpu) (ppu-cycles-per-dot (gbcpu-cpu-speed cpu))))
                          (step-spu spu (gbcpu-clock cpu) (cycles-per-sample (gbcpu-cpu-speed cpu)) (cycles-frame-seq-step (gbcpu-cpu-speed cpu)))
                          (handle-timers cpu gb)
                          (handle-interrupts cpu gb)))
                (spu-queue-audio spu))
                (step-rtc (gbcart-timer (gb-cart gb)))
                (let ((now (get-internal-real-time)))
                  (when (< (- now last-frame-time) +time-units-per-frame+)
                    (when *debug*
                      (format t "timeunits since last frame ~A, Sleeping for: ~A~%"
                              (- now last-frame-time)
                              (coerce (/ (- +time-units-per-frame+
                                            (- now last-frame-time))
                                         internal-time-units-per-second) 'float)))
                    (sleep
                      (/ (- +time-units-per-frame+
                            (- now last-frame-time))
                         internal-time-units-per-second)))
                  (setf last-frame-time now))))
            (:quit ()
                   (cart-write-ram-to-file (gb-cart gb) (cart-ram-filename (gb-cart gb)))
                   t)))))))


(defun gb-reset (gb)
  "resets GB to start at the beginning of the bios with everything cleared"
  (gbppu-reset (gb-ppu gb))
  (gbspu-reset (gb-spu gb))
  (setf (gb-cpu gb) (make-gbcpu)
        (gb-is-bios? gb) t
        (gb-int-ram-bank gb) 1
        (gb-stopped? gb) nil)
  (when (cgb-p gb)
    (setf (cgb-is-speed-armed? gb) nil
          (cgb-is-double-speed? gb) nil))
  nil)

(defun load-cart (gb cart)
  "load CART into GB"
  (gb-reset gb)
  (setf (gb-cart gb) cart)
  nil)

(defun unload-cart (gb)
  "unload CART into GB"
  (gb-reset gb)
  (setf (gb-cart gb) nil)
  nil)

(defun dump-mem-region (gb start end)
  "reads the region of memory from START to END inclusive as a list of bytes."
  (loop for a from start to end
        collect (read-memory-at-addr gb a)))

(defun dump-oam (gb)
  "reads the OAM as a list of bytes."
  (dump-mem-region gb #xfe00 #xfea0))

(defun run (gb)
  "runs the main loop with the global GB"
  (emu-main gb))

(defun start-gb (rom)
  (let ((gb (make-gb)))
    (load-cart gb (make-gbcart-from-rom rom))
    (run gb)))
(defun start-cgb (rom)
  (let ((cgb (make-cgb)))
    (load-cart cgb (make-gbcart-from-rom rom))
    (run cgb)))

