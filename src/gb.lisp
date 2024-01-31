


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
          (#x60 (ppu-write-memory-at-addr (gb-ppu gb) addr val))
          (#x70
           (case (logand addr #x000f)
             (#x0 (if (cgb-p gb) (setf (gb-int-ram-bank gb) (if (> (logand val #x7) 0) (logand val #x7) 1))))
             ((#x2 #x3 #x4 #x5) (ppu-write-memory-at-addr (gb-ppu gb) addr val))))
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
            (#x60 (ppu-read-memory-at-addr (gb-ppu gb) addr))
            (#x70
             (if (= (logand addr #xff) #x70) (if (cgb-p gb) (gb-int-ram-bank gb))))
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
  (if (= (logand (logand (read-memory-at-addr gb #xffff) #x01) (logand (read-memory-at-addr gb #xff0f) #x01)) #x01)
    (do-interrupt cpu gb 0)
  ; LCDC Status interupt
  (if (= (logand (logand (read-memory-at-addr gb #xffff) #x02) (logand (read-memory-at-addr gb #xff0f) #x02)) #x02)
    (do-interrupt cpu gb 1)
  ; Timer interupt
  (if (= (logand (logand (read-memory-at-addr gb #xffff) #x04) (logand (read-memory-at-addr gb #xff0f) #x04)) #x04)
    (do-interrupt cpu gb 2)
  ; Serial Transfer interupt
  (if (= (logand (logand (read-memory-at-addr gb #xffff) #x08) (logand (read-memory-at-addr gb #xff0f) #x08)) #x08)
    (do-interrupt cpu gb 3)
  ; Hi-Lo of P10-P13 interupt
  (if (= (logand (logand (read-memory-at-addr gb #xffff) #x10) (logand (read-memory-at-addr gb #xff0f) #x10)) #x10)
    (do-interrupt cpu gb 4)))))))

(defun set-interrupt-flag (gb bit-pos)
  (write-memory-at-addr gb #xff0f (logior (read-memory-at-addr gb #xff0f) (ash #x01 bit-pos))))

;; TIMER
(defun handle-timers (cpu gb)
  "updates timer registers based on the number of cycles from the last instruction."
  (if (> (gbcpu-div-clock cpu) #xff)
    (progn
      (setf (gbcpu-div-clock cpu) (- (gbcpu-div-clock cpu) #x100))
      (write-memory-at-addr gb #xff04 (logand (+ (read-memory-at-addr gb #xff04) #x01) #xff))))
  (if (= (logand (read-memory-at-addr gb #xff07) #x04) #x04)
    (incr-timer-by-cycles cpu gb (get-cycles-per-timer-tick (gbcpu-cpu-speed cpu) (get-timer-frequency (read-memory-at-addr gb #xff07))))
    (setf (gbcpu-clock cpu) 0)))

(defun incr-timer-by-cycles (cpu gb cycles-per-tick)
  "incements the timer by the number of cycles of the last instuction and triggers interrupt when
  CYCLES-PER-TICK cycles have elapsed since last interrupt"
  (let* ((cycles (+ (gbcpu-clock cpu) (gbcpu-clock-remainder cpu)))
         (ticks (floor cycles cycles-per-tick))
         (remainder (mod cycles cycles-per-tick))
         (cur-ticks (read-memory-at-addr gb #xff05))
         (new-ticks (+ cur-ticks ticks)))
    (setf (gbcpu-clock-remainder cpu) remainder
          (gbcpu-clock cpu) 0)
    (if (> new-ticks #xff)
      (progn (set-interrupt-flag gb 2)
             (write-memory-at-addr gb #xff05 (+ (read-memory-at-addr gb #xff06) (logand new-ticks #xff))))
      (write-memory-at-addr gb #xff05 (logand new-ticks #xff)))))

(defun get-cycles-per-timer-tick (cpu-speed freq)
  "number of cycles per timer tick based on the FREQ of the timer."
  (/ cpu-speed freq))

(defun get-timer-frequency (tac)
  (let ((tac-two-lsb (logand tac #x3)))
    (if (= tac-two-lsb #x01) 262144
      (if (= tac-two-lsb #x02) 65536
        (if (= tac-two-lsb #x03) 16384
          4096)))))

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
  '(#x31 #xfe #xff #xcd #x26 #x06 #x26 #xfe #x0e #xa0 #x22 #x0d
    #x20 #xfc #x0e #x10 #x21 #x30 #xff #x22 #x2f #x0d #x20 #xfb
    #xe0 #xc1 #xe0 #x80 #x3e #x80 #xe0 #x26 #xe0 #x11 #x3e #xf3
    #xe0 #x12 #xe0 #x25 #x3e #x77 #xe0 #x24 #x3e #xfc #xe0 #x47
    #x11 #x04 #x01 #x21 #x10 #x80 #x1a #x47 #xcd #xf1 #x05 #x13
    #x7b #xfe #x34 #x20 #xf5 #xcd #xae #x06 #x3e #x01 #xe0 #x4f
    #xcd #x26 #x06 #xcd #x53 #x06 #x06 #x03 #x21 #xc2 #x98 #x16
    #x03 #x3e #x08 #x0e #x10 #xcd #x86 #x00 #xf5 #xd6 #x20 #xd6
    #x03 #x30 #x06 #xc6 #x20 #xcd #x86 #x00 #x0d #xf1 #x82 #x0d
    #x20 #xeb #xd6 #x2c #xd5 #x11 #x10 #x00 #x19 #xd1 #x05 #x20
    #xde #x15 #x28 #x17 #x15 #x3e #x38 #x2e #xa7 #x01 #x07 #x01
    #x18 #xd3 #xf5 #x3e #x01 #xe0 #x4f #x36 #x08 #xaf #xe0 #x4f
    #xf1 #x22 #xc9 #x11 #xe1 #x05 #x0e #x08 #x21 #x81 #xff #xaf
    #x2f #x22 #x22 #x1a #x1c #xf6 #x20 #x47 #x1a #x1d #xf6 #x84
    #x1f #xcb #x18 #x70 #x2c #x22 #xaf #x22 #x22 #x1a #x1c #x22
    #x1a #x1c #x22 #xaf #x0d #x20 #xe1 #xcd #x18 #x08 #x3e #x91
    #xe0 #x40 #xcd #xbb #x06 #x3e #x30 #xe0 #xc2 #x06 #x04 #xcd
    #x15 #x06 #x3e #x83 #xcd #x1f #x06 #x06 #x05 #xcd #x15 #x06
    #x3e #xc1 #xcd #x1f #x06 #xcd #x3f #x08 #xcd #x09 #x06 #x21
    #xc2 #xff #x35 #x20 #xf4 #xcd #xe4 #x06 #x18 #x10 #xd0 #x00
    #x98 #xa0 #x12 #xd0 #x00 #x80 #x00 #x40 #x00 #x00 #x00 #x00
    #x00 #x00 #xe0 #x50 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x88 #x16 #x36
    #xd1 #xdb #xf2 #x3c #x8c #x92 #x3d #x5c #x58 #xc9 #x3e #x70
    #x1d #x59 #x69 #x19 #x35 #xa8 #x14 #xaa #x75 #x95 #x99 #x34
    #x6f #x15 #xff #x97 #x4b #x90 #x17 #x10 #x39 #xf7 #xf6 #xa2
    #x49 #x4e #x43 #x68 #xe0 #x8b #xf0 #xce #x0c #x29 #xe8 #xb7
    #x86 #x9a #x52 #x01 #x9d #x71 #x9c #xbd #x5d #x6d #x67 #x3f
    #x6b #xb3 #x46 #x28 #xa5 #xc6 #xd3 #x27 #x61 #x18 #x66 #x6a
    #xbf #x0d #xf4 #xb3 #x46 #x28 #xa5 #xc6 #xd3 #x27 #x61 #x18
    #x66 #x6a #xbf #x0d #xf4 #xb3 #x00 #x04 #x05 #x23 #x22 #x03
    #x1f #x0f #x0a #x05 #x13 #x24 #x87 #x25 #x1e #x2c #x15 #x20
    #x1f #x14 #x05 #x21 #x0d #x0e #x05 #x1d #x05 #x12 #x09 #x03
    #x02 #x1a #x19 #x19 #x29 #x2a #x1a #x2d #x2a #x2d #x24 #x26
    #x9a #x2a #x1e #x29 #x22 #x22 #x05 #x2a #x06 #x05 #x21 #x19
    #x2a #x2a #x28 #x02 #x10 #x19 #x2a #x2a #x05 #x00 #x27 #x24
    #x16 #x19 #x06 #x20 #x0c #x24 #x0b #x27 #x12 #x27 #x18 #x1f
    #x32 #x11 #x2e #x06 #x1b #x00 #x2f #x29 #x29 #x00 #x00 #x13
    #x22 #x17 #x12 #x1d #x42 #x45 #x46 #x41 #x41 #x52 #x42 #x45
    #x4b #x45 #x4b #x20 #x52 #x2d #x55 #x52 #x41 #x52 #x20 #x49
    #x4e #x41 #x49 #x4c #x49 #x43 #x45 #x20 #x52 #x20 #x20 #xe8
    #x90 #x90 #x90 #xa0 #xa0 #xa0 #xc0 #xc0 #xc0 #x48 #x48 #x48
    #x00 #x00 #x00 #xd8 #xd8 #xd8 #x28 #x28 #x28 #x60 #x60 #x60
    #xd0 #xd0 #xd0 #x80 #x40 #x40 #x20 #xe0 #xe0 #x20 #x10 #x10
    #x18 #x20 #x20 #x20 #xe8 #xe8 #xe0 #x20 #xe0 #x10 #x88 #x10
    #x80 #x80 #x40 #x20 #x20 #x38 #x20 #x20 #x90 #x20 #x20 #xa0
    #x98 #x98 #x48 #x1e #x1e #x58 #x88 #x88 #x10 #x20 #x20 #x10
    #x20 #x20 #x18 #xe0 #xe0 #x00 #x18 #x18 #x00 #x00 #x00 #x08
    #x90 #xb0 #x90 #xa0 #xb0 #xa0 #xc0 #xb0 #xc0 #x80 #xb0 #x40
    #x88 #x20 #x68 #xde #x00 #x70 #xde #x20 #x78 #x98 #xb6 #x48
    #x80 #xe0 #x50 #x20 #xb8 #xe0 #x88 #xb0 #x10 #x20 #x00 #x10
    #x20 #xe0 #x18 #xe0 #x18 #x00 #x18 #xe0 #x20 #xa8 #xe0 #x20
    #x18 #xe0 #x00 #xc8 #x18 #xe0 #x00 #xe0 #x40 #x20 #x18 #xe0
    #xe0 #x18 #x30 #x20 #xe0 #xe8 #xf0 #xf0 #xf0 #xf8 #xf8 #xf8
    #xe0 #x20 #x08 #x00 #x00 #x10 #xff #x7f #xbf #x32 #xd0 #x00
    #x00 #x00 #x9f #x63 #x79 #x42 #xb0 #x15 #xcb #x04 #xff #x7f
    #x31 #x6e #x4a #x45 #x00 #x00 #xff #x7f #xef #x1b #x00 #x02
    #x00 #x00 #xff #x7f #x1f #x42 #xf2 #x1c #x00 #x00 #xff #x7f
    #x94 #x52 #x4a #x29 #x00 #x00 #xff #x7f #xff #x03 #x2f #x01
    #x00 #x00 #xff #x7f #xef #x03 #xd6 #x01 #x00 #x00 #xff #x7f
    #xb5 #x42 #xc8 #x3d #x00 #x00 #x74 #x7e #xff #x03 #x80 #x01
    #x00 #x00 #xff #x67 #xac #x77 #x13 #x1a #x6b #x2d #xd6 #x7e
    #xff #x4b #x75 #x21 #x00 #x00 #xff #x53 #x5f #x4a #x52 #x7e
    #x00 #x00 #xff #x4f #xd2 #x7e #x4c #x3a #xe0 #x1c #xed #x03
    #xff #x7f #x5f #x25 #x00 #x00 #x6a #x03 #x1f #x02 #xff #x03
    #xff #x7f #xff #x7f #xdf #x01 #x12 #x01 #x00 #x00 #x1f #x23
    #x5f #x03 #xf2 #x00 #x09 #x00 #xff #x7f #xea #x03 #x1f #x01
    #x00 #x00 #x9f #x29 #x1a #x00 #x0c #x00 #x00 #x00 #xff #x7f
    #x7f #x02 #x1f #x00 #x00 #x00 #xff #x7f #xe0 #x03 #x06 #x02
    #x20 #x01 #xff #x7f #xeb #x7e #x1f #x00 #x00 #x7c #xff #x7f
    #xff #x3f #x00 #x7e #x1f #x00 #xff #x7f #xff #x03 #x1f #x00
    #x00 #x00 #xff #x03 #x1f #x00 #x0c #x00 #x00 #x00 #xff #x7f
    #x3f #x03 #x93 #x01 #x00 #x00 #x00 #x00 #x00 #x42 #x7f #x03
    #xff #x7f #xff #x7f #x8c #x7e #x00 #x7c #x00 #x00 #xff #x7f
    #xef #x1b #x80 #x61 #x00 #x00 #xff #x7f #xea #x7f #x5f #x7d
    #x00 #x00 #x78 #x47 #x90 #x32 #x87 #x1d #x61 #x08 #x03 #x90
    #x0f #x18 #x00 #x78 #x81 #x09 #x12 #x15 #x54 #x93 #x99 #x9c
    #x9f #xa2 #x3c #x42 #xb9 #xa5 #xb9 #xa5 #x42 #x3c #x33 #x00
    #x03 #x1c #x0f #x1f #x7b #x1c #x3e #x3c #xfd #xb6 #xf7 #xf7
    #x71 #x01 #x7f #xfc #x30 #xc0 #x7f #x78 #xff #xfd #xdd #xcf
    #x00 #x23 #x60 #xfc #x6f #xd4 #xbc #x35 #x08 #xff #xc8 #x80
    #xf0 #x53 #xf8 #x6a #xdf #x7c #x3d #x81 #x7d #x79 #x3c #xf3
    #x43 #xe7 #x0f #xc7 #x00 #xfc #x01 #xd2 #xb4 #xad #x2b #x41
    #x0e #x34 #x13 #x1c #x41 #x38 #x31 #xff #xef #xf3 #xe0 #x5f
    #xd7 #xd7 #xff #x3f #xe0 #xf6 #xaf #xda #x9f #xfd #xa9 #xe8
    #xfc #xda #xbc #x3e #x7d #xa9 #xe8 #x00 #xff #xcf #x1f #xff
    #xfd #x28 #x1d #x80 #x1c #x3d #x3c #xff #xf4 #x2a #x38 #xa9
    #x3f #xff #x40 #x70 #x00 #xff #xc5 #xc0 #xbf #xf6 #xaa #xcf
    #xe1 #xd2 #x00 #xf3 #xe3 #xf7 #xf3 #xb4 #x27 #x77 #x5f #xf5
    #xfc #x38 #x48 #x00 #xff #xc7 #x3f #xb4 #xad #x28 #xef #xaf
    #xc4 #xcf #x20 #xce #x8e #x9f #x90 #x1e #xff #xff #x42 #x1c
    #xa9 #x33 #x00 #xff #x09 #x9f #x8f #xe2 #x1f #x5f #xfd #x48
    #x3e #x3f #xa7 #xbf #xcf #x3c #x42 #x38 #xa8 #x7f #xaf #xfc
    #x00 #xff #xcf #xff #xff #x3f #x00 #xff #xca #xfe #xff #xd4
    #x00 #xff #xff #x2b #xfe #xfd #x4f #x00 #xfc #xfc #x53 #xff
    #xfc #x0f #xf7 #xfd #x30 #xff #x1e #x3d #xfc #x45 #xff #x87
    #x1f #xf7 #xbf #x03 #xff #x3f #xfe #x5d #x54 #x00 #xff #xfc
    #x01 #xc0 #x83 #x03 #x87 #xd3 #x4f #x54 #x8f #x3c #xd2 #xaa
    #x2a #xfc #x06 #xbe #x3e #xdf #xdf #xdd #xcf #x00 #xc8 #x0e
    #xff #x7b #xfc #xf3 #x15 #xc0 #x7f #xff #xff #xff #xff #xf1
    #x03 #xc2 #xff #x87 #xff #x50 #xf8 #x00 #xff #x03 #xc7 #x83
    #xe3 #x61 #xf1 #xb6 #x53 #x7c #xdf #xda #xaf #xf4 #xa5 #x4a
    #xd7 #xd7 #x55 #xd7 #xff #xf1 #xe0 #x6f #xdb #xf0 #xf9 #xf1
    #x00 #xfb #xf9 #x7f #x7b #xb7 #xff #x3f #x1e #xd0 #x1c #xaa
    #xa4 #xff #xcf #x00 #xfc #x3f #x13 #x1e #x31 #x7c #xf8 #xe5
    #xf5 #xf5 #xd7 #xd7 #x01 #xff #x7f #x4f #x77 #xc7 #x22 #x9f
    #x03 #x7d #x01 #x1d #x24 #x38 #x6d #x00 #x55 #xcd #xf4 #x05
    #x3e #x04 #x0e #x00 #xcb #x20 #xf5 #xcb #x11 #xf1 #xcb #x11
    #x3d #x20 #xf5 #x79 #x22 #x23 #x22 #x23 #xc9 #xe5 #x21 #x0f
    #xff #xcb #x86 #xcb #x46 #x28 #xfc #xe1 #xc9 #xcd #x3f #x08
    #xcd #x09 #x06 #x05 #x20 #xf7 #xc9 #xe0 #x13 #x3e #x87 #xe0
    #x14 #xc9 #x21 #x00 #x80 #xaf #x22 #xcb #x6c #x28 #xfa #xc9
    #xcd #x33 #x06 #x1a #xa1 #x47 #x1c #x1c #x1a #x1d #x1d #xa1
    #xcb #x37 #xb0 #xcb #x41 #x28 #x02 #xcb #x37 #x23 #x22 #xcb
    #x31 #xc9 #xcd #x4d #x06 #xcd #x30 #x06 #x1c #x7b #xc9 #x21
    #x96 #x04 #x11 #x7f #x80 #x0e #x30 #x46 #x05 #x28 #x36 #x04
    #x23 #x37 #xcb #x10 #x38 #x20 #xcb #x20 #x38 #x03 #x2a #x18
    #x20 #xcb #x20 #x20 #x05 #x46 #x23 #x37 #xcb #x10 #x4f #x30
    #x03 #xcb #x3f #xfe #x87 #xcb #x20 #x38 #x02 #xb1 #xfe #xa1
    #x18 #x07 #xcb #x20 #x38 #x03 #x1b #x1a #x13 #x13 #x12 #xcb
    #x20 #x20 #xd1 #x18 #xc6 #x62 #x2e #x80 #x11 #x04 #x01 #x0e
    #xf0 #xcd #x4a #x06 #xc6 #x16 #x5f #xcd #x4a #x06 #xd6 #x16
    #x5f #xfe #x1c #x20 #xee #x23 #x11 #x8e #x04 #x0e #x08 #x1a
    #x13 #x22 #x23 #x0d #x20 #xf9 #xc9 #x3e #x01 #xe0 #x4f #x16
    #x1a #x06 #x02 #xcd #x15 #x06 #x21 #xc0 #x98 #x0e #x03 #x7e
    #xfe #x0f #x28 #x05 #x34 #xe6 #x07 #x28 #x03 #x23 #x18 #xf3
    #x7d #xf6 #x1f #x6f #x23 #x0d #x20 #xeb #x15 #x20 #xde #xc9
    #x06 #x20 #x0e #x20 #x21 #x81 #xff #xc5 #x2a #x5f #x3a #x57
    #x01 #x21 #x04 #x7b #xe6 #x1f #xfe #x1f #x20 #x01 #x0d #x7b
    #xfe #xe0 #x38 #x09 #x7a #xe6 #x03 #xfe #x03 #x20 #x02 #xcb
    #xa9 #x7a #xe6 #x7c #xfe #x7c #x20 #x02 #xcb #x90 #x7b #x81
    #x22 #x7a #x88 #x22 #xc1 #x0d #x20 #xcf #xcd #x09 #x06 #xcd
    #x18 #x08 #xcd #x09 #x06 #x05 #x20 #xbe #x3e #x02 #xe0 #x70
    #x21 #x00 #xd0 #xcd #x29 #x06 #x3c #xcd #x2c #x08 #xcd #x31
    #x08 #xcd #x2c #x08 #xaf #xe0 #x70 #x2f #xe0 #x00 #x57 #x59
    #x2e #x0d #xfa #x43 #x01 #xcb #x7f #xcc #x73 #x07 #xcb #x7f
    #xe0 #x4c #xf0 #x80 #x47 #x28 #x05 #xf0 #xc1 #xa7 #x20 #x06
    #xaf #x4f #x3e #x11 #x61 #xc9 #xcd #x73 #x07 #xe0 #x4c #x3e
    #x01 #xc9 #x21 #x7d #x04 #x4f #x06 #x00 #x09 #x7e #xc9 #x3e
    #x01 #xe0 #x6c #xcd #x9e #x07 #xcb #x7f #xc4 #xdf #x08 #xcb
    #xbf #x47 #x80 #x80 #x47 #xf0 #xc1 #xa7 #x28 #x05 #xcd #x6a
    #x07 #x18 #x01 #x78 #xcd #x09 #x06 #xcd #xf3 #x07 #x3e #x04
    #x11 #x08 #x00 #x2e #x7c #xc9 #x21 #x4b #x01 #x7e #xfe #x33
    #x28 #x05 #x3d #x20 #x40 #x18 #x0c #x2e #x44 #x2a #xfe #x30
    #x20 #x37 #x7e #xfe #x31 #x20 #x32 #x2e #x34 #x0e #x10 #xaf
    #x86 #x2c #x0d #x20 #xfb #x47 #x21 #x00 #x02 #x7d #xd6 #x5e
    #xc8 #x2a #xb8 #x20 #xf8 #x7d #xd6 #x42 #x38 #x0e #xe5 #x7d
    #xc6 #x7a #x6f #x7e #xe1 #x4f #xfa #x37 #x01 #xb9 #x20 #xe5
    #x7d #xc6 #x5d #x6f #x78 #xe0 #x80 #x7e #xc9 #xaf #xc9 #x21
    #xd9 #x02 #x06 #x00 #x4f #x09 #xc9 #xcd #xeb #x07 #x1e #x00
    #x2a #xe5 #x21 #x7e #x03 #x4f #x09 #x16 #x08 #x0e #x6a #xcd
    #x21 #x08 #xe1 #xcb #x5b #x20 #x04 #x1e #x08 #x18 #xe9 #x4e
    #x21 #x7e #x03 #x09 #x16 #x08 #x18 #x05 #x21 #x81 #xff #x16
    #x40 #x1e #x00 #x0e #x68 #x3e #x80 #xb3 #xe2 #x0c #x2a #xe2
    #x15 #x20 #xfb #xc9 #xe0 #x4f #x21 #xee #x00 #xcd #x09 #x06
    #x0e #x51 #x06 #x05 #x2a #xe2 #x0c #x05 #x20 #xfa #xc9 #x3e
    #x20 #xe0 #x00 #xf0 #x00 #x2f #xe6 #x0f #xc8 #x2e #x00 #x2c
    #x1f #x30 #xfc #x3e #x10 #xe0 #x00 #xf0 #x00 #x2f #x17 #x17
    #xe6 #x0c #x85 #x6f #xf0 #xc1 #xbd #xc8 #x7d #xe0 #xc1 #xc5
    #xd5 #xcd #x6a #x07 #xcd #xeb #x07 #x2c #x2c #x4e #x21 #x7f
    #x03 #x09 #x3a #xfe #x7f #x20 #x02 #x23 #x23 #xf5 #x2a #xe5
    #x21 #x81 #xff #xcd #xd5 #x08 #x2e #x83 #xcd #xd5 #x08 #xe1
    #xe0 #x87 #x2a #xe5 #x21 #x82 #xff #xcd #xd5 #x08 #x2e #x84
    #xcd #xd5 #x08 #xe1 #xe0 #x88 #xf1 #x28 #x02 #x23 #x23 #xf0
    #xbb #xe6 #xde #x47 #x2a #xe6 #xde #x80 #x47 #xfa #xbc #xff
    #xcb #x97 #x4e #xcb #x91 #x89 #x1f #xea #xbc #xff #x78 #x1f
    #xea #xbb #xff #x2d #x2a #xe0 #xbf #x2a #xe0 #xc0 #x2a #xe0
    #x85 #x2a #xe0 #x86 #xcd #x09 #x06 #xcd #x18 #x08 #x3e #x30
    #xe0 #xc2 #xd1 #xc1 #xc9 #x11 #x08 #x00 #x4b #x77 #x19 #x0d
    #x20 #xfb #xc9 #xf5 #xcd #x09 #x06 #x3e #x19 #xea #x10 #x99
    #x21 #x2f #x99 #x0e #x0c #x3d #x28 #x08 #x32 #x0d #x20 #xf9
    #x2e #x0f #x18 #xf5 #xf1 #xc9 #x00 #x00 #x00 #x00 #x00 #x00)
  "CGB bios as a list of bytes")

(defun make-bios ()
  "creates the bios array of memory for the gb based on *DMG-BIOS* parameter."
  (make-array #x900 :initial-contents *cgb-bios*))

(defun read-rom-data-from-file (filename)
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
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-b)
    (render-full-background (gb-ppu gb)))
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
              (audio-device (sdl2::open-audio-device +sample-rate+ :f32 2 1024))
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
                    (step-ppu ppu gb)
                    (step-spu spu (gbcpu-clock cpu) (cycles-per-sample (gbcpu-cpu-speed cpu)) (cycles-frame-seq-step (gbcpu-cpu-speed cpu)))
                    (handle-timers cpu gb)
                    (handle-interrupts cpu gb)))
                (spu-queue-audio spu))
                (step-rtc (gbcart-timer (gb-cart gb)))
                (let ((now (get-internal-real-time)))
                  (when (and *debug* (< (- now last-frame-time) +time-units-per-frame+))
                    (format t "timeunits since last frame ~A, Sleeping for: ~A~%"
                            (- now last-frame-time)
                      (coerce (/ (- +time-units-per-frame+
                            (- now last-frame-time))
                         internal-time-units-per-second) 'float))
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
  (if (cgb-p gb)
      (replace (gb-bios gb) *cgb-bios*)
      (replace (gb-bios gb) *dmg-bios*))
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

