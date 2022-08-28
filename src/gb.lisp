


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
  (case (logand addr #xf000)
    ((#x0000 #x1000 #x2000 #x3000 #x4000
      #x5000 #x6000 #x7000 #xa000 #xb000)
     (if (gbcart-p (gb-cart gb)) (cart-write-memory-at-addr (gb-cart gb) addr val)))
    ((#x8000 #x9000)
     (ppu-write-memory-at-addr (gb-ppu gb) addr val))
    ((#xc000 #xd000 #xe000)
     (setf (aref (gb-int-ram gb) (logand addr #x1fff)) val))
    (#xf000
     (case (logand addr #x0f00)
       ((#x100 #x200 #x300 #x400 #x500 #x600 #x700
         #x800 #x900 #xa00 #xb00 #xc00 #xd00)
        (setf (aref (gb-int-ram gb) (logand addr #x1fff)) val))
       (#xe00
        (ppu-write-memory-at-addr (gb-ppu gb) addr val))
       (#xf00
        (case (logand addr #x00f0)
          (#x00
           (case (logand addr #x000f)
             (#x0 (setf (gbinput-reg (gb-input gb)) val))
             (otherwise (setf (aref (gb-zero-page gb) (logand addr #xff)) val))))
          ((#x10 #x20 #x30) (spu-write-memory-at-addr (gb-spu gb) addr val))
          (#x40 (ppu-write-memory-at-addr (gb-ppu gb) addr val))
          (#x50 (if (= (logand addr #xff) #x50) (setf (gb-is-bios? gb) (= val 0))))
          (otherwise (setf (aref (gb-zero-page gb) (logand addr #xff)) val))))))))

(defun read-memory-at-addr (gb addr)
  (case (logand addr #xf000)
    ((#x0000 #x1000 #x2000 #x3000 #x4000
      #x5000 #x6000 #x7000 #xa000 #xb000)
      (if (and (< addr #x100) (gb-is-bios? gb))
          (aref (gb-bios gb) addr)
          (if (gbcart-p (gb-cart gb)) (cart-read-memory-at-addr (gb-cart gb) addr) #xff)))
    ((#x8000 #x9000)
      (ppu-read-memory-at-addr (gb-ppu gb) addr))
    ((#xc000 #xd000 #xe000)
      (aref (gb-int-ram gb) (logand addr #x1fff)))
    (#xf000
      (case (logand addr #x0f00)
        ((#x000 #x100 #x200 #x300 #x400 #x500 #x600
          #x700 #x800 #x900 #xa00 #xb00 #xc00 #xd00)
          (aref (gb-int-ram gb) (logand addr #x1fff)))
        (#xe00
          (ppu-read-memory-at-addr (gb-ppu gb) addr))
        (#xf00
          (case (logand addr #x00f0)
            (#x00
             (case (logand addr #x000f)
               (#x0 (input-read-memory (gb-input gb)))
               (otherwise (aref (gb-zero-page gb) (logand addr #xff)))))
            ((#x10 #x20 #x30) (spu-read-memory-at-addr (gb-spu gb) addr))
            (#x40 (ppu-read-memory-at-addr (gb-ppu gb) addr))
            (#x50 (if (= (logand addr #xff) #x50) (if (gb-is-bios? gb) #xff #x00)))
            (otherwise (aref (gb-zero-page gb) (logand addr #xff)))))))))

(defun get-address-from-memory (gb pc)
  (let* ((lsb (read-memory-at-addr gb pc))
         (msb (read-memory-at-addr gb (+ pc 1))))
    (logior lsb (ash msb 8))))

;; INTERRUPTS
(defun handle-interrupts (cpu gb)
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
  (if (> (gbcpu-div-clock cpu) #xff)
    (progn
      (setf (gbcpu-div-clock cpu) (- (gbcpu-div-clock cpu) #x100))
      (write-memory-at-addr gb #xff04 (logand (+ (read-memory-at-addr gb #xff04) #x01) #xff))))
  (if (= (logand (read-memory-at-addr gb #xff07) #x04) #x04)
    (incr-timer-by-cycles cpu gb (get-cycles-per-timer-tick (get-timer-frequency (read-memory-at-addr gb #xff07))))
    (setf (gbcpu-clock cpu) 0)))

(defun incr-timer-by-cycles (cpu gb cycles-per-tick)
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

(defun get-cycles-per-timer-tick (freq)
  (/ +cpu-speed+ freq))

(defun get-timer-frequency (tac)
  (let ((tac-two-lsb (logand tac #x3)))
    (if (= tac-two-lsb #x01) 262144
      (if (= tac-two-lsb #x02) 65536
        (if (= tac-two-lsb #x03) 16384
          4096)))))

;; utils

(defun bool-as-bit (bool) (if bool 1 0))

(defun make-bios ()
  (make-array #x100 :initial-contents (read-rom-data-from-file "DMG_ROM.bin")))

(defun read-rom-data-from-file (filename)
  (with-open-file (bin filename :element-type '(unsigned-byte 8))
    (loop for b = (read-byte bin nil) while b collect b)))

(defun make-signed-from-unsigned (unsign-byte)
  (if (< unsign-byte 128)
      unsign-byte
      (logior unsign-byte (- (mask-field (byte 1 7) #xff)))))

;; PPU


;; I/O

(defun get-p14-byte (input)
   ; p14 down up left right
  (+ (if (gbinput-down input) #x0 #x8)
     (if (gbinput-up input) #x0 #x4)
     (if (gbinput-left input) #x0 #x2)
     (if (gbinput-right input) #x0 #x1)))

(defun get-p15-byte (input)
 ; p15 start select b a
  (+ (if (gbinput-start input) #x0 #x8)
     (if (gbinput-select input) #x0 #x4)
     (if (gbinput-b input) #x0 #x2)
     (if (gbinput-a input) #x0 #x1)))

(defun input-read-memory (input)
  (let ((input-val (logand (gbinput-reg input) #x30)))
    (if (= input-val #x30) ; both p14 and p15 inactive
      (logior input-val #x0f)
    (if (= input-val #x20)
      (+ input-val (get-p14-byte input))
    (if (= input-val #x10)
      (+ input-val (get-p15-byte input))
      #x0f)))))

(defun handle-keyup (input gb keysym)
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
    (sdl2:push-event :quit))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-p)
    (setf (gb-paused? gb) (not (gb-paused? gb))))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-r)
    (gb-reset gb))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-b)
    (render-full-background (gb-ppu gb) gb))
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

(defun handle-keydown (gb input keysym)
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

;; CPU

;; SPU

(defparameter *out* ())
(defparameter *width* 256)
(defparameter *height* 256)
(defparameter *scale* 3)

(defparameter *debug* nil)

(defun emu-main (gb)
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "CL-Boy" :flags '(:shown :opengl) :w (* *width* *scale*) :h (* *height* *scale*))
      (sdl2:with-renderer (renderer win)
        (let ((texture
                (sdl2:create-texture renderer :rgb24 :streaming +screen-pixel-width+ +screen-pixel-height+))
              (rect (sdl2:make-rect
                      (* (/ (- *width* +screen-pixel-width+) 2) *scale*)
                      (* (/ (- *height* +screen-pixel-height+) 2) *scale*)
                      (* +screen-pixel-width+ *scale*) (* +screen-pixel-height+ *scale*)))
              (audio-device (sdl2::open-audio-device +sample-rate+ :f32 2 1024)))
          (format t "~A~%" audio-device)
          (sdl2::unpause-audio-device audio-device)
          (setf (gbppu-renderer (gb-ppu gb)) renderer)
          (setf (gbppu-texture (gb-ppu gb)) texture)
          (setf (gbspu-device (gb-spu gb)) audio-device)
          (sdl2:with-event-loop (:method :poll)
            (:keydown (:keysym keysym)
                      (handle-keydown gb (gb-input gb) keysym))
            (:keyup (:keysym keysym)
                    (handle-keyup (gb-input gb) gb keysym))
            (:idle ()
              (let ((cpu (gb-cpu gb))
                    (ppu (gb-ppu gb))
                    (spu (gb-spu gb)))
                (when (not (gb-paused? gb))
                  (when (not (gb-stopped? gb))
                    (loop while (step-cpu cpu gb)
                          for cyc = 0 then (+ cyc (gbcpu-clock cpu))
                          while (< cyc 75000) do
                    (if (= (read-memory-at-addr gb #xff02) #x81)
                      (progn
                        (setf *out* (cons (code-char (read-memory-at-addr gb #xff01)) *out*))
                        (write-memory-at-addr gb #xff02 0)))
                    (step-ppu ppu gb)
                    (step-spu spu gb)
                    (handle-timers cpu gb)
                    (handle-interrupts cpu gb)))
                  (sdl2:render-clear renderer)
                  (sdl2:render-copy renderer texture :dest-rect rect))
                (sdl2:render-present renderer)))
            (:quit () t)))))))


(defparameter *gb* (make-gb))

(defun gb-reset (gb)
  (gbppu-reset (gb-ppu gb))
  (gbspu-reset (gb-spu gb))
  (setf (gb-cpu gb) (make-gbcpu)
  ; doing this to reset scroll values. but might not be necessary.
        (gb-is-bios? gb) t
        (gb-stopped? gb) nil)
  nil)

(defun load-cart (cart)
  (gb-reset *gb*)
  (setf (gb-cart *gb*) cart)
  nil)
(defun unload-cart ()
  (gb-reset *gb*)
  (setf (gb-cart *gb*) nil)
  nil)

(defun dump-mem-region (start end)
  (loop for a from start to end
        collect (read-memory-at-addr *gb* a)))

(defun dump-oam ()
  (dump-mem-region #xfe00 #xfea0))

(defun dump-blargg-output ()
  (dump-mem-region #x9800 #x9BFF))

(defun dump-out ()
  (format t "~{~A~}" (reverse *out*)))

(defparameter *silver-cart*  (make-gbcart-from-rom "roms/silver.gbc"))

(defparameter *red-cart* (make-gbcart-from-rom "roms/red.gb"))

(defparameter *ffadv-cart*  (make-gbcart-from-rom "roms/ff-adv.gb"))

(defparameter *kirby-cart* (make-gbcart-from-rom "./roms/kirbys-dl.gb"))

(defparameter *drmario-cart* (make-gbcart-from-rom "./roms/dr_mario.gb"))

(load-cart *red-cart*)

;; test rom memory replace calls
;(load-cart (make-gbcart-from-rom "./opus4.gb"))
;(load-cart (make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/instr_timing/instr_timing.gb")) ; PASSED
;(load-cart (make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/cpu_instrs.gb")) ; PASSED
;(load-cart (make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/01-special.gb")) ; PASSED
;(load-cart (make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/02-interrupts.gb")) ; PASSED
;(load-cart (make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/03-op sp,hl.gb")) ; PASSED
;(load-cart (make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/04-op r,imm.gb")) ; PASSED
;(load-cart (make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/05-op rp.gb")) ; PASSED
;(load-cart (make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/06-ld r,r.gb")) ; PASSED
;(load-cart (make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/07-jr,jp,call,ret,rst.gb")) ; PASSED
;(load-cart (make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/08-misc instrs.gb")) ; PASSED
;(load-cart (make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/09-op r,r.gb")) ; PASSED
;(load-cart (make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/10-bit ops.gb")) ; PASSED
;(load-cart (make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/11-op a,(hl).gb")) ; PASSED

;(load-cart (make-gbcart-from-rom "./mts-20220522-1522-55c535c/emulator-only/mbc1/multicart_rom_8Mb.gb"))


(defun run () (emu-main *gb*))
