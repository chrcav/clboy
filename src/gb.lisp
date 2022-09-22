


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
  "Routes various reads for GB to various other components (i.e. ppu, cart, etc.) based on ADDR."
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
    (incr-timer-by-cycles cpu gb (get-cycles-per-timer-tick (get-timer-frequency (read-memory-at-addr gb #xff07))))
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

(defun get-cycles-per-timer-tick (freq)
  "number of cycles per timer tick based on the FREQ of the timer."
  (/ +cpu-speed+ freq))

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

(defun make-bios ()
  "creates the bios array of memory for the gb based on *DMG-BIOS* parameter."
  (make-array #x100 :initial-contents *dmg-bios*))

(defun read-rom-data-from-file (filename)
  "reads rom data from a file referenced by FILENAME as a list of bytes"
  (with-open-file (bin filename :element-type '(unsigned-byte 8))
    (loop for b = (read-byte bin nil) while b collect b)))

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

(defconstant +cycles-per-internal-time-units+ (floor +cpu-speed+ internal-time-units-per-second))
(defconstant +cycles-per-frame+ (floor +cpu-speed+ 60))
(defconstant +time-units-per-frame+ (truncate (* (/ 60) internal-time-units-per-second)))

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
          (sdl2:game-controller-open 0)
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
                          while (< cyc +cycles-per-frame+) do
                    (step-ppu ppu gb)
                    (step-spu spu (gbcpu-clock cpu))
                    (handle-timers cpu gb)
                    (handle-interrupts cpu gb)))
                (spu-queue-audio spu))
                (step-rtc (gbcart-timer (gb-cart gb)))
                (let ((now (get-internal-real-time)))
                  (when (< (- now last-frame-time) +time-units-per-frame+)
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
            (:quit () t)))))))


(defun gb-reset (gb)
  "resets GB to start at the beginning of the bios with everything cleared"
  (gbppu-reset (gb-ppu gb))
  (gbspu-reset (gb-spu gb))
  (setf (gb-cpu gb) (make-gbcpu)
        (gb-is-bios? gb) t
        (gb-stopped? gb) nil)
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

