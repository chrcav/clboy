


(in-package :clboy)

(defstruct gb
  (cpu (make-gbcpu) :type gbcpu)
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

;; TODO make ram and rom dependent on rom header info
(defstruct gbmmu
  (rom (make-array #x8000 :element-type '(unsigned-byte 8)))
  (rom-offset #x4000)
  (vram (make-array #x2000 :initial-element 0 :element-type '(unsigned-byte 8)))
  (ext-ram (make-array #x2000 :initial-element 0 :element-type '(unsigned-byte 8)))
  (ram-offset #x0000)
  (int-ram (make-array #x2000 :initial-element 0 :element-type '(unsigned-byte 8)))
  (oam (make-array #x100 :initial-element 0 :element-type '(unsigned-byte 8)))
  (zero-page (make-array #x100 :initial-element 0 :element-type '(unsigned-byte 8)))
  (bios (make-bios))
  (carttype 0)
  (is-bios? t :type boolean)
  (ramon nil :type boolean)
  (rombank 1)
  (rommask 1)
  (rambank 0)
  (mode 0))


(defstruct gbppu
  (framebuffer (static-vectors:make-static-vector (* 160 144 3)))
  (framebuffer-a (static-vectors:make-static-vector (* 160 144 4)))
  (bg-buffer (make-array (* 144 160) :initial-element 0 :element-type '(unsigned-byte 8)))
  (cycles 0)
  (cur-line 0))

(defun read-rom-data-from-file (filename) (with-open-file (bin filename :element-type '(unsigned-byte 8))
                                            (loop for b = (read-byte bin nil)
                                                  while b collect b)))

(defun make-bios ()
  (make-array #x100 :initial-contents (read-rom-data-from-file "DMG_ROM.bin")))

(defun replace-memory-with-rom (mmu file)
  (let ((rom (read-rom-data-from-file file)))
    (setf (gbmmu-rom mmu) (make-array (length rom) :element-type '(unsigned-byte 8)))
    (replace (gbmmu-rom mmu) rom)))

(defun get-carttype-from-rom (mmu) (setf (gbmmu-carttype mmu) (read-memory-at-addr mmu #x147)))
(defun get-rommask-from-rom (mmu) (setf (gbmmu-rommask mmu) (- (ash #x02 (read-memory-at-addr mmu #x148)) 1)))

;; TODO see if I can split mbc reg writes into separate functions.
(defun write-memory-at-addr (mmu addr val)
  (case (logand addr #xf000)
    ((#x0000 #x1000)
     (case (gbmmu-carttype mmu)
       ((2 3) (setf (gbmmu-ramon mmu) (if (= val #x0a) t nil)))))
    ((#x2000 #x3000)
     (case (gbmmu-carttype mmu)
       ((1 2 3)
        (let ((rombank (logand (+ (logand (gbmmu-rombank mmu) #x60) (if (> val 1) val 1)) (gbmmu-rommask mmu))))
          (setf (gbmmu-rombank mmu) (if (= rombank 0) 1 rombank)
                (gbmmu-rom-offset mmu) (* (if (= rombank 0) 1 rombank) #x4000))))))
    ((#x4000 #x5000)
     (case (gbmmu-carttype mmu)
       ((1 2 3)
        (if (= (gbmmu-mode mmu) #x01)
        (let ((rambank (logand val #x03)))
          (setf (gbmmu-rambank mmu) rambank
                (gbmmu-ram-offset mmu) (* rambank #x2000)))
        (let ((rombank (logand (+ (logand (gbmmu-rombank mmu) #x1f) (ash (logand val #x03) 5)) (gbmmu-rommask mmu))))
          (setf (gbmmu-rombank mmu) (if (= rombank 0) 1 rombank)
                (gbmmu-rom-offset mmu) (* (if (= rombank 0) 1 rombank) #x4000)))))))
    ((#x6000 #x7000)
     (case (gbmmu-carttype mmu)
       ((2 3) (setf (gbmmu-mode mmu) (logand val #x01)))))
    ((#x8000 #x9000)
     (setf (aref (gbmmu-vram mmu) (logand addr #x1fff)) val))
    ((#xa000 #xb000)
     (setf (aref (gbmmu-ext-ram mmu) (+ (gbmmu-ram-offset mmu) (logand addr #x1fff))) val))
    ((#xc000 #xd000 #xe000)
     (setf (aref (gbmmu-int-ram mmu) (logand addr #x1fff)) val))
    (#xf000
     (case (logand addr #x0f00)
       ((#x100 #x200 #x300 #x400 #x500 #x600 #x700
         #x800 #x900 #xa00 #xb00 #xc00 #xd00)
         (setf (aref (gbmmu-int-ram mmu) (logand addr #x1fff)) val))
       (#xe00
        (if (< addr #xfea0) (setf (aref (gbmmu-oam mmu) (logand addr #xff)) val)))
       (#xf00
          (setf (aref (gbmmu-zero-page mmu) (logand addr #xff)) val))))))

(defun read-memory-at-addr (mmu addr)
  (case (logand addr #xf000)
    ((#x0000 #x1000 #x2000 #x3000)
      (if (< addr #x100)
        (if (gbmmu-is-bios? mmu)
          (aref (gbmmu-bios mmu) addr)
          (aref (gbmmu-rom mmu) addr))
        (aref (gbmmu-rom mmu) addr)))
    ((#x4000 #x5000 #x6000 #x7000)
      (aref (gbmmu-rom mmu) (+ (gbmmu-rom-offset mmu) (logand addr #x3fff))))
    ((#x8000 #x9000)
      (aref (gbmmu-vram mmu) (logand addr #x1fff)))
    ((#xa000 #xb000)
      (aref (gbmmu-ext-ram mmu) (+ (gbmmu-ram-offset mmu) (logand addr #x1fff))))
    ((#xc000 #xd000 #xe000)
      (aref (gbmmu-int-ram mmu) (logand addr #x1fff)))
    (#xf000
     (case (logand addr #x0f00)
       ((#x000 #x100 #x200 #x300 #x400 #x500 #x600
         #x700 #x800 #x900 #xa00 #xb00 #xc00 #xd00)
         (aref (gbmmu-int-ram mmu) (logand addr #x1fff)))
       (#xe00
        (if (< addr #xfea0) (aref (gbmmu-oam mmu) (logand addr #xff)) 0))
       (#xf00
        (aref (gbmmu-zero-page mmu) (logand addr #xff)))))))

(defun get-address-from-memory (mmu pc)
  (let* ((lsb (read-memory-at-addr mmu pc))
         (msb (read-memory-at-addr mmu (+ pc 1))))
    (logior lsb (ash msb 8))))

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

(defun read-sprite (mmu addr)
  (loop for a from addr to (+ addr 3)
        collect (read-memory-at-addr mmu a)))

(defun add-sprites-to-ppu-framebuffer (ppu mmu)
  (let* ((row (gbppu-cur-line ppu))
         (scroll-y (read-memory-at-addr mmu #xff42))
         (scroll-x (read-memory-at-addr mmu #xff43)))
    (loop for addr from #xfe00 to #xfea0
          for sprite = (read-sprite mmu addr)
          collect sprite)
  ))

(defun add-background-to-ppu-framebuffer (ppu mmu)
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
        (setf (aref (gbppu-bg-buffer ppu) (+ (* row 160) col)) colorval)
        (let ((palette-col (logand (ash (read-memory-at-addr mmu #xff47) (* colorval -2)) 3)))
        (setf (aref (gbppu-framebuffer ppu) (+ (* row 160 3) (* col 3))) (aref COLORS (* palette-col 3))
              (aref (gbppu-framebuffer ppu) (+ (* row 160 3) (* col 3) 1)) (aref COLORS (+ (* palette-col 3) 1))
              (aref (gbppu-framebuffer ppu) (+ (* row 160 3) (* col 3) 2)) (aref COLORS (+ (* palette-col 3) 2))
              ;(aref (gbppu-framebuffer-a ppu) (+ (* row 144 4) (* col 4) 3)) #xff
              )))))))

(defun maybe-do-dma (ppu mmu)
  (let ((initial (ash (read-memory-at-addr mmu #xff46) 8)))
    (when (> initial 0)
      (format t "transferring dma~%")
      (loop for i from 0 to 159
            do
            (let ((src (+ initial i))
                  (dest (+ #xffe00 i)))
            (write-memory-at-addr mmu dest (read-memory-at-addr mmu src))))
      (write-memory-at-addr mmu #xff46 0))))

(defun step-ppu (ppu cpu mmu renderer texture)
  (incf (gbppu-cycles ppu) (gbcpu-clock cpu))
  (maybe-do-dma ppu mmu)
  (when (> (gbppu-cycles ppu) (* 456 4))
      (add-background-to-ppu-framebuffer ppu mmu)
      (add-sprites-to-ppu-framebuffer ppu mmu)
      (incf (gbppu-cur-line ppu))
      (write-memory-at-addr mmu #xff44 (gbppu-cur-line ppu))
      (setf (gbppu-cycles ppu) 0))
  (when (= (gbppu-cur-line ppu) 144)
    (set-interrupt-flag mmu 0)
    (sdl2:update-texture
      texture
      (cffi:null-pointer)
      (static-vectors:static-vector-pointer
        (gbppu-framebuffer ppu)) (* 160 3))))

(defun update-input-memory (mmu input)
  (let ((input-val (logand (read-memory-at-addr mmu #xff00) #x30)))
    (when (= input-val #x30) ; both p14 and p15 inactive
      (write-memory-at-addr mmu #xff00 (logior input-val #x00)))
    (when (= input-val #x20) ; p14 down up left right
      (write-memory-at-addr mmu #xff00 (logior input-val #x00)))
    (when (= input-val #x10) ; p15 start select b a
      (write-memory-at-addr mmu #xff00 (logior input-val #x00)))))

(defparameter *out* ())
(defparameter *width* 160)
(defparameter *height* 144)
(defparameter *scale* 3)

(defparameter *debug* nil)

(defun emu-main (gb)
  (let ((cpu (gb-cpu gb))
        (mmu (gb-mmu gb))
        (ppu (gb-ppu gb))
        (input (gb-input gb)))
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "CL-Boy" :flags '(:shown :opengl) :w (* *width* *scale*) :h (* *height* *scale*))
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
                (loop until (> (gbppu-cur-line ppu) 153)
                  for instr = (emu-single-op cpu mmu)
                  do
                    (when (not instr) (sdl2:push-event :quit) (return nil))
                    (when (and *debug* (instruction-p instr))
                       (format t "~X: ~A --> PC=~X~%" (instruction-opcode instr) (instruction-asm instr) (gbcpu-pc cpu)))
                    ;(when (= (gbcpu-pc cpu) #x1e7e) (sdl2:push-event :quit) (return nil))
                    (if (= (read-memory-at-addr mmu #xff02) #x81)
                      (progn
                        (setf *out* (cons (code-char (read-memory-at-addr mmu #xff01)) *out*))
                        (write-memory-at-addr mmu #xff02 0)))
                    (if (= (gbcpu-pc cpu) #x100) (setf (gbmmu-is-bios? mmu) nil))
                    (step-ppu ppu cpu mmu renderer texture)
                    (handle-timers cpu mmu)
                    (handle-interrupts cpu mmu)
                    (update-input-memory mmu input)))
              (setf (gbppu-cur-line ppu) 0)
              (sdl2:render-copy renderer texture)
              (sdl2:render-present renderer))
            (:quit () t))))))))


(defparameter *gb* (make-gb))

(defun gb-reset ()
  (setf *gb* (make-gb))
  (replace-memory-with-rom (gb-mmu *gb*) loaded-rom)
  (get-carttype-from-rom (gb-mmu *gb*))
  (get-rommask-from-rom (gb-mmu *gb*))
  (write-memory-at-addr (gb-mmu *gb*) #xff00 #xff))

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
(get-carttype-from-rom (gb-mmu *gb*))
(get-rommask-from-rom (gb-mmu *gb*))

(write-memory-at-addr (gb-mmu *gb*) #xff00 #xff)

(defun run () (emu-main *gb*))

