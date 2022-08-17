


(in-package :clboy)

;; TODO make ram and rom dependent on rom header info
(defstruct gb
  (cpu (make-gbcpu) :type gbcpu)
  (ppu (make-gbppu))
  (cart (make-gbcart))
  (input (make-gbinput))
  (stopped? nil :type boolean)
  (bios (make-bios))
  (is-bios? t :type boolean)
  (int-ram (make-array #x2000 :initial-element 0 :element-type '(unsigned-byte 8)))
  (zero-page (make-array #x100 :initial-element 0 :element-type '(unsigned-byte 8))))

(defstruct gbcart
  (carttype 0)
  (rom (make-array #x8000 :element-type '(unsigned-byte 8)))
  (rom-offset #x4000)
  (ram (make-array #x2000 :initial-element 0 :element-type '(unsigned-byte 8)))
  (ram-offset #x0000)
  (ramon nil :type boolean)
  (rombank 1)
  (rommask 1)
  (rambank 0)
  (mode 0))

(defstruct gbinput
  (down   nil :type boolean)
  (up     nil :type boolean)
  (left   nil :type boolean)
  (right  nil :type boolean)
  (a      nil :type boolean)
  (b      nil :type boolean)
  (start  nil :type boolean)
  (select nil :type boolean))

(defstruct gbppu
  (framebuffer (static-vectors:make-static-vector (* 160 144 3)))
  (framebuffer-a (static-vectors:make-static-vector (* 160 144 4)))
  (bg-buffer (make-array (* 144 160) :initial-element 0 :element-type '(unsigned-byte 8)))
  (cycles 0)
  (cur-line 0)
  (oam (make-array #x100 :initial-element 0 :element-type '(unsigned-byte 8)))
  (vram (make-array #x2000 :initial-element 0 :element-type '(unsigned-byte 8)))
  (zero-page (make-array #x100 :initial-element 0 :element-type '(unsigned-byte 8)))
  (mode 0))

;; MMU

;; TODO see if I can split mbc reg writes into separate functions.
(defun write-memory-at-addr (gb addr val)
  (case (logand addr #xf000)
    ((#x0000 #x1000 #x2000 #x3000 #x4000
      #x5000 #x6000 #x7000 #xa000 #xb000)
     (cart-write-memory-at-addr (gb-cart gb) addr val))
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
          (#x40 (ppu-write-memory-at-addr (gb-ppu gb) addr val))
          (otherwise (setf (aref (gb-zero-page gb) (logand addr #xff)) val))))))))

(defun read-memory-at-addr (gb addr)
  (case (logand addr #xf000)
    ((#x0000 #x1000 #x2000 #x3000 #x4000
      #x5000 #x6000 #x7000 #xa000 #xb000)
      (if (< addr #x100)
        (if (gb-is-bios? gb)
          (aref (gb-bios gb) addr)
          (cart-read-memory-at-addr (gb-cart gb) addr))
        (cart-read-memory-at-addr (gb-cart gb) addr)))
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
            (#x40 (ppu-read-memory-at-addr (gb-ppu gb) addr))
            (otherwise (aref (gb-zero-page gb) (logand addr #xff)))))))))

(defun get-address-from-memory (gb pc)
  (let* ((lsb (read-memory-at-addr gb pc))
         (msb (read-memory-at-addr gb (+ pc 1))))
    (logior lsb (ash msb 8))))

;; INTERRUPTS
(defun handle-interrupts (cpu gb)
  (setf (gbcpu-halted cpu) #x00)
  (if (= (gbcpu-int-ena cpu) #x01)
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
        (do-interrupt cpu gb 4))))))))

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
  (let* ((cycles (gbcpu-clock cpu))
         (ticks (floor cycles cycles-per-tick))
         (remainder (mod cycles cycles-per-tick))
         (cur-ticks (read-memory-at-addr gb #xff05))
         (new-ticks (+ cur-ticks ticks)))
    (setf (gbcpu-clock cpu) remainder)
    (if (> new-ticks #xff)
      (progn (set-interrupt-flag gb 2)
             (write-memory-at-addr gb #xff05 (+ (read-memory-at-addr gb #xff06) (logand new-ticks #xff))))
      (write-memory-at-addr gb #xff05 (logand new-ticks #xff)))))

(defconstant CPU_SPEED 4194304)
(defun get-cycles-per-timer-tick (freq)
  (/ CPU_SPEED freq))

(defun get-timer-frequency (tac)
  (let ((tac-two-lsb (logand tac #x3)))
    (if (= tac-two-lsb #x01) 262144
      (if (= tac-two-lsb #x02) 65536
        (if (= tac-two-lsb #x03) 16384
          4096)))))

;; utils

(defun make-bios ()
  (make-array #x100 :initial-contents (read-rom-data-from-file "DMG_ROM.bin")))

(defun read-rom-data-from-file (filename)
  (with-open-file (bin filename :element-type '(unsigned-byte 8))
    (loop for b = (read-byte bin nil) while b collect b)))

(defun make-signed-from-unsigned (unsign-byte)
  (if (< unsign-byte 128)
      unsign-byte
      (logior unsign-byte (- (mask-field (byte 1 7) #xff)))))

;; CART

(defun replace-memory-with-rom (cart file)
  (let ((rom (read-rom-data-from-file file)))
    (setf (gbcart-rom cart) (make-array (length rom) :element-type '(unsigned-byte 8)))
    (replace (gbcart-rom cart) rom)))

(defun get-carttype-from-rom (cart) (setf (gbcart-carttype cart) (cart-read-memory-at-addr cart #x147)))
(defun get-rommask-from-rom (cart) (setf (gbcart-rommask cart) (- (ash #x02 (cart-read-memory-at-addr cart #x148)) 1)))

(defun cart-write-memory-at-addr (cart addr val)
  (case (logand addr #xf000)
    ((#x0000 #x1000)
     (case (gbcart-carttype cart)
       ((2 3) (setf (gbcart-ramon cart) (if (= val #x0a) t nil)))))
    ((#x2000 #x3000)
     (case (gbcart-carttype cart)
       ((1 2 3)
         (let ((rombank (logand (+ (logand (gbcart-rombank cart) #x60) (if (> val 1) val 1)) (gbcart-rommask cart))))
           (setf (gbcart-rombank cart) (if (= rombank 0) 1 rombank)
                 (gbcart-rom-offset cart) (* (if (= rombank 0) 1 rombank) #x4000))))))
    ((#x4000 #x5000)
     (case (gbcart-carttype cart)
       ((1 2 3)
         (if (= (gbcart-mode cart) #x01)
           (let ((rambank (logand val #x03)))
             (setf (gbcart-rambank cart) rambank
                   (gbcart-ram-offset cart) (* rambank #x2000)))
           (let ((rombank (logand (+ (logand (gbcart-rombank cart) #x1f) (ash (logand val #x03) 5)) (gbcart-rommask cart))))
             (setf (gbcart-rombank cart) (if (= rombank 0) 1 rombank)
                   (gbcart-rom-offset cart) (* (if (= rombank 0) 1 rombank) #x4000)))))))
    ((#x6000 #x7000)
     (case (gbcart-carttype cart)
       ((2 3) (setf (gbcart-mode cart) (logand val #x01)))))
    ((#xa000 #xb000)
     (setf (aref (gbcart-ram cart) (+ (gbcart-ram-offset cart) (logand addr #x1fff))) val))))

(defun cart-read-memory-at-addr (cart addr)
  (case (logand addr #xf000)
    ((#x0000 #x1000 #x2000 #x3000)
      (aref (gbcart-rom cart) addr))
    ((#x4000 #x5000 #x6000 #x7000)
      (aref (gbcart-rom cart) (+ (gbcart-rom-offset cart) (logand addr #x3fff))))
    ((#xa000 #xb000)
      (aref (gbcart-ram cart) (+ (gbcart-ram-offset cart) (logand addr #x1fff))))))

;; PPU

(defconstant COLORS #(255 255 255
                      192 192 192
                      96 96 96
                       0  0  0))

(defun ppu-write-memory-at-addr (ppu addr val)
  (case (logand addr #xf000)
    ((#x8000 #x9000)
      (setf (aref (gbppu-vram ppu) (logand addr #x1fff)) val))
    (#xf000
     (case (logand addr #x0f00)
       (#xe00 (if (< addr #xfea0) (setf (aref (gbppu-oam ppu) (logand addr #xff)) val)))
       (#xf00
        (setf (aref (gbppu-zero-page ppu) (logand addr #xff)) val))))))

(defun ppu-read-memory-at-addr (ppu addr)
  (case (logand addr #xf000)
    ((#x8000 #x9000)
      (aref (gbppu-vram ppu) (logand addr #x1fff)))
    (#xf000
     (case (logand addr #x0f00)
       (#xe00 (if (< addr #xfea0) (aref (gbppu-oam ppu) (logand addr #xff)) 0))
       (#xf00
         (aref (gbppu-zero-page ppu) (logand addr #xff)))))))

(defun read-sprite (ppu addr)
  (loop for a from addr to (+ addr 3)
        collect (ppu-read-memory-at-addr ppu a)))

(defun sprite-overlaps-scanline? (ppu sprite row)
  (let ((sprite-y (car sprite))
        (sprite-height (if (= (logand (ppu-read-memory-at-addr ppu #xff40) #x4) #x4) 16 8)))
  (and (> sprite-y row) (<= sprite-y (+ row sprite-height)))))

(defun sprite-within-viewport? (ppu sprite)
  (let* ((row (gbppu-cur-line ppu))
         (lcdc (ppu-read-memory-at-addr ppu #xff40))
         (sprite-x (cadr sprite))
         (sprite-y (car sprite))
         (sprite-height (if (= (logand lcdc #x4) #x4) 16 8))
         (scroll-y (ppu-read-memory-at-addr ppu #xff42))
         (scroll-x (ppu-read-memory-at-addr ppu #xff43)))
    (and (> sprite-x scroll-x) (< sprite-x (+ scroll-x 8))
         (> sprite-y scroll-y) (< sprite-y (+ scroll-y sprite-height)))))

(defun add-sprites-to-ppu-framebuffer (ppu)
  (let* ((row (gbppu-cur-line ppu))
         (scroll-y (ppu-read-memory-at-addr ppu #xff42))
         (scroll-x (ppu-read-memory-at-addr ppu #xff43)))
    (loop for addr = #xfe00 then (+ addr 4)
          while (< addr #xfea0)
          for sprite = (read-sprite ppu addr)
          when (sprite-overlaps-scanline? ppu sprite row)
          ;when (sprite-within-viewport? ppu sprite)
          do (add-sprite-to-ppu-framebuffer ppu sprite))
  ))

(defun add-sprite-to-ppu-framebuffer (ppu sprite)
  (let* ((lcdc (ppu-read-memory-at-addr ppu #xff40))
         (row (gbppu-cur-line ppu))
         (sprite-height (if (= (logand lcdc #x4) #x4) 16 8))
         (sprite-y (- (car sprite) sprite-height))
         (sprite-x (- (cadr sprite) 8))
         (tile-no (if (= (logand lcdc #x4) #x4) (logand (caddr sprite) #xfe) (caddr sprite)))
         (sprite-flags (cadddr sprite))
         (sprite-y-offset (- row sprite-y))
         (sprite-xflip (logand (ash sprite-flags -5) #x01))
         (palette-reg (if (= (logand sprite-flags #x10) #x00) #xff48 #xff49))
         (color-addr (+ #x8000 (* tile-no (* sprite-height 2)) (* sprite-y-offset 2)))
         (colorbyte1 (ppu-read-memory-at-addr ppu color-addr))
         (colorbyte2 (ppu-read-memory-at-addr ppu (+ color-addr 1))))
    (loop for i from 0 to 7
          for col = (+ sprite-x i)
      do
      (let* ((colorbitpos (if (= sprite-xflip #x01) (- 0 i) (- i 7)))
            (colorval (+
                         (logand (ash colorbyte1 colorbitpos) #x01)
                         (* (logand (ash colorbyte2 colorbitpos) #x01) 2))))
        (when (or (= (aref (gbppu-bg-buffer ppu) (+ (* row 160) col)) #x00) (= (ash sprite-flags -7) #x00))
        (let ((palette-col (logand (ash (ppu-read-memory-at-addr ppu palette-reg) (* colorval -2)) 3)))
        ;(format t "colorval ~X pallette-col ~X~%" colorval palette-col)
        (setf (aref (gbppu-framebuffer ppu) (+ (* row 160 3) (* col 3))) (aref COLORS (* palette-col 3))
              (aref (gbppu-framebuffer ppu) (+ (* row 160 3) (* col 3) 1)) (aref COLORS (+ (* palette-col 3) 1))
              (aref (gbppu-framebuffer ppu) (+ (* row 160 3) (* col 3) 2)) (aref COLORS (+ (* palette-col 3) 2))
              ;(aref (gbppu-framebuffer-a ppu) (+ (* row 144 4) (* col 4) 3)) #xff
              )))))))

(defun add-background-to-ppu-framebuffer (ppu)
  (let* ((row (gbppu-cur-line ppu))
         (scroll-y (ppu-read-memory-at-addr ppu #xff42))
         (scroll-x (ppu-read-memory-at-addr ppu #xff43))
         (lcdc (ppu-read-memory-at-addr ppu #xff40))
         (tilemap-loc (if (= (logand lcdc #x08) #x08) #x9c00 #x9800))
         (tiledata-loc (if (= (logand lcdc #x10) #x10) #x8000 #x9000)))
    (when (< row 144)
    (loop for col from 0 to 159
      do
      (let* ((yoffset (+ row scroll-y))
             (xoffset (+ col scroll-x))
             (addr (+ tilemap-loc (* (floor yoffset 8) 32) (floor xoffset 8)))
            (tile-no (if (= tiledata-loc #x8000)
                       (ppu-read-memory-at-addr ppu addr)
                       (make-signed-from-unsigned (ppu-read-memory-at-addr ppu addr))))
            (colorbitpos (- 7 (mod xoffset 8)))
            (color-addr (+ tiledata-loc (* tile-no #x10) (* (mod yoffset 8) 2)))
            (colorbyte1 (ppu-read-memory-at-addr ppu color-addr))
            (colorbyte2 (ppu-read-memory-at-addr ppu (+ color-addr 1)))
            (colorval (+
                         (logand (ash colorbyte1 (- 0 colorbitpos)) #x01)
                         (* (logand (ash colorbyte2 (- 0 colorbitpos)) #x01) 2))))
        (setf (aref (gbppu-bg-buffer ppu) (+ (* row 160) col)) colorval)
        (let ((palette-col (logand (ash (ppu-read-memory-at-addr ppu #xff47) (* colorval -2)) 3)))
        (setf (aref (gbppu-framebuffer ppu) (+ (* row 160 3) (* col 3))) (aref COLORS (* palette-col 3))
              (aref (gbppu-framebuffer ppu) (+ (* row 160 3) (* col 3) 1)) (aref COLORS (+ (* palette-col 3) 1))
              (aref (gbppu-framebuffer ppu) (+ (* row 160 3) (* col 3) 2)) (aref COLORS (+ (* palette-col 3) 2))
              ;(aref (gbppu-framebuffer-a ppu) (+ (* row 144 4) (* col 4) 3)) #xff
              )))))))

(defun maybe-do-dma (ppu gb)
  (let ((initial (ash (read-memory-at-addr gb #xff46) 8)))
    (when (> initial 0)
      (loop for i from 0 to 159
            do
            (let ((src (+ initial i))
                  (dest (+ #xfe00 i)))
            (write-memory-at-addr gb dest (read-memory-at-addr gb src))))
      (write-memory-at-addr gb #xff46 0))))

(defun check-ly-lyc (ppu gb)
  (let ((ly (read-memory-at-addr gb #xff44))
        (lyc (read-memory-at-addr gb #xff45)))
    (when (= ly lyc)
      (let ((lcd-stat (read-memory-at-addr gb #xff41))
            (stat-int-ena #x40))
        (write-memory-at-addr gb #xff41 (+ (logand lcd-stat #xfb) #x04))
        (if (= (logand lcd-stat stat-int-ena) stat-int-ena) (set-interrupt-flag gb 1))))))

(defun step-ppu (ppu gb renderer texture)
  (incf (gbppu-cycles ppu) (gbcpu-clock (gb-cpu gb)))
  (maybe-do-dma ppu gb)
  (check-ly-lyc ppu gb)
  (case (gbppu-mode ppu)
    ; in Hblank state
    (0 (when (> (gbppu-cycles ppu) (* 204 4))
         (incf (gbppu-cur-line ppu))
         (write-memory-at-addr gb #xff44 (gbppu-cur-line ppu))
         (if (= (gbppu-cur-line ppu) 144)
           (progn (ppu-mode-transition ppu gb 1)
                  (update-screen ppu gb texture))
           (ppu-mode-transition ppu gb 2))))
    ; in Vblank state
    (1 (when (> (gbppu-cycles ppu) (* 456 4))
         (incf (gbppu-cur-line ppu))
         (write-memory-at-addr gb #xff44 (gbppu-cur-line ppu))
         (if (> (gbppu-cur-line ppu) 153) (ppu-mode-transition ppu gb 2))))
    ; in OAM state
    (2 (when (> (gbppu-cycles ppu) (* 80 4))
         (ppu-mode-transition ppu gb 3)))
    ; in VRAM Read state
    (3 (when (> (gbppu-cycles ppu) (* 172 4))
         (render-scanline ppu)
         (ppu-mode-transition ppu gb 0)))))

(defun ppu-mode-transition (ppu gb mode)
  (setf (gbppu-cycles ppu) 0)
  (setf (gbppu-mode ppu) mode)
  (let ((lcd-stat (read-memory-at-addr gb #xff41))
        (stat-int-ena (ash #x8 mode)))
    (write-memory-at-addr gb #xff41 (+ (logand lcd-stat #xfc) mode))
    (if (< mode 3)
      (if (= (logand lcd-stat stat-int-ena) stat-int-ena) (set-interrupt-flag gb 1)))))


(defun render-scanline (ppu)
    (add-background-to-ppu-framebuffer ppu)
    (add-sprites-to-ppu-framebuffer ppu))

(defun update-screen (ppu gb texture)
  (set-interrupt-flag gb 0)
  (sdl2:update-texture
    texture
    (cffi:null-pointer)
    (static-vectors:static-vector-pointer
      (gbppu-framebuffer ppu)) (* 160 3)))

;; I/O
(defun update-input-memory (gb input)
  (let ((input-val (logand (read-memory-at-addr gb #xff00) #x30)))
    (when (= input-val #x30) ; both p14 and p15 inactive
      (write-memory-at-addr gb #xff00 (logior input-val #x00)))
    (when (= input-val #x20) ; p14 down up left right
      (write-memory-at-addr gb #xff00 (logior input-val #x00)))
    (when (= input-val #x10) ; p15 start select b a
      (write-memory-at-addr gb #xff00 (logior input-val #x00)))))

(defparameter *out* ())
(defparameter *width* 160)
(defparameter *height* 144)
(defparameter *scale* 3)

(defparameter *debug* nil)

(defun emu-main (gb)
  (let ((cpu (gb-cpu gb))
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
                (setf (gbinput-start input) t)))
            (:keyup (:keysym keysym)
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
               (sdl2:push-event :quit))
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-w)
                (set-interrupt-flag gb 4)
                (setf (gbinput-up input) nil))
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-a)
                (set-interrupt-flag gb 4)
                (setf (gbinput-left input) nil))
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-s)
                (set-interrupt-flag gb 4)
                (setf (gbinput-down input) nil))
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-d)
                (set-interrupt-flag gb 4)
                (setf (gbinput-right input) nil))
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-space)
                (setf (gbinput-start input) nil)))
            (:idle ()
              (when (not (gb-stopped? gb))
                (loop until (> (gbppu-cur-line ppu) 153)
                  do
                  (let ((instr (emu-single-op cpu gb)))
                    (when (not instr) (sdl2:push-event :quit) (return nil))
                    (when (and *debug* (instruction-p instr))
                       (format t "~X: ~A --> PC=~X~%" (instruction-opcode instr) (instruction-asm instr) (gbcpu-pc cpu)))
                    ;(when (= (gbcpu-pc cpu) #x1e7e) (sdl2:push-event :quit) (return nil))
                    (if (= (gbcpu-pc cpu) #x100) (setf (gb-is-bios? gb) nil)))
                    (if (= (read-memory-at-addr gb #xff02) #x81)
                      (progn
                        (setf *out* (cons (code-char (read-memory-at-addr gb #xff01)) *out*))
                        (write-memory-at-addr gb #xff02 0)))
                    (step-ppu ppu gb renderer texture)
                    (handle-timers cpu gb)
                    (handle-interrupts cpu gb)
                    (update-input-memory gb input)))
              (setf (gbppu-cur-line ppu) 0)
              (sdl2:render-copy renderer texture)
              (sdl2:render-present renderer))
            (:quit () t))))))))


(defparameter *gb* (make-gb))

(defun gb-reset ()
  (setf *gb* (make-gb))
  (replace-memory-with-rom (gb-cart *gb*) loaded-rom)
  (get-carttype-from-rom (gb-cart *gb*))
  (get-rommask-from-rom (gb-cart *gb*))
  (write-memory-at-addr *gb* #xff00 #xff))

(defun dump-mem-region (start end)
  (loop for a from start to end
        collect (read-memory-at-addr *gb* a)))

(defun dump-oam ()
  (dump-mem-region #xfe00 #xfea0))

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

(replace-memory-with-rom (gb-cart *gb*) loaded-rom)
(get-carttype-from-rom (gb-cart *gb*))
(get-rommask-from-rom (gb-cart *gb*))

(write-memory-at-addr *gb* #xff00 #xff)

(defun run () (emu-main *gb*))

