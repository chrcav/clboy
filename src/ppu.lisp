
(in-package :clboy)

(defconstant +oam-duration-dots+ 80)
(defconstant +vblank-duration-dots+ 4560)
(defconstant +hblank-duration-dots+ 204)
(defconstant +draw-duration-dots+ 172)
(defconstant +screen-pixel-width+ 160)
(defconstant +screen-pixel-height+ 144)
(defconstant +tilemap-pixel-width+ 256)
(defconstant +tilemap-pixel-height+ 256)
(defconstant +tilemap-tile-width+ 32)
(defconstant +tilemap-tile-height+ 32)

(defstruct ppulcdc
  "LCDC register struct"
  (byte-rep 0 :type (unsigned-byte 8))
  (enabled? t :type boolean)
  (win-tilemap-area #x9800)
  (window-enabled? nil :type boolean)
  (tiledata-area #x9000)
  (bg-tilemap-area #x9800)
  (sprite-height 8)
  (obj-enabled? nil :type boolean)
  (bgwin-enabled? t :type boolean))

(defstruct ppustat
  "STAT register struct"
  (lyc-int-enabled? nil :type boolean)
  (mode2-int-enabled? nil :type boolean)
  (mode1-int-enabled? nil :type boolean)
  (mode0-int-enabled? nil :type boolean))

(defstruct gbppu
  "main PPU struct for handling video output"
  (framebuffer (static-vectors:make-static-vector (* +screen-pixel-width+ +screen-pixel-height+ 3)))
  (framebuffer-a (static-vectors:make-static-vector (* +screen-pixel-width+ +screen-pixel-height+ 4)))
  (bg-buffer (make-array (* +tilemap-pixel-width+ +tilemap-pixel-height+) :initial-element 0 :element-type '(unsigned-byte 8)))
  (cycles 0)
  (cur-line 0)
  (cur-line-comp 0)
  (oam (make-array #x100 :initial-element 0 :element-type '(unsigned-byte 8)))
  (vram (make-array #x2000 :initial-element 0 :element-type '(unsigned-byte 8)))
  (scy 0)
  (scx 0)
  (wy 0)
  (wx 0)
  (mode 0)
  (renderer nil)
  (render-rect nil)
  (texture nil)
  (do-dma 0 :type (unsigned-byte 8))
  (lcdc (make-ppulcdc))
  (stat (make-ppustat))
  (bg-palette 0)
  (obj-palette0 0)
  (obj-palette1 0)
  (enabled? t :type boolean))

(defparameter *colors* #((255 255 255) (192 192 192) (96 96 96) (0  0  0)))

(defun gbppu-reset (ppu)
  "resets the PPU slots"
  (setf (gbppu-scy ppu) 0
        (gbppu-scx ppu) 0
        (gbppu-wy ppu) 0
        (gbppu-wx ppu) 0
        (gbppu-wx ppu) 0
        (gbppu-lcdc ppu) (make-ppulcdc)))

(defun ppu-write-lcdc (ppu val)
  "saves the lcdc byte as a ppulcdc struct in PPU based on VAL"
  (setf (gbppu-lcdc ppu)
        (make-ppulcdc
          :byte-rep val
          :enabled? (> (logand val #x80) 0)
          :win-tilemap-area (if (= (logand val #x40) 0) #x9800 #x9c00)
          :window-enabled? (> (logand val #x20) 0)
          :tiledata-area (if (= (logand val #x10) #x10) #x8000 #x9000)
          :bg-tilemap-area (if (= (logand val #x08) 0) #x9800 #x9c00)
          :sprite-height (if (= (logand val #x04) #x04) 16 8)
          :obj-enabled? (> (logand val #x02) 0)
          :bgwin-enabled? (> (logand val #x01) 0))
        (gbppu-enabled? ppu) (> (logand val #x80) 0)))

(defun ppu-read-lcdc (lcdc)
  "constructs a byte from the ppulcdc struct and PPU"
  (logior (ppulcdc-byte-rep lcdc)
          (ash (clboy-utils:bool-as-bit (ppulcdc-enabled? lcdc)) 7)
          (ash (clboy-utils:bool-as-bit (ppulcdc-window-enabled? lcdc)) 5)
          (ash (clboy-utils:bool-as-bit (ppulcdc-obj-enabled? lcdc)) 1)
          (clboy-utils:bool-as-bit (ppulcdc-bgwin-enabled? lcdc))))

(defun ppu-write-stat (ppu val)
  "saves the stat byte as a ppustat struct in PPU based on VAL"
  (setf (gbppu-stat ppu)
        (make-ppustat
          :lyc-int-enabled? (= (logand val #x40) #x40)
          :mode2-int-enabled? (= (logand val #x20) #x20)
          :mode1-int-enabled? (= (logand val #x10) #x10)
          :mode0-int-enabled? (= (logand val #x08) #x08))))

(defun ppu-read-stat (ppu)
  "constructs a byte from the ppustat struct and PPU"
  (logior (ash (clboy-utils:bool-as-bit (ppustat-lyc-int-enabled? (gbppu-stat ppu))) 6)
          (ash (clboy-utils:bool-as-bit (ppustat-mode2-int-enabled? (gbppu-stat ppu))) 5)
          (ash (clboy-utils:bool-as-bit (ppustat-mode1-int-enabled? (gbppu-stat ppu))) 4)
          (ash (clboy-utils:bool-as-bit (ppustat-mode0-int-enabled? (gbppu-stat ppu))) 3)
          (ash (clboy-utils:bool-as-bit (= (gbppu-cur-line ppu) (gbppu-cur-line-comp ppu))) 2)
          (gbppu-mode ppu)))

(defun ppu-write-memory-at-addr (ppu addr val)
  "writes memory from PPU at ADDR which can be vram, oam, or PPU registers to VAL"
  (ecase (logand addr #xf000)
    ((#x8000 #x9000)
      (setf (aref (gbppu-vram ppu) (logand addr #x1fff)) val))
    (#xf000
     (case (logand addr #x0f00)
       (#xe00 (if (< addr #xfea0) (setf (aref (gbppu-oam ppu) (logand addr #xff)) val)))
       (#xf00
        (case (logand addr #x00ff)
          (#x40 (ppu-write-lcdc ppu val))
          (#x41 (ppu-write-stat ppu val))
          (#x42 (setf (gbppu-scy ppu) val))
          (#x43 (setf (gbppu-scx ppu) val))
          (#x45 (setf (gbppu-cur-line-comp ppu) val))
          (#x46 (setf (gbppu-do-dma ppu) val))
          (#x47 (setf (gbppu-bg-palette ppu) val))
          (#x48 (setf (gbppu-obj-palette0 ppu) val))
          (#x49 (setf (gbppu-obj-palette1 ppu) val))
          (#x4a (setf (gbppu-wy ppu) val))
          (#x4b (setf (gbppu-wx ppu) val))
          (otherwise ())))))))

(defun ppu-read-memory-at-addr (ppu addr)
  "reads memory from PPU at ADDR which can be vram, oam, or PPU registers"
  (ecase (logand addr #xf000)
    ((#x8000 #x9000)
      (aref (gbppu-vram ppu) (logand addr #x1fff)))
    (#xf000
     (case (logand addr #x0f00)
       (#xe00 (if (< addr #xfea0) (aref (gbppu-oam ppu) (logand addr #xff)) 0))
       (#xf00
        (case (logand addr #x00ff)
          (#x40 (ppu-read-lcdc (gbppu-lcdc ppu)))
          (#x41 (ppu-read-stat ppu))
          (#x42 (gbppu-scy ppu))
          (#x43 (gbppu-scx ppu))
          (#x44 (gbppu-cur-line ppu))
          (#x45 (gbppu-cur-line-comp ppu))
          (#x46 (gbppu-do-dma ppu))
          (#x47 (gbppu-bg-palette ppu))
          (#x48 (gbppu-obj-palette0 ppu))
          (#x48 (gbppu-obj-palette1 ppu))
          (#x4a (gbppu-wy ppu))
          (#x4b (gbppu-wx ppu))
          (otherwise #xff)))))))

(defun read-sprite (ppu addr)
  "reads the 4 bytes of a sprite at ADDR. sprite y location, sprite x location, sprite tile index,
  and sprite flags"
  (loop for a from addr to (+ addr 3)
        collect (ppu-read-memory-at-addr ppu a)))

(defun sprite-overlaps-scanline? (sprite row sprite-height)
  (and (>= row (- (car sprite) 16)) (< row (+ (- (car sprite) 16) sprite-height))))

(defun sprite-within-viewport? (ppu sprite)
  (and (> (cadr sprite) (gbppu-scx ppu))
       (< (cadr sprite) (+ (gbppu-scx ppu) 8))
       (> (car sprite) (gbppu-scy ppu))
       (< (car sprite) (+ (gbppu-scy ppu) (ppulcdc-sprite-height (gbppu-lcdc ppu))))))

(defun add-sprites-to-ppu-framebuffer (ppu)
  "loops through oam and adds sprites to framebuffer that overlap the current scanline location."
  (loop for addr = #xfe00 then (+ addr 4)
        while (< addr #xfea0)
        for sprite = (read-sprite ppu addr)
        when (sprite-overlaps-scanline? sprite (gbppu-cur-line ppu) (ppulcdc-sprite-height (gbppu-lcdc ppu)))
        do (add-sprite-to-ppu-framebuffer ppu sprite)))

(defun calc-sprite-y-offset (row sprite-y)
   (- row (- sprite-y 16)))

(defun calc-sprite-tile-addr (tile-no sprite-flags sprite-y-offset sprite-height)
  "calculates the memory address of a sprites tile pixel data"
  (+ #x8000 (* tile-no 16)
     (* (if (> (logand sprite-flags #x40) 0)
          (- sprite-height sprite-y-offset) sprite-y-offset) 2)))

(defun add-sprite-to-ppu-framebuffer (ppu sprite)
  "adds a row of pixels from the visible portion of a sprite corresponding to the scanline
  location."
  (let ((tile-no (if (= (ppulcdc-sprite-height (gbppu-lcdc ppu)) 16) (logand (caddr sprite) #xfe) (caddr sprite)))
        (sprite-y-offset (calc-sprite-y-offset (gbppu-cur-line ppu) (car sprite))))
    (render-tile-line ppu
                      (gbppu-framebuffer ppu)
                      (gbppu-cur-line ppu)
                      (calc-sprite-tile-addr tile-no (cadddr sprite) sprite-y-offset (ppulcdc-sprite-height (gbppu-lcdc ppu)))
                      :start-x (- (cadr sprite) 8)
                      :xflip? (> (logand (cadddr sprite) #x20) 0)
                      :priority (ash (cadddr sprite) -7)
                      :palette (if (= (logand (cadddr sprite) #x10) #x00)
                                 (gbppu-obj-palette0 ppu)
                                 (gbppu-obj-palette1 ppu)))))

(defun render-tile-line (ppu framebuffer row tile-row-addr
                         &key (start-x 0) (xflip? nil) (is-background? nil) (priority 0)
                         (palette 0) (framebuffer-width +screen-pixel-width+))
  "adds a row of pixels from the visible portion of a tile corresponding to the scanline
  location."
  (let ((colorbyte1 (ppu-read-memory-at-addr ppu tile-row-addr))
        (colorbyte2 (ppu-read-memory-at-addr ppu (+ tile-row-addr 1))))
  (loop for i from 0 to 7
        for col = (+ start-x i)
        when (and (>= col 0) (< col framebuffer-width))
    do
    (let* ((colorbitpos (if xflip? (- i) (- i 7)))
          (colorval (+
                       (logand (ash colorbyte1 colorbitpos) #x01)
                       (* (logand (ash colorbyte2 colorbitpos) #x01) 2))))
      (when (or is-background?
                (and (> colorval #x00)
                     (or (= (aref (gbppu-bg-buffer ppu) (+ (* row +screen-pixel-width+) col)) #x00)
                         (= priority #x00) )))
        (setf (aref (gbppu-bg-buffer ppu) (+ (* row framebuffer-width) col)) colorval)
        (let ((palette-col (logand (ash palette (* colorval -2)) 3)))
          (replace framebuffer (aref *colors* palette-col)
                   :start1 (+ (* row framebuffer-width 3) (* col 3)))
          ;(setf (aref framebuffer (+ (* row framebuffer-width 3) (* col 3))) (car (aref *colors* palette-col)))
          ;(setf (aref framebuffer (+ (* row framebuffer-width 3) (* col 3) 1)) (cadr (aref *colors* palette-col)))
          ;(setf (aref framebuffer (+ (* row framebuffer-width 3) (* col 3) 2)) (caddr (aref
          ;*colors* palette-col)))
          ))))))

(defun add-window-to-ppu-framebuffer (ppu)
  "adds a row of pixels from the visible portion of the window corresponding to the scanline
  location."
  (let ((row (gbppu-cur-line ppu)))
    (when (and (< row +screen-pixel-height+)
               (>= row (gbppu-wy ppu))
               (>= (gbppu-wy ppu) 0) (< (gbppu-wy ppu) (+ +screen-pixel-height+ 7))
               (>= (gbppu-wx ppu) 0) (< (gbppu-wx ppu) +screen-pixel-width+))
    (loop for col = (- (gbppu-wx ppu) 7) then (+ col 8)
          for tilemapx from 0 to 31
          when (and (>= col 0) (< col +screen-pixel-width+))
      do
      (let* ((addr (+ (ppulcdc-win-tilemap-area (gbppu-lcdc ppu)) (* (floor (- row (gbppu-wy ppu)) 8) 32) tilemapx))
             (tile-no (if (= (ppulcdc-tiledata-area (gbppu-lcdc ppu)) #x8000)
                        (ppu-read-memory-at-addr ppu addr)
                        (make-signed-from-unsigned (ppu-read-memory-at-addr ppu addr)))))
        (render-tile-line ppu
                          (gbppu-framebuffer ppu)
                          row
                          (+ (ppulcdc-tiledata-area (gbppu-lcdc ppu)) (* tile-no #x10) (* (mod (- row (gbppu-wy ppu)) 8) 2))
                          :start-x col
                          :is-background? t
                          :palette (gbppu-bg-palette ppu)))))))

(defun add-background-to-ppu-framebuffer (ppu)
  "adds a row of pixels from the visible portion of the background corresponding to the scanline
  location."
  (when (< (gbppu-cur-line ppu) +screen-pixel-height+)
    (loop for xoffset = (gbppu-scx ppu) then (+ xoffset 8)
          for col = (- (mod (gbppu-scx ppu) 8)) then (+ col 8)
          while (< col +screen-pixel-width+) do
      (let* ((yoffset (+ (gbppu-cur-line ppu) (gbppu-scy ppu)))
             (addr (+ (ppulcdc-bg-tilemap-area (gbppu-lcdc ppu))
                      (mod (* (floor yoffset 8) +tilemap-tile-width+)
                           (* +tilemap-tile-width+ +tilemap-tile-height+))
                      (mod (floor xoffset 8) +tilemap-tile-width+)))
             (tile-no (if (= (ppulcdc-tiledata-area (gbppu-lcdc ppu)) #x8000)
                        (ppu-read-memory-at-addr ppu addr)
                        (make-signed-from-unsigned (ppu-read-memory-at-addr ppu addr)))))
        (render-tile-line ppu
                          (gbppu-framebuffer ppu)
                          (gbppu-cur-line ppu)
                          (+ (ppulcdc-tiledata-area (gbppu-lcdc ppu)) (* tile-no 16) (* (mod yoffset 8) 2))
                          :start-x col
                          :is-background? t
                          :palette (gbppu-bg-palette ppu))))))

(defun maybe-do-dma (ppu gb)
  "checks for DMA and processed copying memory into OAM"
  (let ((initial (ash (gbppu-do-dma ppu) 8)))
    (when (> initial 0)
      (loop for i from 0 to (- +screen-pixel-width+ 1)
            do
            (let ((src (+ initial i))
                  (dest (+ #xfe00 i)))
            (write-memory-at-addr gb dest (read-memory-at-addr gb src))))
      (setf (gbppu-do-dma ppu) 0))))

(defun check-ly-lyc (ppu gb)
  "compares ly and lyc registers, triggers interrupt if they match and the interrupt is enabled"
  (when (= (gbppu-cur-line ppu) (gbppu-cur-line-comp ppu))
    (if (ppustat-lyc-int-enabled? (gbppu-stat ppu)) (set-interrupt-flag gb 1))))

(defun ppu-mode-transition (ppu gb mode)
  "transitions PPU to MODE 0-3 triggers interrupt if enabled"
  (setf (gbppu-cycles ppu) 0)
  (setf (gbppu-mode ppu) mode)
  (if (stat-int-enabled-for-mode? (gbppu-stat ppu) mode) (set-interrupt-flag gb 1)))

(defun stat-int-enabled-for-mode? (stat mode)
  "predicate for ppu MODE interrupt enabled flags in STAT"
  (case mode
    (0 (ppustat-mode0-int-enabled? stat))
    (1 (ppustat-mode1-int-enabled? stat))
    (2 (ppustat-mode2-int-enabled? stat))
    (otherwise nil)))


(defun render-scanline (ppu)
  "processes the background, window, and sprites based on lcdc flags and video memory"
  (when (ppulcdc-bgwin-enabled? (gbppu-lcdc ppu)) ; TODO this should trigger the background losing priority
    (add-background-to-ppu-framebuffer ppu)
    (when (ppulcdc-window-enabled? (gbppu-lcdc ppu))
      (add-window-to-ppu-framebuffer ppu)))
  (when (ppulcdc-obj-enabled? (gbppu-lcdc ppu))
    (add-sprites-to-ppu-framebuffer ppu)))

(defun render-full-background (ppu)
  "Provides a way to draw the entire 256x256 background stored in the tilemap."
  (let ((texture (sdl2:create-texture (gbppu-renderer ppu) :rgb24 :streaming +tilemap-pixel-width+ +tilemap-pixel-height+))
        (framebuffer (static-vectors:make-static-vector (* +tilemap-pixel-width+ +tilemap-pixel-height+ 3))))
    (loop for row from 0 to (- +tilemap-pixel-height+ 1)
          when (< row +tilemap-pixel-height+) do
          (loop for col = 0 then (+ col 8)
                while (< col +tilemap-pixel-width+) do
        (let* ((addr (+ (ppulcdc-bg-tilemap-area (gbppu-lcdc ppu))
                        (* (floor row 8) +tilemap-tile-width+) (floor col 8)))
               (tile-no (if (= (ppulcdc-tiledata-area (gbppu-lcdc ppu)) #x8000)
                          (ppu-read-memory-at-addr ppu addr)
                          (make-signed-from-unsigned (ppu-read-memory-at-addr ppu addr)))))
          (render-tile-line ppu
                            framebuffer
                            row
                            (+ (ppulcdc-tiledata-area (gbppu-lcdc ppu)) (* tile-no 16) (* (mod row 8) 2))
                            :start-x col
                            :is-background? t
                            :framebuffer-width +tilemap-pixel-width+
                            :palette (gbppu-bg-palette ppu)))))
    (sdl2:update-texture
      texture
      (cffi:null-pointer)
      (static-vectors:static-vector-pointer
        framebuffer) (* +tilemap-pixel-width+ 3))
    (sdl2:render-copy (gbppu-renderer ppu) texture)
    (sdl2:render-present (gbppu-renderer ppu))))

(defun update-screen (ppu renderer texture rect)
  "takes the current framebuffer and copies it into the TEXTURE"
  (sdl2:update-texture
    texture
    (cffi:null-pointer)
    (static-vectors:static-vector-pointer (gbppu-framebuffer ppu)) (* +screen-pixel-width+ *scale*))
  (sdl2:render-clear renderer)
  (sdl2:render-copy renderer texture :dest-rect rect)
  (sdl2:render-present renderer))

(defun step-ppu (ppu gb)
  "handles updating the PPU mode and drawing to a texture when the defined number of cycles have
  elapsed. transitions the PPU through the modes and processes vram."
  (when (and (not (gbppu-enabled? ppu))
             (> (gbppu-cycles ppu) 0))
    (setf (gbppu-cycles ppu) 0)
    (ppu-mode-transition ppu gb 0))
  (when (gbppu-enabled? ppu)
    (incf (gbppu-cycles ppu) (gbcpu-clock (gb-cpu gb)))
    (maybe-do-dma ppu gb)
    (check-ly-lyc ppu gb)
    (case (gbppu-mode ppu)
      ; in Hblank state
      (0 (when (> (gbppu-cycles ppu) +hblank-duration-dots+)
           (incf (gbppu-cur-line ppu))
           (if (> (gbppu-cur-line ppu) (- +screen-pixel-height+ 1))
             (progn (ppu-mode-transition ppu gb 1)
                    (set-interrupt-flag gb 0)
                    (update-screen ppu (gbppu-renderer ppu) (gbppu-texture ppu) (gbppu-render-rect ppu))
                    )
             (ppu-mode-transition ppu gb 2))))
      ; in Vblank state
      (1 (when (> (gbppu-cycles ppu) +vblank-duration-dots+)
           (incf (gbppu-cur-line ppu))
           (when (> (gbppu-cur-line ppu) (+ +screen-pixel-height+ 9))
             (setf (gbppu-cur-line ppu) 0)
             (ppu-mode-transition ppu gb 2))))
      ; in OAM state
      (2 (when (> (gbppu-cycles ppu) +oam-duration-dots+)
           (ppu-mode-transition ppu gb 3)))
      ; in VRAM Read state
      (3 (when (> (gbppu-cycles ppu) +draw-duration-dots+)
           (render-scanline ppu)
           (ppu-mode-transition ppu gb 0)))))
  t)
