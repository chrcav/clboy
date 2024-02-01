
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
  (win-tilemap-area #x1800)
  (window-enabled? nil :type boolean)
  (tiledata-area #x1000)
  (bg-tilemap-area #x1800)
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
  (vram-bank 0 :type (unsigned-byte 8))
  (scy 0)
  (scx 0)
  (wy 0)
  (wx 0)
  (mode 0)
  (renderer nil)
  (render-rect nil)
  (texture nil)
  (do-oam-dma 0 :type (unsigned-byte 8))
  (lcdc (make-ppulcdc))
  (stat (make-ppustat))
  (bg-palette 0)
  (obj-palette0 0)
  (obj-palette1 0)
  (enabled? t :type boolean))

(defstruct (cgbppu (:include gbppu
                    (vram (make-array #x4000 :initial-element 0 :element-type '(unsigned-byte 8)))))
  (bg-cram (make-array #x40 :initial-element 0 :element-type '(unsigned-byte 8)))
  (obj-cram (make-array #x40 :initial-element 0 :element-type '(unsigned-byte 8)))
  (bcps-bgpi #xff :type (unsigned-byte 8))
  (ocps-obpi #xff :type (unsigned-byte 8))
  (hdma12 #x0000 :type (unsigned-byte 16))
  (hdma34 #x0000 :type (unsigned-byte 16))
  (hdma-len #x000 :type (unsigned-byte 12))
  (vram-dma-type 0 :type (unsigned-byte 8))
  (opri #x00 :type (unsigned-byte 8)))


(defparameter *colors* #((255 255 255) (192 192 192) (96 96 96) (0  0  0)))

(defun ppu-get-palette-color (cram palette index)
    (ppu-get-cram-palette-color cram palette index))

(defun ppu-get-cram-palette-color (cram palette index)
  (let ((color (logior (aref cram (+ (* palette 8) (* index 2))) (ash (aref cram (+ (* palette 8) (* index 2) 1)) 8))))
    (list
      (round (* (/ 255 31) (logand color #x1f)))
      (round (* (/ 255 31) (logand (ash color -5) #x1f)))
      (round (* (/ 255 31) (logand (ash color -10) #x1f))))))

(defun gbppu-reset (ppu)
  "resets the PPU slots"
  (setf (gbppu-scy ppu) 0
        (gbppu-scx ppu) 0
        (gbppu-wy ppu) 0
        (gbppu-wx ppu) 0
        (gbppu-wx ppu) 0
        (gbppu-vram-bank ppu) 0
        (gbppu-lcdc ppu) (make-ppulcdc))
  (when (cgbppu-p ppu)
    (setf (cgbppu-hdma12 ppu) #x0000
          (cgbppu-hdma34 ppu) #x0000
          (cgbppu-hdma-len ppu) #x000
          (cgbppu-vram-dma-type ppu) 0)))

(defun ppu-write-lcdc (ppu val)
  "saves the lcdc byte as a ppulcdc struct in PPU based on VAL"
  (setf (gbppu-lcdc ppu)
        (make-ppulcdc
          :byte-rep val
          :enabled? (> (logand val #x80) 0)
          :win-tilemap-area (if (= (logand val #x40) 0) #x1800 #x1c00)
          :window-enabled? (> (logand val #x20) 0)
          :tiledata-area (if (= (logand val #x10) #x10) #x0000 #x1000)
          :bg-tilemap-area (if (= (logand val #x08) 0) #x1800 #x1c00)
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
    ((#x0000 #x1000 #x2000 #x3000)
      (setf (aref (gbppu-vram ppu) (logand addr #x3fff)) val))
    ((#x8000 #x9000)
      (setf (aref (gbppu-vram ppu) (+ (logand addr #x1fff) (if (= (gbppu-vram-bank ppu) #x01) #x2000 #x0000))) val))
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
          (#x46 (setf (gbppu-do-oam-dma ppu) val))
          (#x47 (setf (gbppu-bg-palette ppu) val))
          (#x48 (setf (gbppu-obj-palette0 ppu) val))
          (#x49 (setf (gbppu-obj-palette1 ppu) val))
          (#x4a (setf (gbppu-wy ppu) val))
          (#x4b (setf (gbppu-wx ppu) val))
          ((#x4f #x51 #x52 #x53 #x54 #x55 #x68 #x69 #x6a #x6b #x6c)
           (if (cgbppu-p ppu)
               (case (logand addr #x00ff)
                 (#x4f (setf (gbppu-vram-bank ppu) (logand val #x01)))
                 (#x51 (setf (cgbppu-hdma12 ppu) (logior (logand (cgbppu-hdma12 ppu) #x00ff) (ash val 8))))
                 (#x52 (setf (cgbppu-hdma12 ppu) (logior (logand (cgbppu-hdma12 ppu) #xff00) (logand val #xf0))))
                 (#x53 (setf (cgbppu-hdma34 ppu) (logior (logand (cgbppu-hdma34 ppu) #x00ff) (+ (logand (ash val 8) #x1f00) #x8000))))
                 (#x54 (setf (cgbppu-hdma34 ppu) (logior (logand (cgbppu-hdma34 ppu) #xff00) (logand val #xf0))))
                 (#x55 (setf (cgbppu-hdma-len ppu) (ash (logand (+ val 1) #x7f) 4)
                             (cgbppu-vram-dma-type ppu) (if (= (cgbppu-vram-dma-type ppu) 0)
                                                            (if (= (logand val #x80) #x80) 2 1)
                                                            (if (= (logand val #x80) #x00) 0)))
                  (format t "hdma transfer type ~A requested: from x~X to x~X len ~A bytes~%"
                          (cgbppu-vram-dma-type ppu)
                          (cgbppu-hdma12 ppu)
                          (cgbppu-hdma34 ppu)
                          (cgbppu-hdma-len ppu)))
                 (#x68 (setf (cgbppu-bcps-bgpi ppu) val))
                 (#x69 (setf (aref (cgbppu-bg-cram ppu) (logand (cgbppu-bcps-bgpi ppu) #x3f)) val
                             (cgbppu-bcps-bgpi ppu) (if (> (cgbppu-bcps-bgpi ppu) #x7f) (+ (cgbppu-bcps-bgpi ppu) 1) (cgbppu-bcps-bgpi ppu))))
                 (#x6a (setf (cgbppu-ocps-obpi ppu) val))
                 (#x6b (setf (aref (cgbppu-obj-cram ppu) (logand (cgbppu-ocps-obpi ppu) #x3f)) val
                             (cgbppu-ocps-obpi ppu) (if (> (cgbppu-ocps-obpi ppu) #x7f) (+ (cgbppu-ocps-obpi ppu) 1) (cgbppu-ocps-obpi ppu))))
                 (#x6c (setf (cgbppu-opri ppu) val)))))
          (otherwise ())))))))

(defun ppu-read-memory-at-addr (ppu addr)
  "reads memory from PPU at ADDR which can be vram, oam, or PPU registers"
  (ecase (logand addr #xf000)
    ((#x0000 #x1000 #x2000 #x3000)
      (aref (gbppu-vram ppu) (logand addr #x3fff)))
    ((#x8000 #x9000)
      (aref (gbppu-vram ppu) (+ (logand addr #x1fff) (if (= (gbppu-vram-bank ppu) #x01) #x2000 #x0000))))
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
          (#x46 (gbppu-do-oam-dma ppu))
          (#x47 (gbppu-bg-palette ppu))
          (#x48 (gbppu-obj-palette0 ppu))
          (#x48 (gbppu-obj-palette1 ppu))
          (#x4a (gbppu-wy ppu))
          (#x4b (gbppu-wx ppu))
          (#x4f (logior (gbppu-vram-bank ppu) #xef))
          ((#x51 #x52 #x53 #x54 #x55 #x68 #x69 #x6a #x6b #x6c)
           (if (cgbppu-p ppu)
               (case (logand addr #x00ff)
                 ((#x51 #x52 #x53 #x54) #xff)
                 (#x55 (if (= (cgbppu-vram-dma-type ppu) 0) #xff (- (ash (cgbppu-hdma-len ppu) -4) 1)))
                 (#x68 (cgbppu-bcps-bgpi ppu))
                 (#x69 (aref (cgbppu-bg-cram ppu) (cgbppu-bcps-bgpi ppu)))
                 (#x6a (cgbppu-ocps-obpi ppu))
                 (#x6b (aref (cgbppu-obj-cram ppu) (cgbppu-ocps-obpi ppu)))
                 (#x6c (cgbppu-opri ppu)))))
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

(defun calc-sprite-tile-addr (tile-no sprite-flags sprite-y-offset sprite-height &key (is-cgb? nil))
  "calculates the memory address of a sprites tile pixel data"
  (+ (if (and is-cgb? (= (logand sprite-flags #x8) #x8)) #x2000 #x0000)
     (* tile-no 16)
     (* (if (> (logand sprite-flags #x40) 0)
            (- sprite-height sprite-y-offset)
            sprite-y-offset)
        2)))

(defun calc-sprite-tile-no (ppu sprite)
  (if (= (ppulcdc-sprite-height (gbppu-lcdc ppu)) 16) (logand (caddr sprite) #xfe) (caddr sprite)))

(defun add-sprite-to-ppu-framebuffer (ppu sprite)
  "adds a row of pixels from the visible portion of a sprite corresponding to the scanline
  location."
  (let ((sprite-y-offset (calc-sprite-y-offset (gbppu-cur-line ppu) (car sprite))))
    (render-tile-line ppu
                      (gbppu-framebuffer ppu)
                      (gbppu-cur-line ppu)
                      (calc-sprite-tile-addr (calc-sprite-tile-no ppu sprite) (cadddr sprite) sprite-y-offset (ppulcdc-sprite-height (gbppu-lcdc ppu)) :is-cgb? (cgbppu-p ppu))
                      :start-x (- (cadr sprite) 8)
                      :xflip? (> (logand (cadddr sprite) #x20) 0)
                      :priority (ash (cadddr sprite) -7)
                      :cram (if (cgbppu-p ppu) (cgbppu-obj-cram ppu))
                      :palette (if (cgbppu-p ppu)
                                   (logand (cadddr sprite) #x7)
                                   (if (= (logand (cadddr sprite) #x10) #x00)
                                       (gbppu-obj-palette0 ppu)
                                       (gbppu-obj-palette1 ppu))))))

(defun render-tile-line (ppu framebuffer row tile-row-addr
                         &key (start-x 0) (xflip? nil) (is-background? nil) (priority 0) (cram nil)
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
          (replace framebuffer 
                   (if cram
                       (ppu-get-palette-color cram palette colorval)
                       (aref *colors* palette-col))
                   :start1 (+ (* row framebuffer-width 3) (* col 3)))
          ;(setf (aref framebuffer (+ (* row framebuffer-width 3) (* col 3))) (car
          ;(ppu-get-palette-color ppu cram palette-col)))
          ;(setf (aref framebuffer (+ (* row framebuffer-width 3) (* col 3) 1)) (cadr
          ;(ppu-get-palette-color ppu cram palette-col)))
          ;(setf (aref framebuffer (+ (* row framebuffer-width 3) (* col 3) 2)) (caddr
          ;(ppu-get-palette-color ppu cram palette-col)))
          ))))))

(defun calc-bg-tile-no (ppu addr)
  (if (= (ppulcdc-tiledata-area (gbppu-lcdc ppu)) #x0000)
      (ppu-read-memory-at-addr ppu addr)
      (make-signed-from-unsigned (ppu-read-memory-at-addr ppu addr))))

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
      (let ((addr (+ (ppulcdc-win-tilemap-area (gbppu-lcdc ppu)) (* (floor (- row (gbppu-wy ppu)) 8) 32) tilemapx)))
        (render-tile-line ppu
                          (gbppu-framebuffer ppu)
                          row
                          (+ (if (and
                                   (cgbppu-p ppu)
                                   (= (logand (ppu-read-memory-at-addr ppu (+ addr #x2000)) #x8) #x8))
                                 #x2000
                                 #x0000)
                             (ppulcdc-tiledata-area (gbppu-lcdc ppu))
                             (* (calc-bg-tile-no ppu addr) #x10)
                             (* (mod (- row (gbppu-wy ppu)) 8) 2))
                          :start-x col
                          :xflip? (and (cgbppu-p ppu) (> (logand (ppu-read-memory-at-addr ppu (+ addr #x2000)) #x20) 0))
                          :is-background? t
                          :cram (if (cgbppu-p ppu) (cgbppu-bg-cram ppu))
                          :palette (if (cgbppu-p ppu) (logand (ppu-read-memory-at-addr ppu (+ addr #x2000)) #x7) (gbppu-bg-palette ppu))))))))

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
                      (mod (floor xoffset 8) +tilemap-tile-width+))))
        (render-tile-line ppu
                          (gbppu-framebuffer ppu)
                          (gbppu-cur-line ppu)
                          (+ (if (and
                                   (cgbppu-p ppu)
                                   (= (logand (ppu-read-memory-at-addr ppu (+ addr #x2000)) #x8) #x8))
                                 #x2000
                                 #x0000)
                             (ppulcdc-tiledata-area (gbppu-lcdc ppu))
                             (* (calc-bg-tile-no ppu addr) #x10)
                             (* (mod yoffset 8) 2))
                          :start-x col
                          :xflip? (and
                                    (cgbppu-p ppu)
                                    (> (logand (ppu-read-memory-at-addr ppu (+ addr #x2000)) #x20) 0))
                          :is-background? t
                          :cram (if (cgbppu-p ppu) (cgbppu-bg-cram ppu))
                          :palette (if (cgbppu-p ppu)
                                       (logand (ppu-read-memory-at-addr ppu (+ addr #x2000)) #x7)
                                       (gbppu-bg-palette ppu)))))))

;;TODO should only transfer data based on the cycles that have passed for all dma transfers
(defun maybe-do-oam-dma (ppu gb cycles)
  "checks for DMA and processes copying memory into OAM"
  (let ((initial (ash (gbppu-do-oam-dma ppu) 8)))
    (when (> initial 0)
      (loop for i from 0 to (- +screen-pixel-width+ 1)
            do
            (let ((src (+ initial i))
                  (dest (+ #xfe00 i)))
              (write-memory-at-addr gb dest (read-memory-at-addr gb src))))
      (setf (gbppu-do-oam-dma ppu) 0))))

(defun maybe-do-gen-dma (ppu gb cycles)
  "checks for general purpose vram DMA and processes copying memory into VRAM"
  (when (= (cgbppu-vram-dma-type ppu) 1)
    (let ((len (cgbppu-hdma-len ppu))
          (transfer-bytes (* cycles 2))
          (start-src (cgbppu-hdma12 ppu))
          (start-dest (cgbppu-hdma34 ppu)))
      (when (> len 0)
        (loop for i from 0 to transfer-bytes
              do
              (let ((src (+ start-src i))
                    (dest (+ start-dest i)))
                (write-memory-at-addr gb dest (read-memory-at-addr gb src))))
        (if (< (- len transfer-bytes) 0)
            (setf (cgbppu-hdma-len ppu)  #x00
                  (cgbppu-vram-dma-type ppu) 0
                  (cgbppu-hdma12 ppu) #xffff
                  (cgbppu-hdma34 ppu) #xffff)
            (setf (cgbppu-hdma-len ppu)  (- len transfer-bytes)
                  (cgbppu-hdma12 ppu) (+ start-src transfer-bytes)
                  (cgbppu-hdma34 ppu) (+ start-dest transfer-bytes)))))))

(defun maybe-do-h-dma (ppu gb cycles)
  "checks for hblank vram DMA and processes copying memory into VRAM"
  (when (= (cgbppu-vram-dma-type ppu) 2)
    (let ((len (cgbppu-hdma-len ppu))
          (transfer-bytes (* cycles 2))
          (start-src (cgbppu-hdma12 ppu))
          (start-dest (cgbppu-hdma34 ppu)))
      (when (> len 0)
        (loop for i from 0 to transfer-bytes
              do
              (let ((src (+ start-src i))
                    (dest (+ start-dest i)))
                (write-memory-at-addr gb dest (read-memory-at-addr gb src))))
        (if (< (- len transfer-bytes) 0)
            (setf (cgbppu-hdma-len ppu)  #x00
                  (cgbppu-vram-dma-type ppu) 0
                  (cgbppu-hdma12 ppu) #xffff
                  (cgbppu-hdma34 ppu) #xffff)
            (setf (cgbppu-hdma-len ppu)  (- len transfer-bytes)
                  (cgbppu-hdma12 ppu) (+ start-src transfer-bytes)
                  (cgbppu-hdma34 ppu) (+ start-dest transfer-bytes)))))))

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
        (let ((addr (+ (ppulcdc-bg-tilemap-area (gbppu-lcdc ppu))
                        (* (floor row 8) +tilemap-tile-width+) (floor col 8))))
          (render-tile-line ppu
                            framebuffer
                            row
                            (+ (if (and
                                     (cgbppu-p ppu)
                                     (= (logand (ppu-read-memory-at-addr ppu (+ addr #x2000)) #x8) #x8))
                                   #x2000
                                   #x0000)
                               (ppulcdc-tiledata-area (gbppu-lcdc ppu))
                               (* (calc-bg-tile-no ppu addr) 16)
                               (* (mod row 8) 2))
                            :start-x col
                            :cram (if (cgbppu-p ppu) (cgbppu-obj-cram ppu))
                            :is-background? t
                            :framebuffer-width +tilemap-pixel-width+
                            :palette (if (cgbppu-p ppu)
                                       (logand (ppu-read-memory-at-addr ppu (+ addr #x2000)) #x7)
                                       (gbppu-bg-palette ppu))))))
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

(defun step-ppu (ppu gb cycles)
  "handles updating the PPU mode and drawing to a texture when the defined number of cycles have
  elapsed. transitions the PPU through the modes and processes vram."
  (when (and (not (gbppu-enabled? ppu))
             (> (gbppu-cycles ppu) 0))
    (setf (gbppu-cycles ppu) 0)
    (ppu-mode-transition ppu gb 0))
  (when (gbppu-enabled? ppu)
    (incf (gbppu-cycles ppu) cycles)
    (maybe-do-oam-dma ppu gb cycles)
    (if (cgbppu-p ppu) (maybe-do-gen-dma ppu gb cycles))
    (check-ly-lyc ppu gb)
    (case (gbppu-mode ppu)
      ; in Hblank state
      (0 (when (> (gbppu-cycles ppu) +hblank-duration-dots+)
           ;TODO 8 should be replaced with cycles to be more accurate
           (if (cgbppu-p ppu) (maybe-do-h-dma ppu gb 8))
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
