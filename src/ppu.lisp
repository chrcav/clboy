
(in-package :clboy)

(defconstant +dots-per-second+ +default-cpu-speed+)
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
(defconstant +tile-byte-length+ 16)
(defconstant +sprite-tile-max-height+ 16)

(defstruct ppucram
  "CGB cram register struct"
  (auto-inc? nil :type boolean)
  (index 0)
  (ram (make-array #x40 :initial-element 0 :element-type '(unsigned-byte 8))))

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
  (dots 0)
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
  (do-oam-dma #xff :type (unsigned-byte 8))
  (lcdc (make-ppulcdc))
  (stat (make-ppustat))
  (bg-palette 0)
  (obj-palette0 0)
  (obj-palette1 0)
  (enabled? t :type boolean))

(defstruct (cgbppu (:include gbppu
                    (vram (make-array #x4000 :initial-element 0 :element-type '(unsigned-byte 8)))))
  (bg-cram (make-ppucram))
  (obj-cram (make-ppucram))
  (hdma12 #x0000 :type (unsigned-byte 16))
  (hdma34 #x0000 :type (unsigned-byte 16))
  (hdma-len #x000 :type (unsigned-byte 12))
  (vram-dma-type 0 :type (unsigned-byte 8))
  (dmg-priority-mode? nil :type boolean))


(defparameter *colors* #(#xff #x7f #xf7 #x5e #x8c #x31 #x00 #x00))

(defun ppu-cycles-per-dot (cpu-speed) (floor cpu-speed +dots-per-second+))

(defun ppu-get-palette-color (&key (cram *colors*) (palette 0) (index 0))
  (ppu-get-cram-palette-color cram (+ (* palette 8) (* index 2))))

(defun ppu-get-cram-palette-color (cram addr)
  (loop for i from 0 to 2
        for color = (logior
                      (aref cram addr)
                      (ash (aref cram (+ addr 1)) 8))
        then (ash color -5)
        collect (round (* (/ 255 31) (logand color #x1f)))))

(defun gbppu-reset (ppu)
  "resets the PPU slots"
  (setf (gbppu-scy ppu) 0
        (gbppu-scx ppu) 0
        (gbppu-wy ppu) 0
        (gbppu-wx ppu) 0
        (gbppu-vram-bank ppu) 0
        (gbppu-lcdc ppu) (make-ppulcdc))
  (when (cgbppu-p ppu)
    (setf (cgbppu-hdma12 ppu) #x0000
          (cgbppu-hdma34 ppu) #x0000
          (cgbppu-hdma-len ppu) #x000
          (cgbppu-vram-dma-type ppu) 0)))

(defun ppu-write-cram-spec (cram val)
  (setf (ppucram-auto-inc? cram) (if (> val #x7f) t)
        (ppucram-index cram) (logand val #x3f)))

(defun ppu-read-cram-spec (cram)
  (logior (if (ppucram-auto-inc? cram) #x80 #x0)
          (logand (ppucram-index cram) #x3f)))

(defun ppu-write-cram-data (cram val)
  (setf (aref (ppucram-ram cram) (logand (ppucram-index cram) #x3f)) val
        (ppucram-index cram) (if (ppucram-auto-inc? cram) (+ (ppucram-index cram) 1) (ppucram-index cram))))

(defun ppu-read-cram-data (cram)
  (aref (ppucram-ram cram) (logand (ppucram-index cram) #x3f)))

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
          :sprite-height (if (= (logand val #x04) #x04) +sprite-tile-max-height+ 8)
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
                 (#x55 (setf (cgbppu-hdma-len ppu) (ash (+ (logand val #x7f) 1) 4)
                             (cgbppu-vram-dma-type ppu) (if (= (cgbppu-vram-dma-type ppu) 0)
                                                            (if (= (logand val #x80) #x80) 2 1)
                                                            (if (= (logand val #x80) #x00) 0)))
                  (when *debug*
                    (format t "hdma transfer type ~A requested: from x~X to x~X len x~X bytes~%"
                            (cgbppu-vram-dma-type ppu)
                            (cgbppu-hdma12 ppu)
                            (cgbppu-hdma34 ppu)
                            (cgbppu-hdma-len ppu))))
                 (#x68 (ppu-write-cram-spec (cgbppu-bg-cram ppu) val))
                 (#x69 (ppu-write-cram-data (cgbppu-bg-cram ppu) val))
                 (#x6a (ppu-write-cram-spec (cgbppu-obj-cram ppu) val))
                 (#x6b (ppu-write-cram-data (cgbppu-obj-cram ppu) val))
                 (#x6c (setf (cgbppu-dmg-priority-mode? ppu) (= (logand val #x01) #x01))))))
          (otherwise ())))))))

(defun ppu-read-memory-at-addr (ppu addr)
  "reads memory from PPU at ADDR which can be vram, oam, or PPU registers"
  (ecase (logand addr #xf000)
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
          (#x49 (gbppu-obj-palette1 ppu))
          (#x4a (gbppu-wy ppu))
          (#x4b (gbppu-wx ppu))
          (#x4f (logior (gbppu-vram-bank ppu) #xfe))
          ((#x51 #x52 #x53 #x54 #x55 #x68 #x69 #x6a #x6b #x6c)
           (if (cgbppu-p ppu)
               (case (logand addr #x00ff)
                 ((#x51 #x52 #x53 #x54) #xff)
                 (#x55 (if (= (cgbppu-vram-dma-type ppu) 0) #xff (- (ash (cgbppu-hdma-len ppu) -4) 1)))
                 (#x68 (ppu-read-cram-spec (cgbppu-bg-cram ppu)))
                 (#x69 (ppu-read-cram-data (cgbppu-bg-cram ppu)))
                 (#x6a (ppu-read-cram-spec (cgbppu-obj-cram ppu)))
                 (#x6b (ppu-read-cram-data (cgbppu-obj-cram ppu)))
                 (#x6c (if (cgbppu-dmg-priority-mode? ppu) #x01 #x00)))))
          (otherwise #xff)))))))

(defun read-sprite (oam addr)
  "reads the 4 bytes of a sprite at ADDR. sprite y location, sprite x location, sprite tile index,
  and sprite flags"
  (loop for a from addr to (+ addr 3)
        collect (aref oam a)))

(defun sprite-overlaps-scanline? (sprite row sprite-height)
  (and (>= row (- (car sprite) +sprite-tile-max-height+))
       (< row (+ (- (car sprite) +sprite-tile-max-height+) sprite-height))))

(defun render-sprites-on-scanline (row &key (cram nil) (palettes nil) (is-cgb? nil) (cgb-priority? nil)
                                       (obj-enabled? nil) (sprite-height 8) (oam) (vram))
  "loops through oam and adds sprites to framebuffer that overlap the current scanline location."
  (let ((buffer (make-list +screen-pixel-width+ :initial-element nil)))
    (if (and obj-enabled? (< row +screen-pixel-height+))
        (reduce #'(lambda (acc next)
                  (if (null next) acc
                  (map 'list
                       #'(lambda (cur new)
                           (if (null cur) new
                               (if (and (not (null new))
                                        (render-pixel? (cadr new) cur :priority (car new)))
                                   new cur)))
                       acc next)))
              (loop for addr = #x00 then (+ addr 4)
                    while (< addr #xa0)
                    for sprite = (read-sprite oam addr)
                    when (sprite-overlaps-scanline? sprite row sprite-height)
                    collect (render-sprite-on-scanline addr
                                                       :cram cram
                                                       :cgb-priority? cgb-priority?
                                                       :palette (if (= (logand (cadddr sprite) #x10) #x00)
                                                                    (car palettes)
                                                                    (cadr palettes))
                                                       :is-cgb? is-cgb?
                                                       :sprite-height sprite-height
                                                       :vram vram
                                                       :sprite-index (caddr sprite)
                                                       :sprite-flags (cadddr sprite)
                                                       :sprite-x (cadr sprite)
                                                       :sprite-y-offset (calc-sprite-y-offset
                                                                          row
                                                                          (car sprite)
                                                                          (> (logand (cadddr sprite) #x40) 0)
                                                                          sprite-height)))
              :initial-value buffer)
        buffer)))

(defun calc-sprite-y-offset (row sprite-y yflip? sprite-height)
  (let ((offset (+ (- row sprite-y) +sprite-tile-max-height+)))
    (if yflip?
        (- sprite-height offset)
        offset)))

(defun calc-sprite-tile-addr (tile-no sprite-flags sprite-y-offset &key (is-cgb? nil))
  "calculates the memory address of a sprites tile pixel data"
  (+ (if (and is-cgb? (= (logand sprite-flags #x8) #x8)) #x2000 #x0000)
     (* tile-no +tile-byte-length+)
     (* sprite-y-offset
        2)))

(defun calc-sprite-tile-no (sprite-height sprite-index)
  (if (= sprite-height +sprite-tile-max-height+) (logand sprite-index #xfe) sprite-index))

(defun render-sprite-on-scanline (oam-pos
                                         &key (cram nil) (cgb-priority? nil) (palette 0) (is-cgb? nil)
                                         (sprite-height 8) (vram)
                                         (sprite-index 0) (sprite-flags 0)
                                         (sprite-x 0) (sprite-y-offset 0))
  "adds a row of pixels from the visible portion of a sprite corresponding to the scanline
  location."
  (when (< (- sprite-x 8) +screen-pixel-width+)
    (let ((new-buffer (make-array +screen-pixel-width+ :initial-element nil)))
      (replace new-buffer
               (render-tile-line
                 (get-color-bytes vram (calc-sprite-tile-addr
                                         (calc-sprite-tile-no sprite-height
                                                              sprite-index)
                                         sprite-flags
                                         sprite-y-offset
                                         :is-cgb? is-cgb?))
                 :start-x (- sprite-x 8)
                 :xflip? (> (logand sprite-flags #x20) 0)
                 :priority (- 1000
                              (if cgb-priority? oam-pos sprite-x)
                              (if (= (logand sprite-flags #x80) #x80) 1000 0))
                 :cram cram
                 :palette (if is-cgb?
                              (logand sprite-flags #x7)
                              palette))
               :start1 (if (< (- sprite-x 8) 0) 0 (- sprite-x 8))))))

(defun get-color-bytes (vram tile-row-addr)
  (list (aref vram tile-row-addr)
        (aref vram (+ tile-row-addr 1))))

(defun render-pixel? (colorval bg-buffer-val &key (priority 0) (is-background? nil))
  (or is-background?
      (and (> colorval #x00)
           (or (null bg-buffer-val)
               (= (cadr bg-buffer-val) #x00)
               (> priority (car bg-buffer-val))))))

(defun get-color-vals (colorbytes)
  (loop for i from 0 to 7
        collect (+ (logand (ash (car colorbytes) (- i 7)) #x01)
                   (* (logand (ash (cadr colorbytes) (- i 7)) #x01) 2))))

(defun maybe-reverse (l &key (rev? nil))
  (if rev?
      (reverse l)
      l))

(defun render-tile-line (colorbytes &key (start-x 0) (xflip? nil) (priority 0) (cram nil)
                                    (palette 0) (framebuffer-width +screen-pixel-width+))
  "adds a row of pixels from the visible portion of a tile corresponding to the scanline
  location."
  (when (< start-x framebuffer-width)
    (mapcan #'(lambda (col colorval)
                (if (not (null col))
                    (list
                      (list
                        priority
                        colorval
                        (if cram
                            (ppu-get-palette-color :cram cram :palette palette :index colorval)
                            (ppu-get-palette-color :index (logand (ash palette (* colorval -2)) 3)))))))
            (loop for i from 0 to 7
                  for col = (+ start-x i)
                  collect (if (and (>= col 0) (< col framebuffer-width)) col))
            (maybe-reverse (get-color-vals colorbytes) :rev? xflip?))))

(defun calc-bg-tile-addr (tiledata-area vram tilemap-addr tile-yoffset &key (bg-attr nil))
  (+ (if (and
           (not (null bg-attr))
           (= (logand bg-attr #x8) #x8))
         #x2000
         #x0000)
     tiledata-area
     (* (calc-bg-tile-no tiledata-area (aref vram tilemap-addr)) +tile-byte-length+)
     (* (if (and (not (null bg-attr))
                 (> (logand bg-attr #x40) 0))
            (- 8 (mod tile-yoffset 8))
            (mod tile-yoffset 8))
        2)))

(defun calc-bg-tile-no (tiledata-area tile-byte)
  (if (= tiledata-area #x0000)
      tile-byte
      (make-signed-from-unsigned tile-byte)))

(defun render-window-scanline (row &key (is-cgb? nil) (cram nil) (palette 0)
                                   (bgwin-enabled? nil) (window-enabled? nil) (tilemap-area #x1800)
                                   (tiledata-area #x0000) (vram nil) (wy 0) (wx 0))
  "adds a row of pixels from the visible portion of the window corresponding to the scanline
  location."
  (if (and bgwin-enabled? ; TODO this should trigger the background losing priority
           window-enabled?
           (< row +screen-pixel-height+)
           (>= row wy)
           (>= wy 0) (< wy (+ +screen-pixel-height+ 7))
           (>= wx 0) (< wx +screen-pixel-width+))
      (loop for col = (- wx 7) then (+ col 8)
            for tilemapx from 0 to (- +tilemap-tile-width+ 1)
            for tilemap-addr = (+ tilemap-area
                                  (* (floor (- row wy) 8) +tilemap-tile-width+)
                                  tilemapx)
            when (and (>= col 0) (< col +screen-pixel-width+))
            nconcing
            (render-tile-line
              (get-color-bytes vram (calc-bg-tile-addr tiledata-area
                                                       vram
                                                       tilemap-addr
                                                       (- row wy)
                                                       :bg-attr (if is-cgb?
                                                                    (aref vram
                                                                          (+ tilemap-addr #x2000)))))
              :start-x col
              :xflip? (and is-cgb? (> (logand (aref vram (+ tilemap-addr #x2000)) #x20) 0))
              :priority (if (and is-cgb? (= (logand (aref vram (+ tilemap-addr #x2000)) #x80) #x80))
                            10000
                            100)
              :cram cram
              :palette (if is-cgb?
                           (logand (aref vram (+ tilemap-addr #x2000)) #x7)
                           palette)))
      (make-list +screen-pixel-width+)))

(defun render-background-scanline (row &key (cram nil) (palette 0) (is-cgb? nil) (bgwin-enabled? nil)
                                       (tilemap-area #x1800) (tiledata-area #x0000) (scy 0) (scx 0)
                                       (vram nil))
  "adds a row of pixels from the visible portion of the background corresponding to the scanline
  location."
  (when (and bgwin-enabled? ; TODO this should trigger the background losing priority
             (< row +screen-pixel-height+))
    (let ((yoffset (+ row scy)))
      (loop for xoffset = scx then (+ xoffset 8)
            for col = (- (mod scx 8)) then (+ col 8)
            for tilemap-addr = (+ tilemap-area
                                  (mod (* (floor yoffset 8) +tilemap-tile-width+)
                                       (* +tilemap-tile-width+ +tilemap-tile-height+))
                                  (mod (floor xoffset 8) +tilemap-tile-width+))
            while (< col +screen-pixel-width+)
            nconcing
            (render-tile-line
              (get-color-bytes vram (calc-bg-tile-addr tiledata-area
                                                       vram
                                                       tilemap-addr
                                                       yoffset
                                                       :bg-attr (if is-cgb?
                                                                    (aref vram
                                                                          (+ tilemap-addr #x2000)))))
              :start-x col
              :xflip? (and
                        is-cgb?
                        (> (logand (aref vram (+ tilemap-addr #x2000)) #x20) 0))
              :priority (if (and is-cgb? (= (logand (aref vram (+ tilemap-addr #x2000)) #x80) #x80))
                            10000
                            100)
              :cram cram
              :palette (if is-cgb?
                           (logand (aref vram (+ tilemap-addr #x2000)) #x7)
                           palette))))))

;;TODO should only transfer data based on the dots that have passed for all dma transfers
(defun maybe-do-oam-dma (ppu gb dots)
  "checks for DMA and processes copying memory into OAM"
  (when (< (gbppu-do-oam-dma ppu) #xe0)
    (loop for src = (ash (gbppu-do-oam-dma ppu) 8) then (+ src 1)
          for dest = #xfe00 then (+ dest 1)
          while (< dest #xfea0)
          do (write-memory-at-addr gb dest (read-memory-at-addr gb src)))
    (setf (gbppu-do-oam-dma ppu) #xff)))

(defun maybe-do-gen-dma (ppu gb)
  "checks for general purpose vram DMA and processes copying memory into VRAM"
  (when (= (cgbppu-vram-dma-type ppu) 1)
    (let ((transfer-bytes (cgbppu-hdma-len ppu))
          (start-src (cgbppu-hdma12 ppu))
          (start-dest (cgbppu-hdma34 ppu)))
      (loop for i from 0 to (- transfer-bytes 1)
            for src = (+ start-src i)
            for dest = (+ start-dest i)
            do (ppu-write-memory-at-addr ppu dest (read-memory-at-addr gb src)))
      (setf (cgbppu-hdma-len ppu)  #x00
            (cgbppu-vram-dma-type ppu) 0
            (cgbppu-hdma12 ppu) #x0000
            (cgbppu-hdma34 ppu) #x0000))))

(defun maybe-do-h-dma (ppu gb dots)
  "checks for hblank vram DMA and processes copying memory into VRAM"
  (when (= (cgbppu-vram-dma-type ppu) 2)
    (let ((len (cgbppu-hdma-len ppu))
          (transfer-bytes (* dots 2))
          (start-src (cgbppu-hdma12 ppu))
          (start-dest (cgbppu-hdma34 ppu)))
      (when (> len 0)
        (loop for i from 0 to (- transfer-bytes 1)
              for src = (+ start-src i)
              for dest = (+ start-dest i)
              while (< i len)
              do (ppu-write-memory-at-addr ppu dest (read-memory-at-addr gb src)))
        (if (<= (- len transfer-bytes) 0)
            (setf (cgbppu-hdma-len ppu)  #x00
                  (cgbppu-vram-dma-type ppu) 0
                  (cgbppu-hdma12 ppu) #x0000
                  (cgbppu-hdma34 ppu) #x0000)
            (setf (cgbppu-hdma-len ppu)  (- len transfer-bytes)
                  (cgbppu-hdma12 ppu) (+ start-src transfer-bytes)
                  (cgbppu-hdma34 ppu) (+ start-dest transfer-bytes)))))))

(defun check-ly-lyc (ppu gb)
  "compares ly and lyc registers, triggers interrupt if they match and the interrupt is enabled"
  (when (= (gbppu-cur-line ppu) (gbppu-cur-line-comp ppu))
    (if (ppustat-lyc-int-enabled? (gbppu-stat ppu)) (set-interrupt-flag gb 1))))

(defun ppu-mode-transition (ppu gb mode)
  "transitions PPU to MODE 0-3 triggers interrupt if enabled"
  (setf (gbppu-dots ppu) 0)
  (setf (gbppu-mode ppu) mode)
  (if (stat-int-enabled-for-mode? (gbppu-stat ppu) mode) (set-interrupt-flag gb 1)))

(defun stat-int-enabled-for-mode? (stat mode)
  "predicate for ppu MODE interrupt enabled flags in STAT"
  (case mode
    (0 (ppustat-mode0-int-enabled? stat))
    (1 (ppustat-mode1-int-enabled? stat))
    (2 (ppustat-mode2-int-enabled? stat))
    (otherwise nil)))


(defun render-scanline (ppu row)
  "processes the background, window, and sprites based on lcdc flags and video memory"
  (replace (gbppu-framebuffer ppu)
           (mapcan #'identity
                   (map 'list
                        #'(lambda (bg win obj)
                            (if (and (not (null obj))
                                     (render-pixel? (cadr obj) bg :priority (car obj))
                                     (render-pixel? (cadr obj) win :priority (car obj)))
                                (caddr obj)
                                (if (and (not (null win))
                                         (render-pixel? (cadr win) bg :priority (car win) :is-background? t))
                                    (caddr win)
                                    (caddr bg))))
                        (render-background-scanline row
                                                    :cram (if (cgbppu-p ppu) (ppucram-ram (cgbppu-bg-cram ppu)))
                                                    :palette (gbppu-bg-palette ppu)
                                                    :is-cgb? (cgbppu-p ppu)
                                                    :bgwin-enabled? (ppulcdc-bgwin-enabled? (gbppu-lcdc ppu))
                                                    :tilemap-area (ppulcdc-bg-tilemap-area (gbppu-lcdc ppu))
                                                    :tiledata-area (ppulcdc-tiledata-area (gbppu-lcdc ppu))
                                                    :scy (gbppu-scy ppu)
                                                    :scx (gbppu-scx ppu)
                                                    :vram (gbppu-vram ppu))
                        (render-window-scanline row
                                                :cram (if (cgbppu-p ppu) (ppucram-ram (cgbppu-bg-cram ppu)))
                                                :palette (gbppu-bg-palette ppu)
                                                :is-cgb? (cgbppu-p ppu)
                                                :bgwin-enabled? (ppulcdc-bgwin-enabled? (gbppu-lcdc ppu))
                                                :window-enabled? (ppulcdc-window-enabled? (gbppu-lcdc ppu))
                                                :tilemap-area (ppulcdc-win-tilemap-area (gbppu-lcdc ppu))
                                                :tiledata-area (ppulcdc-tiledata-area (gbppu-lcdc ppu))
                                                :wy (gbppu-wy ppu)
                                                :wx (gbppu-wx ppu)
                                                :vram (gbppu-vram ppu))
                        (render-sprites-on-scanline row
                                                    :cram (if (cgbppu-p ppu) (ppucram-ram (cgbppu-obj-cram ppu)))
                                                    :is-cgb? (cgbppu-p ppu)
                                                    :cgb-priority? (and (cgbppu-p ppu) (not (cgbppu-dmg-priority-mode? ppu)))
                                                    :palettes (list (gbppu-obj-palette0 ppu)
                                                                    (gbppu-obj-palette1 ppu))
                                                    :obj-enabled? (ppulcdc-obj-enabled? (gbppu-lcdc ppu))
                                                    :sprite-height (ppulcdc-sprite-height (gbppu-lcdc ppu))
                                                    :oam (gbppu-oam ppu)
                                                    :vram (gbppu-vram ppu))))
           :start1 (* row +screen-pixel-width+ 3)))

(defun render-full-background (ppu)
  "Provides a way to draw the entire 256x256 background stored in the tilemap."
  (let ((texture (sdl2:create-texture (gbppu-renderer ppu) :rgb24 :streaming +tilemap-pixel-width+ +tilemap-pixel-height+))
        (framebuffer (static-vectors:make-static-vector (* +tilemap-pixel-width+ +tilemap-pixel-height+ 3))))
    (replace framebuffer
             (mapcan #'caddr
                     (loop for row from 0 to (- +tilemap-pixel-height+ 1)
                           when (< row +tilemap-pixel-height+) nconcing
                           (loop for col = 0 then (+ col 8)
                                 while (< col +tilemap-pixel-width+) nconcing
                                 (let ((tilemap-addr (+ (ppulcdc-bg-tilemap-area (gbppu-lcdc ppu))
                                                        (* (floor row 8) +tilemap-tile-width+) (floor col 8))))
                                   (render-tile-line
                                     (get-color-bytes (gbppu-vram ppu) (calc-bg-tile-addr (ppulcdc-tiledata-area (gbppu-lcdc ppu)) (gbppu-vram ppu) tilemap-addr row :bg-attr (if (cgbppu-p ppu) (aref (gbppu-vram ppu) (+ tilemap-addr #x2000)))))
                                     :start-x col
                                     :cram (if (cgbppu-p ppu) (ppucram-ram (cgbppu-bg-cram ppu)))
                                     :framebuffer-width +tilemap-pixel-width+
                                     :palette (if (cgbppu-p ppu)
                                                  (logand (aref (gbppu-vram ppu) (+ tilemap-addr #x2000)) #x7)
                                                  (gbppu-bg-palette ppu))))))))
    (sdl2:update-texture
      texture
      (cffi:null-pointer)
      (static-vectors:static-vector-pointer
        framebuffer) (* +tilemap-pixel-width+ 3))
    (sdl2:render-copy (gbppu-renderer ppu) texture)
    (sdl2:render-present (gbppu-renderer ppu))))

(defun render-full-tiledata (ppu)
  "Provides a way to draw the entire 256x256 background stored in the tilemap."
  (let ((texture (sdl2:create-texture (gbppu-renderer ppu) :rgb24 :streaming +tilemap-pixel-width+ +tilemap-pixel-height+))
        (framebuffer (static-vectors:make-static-vector (* +tilemap-pixel-width+ +tilemap-pixel-height+ 3))))
    (loop for addr = 0 then (+ addr 2)
          for line-start = (+ (* (mod (floor addr 2) #x100) +tilemap-pixel-width+) (* (floor addr #x200) 8))
          while (< addr (if (cgbppu-p ppu) #x4000 #x2000))
          when (< line-start (* +tilemap-pixel-height+ +tilemap-pixel-width+)) do
          (replace framebuffer
                   (mapcan #'caddr
                           (render-tile-line
                             (get-color-bytes (gbppu-vram ppu) addr)
                             :start-x (mod line-start +tilemap-pixel-width+)
                             :cram (if (cgbppu-p ppu) (ppucram-ram (cgbppu-bg-cram ppu)))
                             :framebuffer-width +tilemap-pixel-width+
                             :palette (if (cgbppu-p ppu)
                                          0
                                          (gbppu-bg-palette ppu))))
                   :start1 (* line-start 3)))
    (sdl2:update-texture
      texture
      (cffi:null-pointer)
      (static-vectors:static-vector-pointer framebuffer)
      (* +tilemap-pixel-width+ 3))
    (sdl2:render-copy (gbppu-renderer ppu) texture)
    (sdl2:render-present (gbppu-renderer ppu))))

(defun update-screen (ppu renderer texture rect)
  "takes the current framebuffer and copies it into the TEXTURE"
  (sdl2:update-texture
    texture
    (cffi:null-pointer)
    (static-vectors:static-vector-pointer (gbppu-framebuffer ppu))
    (* +screen-pixel-width+ *scale*))
  (sdl2:render-clear renderer)
  (sdl2:render-copy renderer texture :dest-rect rect)
  (sdl2:render-present renderer))

(defun step-ppu (ppu gb dots)
  "handles updating the PPU mode and drawing to a texture when the defined number of dots have
  elapsed. transitions the PPU through the modes and processes vram."
  (when (and (not (gbppu-enabled? ppu))
             (> (gbppu-dots ppu) 0))
    (setf (gbppu-dots ppu) 0)
    (ppu-mode-transition ppu gb 0))
  (when (gbppu-enabled? ppu)
    (incf (gbppu-dots ppu) dots)
    (maybe-do-oam-dma ppu gb dots)
    (if (cgbppu-p ppu) (maybe-do-gen-dma ppu gb))
    (check-ly-lyc ppu gb)
    (case (gbppu-mode ppu)
      ; in Hblank state
      (0 (when (> (gbppu-dots ppu) +hblank-duration-dots+)
           ;TODO 8 should be replaced with dots to be more accurate
           (if (cgbppu-p ppu) (maybe-do-h-dma ppu gb 8))
           (incf (gbppu-cur-line ppu))
           (if (> (gbppu-cur-line ppu) (- +screen-pixel-height+ 1))
               (progn (ppu-mode-transition ppu gb 1)
                      (set-interrupt-flag gb 0)
                      (update-screen ppu (gbppu-renderer ppu) (gbppu-texture ppu) (gbppu-render-rect ppu))
                      )
               (ppu-mode-transition ppu gb 2))))
      ; in Vblank state
      (1 (when (> (gbppu-dots ppu) +vblank-duration-dots+)
           (incf (gbppu-cur-line ppu))
           (when (> (gbppu-cur-line ppu) (+ +screen-pixel-height+ 9))
             (setf (gbppu-cur-line ppu) 0)
             (ppu-mode-transition ppu gb 2))))
      ; in OAM state
      (2 (when (> (gbppu-dots ppu) +oam-duration-dots+)
           (ppu-mode-transition ppu gb 3)))
      ; in VRAM Read state
      (3 (when (> (gbppu-dots ppu) +draw-duration-dots+)
           (render-scanline ppu (gbppu-cur-line ppu))
           (ppu-mode-transition ppu gb 0)))))
  t)

(defun ppu-dump-vram (ppu)
  (ppu-dump-vram-range ppu 0 (if (cgbppu-p ppu) #x3fff #x1fff)))
(defun ppu-dump-vram-range (ppu start end)
  (loop for block = start then (+ block #x10)
        while (< block end)
        do (format t "x~X" block)
        (loop for addr from block to (+ block #xf)
              do (format t " x~X" (aref (gbppu-vram ppu) addr)))
        (format t "~%")))

(defun ppu-dump-tilemap-bytes (ppu)
  (ppu-dump-tilemap-bytes-range ppu #x1800 #x1fff))
(defun ppu-dump-tilemap-bytes-range (ppu start end)
  (format t "~A"
          (loop for addr from start to end
                collect (list (format nil "~X" (aref (gbppu-vram ppu) addr))
                              (format nil "~X" (if (cgbppu-p ppu) (aref (gbppu-vram ppu) (+ addr #x2000)) 0))))))
(defun ppu-dump-bg-cram (ppu)
  (if (cgbppu-p ppu) (format t "~A~%" (ppucram-ram (cgbppu-bg-cram ppu)))))
(defun ppu-dump-obj-cram (ppu)
  (if (cgbppu-p ppu) (format t "~A~%" (ppucram-ram (cgbppu-obj-cram ppu)))))
