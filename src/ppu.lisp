
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

(defstruct gbppu
  (framebuffer (static-vectors:make-static-vector (* +screen-pixel-width+ +screen-pixel-height+ 3)))
  (framebuffer-a (static-vectors:make-static-vector (* +screen-pixel-width+ +screen-pixel-height+ 4)))
  (bg-buffer (make-array (* +tilemap-pixel-width+ +tilemap-pixel-height+) :initial-element 0 :element-type '(unsigned-byte 8)))
  (cycles 0)
  (cur-line 0)
  (oam (make-array #x100 :initial-element 0 :element-type '(unsigned-byte 8)))
  (vram (make-array #x2000 :initial-element 0 :element-type '(unsigned-byte 8)))
  (zero-page (make-array #x100 :initial-element 0 :element-type '(unsigned-byte 8)))
  (scy 0)
  (scx 0)
  (wy 0)
  (wx 0)
  (mode 0)
  (renderer nil)
  (texture nil)
  (ppu-enabled? t :type boolean)
  (window-enabled? nil :type boolean)
  (obj-enabled? nil :type boolean)
  (bgwin-enabled? t :type boolean))

(defconstant COLORS #(255 255 255
                      192 192 192
                      96 96 96
                       0  0  0))

(defun gbppu-reset (ppu)
  (setf (gbppu-scy ppu) 0
        (gbppu-scx ppu) 0
        (gbppu-wy ppu) 0
        (gbppu-wx ppu) 0
        (gbppu-wx ppu) 0
        (gbppu-window-enabled? ppu) nil
        (gbppu-obj-enabled? ppu) nil))

(defun write-new-lcdc-control (ppu val)
  (setf (aref (gbppu-zero-page ppu) #x40) val
        (gbppu-ppu-enabled? ppu) (> (logand val #x80) 0)
        (gbppu-window-enabled? ppu) (> (logand val #x20) 0)
        (gbppu-obj-enabled? ppu) (> (logand val #x02) 0)
        (gbppu-bgwin-enabled? ppu) (> (logand val #x01) 0)))

(defun ppu-write-memory-at-addr (ppu addr val)
  (ecase (logand addr #xf000)
    ((#x8000 #x9000)
      (setf (aref (gbppu-vram ppu) (logand addr #x1fff)) val))
    (#xf000
     (case (logand addr #x0f00)
       (#xe00 (if (< addr #xfea0) (setf (aref (gbppu-oam ppu) (logand addr #xff)) val)))
       (#xf00
        (case (logand addr #x00ff)
          (#x40 (write-new-lcdc-control ppu val))
          (#x42 (setf (gbppu-scy ppu) val))
          (#x43 (setf (gbppu-scx ppu) val))
          (#x4a (setf (gbppu-wy ppu) val))
          (#x4b (setf (gbppu-wx ppu) val))
          (otherwise (setf (aref (gbppu-zero-page ppu) (logand addr #xff)) val))))))))

(defun ppu-read-memory-at-addr (ppu addr)
  (ecase (logand addr #xf000)
    ((#x8000 #x9000)
      (aref (gbppu-vram ppu) (logand addr #x1fff)))
    (#xf000
     (case (logand addr #x0f00)
       (#xe00 (if (< addr #xfea0) (aref (gbppu-oam ppu) (logand addr #xff)) 0))
       (#xf00
        (case (logand addr #x00ff)
          (#x42 (gbppu-scy ppu))
          (#x43 (gbppu-scx ppu))
          (#x4a (gbppu-wy ppu))
          (#x4b (gbppu-wx ppu))
          (otherwise (aref (gbppu-zero-page ppu) (logand addr #xff)))))))))

(defun read-sprite (ppu addr)
  (loop for a from addr to (+ addr 3)
        collect (ppu-read-memory-at-addr ppu a)))

(defun sprite-overlaps-scanline? (ppu sprite row)
  (let ((sprite-height (if (= (logand (ppu-read-memory-at-addr ppu #xff40) #x4) #x4) 16 8))
        (sprite-y (- (car sprite) 16)))
  (and (>= row sprite-y) (< row (+ sprite-y sprite-height)))))

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
         (sprite-y (- (car sprite) 16))
         (sprite-x (- (cadr sprite) 8))
         (tile-no (if (= (logand lcdc #x4) #x4) (logand (caddr sprite) #xfe) (caddr sprite)))
         (sprite-flags (cadddr sprite))
         (sprite-y-offset (- row sprite-y))
         (sprite-xflip? (> (logand sprite-flags #x20) 0))
         (sprite-yflip? (> (logand sprite-flags #x40) 0))
         (palette-reg (if (= (logand sprite-flags #x10) #x00) #xff48 #xff49))
         (color-addr
           (+ #x8000 (* tile-no 16)
              (* (if sprite-yflip? (- sprite-height sprite-y-offset) sprite-y-offset) 2))))
    (render-tile-line ppu (gbppu-framebuffer ppu) row color-addr :start-x sprite-x :xflip? sprite-xflip? :priority (ash sprite-flags -7) :palette-reg palette-reg)))

(defun render-tile-line (ppu framebuffer row tile-row-addr &key (start-x 0) (xflip? nil) (is-background? nil) (priority 0) (palette-reg #xff47) (framebuffer-width +screen-pixel-width+))
  (let* ((colorbyte1 (ppu-read-memory-at-addr ppu tile-row-addr))
         (colorbyte2 (ppu-read-memory-at-addr ppu (+ tile-row-addr 1))))
  (loop for i from 0 to 7
        for col = (+ start-x i)
        when (and (>= col 0) (< col framebuffer-width))
    do
    (let* ((colorbitpos (if xflip? (- 0 i) (- i 7)))
          (colorval (+
                       (logand (ash colorbyte1 colorbitpos) #x01)
                       (* (logand (ash colorbyte2 colorbitpos) #x01) 2))))
      (when (or is-background?
                (and (> colorval #x00)
                     (or (= (aref (gbppu-bg-buffer ppu) (+ (* row +screen-pixel-width+) col)) #x00)
                         (= priority #x00) )))
        (setf (aref (gbppu-bg-buffer ppu) (+ (* row framebuffer-width) col)) colorval)
        (let ((palette-col (logand (ash (ppu-read-memory-at-addr ppu palette-reg) (* colorval -2)) 3)))
          (setf (aref framebuffer (+ (* row framebuffer-width 3) (* col 3)))
                (aref COLORS (* palette-col 3))
                (aref framebuffer (+ (* row framebuffer-width 3) (* col 3) 1))
                (aref COLORS (+ (* palette-col 3) 1))
                (aref framebuffer (+ (* row framebuffer-width 3) (* col 3) 2))
                (aref COLORS (+ (* palette-col 3) 2))
                ;(aref (gbppu-framebuffer-a ppu) (+ (* row framebuffer-width 4) (* col 4) 3)) #xff
                )))))))

(defun add-window-to-ppu-framebuffer (ppu)
  (let* ((row (gbppu-cur-line ppu))
         (wy (gbppu-wy ppu))
         (wx (gbppu-wx ppu))
         (lcdc (ppu-read-memory-at-addr ppu #xff40))
         (tilemap-loc (if (= (logand lcdc #x40) #x40) #x9c00 #x9800))
         (tiledata-loc (if (= (logand lcdc #x10) #x10) #x8000 #x9000)))
    (when (and (< row +screen-pixel-height+)
               (>= row wy)
               (>= wy 0) (< wy (+ +screen-pixel-height+ 7))
               (>= wx 0) (< wx +screen-pixel-width+))
    (loop for col = (- wx 7) then (+ col 8)
          for tilemapx from 0 to 31
          when (and (>= col 0) (< col +screen-pixel-width+))
      do
      (let* ((yoffset (- row wy))
             (addr (+ tilemap-loc (* (floor yoffset 8) 32) tilemapx))
             (tile-no (if (= tiledata-loc #x8000)
                        (ppu-read-memory-at-addr ppu addr)
                        (make-signed-from-unsigned (ppu-read-memory-at-addr ppu addr))))
             (color-addr (+ tiledata-loc (* tile-no #x10) (* (mod yoffset 8) 2))))
        (render-tile-line ppu (gbppu-framebuffer ppu) row color-addr :start-x col :is-background? t))))))

(defun add-background-to-ppu-framebuffer (ppu)
  (let* ((row (gbppu-cur-line ppu))
         (scroll-y (ppu-read-memory-at-addr ppu #xff42))
         (scroll-x (ppu-read-memory-at-addr ppu #xff43))
         (lcdc (ppu-read-memory-at-addr ppu #xff40))
         (tilemap-loc (if (= (logand lcdc #x08) #x08) #x9c00 #x9800))
         (tiledata-loc (if (= (logand lcdc #x10) #x10) #x8000 #x9000)))
    (when (< row +screen-pixel-height+)
    (loop for col = 0 then (+ col (- 8 (mod xoffset 8)))
          for xoffset = (+ col scroll-x)
          while (< col +screen-pixel-width+) do
      (let* ((yoffset (+ row scroll-y))
             (addr (+ tilemap-loc
                      (mod (* (floor yoffset 8) +tilemap-tile-width+)
                           (* +tilemap-tile-width+ +tilemap-tile-height+))
                      (mod (floor xoffset 8) +tilemap-tile-width+)))
             (tile-no (if (= tiledata-loc #x8000)
                       (ppu-read-memory-at-addr ppu addr)
                       (make-signed-from-unsigned (ppu-read-memory-at-addr ppu addr))))
             (color-addr (+ tiledata-loc (* tile-no #x10) (* (mod yoffset 8) 2))))
        (render-tile-line ppu (gbppu-framebuffer ppu) row color-addr :start-x col :is-background? t))))))

(defun maybe-do-dma (ppu gb)
  (let ((initial (ash (read-memory-at-addr gb #xff46) 8)))
    (when (> initial 0)
      (loop for i from 0 to (- +screen-pixel-width+ 1)
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

(defun ppu-mode-transition (ppu gb mode)
  (setf (gbppu-cycles ppu) 0)
  (setf (gbppu-mode ppu) mode)
  (let ((lcd-stat (read-memory-at-addr gb #xff41))
        (stat-int-ena (ash #x8 mode)))
    (write-memory-at-addr gb #xff41 (+ (logand lcd-stat #xfc) mode))
    (if (< mode 3)
      (if (= (logand lcd-stat stat-int-ena) stat-int-ena) (set-interrupt-flag gb 1)))))


(defun render-scanline (ppu)
  (when (gbppu-bgwin-enabled? ppu) ; TODO this should trigger the background losing priority
    (add-background-to-ppu-framebuffer ppu)
    (when (gbppu-window-enabled? ppu)
      (add-window-to-ppu-framebuffer ppu)))
  (when (gbppu-obj-enabled? ppu)
    (add-sprites-to-ppu-framebuffer ppu)))

(defun render-full-background (ppu gb)
  (let ((texture (sdl2:create-texture (gbppu-renderer ppu) :rgb24 :streaming +tilemap-pixel-width+ +tilemap-pixel-height+))
        (framebuffer (static-vectors:make-static-vector (* +tilemap-pixel-width+ +tilemap-pixel-height+ 3))))
  (loop for row from 0 to (- +tilemap-pixel-height+ 1) do
  (let* ((lcdc (ppu-read-memory-at-addr ppu #xff40))
         (tilemap-loc (if (= (logand lcdc #x08) #x08) #x9c00 #x9800))
         (tiledata-loc (if (= (logand lcdc #x10) #x10) #x8000 #x9000)))
    (when (< row +tilemap-pixel-height+)
    (loop for col = 0 then (+ col 8)
          while (< col +tilemap-pixel-width+) do
      (let* ((addr (+ tilemap-loc (* (floor row 8) +tilemap-tile-width+) (floor col 8)))
             (tile-no (if (= tiledata-loc #x8000)
                       (ppu-read-memory-at-addr ppu addr)
                       (make-signed-from-unsigned (ppu-read-memory-at-addr ppu addr))))
             (color-addr (+ tiledata-loc (* tile-no #x10) (* (mod row 8) 2))))
        (render-tile-line ppu framebuffer row color-addr :start-x col :is-background? t :framebuffer-width +tilemap-pixel-width+))))))
  (sdl2:update-texture
    texture
    (cffi:null-pointer)
    (static-vectors:static-vector-pointer
      framebuffer) (* +tilemap-pixel-width+ 3))
  (sdl2:render-copy (gbppu-renderer ppu) texture)
  (sdl2:render-present (gbppu-renderer ppu))
  ))

(defun update-screen (ppu gb texture)
  (set-interrupt-flag gb 0)
  (sdl2:update-texture
    texture
    (cffi:null-pointer)
    (static-vectors:static-vector-pointer
      (gbppu-framebuffer ppu)) (* +screen-pixel-width+ 3)))

(defun step-ppu (ppu gb)
  (when (and (not (gbppu-ppu-enabled? ppu))
             (> (gbppu-cycles ppu) 0))
    (setf (gbppu-cycles ppu) 0)
    (ppu-mode-transition ppu gb 0))
  (when (gbppu-ppu-enabled? ppu)
    (incf (gbppu-cycles ppu) (gbcpu-clock (gb-cpu gb)))
    (maybe-do-dma ppu gb)
    (check-ly-lyc ppu gb)
    (case (gbppu-mode ppu)
      ; in Hblank state
      (0 (when (> (gbppu-cycles ppu) +hblank-duration-dots+)
           (incf (gbppu-cur-line ppu))
           (write-memory-at-addr gb #xff44 (gbppu-cur-line ppu))
           (if (> (gbppu-cur-line ppu) (- +screen-pixel-height+ 1))
             (progn (ppu-mode-transition ppu gb 1)
                    (update-screen ppu gb (gbppu-texture ppu)))
             (ppu-mode-transition ppu gb 2))))
      ; in Vblank state
      (1 (when (> (gbppu-cycles ppu) +vblank-duration-dots+)
           (incf (gbppu-cur-line ppu))
           (write-memory-at-addr gb #xff44 (gbppu-cur-line ppu))
           (when (> (gbppu-cur-line ppu) (+ +screen-pixel-height+ 9))
             (setf (gbppu-cur-line ppu) 0)
             (write-memory-at-addr gb #xff44 0)
             (ppu-mode-transition ppu gb 2))))
      ; in OAM state
      (2 (when (> (gbppu-cycles ppu) +oam-duration-dots+)
           (ppu-mode-transition ppu gb 3)))
      ; in VRAM Read state
      (3 (when (> (gbppu-cycles ppu) +draw-duration-dots+)
           (render-scanline ppu)
           (ppu-mode-transition ppu gb 0)))))
  t)
