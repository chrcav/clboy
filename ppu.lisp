
(in-package :clboy)

(defstruct gbppu
  (framebuffer (static-vectors:make-static-vector (* 160 144 3)))
  (framebuffer-a (static-vectors:make-static-vector (* 160 144 4)))
  (bg-buffer (make-array (* 144 160) :initial-element 0 :element-type '(unsigned-byte 8)))
  (cycles 0)
  (cur-line 0)
  (oam (make-array #x100 :initial-element 0 :element-type '(unsigned-byte 8)))
  (vram (make-array #x2000 :initial-element 0 :element-type '(unsigned-byte 8)))
  (zero-page (make-array #x100 :initial-element 0 :element-type '(unsigned-byte 8)))
  (mode 0)
  (ppu-enabled? t :type boolean)
  (window-enabled? nil :type boolean)
  (obj-enabled? nil :type boolean)
  (bgwin-enabled? t :type boolean))

(defconstant COLORS #(255 255 255
                      192 192 192
                      96 96 96
                       0  0  0))

(defun write-new-lcdc-control (ppu val)
  (setf (aref (gbppu-zero-page ppu) #x40) val
        (gbppu-ppu-enabled? ppu) (> (logand val #x80) 0)
        (gbppu-window-enabled? ppu) (> (logand val #x20) 0)
        (gbppu-obj-enabled? ppu) (> (logand val #x02) 0)
        (gbppu-bgwin-enabled? ppu) (> (logand val #x01) 0)
        ))

(defun ppu-write-memory-at-addr (ppu addr val)
  (case (logand addr #xf000)
    ((#x8000 #x9000)
      (setf (aref (gbppu-vram ppu) (logand addr #x1fff)) val))
    (#xf000
     (case (logand addr #x0f00)
       (#xe00 (if (< addr #xfea0) (setf (aref (gbppu-oam ppu) (logand addr #xff)) val)))
       (#xf00
        (case (logand addr #x00ff)
          (#x40 (write-new-lcdc-control ppu val))
          (otherwise (setf (aref (gbppu-zero-page ppu) (logand addr #xff)) val))))))))

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
          when (and (>= col 0) (< col 160))
      do
      (let* ((colorbitpos (if (= sprite-xflip #x01) (- 0 i) (- i 7)))
            (colorval (+
                         (logand (ash colorbyte1 colorbitpos) #x01)
                         (* (logand (ash colorbyte2 colorbitpos) #x01) 2))))
        (when (or (= (aref (gbppu-bg-buffer ppu) (+ (* row 160) col)) #x00)
                  (and (= (ash sprite-flags -7) #x00) (> colorval #x00)))
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

(defun ppu-mode-transition (ppu gb mode)
  (setf (gbppu-cycles ppu) 0)
  (setf (gbppu-mode ppu) mode)
  (let ((lcd-stat (read-memory-at-addr gb #xff41))
        (stat-int-ena (ash #x8 mode)))
    (write-memory-at-addr gb #xff41 (+ (logand lcd-stat #xfc) mode))
    (if (< mode 3)
      (if (= (logand lcd-stat stat-int-ena) stat-int-ena) (set-interrupt-flag gb 1)))))


(defun render-scanline (ppu)
  (when (gbppu-bgwin-enabled? ppu)
    (add-background-to-ppu-framebuffer ppu)
    (when (gbppu-window-enabled? ppu)))
  (when (gbppu-obj-enabled? ppu)
    (add-sprites-to-ppu-framebuffer ppu)))

(defun update-screen (ppu gb renderer texture)
  (set-interrupt-flag gb 0)
  (sdl2:update-texture
    texture
    (cffi:null-pointer)
    (static-vectors:static-vector-pointer
      (gbppu-framebuffer ppu)) (* 160 3))
    (sdl2:render-copy renderer texture)
    (sdl2:render-present renderer))
