

(in-package :clboy)

(defconstant +sample-rate+ 44100)
(defconstant +audio-buffer-size+ 1024)
(defconstant +cycles-per-sample+ (floor +cpu-speed+ +sample-rate+))
(defconstant +cycles-frame-seq-step+ (floor +cpu-speed+ 512))
(defconstant +audio-normalize-factor+ 0.1)

(defstruct gbspu
  "struct for SPU GameBoy hardware. 4 channel audio, 2 square waves, 1 wave sequencer, and
  1 noise channel."
  (ch1 (make-channel))
  (ch2 (make-channel))
  (ch3 (make-channel))
  (ch4 (make-channel))
  (wave-ram (make-array 16 :initial-element 0 :element-type '(unsigned-byte 8)))
  (ena? nil :type boolean)
  (prev-angle 0)
  (buffer (static-vectors:make-static-vector +audio-buffer-size+ :element-type 'single-float :initial-element 0.0))
  (buffer-index 0)
  (left-vol 7)
  (right-vol 7)
  (frame-sequencer-delay 0)
  (frame-sequencer 0)
  (delay 0)
  (cycles 0)
  (device nil))

(defstruct channel
  "struct for a single GameBoy audio channel"
  (r0 0 :type (unsigned-byte 8))
  (r1 0 :type (unsigned-byte 8))
  (r2 0 :type (unsigned-byte 8))
  (r3 0 :type (unsigned-byte 8))
  (r4 0 :type (unsigned-byte 8))
  (seq #x0f)
  (timer 0)
  (len 0)
  (vol 0)
  (env-tick 0)
  (sweep-period 0)
  (right-output t)
  (left-output t)
  (cur-bit 0)
  (cur-addr 0)
  (lfsr 0 :type (unsigned-byte 15))
  (ena? nil :type boolean)
  (dac-ena? t :type boolean))

(defun gbspu-reset (spu)
  "resets the SPU to be disabled"
  (setf (gbspu-ena? spu) nil
        (channel-ena? (gbspu-ch1 spu)) nil
        (channel-ena? (gbspu-ch2 spu)) nil
        (channel-ena? (gbspu-ch3 spu)) nil
        (channel-ena? (gbspu-ch4 spu)) nil))

(defun spu-read-memory-at-addr (spu addr)
  "reads the available register data for SPU at ADDR"
  (case (logand addr #xff)
    ((#x10 #x11 #x12 #x13 #x14      #x16 #x17
      #x18 #x19 #x1a #x1b #x1c #x1d #x1e #x1f
      #x20 #x21 #x22 #x23)
     (spu-read-sound-channel spu addr))
    (#x24 (read-sound-vol spu))
    (#x25 (read-sound-pan spu))
    (#x26 (read-sound-ena spu))
    ((#x30 #x31 #x32 #x33 #x34 #x35 #x36 #x37
      #x38 #x39 #x3a #x3b #x3c #x3d #x3e #x3f)
     (aref (gbspu-wave-ram spu) (logand addr #xf)))
    (otherwise #xff)))

(defun spu-write-memory-at-addr (spu addr val)
  "writes the available register data for SPU at ADDR to VAL"
  (case (logand addr #xff)
    ((#x10 #x11 #x12 #x13 #x14      #x16 #x17
      #x18 #x19 #x1a #x1b #x1c #x1d #x1e #x1f
      #x20 #x21 #x22 #x23)
      (spu-write-sound-channel spu addr val))
    (#x24 (write-sound-vol spu val))
    (#x25 (write-sound-pan spu val))
    (#x26 (setf (gbspu-ena? spu) (= (logand val #x80) #x80)))
    ((#x30 #x31 #x32 #x33 #x34 #x35 #x36 #x37
      #x38 #x39 #x3a #x3b #x3c #x3d #x3e #x3f)
     (setf (aref (gbspu-wave-ram spu) (logand addr #xf)) val))
    (otherwise ())))

(defun spu-read-sound-channel (spu addr)
  "reads channel register data"
  (case addr
    (#xff10 (channel-r0 (gbspu-ch1 spu)))
    (#xff11 (channel-r1 (gbspu-ch1 spu)))
    (#xff12 (channel-r2 (gbspu-ch1 spu)))
    (#xff13 (channel-r3 (gbspu-ch1 spu)))
    (#xff14 (channel-r4 (gbspu-ch1 spu)))
    (#xff16 (channel-r1 (gbspu-ch2 spu)))
    (#xff17 (channel-r2 (gbspu-ch2 spu)))
    (#xff18 (channel-r3 (gbspu-ch2 spu)))
    (#xff19 (channel-r4 (gbspu-ch2 spu)))
    (#xff1a (channel-r0 (gbspu-ch3 spu)))
    (#xff1b (channel-r1 (gbspu-ch3 spu)))
    (#xff1c (channel-r2 (gbspu-ch3 spu)))
    (#xff1d (channel-r3 (gbspu-ch3 spu)))
    (#xff1e (channel-r4 (gbspu-ch3 spu)))
    (#xff20 (channel-r1 (gbspu-ch4 spu)))
    (#xff21 (channel-r2 (gbspu-ch4 spu)))
    (#xff22 (channel-r3 (gbspu-ch4 spu)))
    (#xff23 (channel-r4 (gbspu-ch4 spu)))
    (otherwise #xff)))

(defun spu-write-sound-channel (spu addr val)
  "writes channel register data and sets up channel parameters"
  (case addr
    (#xff10 (setf (channel-r0 (gbspu-ch1 spu)) val))
    (#xff11 (set-channel-r1 (gbspu-ch1 spu) val))
    (#xff12 (set-channel-r2 (gbspu-ch1 spu) val))
    (#xff13 (set-channel-r3 (gbspu-ch1 spu) val))
    (#xff14 (set-channel-r4 (gbspu-ch1 spu) val))
    (#xff16 (set-channel-r1 (gbspu-ch2 spu) val))
    (#xff17 (set-channel-r2 (gbspu-ch2 spu) val))
    (#xff18 (set-channel-r3 (gbspu-ch2 spu) val))
    (#xff19 (set-channel-r4 (gbspu-ch2 spu) val))
    (#xff1a (set-wave-channel-r0 (gbspu-ch3 spu) val))
    (#xff1b (set-wave-channel-r1 (gbspu-ch3 spu) val))
    (#xff1c (set-wave-channel-r2 (gbspu-ch3 spu) val))
    (#xff1d (set-channel-r3 (gbspu-ch3 spu) val))
    (#xff1e (set-wave-channel-r4 (gbspu-ch3 spu) val))
    (#xff20 (set-channel-r1 (gbspu-ch4 spu) val))
    (#xff21 (set-channel-r2 (gbspu-ch4 spu) val))
    (#xff22 (set-channel-r3 (gbspu-ch4 spu) val))
    (#xff23 (set-channel-r4 (gbspu-ch4 spu) val))
    (otherwise ())))

(defun set-wave-channel-r0 (channel val)
  (setf (channel-r0 channel) val
        (channel-dac-ena? channel) (= (logand val #x80) #x80)))
(defun set-wave-channel-r1 (channel val)
  (setf (channel-r1 channel) val
        (channel-len channel) (- 256 val)))
(defun set-wave-channel-r2 (channel val)
  (setf (channel-r2 channel) val
        (channel-vol channel)
        (case (logand (ash val -5) #x3)
          (#x0 4)
          (#x1 0)
          (#x2 1)
          (#x3 2))))

(defun set-wave-channel-r4 (channel val)
    (setf (channel-r4 channel) val)
    (if (> val #x7f) (setf (channel-ena? channel) t))
    (when (= (logand val #x40) #x40)
      (setf (channel-len channel) (channel-r1 channel))))

(defun set-channel-r1 (channel val)
  (setf (channel-r1 channel) val
        (channel-len channel) (logand val #x1f)
        (channel-seq channel) (get-channel-seq-from-byte val)))

(defun get-channel-seq-from-byte (val)
  (let ((seq-index (ash val -6)))
    (case seq-index
      (#x0 #x7f)
      (#x1 #x3f)
      (#x2 #x0f)
      (#x3 #x03))))

(defun set-channel-r2 (channel val)
  (setf (channel-r2 channel) val
        (channel-vol channel) (logand (ash val -4) #xf)
        (channel-env-tick channel) (logand val #x7)))

(defun set-channel-r3 (channel val)
  (setf (channel-r3 channel) val))

(defun set-channel-r4 (channel val)
    (setf (channel-r4 channel) val)
    (when (> val #x7f)
      (setf (channel-ena? channel) t
            (channel-timer channel) 0
            (channel-vol channel) (logand (ash (channel-r2 channel) -4) #xf)
            (channel-env-tick channel) (logand (channel-r2 channel) #x7)
            (channel-lfsr channel) #xff
            (channel-len channel) (logand (channel-r1 channel) #x1f))
      (channel-sweep-trigger channel)))

(defun channel-sweep-trigger (channel)
  (step-channel-sweep channel))

(defun read-sound-vol (spu)
  (+ (ash (gbspu-left-vol spu) 4)
     (gbspu-right-vol spu)))

(defun read-sound-pan (spu)
  "reads the stereo output state of each channel"
  (+ (ash (clboy-utils:bool-as-bit (channel-left-output  (gbspu-ch4 spu))) 7)
     (ash (clboy-utils:bool-as-bit (channel-left-output  (gbspu-ch3 spu))) 6)
     (ash (clboy-utils:bool-as-bit (channel-left-output  (gbspu-ch2 spu))) 5)
     (ash (clboy-utils:bool-as-bit (channel-left-output  (gbspu-ch1 spu))) 4)
     (ash (clboy-utils:bool-as-bit (channel-right-output (gbspu-ch4 spu))) 3)
     (ash (clboy-utils:bool-as-bit (channel-right-output (gbspu-ch3 spu))) 2)
     (ash (clboy-utils:bool-as-bit (channel-right-output (gbspu-ch2 spu))) 1)
     (clboy-utils:bool-as-bit (channel-right-output (gbspu-ch1 spu)))))

(defun read-sound-ena (spu)
  "reads the channel enabled flags and overall SPU enabled flags"
  (+ (ash (clboy-utils:bool-as-bit (gbspu-ena? spu)) 7)
     (ash (clboy-utils:bool-as-bit (channel-ena? (gbspu-ch4 spu))) 3)
     (ash (clboy-utils:bool-as-bit (channel-ena? (gbspu-ch3 spu))) 2)
     (ash (clboy-utils:bool-as-bit (channel-ena? (gbspu-ch2 spu))) 1)
     (clboy-utils:bool-as-bit (channel-ena? (gbspu-ch1 spu)))))

(defun write-sound-vol (spu val)
  "write the volume for the SPU to VAL"
  (setf (gbspu-left-vol spu) (logand (ash val -4) #x7)
        (gbspu-right-vol spu) (logand val #x7)))

(defun write-sound-pan (spu val)
  "update the panning flags for each channel"
  (setf (channel-left-output  (gbspu-ch4 spu)) (> (logand val #x80) 0)
        (channel-left-output  (gbspu-ch3 spu)) (> (logand val #x40) 0)
        (channel-left-output  (gbspu-ch2 spu)) (> (logand val #x20) 0)
        (channel-left-output  (gbspu-ch1 spu)) (> (logand val #x10) 0)
        (channel-right-output (gbspu-ch4 spu)) (> (logand val #x08) 0)
        (channel-right-output (gbspu-ch3 spu)) (> (logand val #x04) 0)
        (channel-right-output (gbspu-ch2 spu)) (> (logand val #x02) 0)
        (channel-right-output (gbspu-ch1 spu)) (> (logand val #x01) 0)))

(defun channel-freq (channel)
  "calculate the frequency for a CHANNEL based r4 and r3 registers"
  (let ((freq-msb (ash (logand (channel-r4 channel) #x7) 8))
        (freq-lsb (channel-r3 channel)))
    (- 2048 (logior freq-msb freq-lsb))))

(defun set-channel-freq (channel freq)
  "sets the frequency for a CHANNEL into r4 and r3 registers"
  (let ((freq-msb (logand (ash freq -8) #x7))
        (freq-lsb (logand freq #xff)))
    (setf (channel-r4 channel) (logior (channel-r4 channel) freq-msb)
          (channel-r3 channel) freq-lsb)))

(defun channel-noise-freq (channel)
  (let ((r3-shift (ash (channel-r3 channel) -4))
        (r3-ratio (logand (channel-r3 channel) #x07)))
    (ash (noise-divisor r3-ratio) r3-shift)))

(defun noise-divisor (div-ratio)
  (case div-ratio
    (0 8)
    (1 16)
    (2 32)
    (3 48)
    (4 64)
    (5 80)
    (6 96)
    (7 112)))

(defun gen-lfsr (channel)
	(let* ((lfsr (channel-lfsr channel))
         (xor-res (logxor (ldb (byte 1 0) (logand lfsr #x01)) (ldb (byte 1 0) (logand (ash lfsr -1) #x01))))
         (new-lfsr (logior (logior (ash lfsr -1) (ash xor-res 14)))))
    (if (= (logand (channel-r3 channel) #x8) #x8)
      (logand (logior new-lfsr (ash xor-res 6)) #x7f)
      new-lfsr)))

(defun step-sqare-channel (channel spu)
  "step the square wave CHANNEL based on the frame sequencer"
  (when (channel-ena? channel)
      (when (and (= (logand (gbspu-frame-sequencer spu) #x3) #x2)
                 (= (gbspu-frame-sequencer-delay spu) 0)
                 (or (> (logand (channel-r0 channel) #x7) 0)
                     (> (logand (ash (channel-r0 channel) -4) #x7) 0)))
        (step-channel-sweep channel))

      (step-channel-seq channel)

      (when (and (= (mod (gbspu-frame-sequencer spu) 2) 0)
                 (= (gbspu-frame-sequencer-delay spu) 0)
                 (= (logand (channel-r4 channel) #x40) #x40))
        (step-channel-len channel))

      (when (and (= (gbspu-frame-sequencer spu) 7)
                 (= (gbspu-frame-sequencer-delay spu) 0)
                 (> (logand (channel-r2 channel) 7) 0))
        (step-channel-env channel))))

(defun step-wave-channel (channel spu)
  "step the wave sequencer CHANNEL based on the frame sequencer"
  (when (channel-ena? channel)
      (when (and (= (channel-timer channel) 0)
                 (= (channel-cur-bit channel) 0))
        (incf (channel-cur-addr channel))
        (if (> (channel-cur-addr channel) #xf) (setf (channel-cur-addr channel) 0))
        (setf (channel-seq channel) (spu-read-memory-at-addr spu (+ #xff30 (channel-cur-addr channel)))))

      (step-wave-channel-seq channel)

      (when (and (= (mod (gbspu-frame-sequencer spu) 2) 0)
                 (= (gbspu-frame-sequencer-delay spu) 0)
                 (= (logand (channel-r4 channel) #x40) #x40))
        (step-channel-len channel))))

(defun step-noise-channel (channel spu)
  "step the noise CHANNEL based on the frame sequencer"
  (when (channel-ena? channel)
      (step-noise-channel-seq channel)

      (when (and (= (mod (gbspu-frame-sequencer spu) 2) 0)
                 (= (gbspu-frame-sequencer-delay spu) 0)
                 (= (logand (channel-r4 channel) #x40) #x40))
        (step-channel-len channel))

      (when (and (= (gbspu-frame-sequencer spu) 7)
                 (= (gbspu-frame-sequencer-delay spu) 0)
                 (> (logand (channel-r2 channel) 7) 0))
        (step-channel-env channel))))

(defun step-wave-channel-seq (channel)
  (if (>= (incf (channel-timer channel)) (* (channel-freq channel) 2))
    (setf (channel-timer channel) 0
          (channel-cur-bit channel) (if (= (channel-cur-bit channel) 0) 1 0))))

(defun get-wave-channel-output (channel)
  (let* ((wave-val
           (logand (ash (channel-seq channel)
                (if (= (channel-cur-bit channel) 0) -4 0)) #xf))
         (wave-val-with-vol (ash wave-val (- (channel-vol channel))))
         (output (channel-dac channel wave-val-with-vol)))
    (channel-stereo-output channel output)))

(defun channel-dac (channel out)
  (if (channel-ena? channel)
      (coerce (/ out 100) 'single-float)
      0.0))

(defun step-channel-seq (channel)
  (if (>= (incf (channel-timer channel)) (* (channel-freq channel) 4))
    (setf (channel-timer channel) 0
          (channel-cur-bit channel) (mod (+ (channel-cur-bit channel) 1) 8))))
(defun get-square-channel-output (channel)
  (let ((output
          (channel-dac channel
                       (* (logand
                            (ash (channel-seq channel)
                                 (- 0 (channel-cur-bit channel)))
                            #x1)
                          (channel-vol channel)))))
    (channel-stereo-output channel output)))

(defun step-noise-channel-seq (channel)
  (if (>= (incf (channel-timer channel)) (channel-noise-freq channel))
    (setf (channel-timer channel) 0
          (channel-lfsr channel) (gen-lfsr channel))))
(defun get-noise-channel-output (channel)
  (let ((output (channel-dac channel
      (* (logand (channel-lfsr channel) #x1)
         (channel-vol channel)))))
    (channel-stereo-output channel output)))

(defun channel-stereo-output (channel out)
  (list
    (if (channel-left-output channel) out 0.0)
    (if (channel-right-output channel) out 0.0)))

(defun step-channel-len (channel)
    (decf (channel-len channel))
    (if (<= (channel-len channel) 0)
      (setf (channel-ena? channel) nil)))

(defun step-channel-env (channel)
    (decf (channel-env-tick channel))
    (when (<= (channel-env-tick channel) 0)
      (setf (channel-env-tick channel) (logand (channel-r2 channel) #x7))
      (let ((new-vol (+ (channel-vol channel) (if (= (logand (channel-r2 channel) #x8) #x8) 1 -1))))
        (if (and (>= new-vol 0) (<= new-vol 15))
          (setf (channel-vol channel) new-vol)))))

(defun step-channel-sweep (channel)
    (decf (channel-sweep-period channel))
    (when (<= (channel-sweep-period channel) 0)
      (setf (channel-sweep-period channel) (logand (ash (channel-r0 channel) -4) #x7))
      (if (= (channel-sweep-period channel) 0) (setf (channel-sweep-period channel) 8))
      (when (> (logand (channel-r0 channel) #x7) 0)
        (let* ((channel-sweep (logand (channel-r0 channel) #x7))
               (cur-freq (channel-freq channel))
               (new-freq (funcall
                           (if (= (logand (channel-r0 channel) #x8) #x8) #'- #'+)
                           cur-freq (ash cur-freq (- channel-sweep)))))
               (if (< new-freq 2048) (set-channel-freq channel new-freq) (setf (channel-ena? channel) nil))))))

(defun spu-queue-audio (spu)
  "queue data from the buffer into the SDL2 audio device"
  ;(sdl2:queue-audio (gbspu-device spu) (gbspu-buffer spu))
  (sdl2-ffi.functions:sdl-queue-audio
    (gbspu-device spu)
    (static-vectors:static-vector-pointer (gbspu-buffer spu))
    (* (gbspu-buffer-index spu) 4))
  (setf (gbspu-buffer-index spu) 0))

(defun step-spu (spu cycles)
  "step the channels by the number of elapsed cycles"
  (when (gbspu-ena? spu)
    (loop
      repeat cycles do
      (incf (gbspu-frame-sequencer-delay spu))
      (when (= (gbspu-frame-sequencer-delay spu) +cycles-frame-seq-step+)
        (setf (gbspu-frame-sequencer spu) (mod (+ (gbspu-frame-sequencer spu) 1) 8)
              (gbspu-frame-sequencer-delay spu) 0))
      (incf (gbspu-delay spu))
      (step-sqare-channel (gbspu-ch1 spu) spu)
      (step-sqare-channel (gbspu-ch2 spu) spu)
      (step-wave-channel (gbspu-ch3 spu) spu)
      (step-noise-channel (gbspu-ch4 spu) spu)
      (when (= (gbspu-delay spu) +cycles-per-sample+)
        (setf (gbspu-delay spu) 0)
        (let ((output (mapcar #'+ (get-square-channel-output (gbspu-ch1 spu))
                   (get-square-channel-output (gbspu-ch2 spu))
                   (get-wave-channel-output (gbspu-ch3 spu))
                   (get-noise-channel-output (gbspu-ch4 spu))
                   )))
        (setf (aref (gbspu-buffer spu) (gbspu-buffer-index spu))
              (* (+ (gbspu-left-vol spu) 1) (car output) +audio-normalize-factor+)
              (aref (gbspu-buffer spu) (+ (gbspu-buffer-index spu) 1))
              (* (+ (gbspu-right-vol spu) 1) (cadr output) +audio-normalize-factor+))
        (incf (gbspu-buffer-index spu) 2)))
      (when (= (gbspu-buffer-index spu) +audio-buffer-size+)
        (spu-queue-audio spu)))))
