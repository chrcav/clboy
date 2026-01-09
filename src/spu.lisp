

(in-package :clboy)

(defconstant +sample-rate+ 44100)
(defconstant +audio-buffer-size+ 64)
(defconstant +audio-normalize-factor+ 0.1)

(defun cycles-per-sample (cpu-speed) (floor cpu-speed +sample-rate+))
(defun cycles-frame-seq-step (cpu-speed) (floor cpu-speed 512))

(defstruct gbspu
  "struct for SPU GameBoy hardware. 4 channel audio, 2 square waves, 1 wave sequencer, and
  1 noise channel."
  (ch1 (make-channel :has-sweep? t))
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
  (duty-timer 0)
  (len 0)
  (vol 0)
  (env-delay 0)
  (sweep-delay 0)
  (sweep-ena? nil)
  (has-sweep? nil)
  (right-output t)
  (left-output t)
  (cur-bit 0)
  (cur-addr 0)
  (shadow-freq 0)
  (lfsr 0 :type (unsigned-byte 15))
  (ena? nil :type boolean)
  (muted? nil :type boolean)
  (dac-ena? t :type boolean))

(defun channel-sweep-pace (channel)
  (logand (ash (channel-r0 channel) -4) #x7))
(defun channel-sweep-neg? (channel)
  (> (logand (channel-r0 channel) #x8) 0))
(defun channel-sweep-step (channel)
  (logand (channel-r0 channel) #x7))

(defun channel-init-vol (channel)
  (logand (ash (channel-r2 channel) -4) #xf))
(defun channel-env-rising? (channel)
  (> (logand (channel-r2 channel) #x8) 0))
(defun channel-env-pace (channel)
  (logand (channel-r2 channel) #x7))

(defun channel-lfsr-width-mode-bit (channel)
  (= (logand (channel-r3 channel) #x8) #x8))

(defun channel-len-ena? (channel)
  (= (logand (channel-r4 channel) #x40) #x40))

(defun get-channel-seq-from-byte (val)
  (let ((seq-index (ash val -6)))
    (case seq-index
      (#x0 #x7f)
      (#x1 #x3f)
      (#x2 #x0f)
      (#x3 #x03))))

(defun channel-freq (channel)
  "calculate the frequency for a CHANNEL based r4 and r3 registers"
  (let ((freq-msb (ash (logand (channel-r4 channel) #x7) 8))
        (freq-lsb (channel-r3 channel)))
    (logior freq-msb freq-lsb)))

(defun writeback-channel-freq (channel freq)
  "sets the frequency for a CHANNEL into r4 and r3 registers"
  (let ((freq-msb (logand (ash freq -8) #x7))
        (freq-lsb (logand freq #xff)))
    (setf (channel-r4 channel) (logior (logand (channel-r4 channel) #xf8) freq-msb)
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

(defun gbspu-reset (spu)
  "resets the SPU to be disabled"
  (setf (gbspu-ena? spu) nil
        (gbspu-ch1 spu) (make-channel)
        (gbspu-ch2 spu) (make-channel)
        (gbspu-ch3 spu) (make-channel)
        (gbspu-ch4 spu) (make-channel)))

(defun channel-reset (channel)
  (setf (channel-ena? channel) nil
        (channel-r0 channel) 0
        (channel-r1 channel) 0
        (channel-r2 channel) 0
        (channel-r3 channel) 0
        (channel-r4 channel) 0))

(defun spu-read-memory-at-addr (spu addr)
  "reads the available register data for SPU at ADDR"
  (case (logand addr #xff)
    ((#x10 #x11 #x12 #x13 #x14      #x16 #x17
      #x18 #x19 #x1a #x1b #x1c #x1d #x1e #x1f
      #x20 #x21 #x22 #x23)
     (spu-read-sound-channel spu addr))
    (#x24 (read-sound-vol spu))
    (#x25 (read-sound-pan spu))
    (#x26 (logior (read-sound-ena spu) #x70))
    ((#x30 #x31 #x32 #x33 #x34 #x35 #x36 #x37
      #x38 #x39 #x3a #x3b #x3c #x3d #x3e #x3f)
     (aref (gbspu-wave-ram spu) (logand addr #xf)))
    (#x76 (logior (square-channel-digital-ouput (gbspu-ch1 spu))
                  (ash (square-channel-digital-ouput (gbspu-ch2 spu)) 4)))
    (#x77 (logior (wave-channel-digital-output (gbspu-ch3 spu))
                  (ash (noise-channel-digital-ouput (gbspu-ch4 spu)) 4)))
    (otherwise #xff)))
(defun gbspu-toggle-audio-channel (spu ch)
  "toggles audio change CH on and off"
  (case ch
    (1
     (setf (channel-muted? (gbspu-ch1 spu)) (not (channel-muted? (gbspu-ch1 spu)))))
    (2
     (setf (channel-muted? (gbspu-ch2 spu)) (not (channel-muted? (gbspu-ch2 spu)))))
    (3
     (setf (channel-muted? (gbspu-ch3 spu)) (not (channel-muted? (gbspu-ch3 spu)))))
    (4
     (setf (channel-muted? (gbspu-ch4 spu)) (not (channel-muted? (gbspu-ch4 spu)))))
  ))

(defun set-gbspu-ena? (spu val)
  (setf (gbspu-ena? spu) (= (logand val #x80) #x80))
  (when (not (gbspu-ena? spu))
    (gbspu-reset spu)))

(defun spu-write-memory-at-addr (spu addr val)
  "writes the available register data for SPU at ADDR to VAL"
  (case (logand addr #xff)
    ((#x10 #x11 #x12 #x13 #x14      #x16 #x17
      #x18 #x19 #x1a #x1b #x1c #x1d #x1e #x1f
      #x20 #x21 #x22 #x23)
      (spu-write-sound-channel spu addr val))
    (#x24 (write-sound-vol spu val))
    (#x25 (write-sound-pan spu val))
    (#x26 (set-gbspu-ena? spu val))
    ((#x30 #x31 #x32 #x33 #x34 #x35 #x36 #x37
      #x38 #x39 #x3a #x3b #x3c #x3d #x3e #x3f)
     (setf (aref (gbspu-wave-ram spu) (logand addr #xf)) val))
    ((#x76 #x77) ())
    (otherwise ())))

(defun spu-read-sound-channel (spu addr)
  "reads channel register data"
  (case addr
    (#xff10 (logior (channel-r0 (gbspu-ch1 spu)) #x7f))
    (#xff11 (logior (channel-r1 (gbspu-ch1 spu)) #x3f))
    (#xff12 (logior (channel-r2 (gbspu-ch1 spu)) #x00))
    (#xff13 (logior (channel-r3 (gbspu-ch1 spu)) #xff))
    (#xff14 (logior (channel-r4 (gbspu-ch1 spu)) #xbf))
    (#xff16 (logior (channel-r1 (gbspu-ch2 spu)) #x3f))
    (#xff17 (logior (channel-r2 (gbspu-ch2 spu)) #x00))
    (#xff18 (logior (channel-r3 (gbspu-ch2 spu)) #xff))
    (#xff19 (logior (channel-r4 (gbspu-ch2 spu)) #xbf))
    (#xff1a (logior
              (ash (clboy-utils:bool-as-bit (channel-dac-ena? (gbspu-ch3 spu))) 7) #x7f))
    (#xff1b (logior (channel-r1 (gbspu-ch3 spu)) #xff))
    (#xff1c (logior (channel-r2 (gbspu-ch3 spu)) #x9f))
    (#xff1d (logior (channel-r3 (gbspu-ch3 spu)) #xff))
    (#xff1e (logior (channel-r4 (gbspu-ch3 spu)) #xbf))
    (#xff20 (logior (channel-r1 (gbspu-ch4 spu)) #xff))
    (#xff21 (logior (channel-r2 (gbspu-ch4 spu)) #x00))
    (#xff22 (logior (channel-r3 (gbspu-ch4 spu)) #x00))
    (#xff23 (logior (channel-r4 (gbspu-ch4 spu)) #xbf))
    (otherwise #xff)))

(defun spu-write-sound-channel (spu addr val)
  "writes channel register data and sets up channel parameters"
  (case addr
    (#xff10 (setf (channel-r0 (gbspu-ch1 spu)) val))
    (#xff11 (set-channel-r1 (gbspu-ch1 spu) val))
    (#xff12 (set-channel-r2 (gbspu-ch1 spu) val))
    (#xff13 (setf (channel-r3 (gbspu-ch1 spu)) val))
    (#xff14 (set-square-channel-r4 (gbspu-ch1 spu) val))
    (#xff16 (set-channel-r1 (gbspu-ch2 spu) val))
    (#xff17 (set-channel-r2 (gbspu-ch2 spu) val))
    (#xff18 (setf (channel-r3 (gbspu-ch2 spu)) val))
    (#xff19 (set-square-channel-r4 (gbspu-ch2 spu) val))
    (#xff1a (set-wave-channel-r0 (gbspu-ch3 spu) val))
    (#xff1b (setf (channel-r1 (gbspu-ch3 spu)) val))
    (#xff1c (set-wave-channel-r2 (gbspu-ch3 spu) val))
    (#xff1d (setf (channel-r3 (gbspu-ch3 spu)) val))
    (#xff1e (set-wave-channel-r4 (gbspu-ch3 spu) val))
    (#xff20 (set-channel-r1 (gbspu-ch4 spu) val))
    (#xff21 (set-channel-r2 (gbspu-ch4 spu) val))
    (#xff22 (setf (channel-r3 (gbspu-ch4 spu)) val))
    (#xff23 (set-noise-channel-r4 (gbspu-ch4 spu) val))
    (otherwise ())))

(defun set-wave-channel-r0 (channel val)
  (setf (channel-r0 channel) val
        (channel-dac-ena? channel) (> (logand val #x80) 0)
        (channel-ena? channel) (if (> (logand val #x80) 0) (channel-ena? channel) nil)))

(defun set-channel-r1 (channel val)
  (setf (channel-r1 channel) val
        (channel-seq channel) (get-channel-seq-from-byte val)))

(defun set-wave-channel-r2 (channel val)
  (setf (channel-r2 channel) val
        (channel-vol channel)
        (case (logand (ash val -5) #x3)
          (#x0 4)
          (#x1 0)
          (#x2 1)
          (#x3 2))))

(defun set-channel-r2 (channel val)
  (setf (channel-r2 channel) val
        (channel-dac-ena? channel) (> (logand val #xf8) 0)
        (channel-ena? channel) (if (> (logand val #xf8) 0) (channel-ena? channel) nil)))

(defun set-wave-channel-r4 (channel val)
    (setf (channel-r4 channel) val)
    (when (> val #x7f)
      (setf (channel-ena? channel) t
            (channel-duty-timer channel) (* (- 2048 (channel-freq channel)) 2))
      (when (= (logand val #x40) #x40)
        (setf (channel-len channel)
              (if (> (channel-len channel) 0)
                  (channel-len channel)
                  (- 256 (channel-r1 channel)))))))

(defun set-noise-channel-r4 (channel val)
  (setf (channel-r4 channel) val)
  (when (> val #x7f)
    (channel-trigger channel)
    (setf (channel-duty-timer channel) (* (- 2048 (channel-freq channel)) 4)
          (channel-lfsr channel) #xff)))

(defun set-square-channel-r4 (channel val)
  (setf (channel-r4 channel) val)
  (when (> val #x7f)
    (channel-trigger channel)
    (setf (channel-duty-timer channel) (* (- 2048 (channel-freq channel)) 4))
    (channel-sweep-trigger channel)))


(defun channel-trigger (channel)
  (setf (channel-ena? channel) t
        (channel-vol channel) (channel-init-vol channel)
        (channel-env-delay channel) (channel-env-pace channel)
        (channel-len channel)
        (if (> (channel-len channel) 0)
            (channel-len channel)
            (- 64 (logand (channel-r1 channel) #x1f)))))

(defun channel-sweep-trigger (channel)
  (when (channel-has-sweep? channel)
    (setf (channel-shadow-freq channel) (channel-freq channel))
    (if (= (channel-sweep-pace channel) 0)
        (setf (channel-sweep-delay channel) 8)
        (setf (channel-sweep-delay channel) (channel-sweep-pace channel)))
    (setf (channel-sweep-ena? channel)
          (and (> (channel-sweep-pace channel) 0 )
               (> (channel-sweep-step channel) 0)))
    (when (> (channel-sweep-step channel) 0)
      (sweep-freq-calc channel))))

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

(defun gen-lfsr (channel)
  (let* ((lfsr (channel-lfsr channel))
         (xor-res (logxor (ldb (byte 1 0) (logand lfsr #x01)) (ldb (byte 1 0) (logand (ash lfsr -1) #x01))))
         (new-lfsr (logior (ash lfsr -1) (ash xor-res 14))))
    (if (channel-lfsr-width-mode-bit channel)
        (logand (logior new-lfsr (ash xor-res 6)) #x7f)
        new-lfsr)))

(defun step-square-channel (channel frame-sequencer)
  "step the square wave CHANNEL based on the frame sequencer"
  (when (channel-ena? channel)
      (when (and (channel-has-sweep? channel)
                 (or (= frame-sequencer #x2) (= frame-sequencer #x6))
                 (channel-sweep-ena? channel))
        (step-channel-sweep channel))


      (when (and (= (mod frame-sequencer 2) 0)
                 (channel-len-ena? channel))
        (step-channel-len channel))

      (when (and (= frame-sequencer 7)
                 (> (channel-env-pace channel) 0))
        (step-channel-env channel))))

(defun step-wave-channel (channel frame-sequencer)
  "step the wave sequencer CHANNEL based on the FRAME-SEQUENCER"
  (when (channel-ena? channel)

      (when (and (= (mod frame-sequencer 2) 0)
                 (channel-len-ena? channel))
        (step-channel-len channel))))

(defun step-noise-channel (channel frame-sequencer)
  "step the noise CHANNEL based on the FRAME-SEQUENCER"
  (when (channel-ena? channel)
      (when (and (= (mod frame-sequencer 2) 0)
                 (channel-len-ena? channel))
        (step-channel-len channel))

      (when (and (= frame-sequencer 7)
                 (> (channel-env-pace channel) 0))
        (step-channel-env channel))))

(defun step-wave-channel-seq (channel spu cycles)
  (when (<= (decf (channel-duty-timer channel) cycles) 0)
    (when (= (channel-cur-bit channel) 0)
      (incf (channel-cur-addr channel))
      (if (> (channel-cur-addr channel) #xf) (setf (channel-cur-addr channel) 0))
      (setf (channel-seq channel) (spu-read-memory-at-addr spu (+ #xff30 (channel-cur-addr channel)))))
    (incf (channel-duty-timer channel) (* (- 2048 (channel-freq channel)) 2))
    (setf (channel-cur-bit channel) (if (= (channel-cur-bit channel) 0) 1 0))))

(defun wave-channel-digital-output (channel)
  (ash (logand
         (ash (channel-seq channel)
              (if (= (channel-cur-bit channel) 0) -4 0))
         #xf)
       (- (channel-vol channel))))

(defun get-wave-channel-output (channel)
  (channel-stereo-output
    channel
    (channel-dac
      channel
      (wave-channel-digital-output channel))))

(defun channel-dac (channel out)
  (if (and (channel-dac-ena? channel) (not (channel-muted? channel)))
      (if (channel-ena? channel)
          (coerce (/ out 100) 'single-float)
          0.0)
      0.0))

(defun step-channel-seq (channel cycles)
  (when (<= (decf (channel-duty-timer channel) cycles) 0)
    (incf (channel-duty-timer channel) (* (- 2048 (channel-freq channel)) 4))
    (setf (channel-cur-bit channel) (mod (+ (channel-cur-bit channel) 1) 8))))

(defun square-channel-digital-ouput (channel)
  (* (logand
       (ash (channel-seq channel)
            (- 0 (channel-cur-bit channel)))
       #x1)
     (channel-vol channel)))

(defun get-square-channel-output (channel)
  (channel-stereo-output
    channel
    (channel-dac channel
                 (square-channel-digital-ouput channel))))

(defun step-noise-channel-seq (channel cycles)
  (when (<= (decf (channel-duty-timer channel) cycles) 0)
    (incf (channel-duty-timer channel) (channel-noise-freq channel))
    (setf (channel-lfsr channel) (gen-lfsr channel))))

(defun noise-channel-digital-ouput (channel)
  (* (logand (channel-lfsr channel) #x1)
     (channel-vol channel)))

(defun get-noise-channel-output (channel)
  (channel-stereo-output channel
                         (channel-dac channel
                                      (noise-channel-digital-ouput channel))))

(defun channel-stereo-output (channel out)
  (list
    (if (channel-left-output channel) out 0.0)
    (if (channel-right-output channel) out 0.0)))

(defun step-channel-len (channel)
  (if (<= (decf (channel-len channel)) 0)
      (setf (channel-ena? channel) nil)))

(defun step-channel-env (channel)
  (when (<= (decf (channel-env-delay channel)) 0)
    (setf (channel-env-delay channel) (channel-env-pace channel))
    (let ((new-vol (+ (channel-vol channel) (if (channel-env-rising? channel) 1 -1))))
      (if (and (>= new-vol 0) (<= new-vol 15))
          (setf (channel-vol channel) new-vol)))))

(defun step-channel-sweep (channel)
  (when (= (decf (channel-sweep-delay channel)) 0)
    (setf (channel-sweep-delay channel) (channel-sweep-pace channel))
    (if (= (channel-sweep-delay channel) 0) (setf (channel-sweep-delay channel) 8))
    (when (and (> (channel-sweep-pace channel) 0)
               (channel-sweep-ena? channel))
      (let ((new-freq (sweep-freq-calc channel)))
        (when (and (< new-freq 2048) (> (channel-sweep-step channel) 0))
          (setf (channel-shadow-freq channel) new-freq)
          (writeback-channel-freq channel new-freq)
          (sweep-freq-calc channel))))))

(defun sweep-freq-calc (channel)
  (let ((new-freq
          (calc-new-freq (channel-sweep-step channel)
                         (channel-shadow-freq channel)
                         (channel-sweep-neg? channel))))
    (if (>= new-freq 2048) (setf
                             ;(channel-sweep-ena? channel) nil
                             (channel-ena? channel) nil))
    new-freq))

(defun calc-new-freq (channel-sweep cur-freq is-sweep-neg)
  (let ((new-freq (funcall
                    (if is-sweep-neg #'- #'+)
                    cur-freq (ash cur-freq (- channel-sweep)))))
    (if (and *debug* (> channel-sweep 0))
        (format t "~A ~A ~A = ~A~%"
                cur-freq
                (if is-sweep-neg '- '+)
                (ash cur-freq (- channel-sweep))
                new-freq))
    new-freq))

(defun spu-queue-audio (spu)
  "queue data from the buffer into the SDL2 audio device"
  ;(sdl2:queue-audio (gbspu-device spu) (gbspu-buffer spu))
  (loop while (> (sdl2-ffi.functions:sdl-get-queued-audio-size (gbspu-device spu)) (* 4096 4)))
  (sdl2-ffi.functions:sdl-queue-audio
    (gbspu-device spu)
    (static-vectors:static-vector-pointer (gbspu-buffer spu))
    (* (gbspu-buffer-index spu) 4))
  (setf (gbspu-buffer-index spu) 0))

(defun step-spu (spu cycles cycles-per-sample cycles-frame-seq-step)
  "step the channels by the number of elapsed cycles"
  (when (gbspu-ena? spu)
    (step-channel-seq (gbspu-ch1 spu) cycles)
    (step-channel-seq (gbspu-ch2 spu) cycles)
    (step-wave-channel-seq (gbspu-ch3 spu) spu cycles)
    (step-noise-channel-seq (gbspu-ch4 spu) cycles)
    (when (<= (decf (gbspu-frame-sequencer-delay spu) cycles) 0)
      (setf (gbspu-frame-sequencer spu) (mod (+ (gbspu-frame-sequencer spu) 1) 8))
      (incf (gbspu-frame-sequencer-delay spu) cycles-frame-seq-step)
      (step-square-channel (gbspu-ch1 spu) (gbspu-frame-sequencer spu))
      (step-square-channel (gbspu-ch2 spu) (gbspu-frame-sequencer spu))
      (step-wave-channel (gbspu-ch3 spu) (gbspu-frame-sequencer spu))
      (step-noise-channel (gbspu-ch4 spu) (gbspu-frame-sequencer spu)))
    (when (<= (decf (gbspu-delay spu) cycles) 0)
      (incf (gbspu-delay spu) cycles-per-sample)
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
      (spu-queue-audio spu))))
