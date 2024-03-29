
(defpackage :clboy
  (:use :common-lisp)
  (:export run load-cart dump-mem-region make-gbcart-from-rom start-gb start-cgb))

(in-package :clboy)



(defstruct gb
  "defines a GameBoy system"
  (cpu (make-gbcpu))
  (ppu (make-gbppu))
  (spu (make-gbspu))
  (cart nil)
  (input (make-gbinput))
  (stopped? nil :type boolean)
  (paused? nil :type boolean)
  (bios (make-dmg-bios))
  (is-bios? t :type boolean)
  (div-clock 0 :type (unsigned-byte 8))
  (tima 0 :type (unsigned-byte 8))
  (tma 0 :type (unsigned-byte 8))
  (tac 0 :type (unsigned-byte 8))
  (int-ram (make-array #x8000 :initial-element 0 :element-type '(unsigned-byte 8)))
  (int-ram-bank 1 :type (unsigned-byte 8))
  (zero-page (make-array #x100 :initial-element 0 :element-type '(unsigned-byte 8))))

(defstruct (cgb (:include gb
                 (bios (make-cgb-bios))
                 (int-ram (make-array #x8000 :initial-element 0 :element-type '(unsigned-byte 8)))
                 (ppu (make-cgbppu))))
  "defines a GameBoy Color system"
  (is-speed-armed? nil :type boolean)
  (is-double-speed? nil :type boolean))
