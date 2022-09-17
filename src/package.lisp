
(defpackage :clboy
  (:use :common-lisp)
  (:export run load-cart dump-mem-region make-gbcart-from-rom))

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
  (bios (make-bios))
  (is-bios? t :type boolean)
  (int-ram (make-array #x2000 :initial-element 0 :element-type '(unsigned-byte 8)))
  (zero-page (make-array #x100 :initial-element 0 :element-type '(unsigned-byte 8))))
