


(defpackage :clboy-test.gb
  (:use :common-lisp :clboy :try)
  (:import-from :clboy
                #:make-gb
                #:dump-mem-region
                )
  (:export #:test-roms))

(in-package :clboy-test.gb)


(defun dump-blargg-output (gb)
  "reads the blargg test rom memory region where output is stored as a list of bytes"
  (clboy:dump-mem-region gb #x9800 #x9BFF))

;; test rom memory replace calls
(deftest test-roms ()
  (test-blargg-01-special))

;(clboy:load-cart (clboy:make-gbcart-from-rom "./opus4.gb"))
;(clboy:load-cart (clboy:make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/instr_timing/instr_timing.gb")) ; PASSED
;(clboy:load-cart (clboy:make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/cpu_instrs.gb")) ; PASSED
(deftest test-blargg-01-special ()
  (let ((gb (make-gb)))
    (clboy:load-cart gb (clboy:make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/01-special.gb")) ; PASSED
    (clboy:run gb)))
;(clboy:load-cart (clboy:make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/02-interrupts.gb")) ; PASSED
;(clboy:load-cart (clboy:make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/03-op sp,hl.gb")) ; PASSED
;(clboy:load-cart (clboy:make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/04-op r,imm.gb")) ; PASSED
;(clboy:load-cart (clboy:make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/05-op rp.gb")) ; PASSED
;(clboy:load-cart (clboy:make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/06-ld r,r.gb")) ; PASSED
;(clboy:load-cart (clboy:make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/07-jr,jp,call,ret,rst.gb")) ; PASSED
;(clboy:load-cart (clboy:make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/08-misc instrs.gb")) ; PASSED
;(clboy:load-cart (clboy:make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/09-op r,r.gb")) ; PASSED
;(clboy:load-cart (clboy:make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/10-bit ops.gb")) ; PASSED
;(clboy:load-cart (clboy:make-gbcart-from-rom "~/repos/github/retrio/gb-test-roms/cpu_instrs/individual/11-op a,(hl).gb")) ; PASSED

;(clboy:load-cart (clboy:make-gbcart-from-rom "./mts-20220522-1522-55c535c/emulator-only/mbc1/multicart_rom_8Mb.gb"))


