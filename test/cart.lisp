


(defpackage :clboy-test.cart
  (:use :common-lisp :clboy :try)
  (:import-from :clboy
                #:make-gbcart
                #:gbcart-rom
                #:gbcart-ram
                #:gbcart-ramon
                #:gbcart-rombank
                #:gbcart-rambank
                #:gbcart-rommask
                #:gbcart-rammask
                #:gbcart-timer
                #:rtc-latched?
                #:cart-read-memory-at-addr
                #:cart-write-memory-at-addr)
  (:export #:test-carts))

(in-package :clboy-test.cart)


;; test rom memory replace calls
(deftest test-carts ()
  (test-cart-nombc)
  (test-cart-mbc3))

;; no MBC tests
(deftest test-cart-nombc ()
  (test-nombc-reads)
  (test-nombc-writes))

(deftest test-nombc-reads ()
  (test-nombc-rom-reads)
  (test-nombc-ram-reads))

(deftest test-nombc-rom-reads ()
  (loop repeat 20
        for cart = (gen-nombc-cart #x8000 #x2000)
        for addr = (random #x8000) do
        (is (= (cart-read-memory-at-addr cart addr) (aref (gbcart-rom cart) addr)))))

(deftest test-nombc-ram-reads ()
  (loop repeat 20
        for cart = (gen-nombc-cart #x8000 #x2000)
        for addr = (+ #xa000 (random #x2000)) do
        (is (= (cart-read-memory-at-addr cart addr) (aref (gbcart-ram cart) (logand addr #x1fff))))))

(deftest test-nombc-writes ()
  (test-nombc-rom-writes)
  (test-nombc-ram-writes))

(deftest test-nombc-rom-writes ()
  (loop repeat 20
        for cart = (gen-nombc-cart #x8000 #x2000)
        for addr = (random #x8000) do
        (let ((orig (cart-read-memory-at-addr cart addr)))
          (cart-write-memory-at-addr cart addr #xff)
          (is (= (cart-read-memory-at-addr cart addr) orig)))))

(deftest test-nombc-ram-writes ()
  (loop repeat 20
        for cart = (gen-nombc-cart #x8000 #x2000)
        for addr = (+ #xa000 (random #x2000)) do
        (cart-write-memory-at-addr cart addr #xff)
        (is (= (aref (gbcart-ram cart) (logand addr #x1fff)) #xff))))


(defun gen-nombc-cart (romsize ramsize)
  (make-gbcart :carttype 0
               :rom (make-array romsize :initial-contents (loop for addr from 0 to (- romsize 1) collect addr))
               :ram (make-array ramsize :initial-contents (loop for addr from 0 to (- ramsize 1) collect addr))))

;; TODO MBC1 tests
;; TODO MBC2 tests
;; MBC3 tests
(deftest test-cart-mbc3 ()
  (loop for carttype in '(#x0f #x10 #x11 #x12 #x13) do
    (test-mbc3-reads carttype)
    (test-mbc3-writes carttype)))

(deftest test-mbc3-reads (carttype)
  (test-mbc3-rom-reads carttype)
  (test-mbc3-ram-reads carttype))

(deftest test-mbc3-rom-reads (carttype)
  (test-mbc3-rom-reads-bank1 carttype)
  (test-mbc3-rom-reads-bank2 carttype))

(deftest test-mbc3-rom-reads-bank1 (carttype)
  (loop repeat 20
        for cart = (gen-mbc3-cart carttype #x8000 #x2000)
        for addr = (random #x4000) do
        (is (= (cart-read-memory-at-addr cart addr) (aref (gbcart-rom cart) addr)))))

(deftest test-mbc3-rom-reads-bank2 (carttype)
  (loop repeat 20
        for cart = (gen-mbc3-cart carttype #x8000 #x2000)
        for addr = (+ (random #x4000) #x4000) do
        (is (= (cart-read-memory-at-addr cart addr) (aref (gbcart-rom cart) addr)))))

(deftest test-mbc3-ram-reads (carttype)
  (loop repeat 20
        for cart = (gen-mbc3-cart carttype #x8000 #x2000)
        for addr = (+ #xa000 (random #x2000)) do
        (is (= (cart-read-memory-at-addr cart addr) (aref (gbcart-ram cart) (logand addr #x1fff))))))

(deftest test-mbc3-writes (carttype)
  (test-mbc3-rom-writes carttype)
  (test-mbc3-ram-writes carttype))

(deftest test-mbc3-rom-writes (carttype)
  (test-mbc3-ramon-write carttype)
  (test-mbc3-rombank-write carttype)
  (test-mbc3-rambank-write carttype)
  (test-mbc3-latch-write carttype)
  (test-mbc3-rom-writes-bank1 carttype))

(deftest test-mbc3-ramon-write (carttype)
  (let ((cart (gen-mbc3-cart carttype #x8000 #x2000 :ramon nil)))
    (cart-write-memory-at-addr cart #x1000 #x0a)
    (is (gbcart-ramon cart) :ctx ("gbcart-ramon is ~A should be T" (gbcart-ramon cart)))
    (cart-write-memory-at-addr cart #x1000 #xa0)
    (is (not (gbcart-ramon cart)) :ctx ("gbcart-ramon is ~A should be NIL" (gbcart-ramon cart)))))

(deftest test-mbc3-rombank-write (carttype)
  (loop for cart = (gen-mbc3-cart carttype #x20000 #x2000)
        for bank from 0 to 8 do
        (cart-write-memory-at-addr cart #x3000 bank)
        (is (= (gbcart-rombank cart) (logand (if (= bank 0) 1 bank) (gbcart-rommask cart)))
            :ctx ("gbcart-rombank is ~A should be ~A" (gbcart-rombank cart) bank))
        (is (= (cart-read-memory-at-addr cart #x4500)
               (aref (gbcart-rom cart)
                     (logand (+ (* (if (= bank 0) 1 bank) #x4000) (logand #x4500 #x1fff)) #x1ffff))))))

(deftest test-mbc3-rambank-write (carttype)
  (loop for cart = (gen-mbc3-cart carttype #x8000 #x8000)
        for bank from 0 to 4 do
        (cart-write-memory-at-addr cart #x5000 bank)
        (is (= (gbcart-rambank cart) (logand bank (gbcart-rammask cart)))
            :ctx ("gbcart-rambank is ~A should be ~A" (gbcart-rambank cart) bank))
        (is (= (cart-read-memory-at-addr cart #xa500)
               (aref (gbcart-ram cart)
                     (logand (+ (* bank #x2000) (logand #xa500 #x1fff)) #x7fff))))))

(deftest test-mbc3-latch-write (carttype)
  (let ((cart (gen-mbc3-cart carttype #x8000 #x2000 :ramon nil)))
    (cart-write-memory-at-addr cart #x7000 #x00)
    (is (not (rtc-latched? (gbcart-timer cart)))
        :ctx ("gbcart-ramon is ~A should be NIL" (rtc-latched? (gbcart-timer cart))))
    (cart-write-memory-at-addr cart #x7000 #x01)
    (is (rtc-latched? (gbcart-timer cart))
        :ctx ("gbcart-timer is ~A should be T" (rtc-latched? (gbcart-timer cart))))
    (cart-write-memory-at-addr cart #x7000 #x00)
    (is (rtc-latched? (gbcart-timer cart))
        :ctx ("gbcart-timer is ~A should be T" (rtc-latched? (gbcart-timer cart))))
    (cart-write-memory-at-addr cart #x7000 #x01)
    (is (not (rtc-latched? (gbcart-timer cart)))
        :ctx ("gbcart-timer is ~A should be NIL" (rtc-latched? (gbcart-timer cart))))))

(deftest test-mbc3-rom-writes-bank1 (carttype)
  (loop repeat 20
        for cart = (gen-mbc3-cart carttype #x8000 #x2000)
        for addr = (random #x8000) do
        (let ((orig (cart-read-memory-at-addr cart addr)))
          (cart-write-memory-at-addr cart addr #xff)
          (is (= (cart-read-memory-at-addr cart addr) orig)))))


(deftest test-mbc3-ram-writes (carttype)
  (loop repeat 20
        for cart = (gen-mbc3-cart carttype #x8000 #x2000)
        for addr = (+ #xa000 (random #x2000)) do
        (cart-write-memory-at-addr cart addr #xff)
        (is (= (aref (gbcart-ram cart) (logand addr #x1fff)) #xff))))


(defun gen-mbc3-cart (carttype romsize ramsize &key (ramon t))
  (make-gbcart :carttype carttype ;; through #x13
               :rom (make-array romsize :initial-contents (loop for addr from 0 to (- romsize 1) collect addr))
               :rommask (- (floor romsize #x4000) 1)
               :rombank 1
               :ram (make-array ramsize :initial-contents (loop for addr from 0 to (- ramsize 1) collect addr))
               :ramon ramon
               :rammask (- (floor ramsize #x2000) 1)
               :rambank 0))

;; TODO MBC5 tests
