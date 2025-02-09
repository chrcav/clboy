
(defsystem "clboy"
           :description "clboy: just another GB emulator"
           :version "0.0.1"
           :author "Christopher Cavanaugh <christopher AT chrcav DOT dev>"
           :license "MIT"
           :depends-on (:sdl2
                        :static-vectors)
           :components ((:module "src"
                         :serial t
                         :components
                           ((:file "package")
                            (:file "utils")
                            (:file "profile")
                            (:file "cpu")
                            (:file "opcodes")
                            (:file "ppu")
                            (:file "spu")
                            (:file "cart")
                            (:file "gb"))))
           :in-order-to ((test-op (test-op clboy/test))))

(defsystem "clboy/test"
           :description "tests for clboy system"
           :version "0.0.1"
           :author "Christopher Cavanaugh <christopher AT chrcav DOT dev>"
           :license "MIT"
           :depends-on (:clboy
                        :try)
           :components ((:module "test"
                         :serial t
                         :components
                         ((:file "opcodes")
                          (:file "cart")
                          (:file "gb")
                          (:file "tests"))))
           :perform (test-op (o s)
                      (uiop:symbol-call '#:clboy-test '#:test)))
