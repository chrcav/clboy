
(defsystem "clboy"
           :description "clboy: just another GB emulator"
           :version "0.0.1"
           :author "Christopher Cavanaugh <chriswcav AT gmail DOT com>"
           :license "MIT"
           :depends-on (:sdl2
                        :static-vectors)
           :components ((:module "src"
                         :serial t
                         :components
                           ((:file "package")
                           (:file "opcodes")
                           (:file "cpu")
                           (:file "ppu")
                           (:file "spu")
                           (:file "cart")
                           (:file "gb")))))

(defsystem "clboy/test"
           :description "tests for clboy system"
           :version "0.0.1"
           :author "Christopher Cavanaugh <chriswcav AT gmail DOT com>"
           :license "MIT"
           :depends-on (:clboy
                        :try)
           :components ((:module "test"
                         :serial t
                         :components
                         ((:file "cpu")
                          (:file "tests")))))
