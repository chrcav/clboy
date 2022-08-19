
(defsystem "clboy"
           :description "clboy: just another GB emulator"
           :version "0.0.1"
           :author "Christopher Cavanaugh <chriswcav AT gmail DOT com>"
           :license "MIT"
           :serial t
           :depends-on (:sdl2
                        :static-vectors)
           :components ((:file "package")
                        (:file "opcodes")
                        (:file "cpu")
                        (:file "ppu")
                        (:file "spu")
                        (:file "cart")
                        (:file "gb")
                        ))
