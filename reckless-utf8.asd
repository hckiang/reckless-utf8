(defsystem "reckless-utf8"
  :description "GC-friendly UTF8 encoder/decoder"
  :version "0.9.10"
  :author "Woodrow Hao Chi Kiang"
  :license "BSD-2-Clause"
  :depends-on ("uiop" "serapeum")
  :serial t
  :components ((:module "reckless-utf8"
                :pathname "src/"
                :serial t
                :components ((:file "package")
                             (:file "conditions")
                             (:file "reckless-utf8")))))
