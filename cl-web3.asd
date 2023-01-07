(asdf:defsystem :cl-web3
  :version "0.1.0"
  :licence "MIT"
  :description "cl-web3 is common web3 rpc access client by lisp; Applicable to all evm compatible blockchains."
  :maintainer "Wilfrider Wu <59bc26c3@protonmail.com>"
  :components ((:static-file "cl-web3.asd")
               (:module :src
                        :components ((:file "package")
                                     (:file "globalVarDef")
                                     (:file "cmmFunDef")
                                     (:file "web3FunDef"))))
  :depends-on (:cffi
               :uffi
               :flexi-streams
               :cl-ppcre
               :split-sequence
               :parse-float
               :cl-json
               :ironclad))



(asdf:defsystem :cl-web3.test
  :components ((:module :test
                        :components ((:file "package")
                                     (:file "web3Test"))))
  :depends-on (:rt
               :cl-web3
               :flexi-streams
               :ironclad
               ))
