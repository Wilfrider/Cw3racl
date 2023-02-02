(defpackage :web3
  (:nicknames :cl-web3)
  (:use :common-lisp)
  (:export
   #:wtLogInfo
   #:wtLogWarn

   #:ChainPrc
   #:AccountAddr
   #:aLndPlAdr
   #:UniswapRouterAdr

   #:METHODID-APPROVE

   #:get-block-number
   #:get-gas-price
   #:get-trans-nones
   #:send-contract-call
   #:get-atoken-address
   #:get-current-balance
   #:get-token-current-balance
   #:send-eth
   #:send-contract-transaction))
