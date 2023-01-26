(defpackage :web3
  (:nicknames :cl-web3)
  (:use :common-lisp)
  (:export
   #:wtLogInfo

   #:ChainPrc
   #:AccountAddr
   #:aLndPlAdr
   #:UniswapRouterAdr

   #:get-block-number
   #:get-eth-gasPrice
   #:getTransactionCount
   #:snd1contractCall
   #:get-aTokenAddr
   #:getCurtBalance
   #:getTkCurtBalance
   ))
