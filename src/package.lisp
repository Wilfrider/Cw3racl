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
   #:get-current-balance
   #:get-token-current-balance
   #:send-eth
   #:send-contract-transaction
   #:get-atoken-address
   #:aave-deposit-token
   #:aave-get-token-deposited-amount
   #:aave-withdraw-token
   #:swap-native-token-to-other-token-by-uniswapv2
   ))
