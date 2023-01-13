(in-package :cl-web3)

(defparameter METHODID-APPROVE (CreateMethodId-n0x "approve(address,uint256)"))
(defparameter METHODID-BALANCEOF-n0x (CreateMethodId-n0x "balanceOf(address)"))
(defparameter METHODID-BALANCEOF (concatenate 'string "0x" METHODID-BALANCEOF-n0x))
(defparameter METHODID-TRANSFER (CreateMethodId-n0x "transfer(address,uint256)"))
(defparameter METHODID-ALLOWANCE-n0x (CreateMethodId-n0x "allowance(address,address)"))
(defparameter METHODID-ALLOWANCE (concatenate 'string "0x" METHODID-ALLOWANCE-n0x))
(defparameter METHODID-TOTALSUPPLY (CreateMethodId "totalSupply()"))
(defparameter METHODID-TOTALSUPPLY-n0x (CreateMethodId-n0x "totalSupply()"))
(defparameter METHODID-DECIMALS-n0x (CreateMethodId-n0x "decimals()"))
(defparameter METHODID-DECIMALS (CreateMethodId "decimals()"))
(defparameter METHODID-IERC20-sbl-n0x (CreateMethodId-n0x "symbol()"))
(defparameter METHODID-IERC20-sbl (CreateMethodId "symbol()"))
(defparameter METHODID-IERC20-withdraw (CreateMethodId-n0x "withdraw(uint256)"))

(defparameter METHODID-UniV3-GETPOOL (CreateMethodId "getPool(address,address,uint24)"))


(defparameter METHODID-DEPOSIT (CreateMethodId-n0x "deposit(address,uint256,address,uint16)"))
(defparameter METHODID-WITHDRAW (CreateMethodId-n0x "withdraw(address,uint256,address)"))
(defparameter METHODID-DEPOSITETH (CreateMethodId-n0x "depositETH(address,address,uint16)"))
(defparameter METHODID-WITHDRAWETH (CreateMethodId-n0x "withdrawETH(address,uint256,address)"))
(defparameter METHODID-GETRESERVEDATA (CreateMethodId "getReserveData(address)"))
(defparameter METHODID-GETRESERVETOKENSADDRESSES (CreateMethodId "getReserveTokensAddresses(address)"))
(defparameter METHODID-GETWETHADDRESS (CreateMethodId "getWETHAddress()"))

(defparameter METHODID-UNISWAP-GETAMOUNTSOUT  (CreateMethodId "getAmountsOut(uint256,address[])"))

(defparameter METHODID-UNISWAP-ALLPAIRSLENGTH (CreateMethodId "allPairsLength()"))
(defparameter METHODID-UNISWAP-ALLPAIRS (CreateMethodId-n0x "allPairs(uint256)"))

(defparameter METHODID-UNISWAP-getPair (CreateMethodId "getPair(address,address)"))
(defparameter METHODID-UNISWAP-getPair-n0x (CreateMethodId-n0x "getPair(address,address)"))
(defparameter METHODID-UNISWAP-getReserves (CreateMethodId "getReserves()"))
(defparameter METHODID-UNISWAP-getReserves-n0x (CreateMethodId-n0x "getReserves()"))
(defparameter METHODID-UNISWAP-PairToken0 (CreateMethodId-n0x "token0()"))
(defparameter METHODID-UNISWAP-PairToken1 (CreateMethodId-n0x "token1()"))
(defparameter METHODID-querySwapByBalancer (CreateMethodId "querySwapByBalancer(uint256[])"))

(defclass ChainPrc ()
  ((AccountAddr :initarg :AccountAddr :accessor AccountAddr)
   (MultiCallAdr :initarg :MultiCallAdr :accessor MultiCallAdr)
   (ChainId :initarg :ChainId :accessor ChainId)
   (RpcSvr :initarg :RpcSvr :accessor RpcSvr)
   (WMainTkName :initarg :WMainTkName :accessor WMainTkName)

   (aLndPlAdr  :initarg :aLndPlAdr :accessor aLndPlAdr)
   (aOracleAdr :initarg :aOracleAdr :accessor aOracleAdr)
   (prtlDtPrvderAddr :initarg :prtlDtPrvderAddr :accessor prtlDtPrvderAddr)
   (wethGtWayAdr  :initarg :wethGtWayAdr :accessor wethGtWayAdr)

   (UniswapRouterAdr :initarg :UniswapRouterAdr :accessor UniswapRouterAdr)
   (UniswpFctryAdr :initform nil :accessor UniswpFctryAdr)

   (gasPrice  :initform nil :accessor gasPrice)
   (swapNonce  :initform nil :accessor swapNonce)
   (LastChainErrInfo :initform nil :accessor LastChainErrInfo) )
  (:default-initargs
   :ChainId (error "ChinaId required.")
    :RpcSvr (error "RpcSvr required.")
    :WMainTkName (error "Mainmnyname required.")))
