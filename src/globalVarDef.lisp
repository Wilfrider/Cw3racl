(in-package :cl-web3)

(uffi:def-enum gLogLevel ((:Debug 0) :Info :Key :Warn :Error) :separator-string "-")
(defparameter *gcur-LogLevel* gLogLevel-Debug)

(defparameter gCurUsrHomeDir (sb-unix:uid-homedir (sb-unix:unix-getuid)))

(defparameter gTimestampOffset (* 3600 8))

(defparameter gBalancerSubgraphBaseUrl "https://api.thegraph.com/subgraphs/name/balancer-labs")

(defparameter gMaxInt256 #xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)

(defparameter gBlcVaultAdr "BA12222222228d8Ba445958a75a0704d566BF2C8")

(defparameter gUniV3QuoterAdr "b27308f9F90D607463bb33eA1BeBb41C27CE5AB6")
(defparameter gUniV3SwapRouterAdr  "E592427A0AEce92De3Edee1F18E0157C05861564")
(defparameter gUniV3FactoryAdr "1F98431c8aD98523631AE4a59f267346ea31F984")

(defparameter gGasPrcScale 1.2)

(defparameter APPROVE-GASLIMINT 80000)
