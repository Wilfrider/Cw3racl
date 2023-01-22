(in-package :web3-example)

(defparameter goerliChainObj (make-instance 'ChainPrc :AccountAddr "50118204D3d095519890ABDB488E634390FcA463" :MultiCallAdr "5BA1e12693Dc8F9c48aAD8770482f4739bEeD696" :ChainId 5
                                            :RpcSvr "https://goerli.infura.io/v3/9aa3d95b3bc440fa88ea12eaa4456161" :WMainTkName "WETH"
                                            :aLndPlAdr "368EedF3f56ad10b9bC57eed4Dac65B26Bb667f6" :aOracleAdr nil :prtlDtPrvderAddr "9BE876c6DC42215B00d7efe892E2691C3bc35d10" :wethGtWayAdr "d5B55D3Ed89FDa19124ceB5baB620328287b915d"
                                            :UniswapRouterAdr "1b02da8cb0d097eb8d57a175b88c7d8b47997506"))

(wtLogInfo "block-number:~a" (get-block-number goerliChainObj))

(wtLogInfo "eth_gasPrice:~a" (get-eth-gasPrice goerliChainObj))

(wtLogInfo "getTransactionCount:~a" (getTransactionCount  goerliChainObj))

(wtLogInfo "getUserAccountData:~a" (snd1contractCall goerliChainObj (aLndPlAdr goerliChainObj) "getUserAccountData(address)" (list (AccountAddr goerliChainObj))))
