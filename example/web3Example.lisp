(in-package :web3-example)

(defparameter goerliChainObj (make-instance 'ChainPrc :AccountAddr "3382bfe6977E0B1e8AfbC349d7Ee6811C507D1bf" :PrivateKey (ironclad:hex-string-to-byte-array "9248135837d089748b62124b3ffaacc13d1350131c7af0ae95b9df6283a828e0")
                                            :MultiCallAdr "5BA1e12693Dc8F9c48aAD8770482f4739bEeD696" :ChainId 5
                                            :RpcSvr "https://goerli.infura.io/v3/9aa3d95b3bc440fa88ea12eaa4456161" :WMainTkName "WETH"
                                            :aLndPlAdr "368EedF3f56ad10b9bC57eed4Dac65B26Bb667f6" :aOracleAdr nil :prtlDtPrvderAddr "9BE876c6DC42215B00d7efe892E2691C3bc35d10" :wethGtWayAdr "d5B55D3Ed89FDa19124ceB5baB620328287b915d"
                                            :UniswapRouterAdr "1b02da8cb0d097eb8d57a175b88c7d8b47997506"))

(wtLogInfo "block-number:~a" (get-block-number goerliChainObj))

(wtLogInfo "eth_gasPrice:~a" (get-gas-price goerliChainObj))

(wtLogInfo "getTransactionCount:~a" (get-trans-nones  goerliChainObj))

(wtLogInfo "getUserAccountData:~a" (send-contract-call goerliChainObj (aLndPlAdr goerliChainObj) "getUserAccountData(address)" (list (AccountAddr goerliChainObj))))

(defparameter gWethAdr "2e3A2fb8473316A02b8A297B982498E661E1f6f5")

(wtLogInfo "get-atoken-address:~a" (get-atoken-address  goerliChainObj gWethAdr))

(wtLogInfo "getCurtBalance:~a" (get-current-balance   goerliChainObj))

(wtLogInfo "getTkCurtBalance:~a" (get-token-current-balance  goerliChainObj gWethAdr))

(defparameter gToAddr "8118b91E267E1f0A6D793fBa841263BeeA86b16A")

(wtLogInfo "current account balance:~a before sndval" (get-current-balance  goerliChainObj))

(wtLogInfo "from current accountaddr snd 2.1 eth to address:~a returned hash is:~a" gToAddr (send-eth goerliChainObj gToAddr "2.1" 18 240000))

(wtLogInfo "current account balance:~a after sndval" (get-current-balance  goerliChainObj))

(wtLogInfo "from current accountaddr APPROVE returned hash is:~a" (send-contract-transaction goerliChainObj gWethAdr METHODID-APPROVE (list (UniswapRouterAdr goerliChainObj) #x1229990) 80000))

(wtLogInfo "deposit 1.3 weth to AAVE for get interest returned hash:~a" (aave-deposit-token goerliChainObj gWethAdr "1.3" 18))

(wtLogInfo "get current account deposited amount of weth at AAVE  is:~a" (aave-get-token-deposited-amount  goerliChainObj gWethAdr))

(wtLogInfo "withdraw 1.3 weth from AAVE returned hash is:~a" (aave-withdraw-token  goerliChainObj gWethAdr "1.3" 18))

(defparameter gUniAdr "8118b91E267E1f0A6D793fBa831263BeeA86b16A")

(wtLogInfo "swap-native-token-to-other-token-by-uniswapv2 returned hash is:~a" (swap-native-token-to-other-token-by-uniswapv2 goerliChainObj "1" 18 (list gWethAdr gUniAdr) "5.5" 18))

(wtLogInfo "swap-1Token-to-native-token-by-uniswapv2 returned hash is:~a" (swap-1Token-to-native-token-by-uniswapv2 goerliChainObj "1" 18 (list gUniAdr gWethAdr) "5.5" 18))

(defparameter gLinkAdr "9918b91E267E1f0A6D793fBa831263BeeA86b16A")

(wtLogInfo "swap-1Token-to-other-token-by-uniswapv2 returned hash is:~a" (swap-1Token-to-other-token-by-uniswapv2 goerliChainObj "1" 18 (list gLinkAdr gUniAdr) "5.5" 18))
