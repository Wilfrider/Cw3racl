(in-package :web3)

(defun make-request (curObj method params)
  (let (orgRet response
               (raw-body (cl-json:encode-json-to-string `(("jsonrpc" . "2.0")
                                                          ("method" . ,method)
                                                          ("params" . ,params)
                                                          ("id" . ,(ChainId curObj))))))
    (setf orgRet  (htppClientAccess (RpcSvr curObj) :reqHeaders "Content-Type:application/json<|>" :postFields raw-body))
    (when (and orgRet (= 200  (car orgRet)))
      (setf orgRet (third orgRet))
      (unless (StrIsJson orgRet)
        (setf (LastChainErrInfo curObj) orgRet)
        (return-from make-request nil))

      (setf response (inline-destructure-response orgRet))
      (if (inline-response-error response)
          (progn
            (setf (LastChainErrInfo curObj) orgRet)
            (return-from make-request nil))
          (return-from make-request (cdr (assoc :result response)))))
    (setf (LastChainErrInfo curObj) orgRet))
  nil)

(defun get-block-number (curObj)
  (let ((curLastBlockNumber (make-request curObj  "eth_blockNumber" nil)))
    (if curLastBlockNumber
        (parse-integer (subseq curLastBlockNumber 2) :RADIX 16)
        (progn (wtLogWarn "ChainId:~a getblockNumberErr, ret:~a" (ChainId curObj) curLastBlockNumber) nil) )))


(defun get-gas-price (curObj)
  (let ((gasPrc (make-request curObj  "eth_gasPrice" (list))))
    (if gasPrc
        (setf gasPrc (floor (* gGasPrcScale (parse-integer (subseq gasPrc 2) :RADIX 16))) (gasPrice curObj) gasPrc)
        (wtLogWarn "get eth gasPrice curObj Err: ~a" (LastChainErrInfo curObj)))
    gasPrc))

(defun get-trans-nones (curObj)
  (let ((nonce (make-request curObj  "eth_getTransactionCount" (list (format nil "0x~a" (AccountAddr curObj)) "latest"))))
    (if nonce
        (setf nonce  (parse-integer (subseq nonce 2) :RADIX 16) (swapNonce curObj) nonce)
        (wtLogWarn "get-trans-nones Err:~a" (LastChainErrInfo curObj)))
    nonce))

(defun EncAbiForSnd1contractCall (MethodId &optional strOpDataLst)
  (if (find #\( MethodId) (setf MethodId (CreateMethodId MethodId)))
  (let ((opData MethodId) curLen)
    (dolist (curDt strOpDataLst)
      (when (integerp curDt) (setf curDt (ironclad:byte-array-to-hex-string (ironclad:integer-to-octets curDt))))
      (setf curLen (length curDt))
      (if (<= curLen 64)
          (setf opData (inline-FullStrTo64bytesAtFront curDt :headStr opData))
          (setf opData (concatenate 'string opData curDt (make-string (- 64 (if (> (mod curLen 64) 0) (mod curLen 64) 64)) :initial-element #\0)))))
    opData))

(defun snd1ContractCallByEncedStr (curObj ContractAddr Enced0xStr &optional (blockNum "latest"))
  (when (numberp blockNum) (setf blockNum (format nil "0x~x" blockNum)))
  (setf ContractAddr (format nil "0x~a" ContractAddr))
  (make-request curObj  "eth_call" (list  (list (cons "to"  ContractAddr) (cons "data" Enced0xStr)) blockNum)))

(defun send-contract-call (curObj ContractAddr MethodId &optional strOpDataLst (blockNum "latest"))
  (unless ContractAddr (return-from send-contract-call nil))

  (let ((opData (EncAbiForSnd1contractCall MethodId strOpDataLst)))
    (snd1ContractCallByEncedStr curObj ContractAddr opData blockNum)))

(defmethod initialize-instance :after ((curObj  ChainPrc) &key)
  (unless (UniswpFctryAdr curObj)
    (if (UniswapRouterAdr curObj)
        (let ((ret (send-contract-call curObj   (UniswapRouterAdr curObj) "factory()")))
          (if (= (length ret) 66)
              (setf (UniswpFctryAdr curObj) (subseq ret 26))
              (wtLogWarn "get uniFactory err!~a, ~a" (LastChainErrInfo curObj) ret))))))

(defun get-current-balance (curObj &optional (OwnAdr nil) (blockNum "latest"))
  (let (retBln)
    (unless OwnAdr (setf OwnAdr (AccountAddr curObj)))
    (when (numberp blockNum) (setf blockNum (format nil "0x~x" blockNum)))
    (setf retBln (make-request curObj  "eth_getBalance" (list (format nil "0x~a" OwnAdr) blockNum)))
    (if (and retBln (> (length retBln) 2))
        (setf retBln (parse-integer (subseq retBln 2) :RADIX 16))
        (progn (wtLogWarn "ChainId:~a, get-balance OwnAddr:~a  Err, retBln:~a, ErrInfo:~a" (ChainId curObj) OwnAdr retBln (LastChainErrInfo curObj)) (setf retBln nil)))
    retBln))

(defun get-token-current-balance (curObj ContractAdr &key (OwnAddr nil) (blockNum "latest"))
  (let* (retBln)
    (unless OwnAddr (setf OwnAddr (AccountAddr curObj)))
    (setf retBln (send-contract-call curObj ContractAdr METHODID-BALANCEOF (list OwnAddr) blockNum))
    (if (and retBln (> (length retBln) 2))
        (return-from get-token-current-balance (parse-integer (subseq retBln 2) :RADIX 16)))

    (setf blockNum (LastChainErrInfo curObj))
    (if (> (length blockNum) 50) (setf blockNum (concatenate 'string (subseq blockNum 0 50) "...")))
    (wtLogWarn "ChainId:~a, OwnAddr:~a, get-token-current-balanceErr:~a, retBln:~a, ContractAddr:~a" (ChainId curObj) OwnAddr blockNum retBln ContractAdr))
  nil)

(defun secp256k1-sign (curObj toAddr sndVal sndValDcmls maxGasLimint &key (dataAry #()))
  (let* (nonceAry
         gasPrcAry
         (GasLimitAry (ironclad:integer-to-octets maxGasLimint))
         (prk (PrivateKey curObj)) (prkLen (length prk))
         (toAddrAry  (ironclad:hex-string-to-byte-array toAddr))
         (sndValAry (cond ((not sndVal) #()) ('t (inline-ConvertSndValToVector sndVal sndValDcmls))))
         (vAry (ironclad:integer-to-octets (ChainId curObj))) (rAry #()) (sAry #())
         recoveryParam signRet rlpEncodeAry h256HashAry)

    (unless prk (wtLogWarn "PrivateKeyNotSet") (return-from secp256k1-sign nil))

    (setf gasPrcAry (get-gas-price curObj))
    (unless gasPrcAry (return-from secp256k1-sign nil))
    (setf gasPrcAry (ironclad:integer-to-octets gasPrcAry))

    (setf nonceAry (get-trans-nones curObj))
    (unless nonceAry (return-from secp256k1-sign nil))
    (setf nonceAry (ironclad:integer-to-octets  nonceAry))


    (setf rlpEncodeAry (inline-rlp-encode (list nonceAry gasPrcAry GasLimitAry toAddrAry sndValAry dataAry vAry rAry sAry)) h256HashAry (keccak256Hash rlpEncodeAry))

    (with-foreign-arrays ((pprvKey prk `(:array :uint8 ,prkLen)) (ph256HashAry h256HashAry `(:array :uint8 ,(length h256HashAry)))
                          (pLen65SigOut (make-array 65 :element-type '(unsigned-byte 8)) `(:array :uint8 64)))
      (setf signRet (ECDSA-sign-Wrap-Impl pprvKey ph256HashAry pLen65SigOut))
      (if signRet
          (progn (wtLogWarn "web3Wrap-ECDSA-signFail:~a" signRet) (return-from secp256k1-sign nil))
          (progn
            (setf signRet (cffi:foreign-array-to-lisp pLen65SigOut  `(:array :uint8 65) :element-type 'UNSIGNED-BYTE))
            (setf recoveryParam (elt signRet 64))
            (setf rAry (subseq signRet 0 32) sAry (subseq signRet 32 64) vAry (ironclad:integer-to-octets (+ 27 recoveryParam 8 (* (ChainId curObj) 2))))
            (concatenate 'string "0x" (ironclad:byte-array-to-hex-string (inline-rlp-encode (list nonceAry gasPrcAry GasLimitAry toAddrAry sndValAry dataAry vAry rAry sAry))))
            )))))


(defun sign-contract-1WaitRunmethod (curObj contractAddr methodIdstr paramsAryLst sndValDcmls maxGasLimint &key (sndEthStrVal nil))
  (let ((startPos 4) dataAry (prmLen (length paramsAryLst)))
    (if methodIdstr
        (progn
          (dolist (curPrm paramsAryLst) (if (and (stringp curPrm) (> (length curPrm) 64)) (incf prmLen (+ (floor (/ (length curPrm) 64)) (if (= 0 (mod (length curPrm) 64)) -1 0))) ))

          (setf dataAry (make-array (+ 4 (* 32 prmLen)) :element-type '(unsigned-byte 8) :initial-element 0 :adjustable t))

          (if (find #\( methodIdstr) (setf methodIdstr (CreateMethodId-n0x methodIdstr) ))
          (replace dataAry (ironclad:hex-string-to-byte-array methodIdstr))

          (dolist (curPrm paramsAryLst)
            (if (stringp curPrm) (setf curPrm (ironclad:hex-string-to-byte-array curPrm)))
            (if (integerp curPrm) (setf curPrm (ironclad:integer-to-octets curPrm)))
            (if (>= 32 (length curPrm)) (setf prmLen (+ startPos (- 32 (length curPrm)))) (setf prmLen startPos))
            (if (= 0 (length curPrm)) (setf curPrm #(0)))
            (replace dataAry curPrm :start1 prmLen) (incf startPos (*  32 (+ (floor (/ (length curPrm) 32)) (if (= 0 (mod (length curPrm) 32)) 0 1))))))
        (setf dataAry paramsAryLst))
    (secp256k1-sign curObj contractAddr sndEthStrVal sndValDcmls maxGasLimint :dataAry dataAry)))

(defun send-eth (curObj toAddr strSndVal sndValDcmls maxGasLimint &key (contractAddr nil))
  (let (sndStr)
    (if contractAddr
        (setf sndStr (sign-contract-1WaitRunmethod curObj
                                                   contractAddr METHODID-TRANSFER
                                                   (list (ironclad:hex-string-to-byte-array toAddr) (inline-ConvertSndValToVector strSndVal sndValDcmls))
                                                   nil maxGasLimint))
        (progn
          (setf contractAddrOrName (subseq (WMainTkName curObj) 1))
          (setf sndStr (secp256k1-sign curObj toAddr strSndVal sndValDcmls  maxGasLimint))))
    (unless sndStr (return-from send-eth nil))
    (make-request curObj  "eth_sendRawTransaction" (list sndStr))))

(defun send-contract-transaction (curObj contractAddr strMethodId opDataAryLst maxGasLimint  &key (sndEthStrVal nil) (sndValDcmls nil))
  (let ((sndStr (sign-contract-1WaitRunmethod curObj contractAddr strMethodId
                                              opDataAryLst sndValDcmls maxGasLimint :sndEthStrVal sndEthStrVal)))
    (unless sndStr (return-from send-contract-transaction nil))
    (make-request curObj  "eth_sendRawTransaction" (list sndStr))))


(defun get-atoken-address (curObj curtkAddr)
  (let ((ContractAddr (send-contract-call curObj (prtlDtPrvderAddr curObj) METHODID-GETRESERVETOKENSADDRESSES (list curtkAddr))))
    (if (and ContractAddr (> (length ContractAddr) 66))
        (progn (setf ContractAddr (subseq ContractAddr 26 66))
               (if (> (length (string-left-trim "0" ContractAddr)) 0)
                   (return-from get-atoken-address ContractAddr)))))
  (wtLogWarn "get-atoken-address:~a, err:~a" curtkAddr (LastChainErrInfo curObj))
  nil)

(defun aave-deposit-token (curObj tokenContractAddr strDpstAmount decimals)
  (let (ApprTknVal retHash)

    (setf ApprTknVal (inline-ConvertSndValToVector strDpstAmount decimals))

    (setf retHash (send-contract-transaction curObj tokenContractAddr METHODID-APPROVE (list (ironclad:hex-string-to-byte-array (aLndPlAdr curObj)) ApprTknVal) APPROVE-GASLIMINT))

    (if retHash
        (send-contract-transaction curObj (aLndPlAdr curObj) METHODID-DEPOSIT (list (ironclad:hex-string-to-byte-array tokenContractAddr) ApprTknVal
                                                                                    (ironclad:hex-string-to-byte-array (AccountAddr curObj)) (ironclad:integer-to-octets 0)) 900000)
        (wtLogWarn "METHODID-APPROVE err, lastErr:~a" (LastChainErrInfo curObj)))))


(defun aave-get-token-deposited-amount (curObj tokenContractAddr) ;; 取得当前用户的储备值(包含利息) (aaveGetDepositVal gkovanChinaPrcObj "USDT")
  (let (atokenAdr)

    (setf atokenAdr (get-atoken-address curObj  tokenContractAddr))

    (if atokenAdr
        (get-token-current-balance curObj atokenAdr)
        (wtLogWarn "get ~a A-tokenAddr Fail:~a" tokenContractAddr (LastChainErrInfo curObj)))))

(defun aave-withdraw-token (curObj tokenContractAddr strWthdAmount decimals)
  (let (TknWthdrVal)

    (setf TknWthdrVal (inline-ConvertSndValToVector strWthdAmount decimals))

    (send-contract-transaction curObj (aLndPlAdr curObj) METHODID-WITHDRAW (list (ironclad:hex-string-to-byte-array  tokenContractAddr) TknWthdrVal
                                                                                 (ironclad:hex-string-to-byte-array (AccountAddr curObj))) 900000)))

(defun getAmountsOut (curObj amountIn pathLst)
  (let ((dataLst (list (ironclad:byte-array-to-hex-string (ironclad:integer-to-octets amountIn))
                       (ironclad:byte-array-to-hex-string (ironclad:integer-to-octets (* 2 32)))
                       (ironclad:byte-array-to-hex-string (ironclad:integer-to-octets (length pathLst)))))
        retVals)

    (setf dataLst (concatenate 'list dataLst pathLst))
    (setf retVals (send-contract-call curObj (UniswapRouterAdr curObj) METHODID-UNISWAP-GETAMOUNTSOUT dataLst))

    (when retVals
      (setf retVals (subseq retVals (+ 2 (* (+ 1 (length pathLst)) 64))))
      (setf retVals (parse-integer retVals :RADIX 16)))

    retVals))

(defparameter GMOTHEDID-GETCURBLOCKTIMESTAMP (CreateMethodId "getCurrentBlockTimestamp()"))

(defun getLastBlockTimestamp (curObj)
  (let ((curBlkTimestmp (send-contract-call curObj (MultiCallAdr curObj) GMOTHEDID-GETCURBLOCKTIMESTAMP)))

    (if curBlkTimestmp
        (+ (parse-integer (subseq curBlkTimestmp 2) :RADIX 16) gContractTrantTimeout)

        (progn (wtLogWarn "getLastBlockTimestampErr, ChainErrInfo:~a" (LastChainErrInfo curObj))
               (return-from getLastBlockTimestamp nil)))))

(defparameter METHODID-UNISWAP-swapExactETHForTokens (CreateMethodId-n0x "swapExactETHForTokensSupportingFeeOnTransferTokens(uint256,address[],address,uint256)"))

(defun swap-native-token-to-other-token-by-uniswapv2 (curObj strInputCnt inDecimal pathAddrLst strRequMinOut outDecimal)
  (let ((inputAmount (inline-ConvertSndValToVector strInputCnt inDecimal)) (RequMinOutAmount (inline-ConvertSndValToVector strRequMinOut outDecimal))
        (lastBlockTime (getLastBlockTimestamp curObj))
        realAmountOut)

    (unless lastBlockTime (return-from swap-native-token-to-other-token-by-uniswapv2 nil))

    (setf realAmountOut (getAmountsOut curObj inputAmount pathAddrLst))
    (unless realAmountOut
      (wtLogWarn "getAmountsOutErr, ChainErrInfo:~a" (LastChainErrInfo curObj))
      (return-from swap-native-token-to-other-token-by-uniswapv2 nil))

    (when (< realAmountOut RequMinOutAmount)
      (wtLogWarn "can't execute the swap realamountout(~a) < requminoutamount(~a)" realAmountOut RequMinOutAmount)
      (return-from swap-native-token-to-other-token-by-uniswapv2 nil))


    (send-contract-transaction curObj (UniswapRouterAdr curObj) METHODID-UNISWAP-swapExactETHForTokens
                               (concatenate 'list
                                            (list (ironclad:integer-to-octets RequMinOutAmount) (ironclad:integer-to-octets (* 4 #x20))
                                                  (ironclad:hex-string-to-byte-array  (AccountAddr curObj)) (ironclad:integer-to-octets lastBlockTime)
                                                  (ironclad:integer-to-octets (length pathAddrLst)))
                                            (map 'list #'(lambda (curAddr) (ironclad:hex-string-to-byte-array  curAddr)) pathAddrLst))
                               900000 :sndEthStrVal strInputCnt :sndValDcmls inDecimal)))

(defun swap-1Token-to-other-by-uniswapv2 (curObj strInputCnt inDecimal pathAddrLst strRequMinOut outDecimal swapFun)
  (let ((inputAmount (inline-ConvertSndValToVector strInputCnt inDecimal)) (RequMinOutAmount (inline-ConvertSndValToVector strRequMinOut outDecimal))
        (lastBlockTime (getLastBlockTimestamp curObj))
        realAmountOut retHash)

    (unless lastBlockTime (return-from swap-1Token-to-other-by-uniswapv2 nil))

    (setf realAmountOut (getAmountsOut curObj inputAmount pathAddrLst))
    (unless realAmountOut
      (wtLogWarn "getAmountsOutErr, ChainErrInfo:~a" (LastChainErrInfo curObj))
      (return-from swap-1Token-to-other-by-uniswapv2 nil))

    (when (< realAmountOut RequMinOutAmount)
      (wtLogWarn "can't execute the swap realamountout(~a) < requminoutamount(~a)" realAmountOut RequMinOutAmount)
      (return-from swap-1Token-to-other-by-uniswapv2 nil))


    (setf retHash (send-contract-transaction curObj (car pathAddrLst) METHODID-APPROVE (list (ironclad:hex-string-to-byte-array (UniswapRouterAdr curObj)) inputAmount) APPROVE-GASLIMINT))

    (if retHash
        (send-contract-transaction curObj (UniswapRouterAdr curObj) swapFun
                                   (concatenate 'list
                                                (list (ironclad:integer-to-octets inputAmount) (ironclad:integer-to-octets RequMinOutAmount) (ironclad:integer-to-octets #xa0)
                                                      (ironclad:hex-string-to-byte-array  (AccountAddr curObj)) (ironclad:integer-to-octets lastBlockTime) (ironclad:integer-to-octets  pathAddrLst))
                                                (map 'list #'(lambda (curAddr) (ironclad:hex-string-to-byte-array  curAddr)) pathAddrLst))
                                   900000)
        (wtLogWarn "METHODID-APPROVE err for Uinswap, lastErr:~a" (LastChainErrInfo curObj)))))


(defparameter METHODID-UNISWAP-swapExactTokensForETH (CreateMethodId-n0x "swapExactTokensForETHSupportingFeeOnTransferTokens(uint256,uint256,address[],address,uint256)"))

(defun swap-1Token-to-native-token-by-uniswapv2 (curObj strInputCnt inDecimal pathAddrLst strRequMinOut outDecimal)
  (swap-1Token-to-other-by-uniswapv2 curObj strInputCnt inDecimal pathAddrLst strRequMinOut outDecimal METHODID-UNISWAP-swapExactTokensForETH))

(defparameter METHODID-UNISWAP-swapExactTokensForTokens (CreateMethodId-n0x "swapExactTokensForTokensSupportingFeeOnTransferTokens(uint256,uint256,address[],address,uint256)"))

(defun swap-1Token-to-other-token-by-uniswapv2 (curObj strInputCnt inDecimal pathAddrLst strRequMinOut outDecimal)
  (swap-1Token-to-other-by-uniswapv2 curObj strInputCnt inDecimal pathAddrLst strRequMinOut outDecimal METHODID-UNISWAP-swapExactTokensForTokens))
