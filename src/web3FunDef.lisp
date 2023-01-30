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


(defun get-eth-gasPrice (curObj)
  (let ((gasPrc (make-request curObj  "eth_gasPrice" (list))))
    (if gasPrc
        (setf gasPrc (floor (* gGasPrcScale (parse-integer (subseq gasPrc 2) :RADIX 16))) (gasPrice curObj) gasPrc)
        (wtLogWarn "get eth gasPrice curObj Err: ~a" (LastChainErrInfo curObj)))
    gasPrc))

(defun getTransactionCount (curObj)
  (let ((nonce (make-request curObj  "eth_getTransactionCount" (list (format nil "0x~a" (AccountAddr curObj)) "latest"))))
    (if nonce
        (setf nonce  (parse-integer (subseq nonce 2) :RADIX 16) (swapNonce curObj) nonce)
        (wtLogWarn "getTransactionCount curObj Err:~a" (LastChainErrInfo curObj)))
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

(defun snd1contractCall (curObj ContractAddr MethodId &optional strOpDataLst (blockNum "latest"))
  (unless ContractAddr (return-from snd1contractCall nil))

  (let ((opData (EncAbiForSnd1contractCall MethodId strOpDataLst)))
    (snd1ContractCallByEncedStr curObj ContractAddr opData blockNum)))

(defmethod initialize-instance :after ((curObj  ChainPrc) &key)
  (unless (UniswpFctryAdr curObj)
    (if (UniswapRouterAdr curObj)
        (let ((ret (snd1contractCall curObj   (UniswapRouterAdr curObj) "factory()")))
          (if (= (length ret) 66)
              (setf (UniswpFctryAdr curObj) (subseq ret 26))
              (wtLogWarn "get uniFactory err!~a, ~a" (LastChainErrInfo curObj) ret))))))

(defun get-aTokenAddr (curObj curtkAddr)
  (let ((ContractAddr (snd1contractCall curObj (prtlDtPrvderAddr curObj) METHODID-GETRESERVETOKENSADDRESSES (list curtkAddr))))
    (if (and ContractAddr (> (length ContractAddr) 66))
        (progn (setf ContractAddr (subseq ContractAddr 26 66))
               (if (> (length (string-left-trim "0" ContractAddr)) 0)
                   (return-from get-aTokenAddr ContractAddr)))))
  (wtLogWarn "get-aTokenAddr:~a, err:~a" curtkAddr (LastChainErrInfo curObj))
  nil)

(defun getCurtBalance (curObj &optional (OwnAdr nil) (blockNum "latest"))
  (let (retBln)
    (unless OwnAdr (setf OwnAdr (AccountAddr curObj)))
    (when (numberp blockNum) (setf blockNum (format nil "0x~x" blockNum)))
    (setf retBln (make-request curObj  "eth_getBalance" (list (format nil "0x~a" OwnAdr) blockNum)))
    (if (and retBln (> (length retBln) 2))
        (setf retBln (parse-integer (subseq retBln 2) :RADIX 16))
        (progn (wtLogWarn "ChainId:~a, get-balance OwnAddr:~a  Err, retBln:~a, ErrInfo:~a" (ChainId curObj) OwnAdr retBln (LastChainErrInfo curObj)) (setf retBln nil)))
    retBln))

(defun getTkCurtBalance (curObj ContractAdr &key (OwnAddr nil) (blockNum "latest"))
  (let* (retBln)
    (unless OwnAddr (setf OwnAddr (AccountAddr curObj)))
    (setf retBln (snd1contractCall curObj ContractAdr METHODID-BALANCEOF (list OwnAddr) blockNum))
    (if (and retBln (> (length retBln) 2))
        (return-from getTkCurtBalance (parse-integer (subseq retBln 2) :RADIX 16)))

    (setf blockNum (LastChainErrInfo curObj))
    (if (> (length blockNum) 50) (setf blockNum (concatenate 'string (subseq blockNum 0 50) "...")))
    (wtLogWarn "ChainId:~a, OwnAddr:~a, getTkCurtBalanceErr:~a, retBln:~a, ContractAddr:~a" (ChainId curObj) OwnAddr blockNum retBln ContractAdr))
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

    (setf gasPrcAry (get-eth-gasPrice curObj))
    (unless gasPrcAry (return-from secp256k1-sign nil))
    (setf gasPrcAry (ironclad:integer-to-octets gasPrcAry))

    (setf nonceAry (getTransactionCount curObj))
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

(defun sendValue (curObj toAddr strSndVal sndValDcmls maxGasLimint &key (contractAddr nil))
  (let (sndStr)
    (if contractAddr
        (setf sndStr (sign-contract-1WaitRunmethod curObj
                                                   contractAddr METHODID-TRANSFER
                                                   (list (ironclad:hex-string-to-byte-array toAddr) (inline-ConvertSndValToVector strSndVal sndValDcmls))
                                                   nil maxGasLimint))
        (progn
          (setf contractAddrOrName (subseq (WMainTkName curObj) 1))
          (setf sndStr (secp256k1-sign curObj toAddr strSndVal sndValDcmls  maxGasLimint))))
    (unless sndStr (return-from sendValue nil))
    (make-request curObj  "eth_sendRawTransaction" (list sndStr))))

(defun snd1contractTrans (curObj contractAddr strMethodId opDataAryLst maxGasLimint)
  (let ((sndStr (sign-contract-1WaitRunmethod curObj contractAddr strMethodId
                                              opDataAryLst nil maxGasLimint)))
    (unless sndStr (return-from snd1contractTrans nil))
    (make-request curObj  "eth_sendRawTransaction" (list sndStr))))
