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
        (progn (wtLogWarn "ChinaId:~a getblockNumberErr, ret:~a" (ChainId curObj) curLastBlockNumber) nil) )))


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
