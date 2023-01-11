(in-package :cl-web3)

(progn
  (cffi:define-foreign-library libWeb3Ext (:linux "~/DepLib809.so"))
  (cffi:use-foreign-library libWeb3Ext)

  (cffi:defcstruct curlWrap-OutStt
    (pHeader :pointer)
    (HeaderLen :int)
    (pBody :pointer)
    (BodyLen :int))
  (cffi:defcfun ("curl_Wrap" curl-Wrap-Impl) :int (reqUrl :pointer) (proxy :pointer) (cnTimeout :int) (tcpNoDelay :int) (pHeaders :pointer) (postFields :pointer) (postLen :int) (pOutStt :pointer)))


(defmacro with-foreign-arrays (bindings &body body)
  (if bindings
      `(cffi:with-foreign-array ,(first bindings)
         (with-foreign-arrays ,(rest bindings)
           ,@body))
      `(progn ,@body)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))



(defun OutTimeFmtStr (timeStamp &key (HMS-spliteSbl #\:))
  (let ((curDateTime (multiple-value-list (decode-universal-time (+ timeStamp gTimestampOffset)))))
    (format nil "~2,'0d-~2,'0d_~2,'0d~a~2,'0d~a~2,'0d" (elt curDateTime 4) (elt curDateTime 3) (elt curDateTime 2) HMS-spliteSbl (elt curDateTime 1) HMS-spliteSbl (elt curDateTime 0))))

(defmacro gvLogOut (logLvl &body body)
  (with-gensyms (prxLogInfo realLogTxt outTxt pDecRet)
    `(if (>= ,logLvl *gcur-LogLevel*)
         (let* ((,prxLogInfo (format nil "logTime:~a, logLvel:~6a--" (OutTimeFmtStr (get-universal-time))
                                     (case ,logLvl (#.gLogLevel-Info "Info") (#.gLogLevel-Debug "Dbg") (#.gLogLevel-Key "key") (#.gLogLevel-Warn "Warn") (otherwise "Error"))))
                (,realLogTxt (format nil ,@body))
                (,pDecRet nil)
                (,outTxt (format nil "~a~a~a" ,prxLogInfo ,realLogTxt #\Newline)))
           (write-line ,outTxt *standard-output*)))))


(defmacro wtLogInfo (&body body) `(gvLogOut gLogLevel-Info ,@body))
;; (wtLogInfo "this is a test")


(defun keccak256Hash (inputAry &key)
  (let ((retArray (make-array `,(length inputAry) :element-type '(unsigned-byte 8) :initial-contents `,inputAry)) (digester (ironclad:make-digest :keccak/256)))
    (ironclad:update-digest digester retArray)
    (ironclad:produce-digest digester)))

(defun CreateMethodId-n0x (MethodDescript) (subseq (ironclad:byte-array-to-hex-string (keccak256Hash (ironclad:ascii-string-to-byte-array MethodDescript))) 0 8))
(defun CreateMethodId (MethodDescript) (concatenate 'string "0x" (CreateMethodId-n0x MethodDescript)))

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

(defparameter gContractTrantTimeout 20)
