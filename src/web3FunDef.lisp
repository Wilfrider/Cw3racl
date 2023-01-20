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
