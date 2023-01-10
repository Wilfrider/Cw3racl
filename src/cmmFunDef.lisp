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
