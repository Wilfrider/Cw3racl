(in-package :cl-web3)

(progn
  (cffi:define-foreign-library libWeb3Ext (:linux "~/DepLib809.so"))
  (cffi:use-foreign-library libWeb3Ext)

  (cffi:defcstruct curlWrap-OutStt
    (pHeader :pointer)
    (HeaderLen :int)
    (pBody :pointer)
    (BodyLen :int))

  (cffi:defcfun ("FreeMallocPoint" FreeMallocPoint) :void (pPt :pointer))
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
  (with-gensyms (prxLogInfo realLogTxt outTxt)
    `(if (>= ,logLvl *gcur-LogLevel*)
         (let* ((,prxLogInfo (format nil "logTime:~a, logLvel:~6a--" (OutTimeFmtStr (get-universal-time))
                                     (case ,logLvl (#.gLogLevel-Info "Info") (#.gLogLevel-Debug "Dbg") (#.gLogLevel-Key "key") (#.gLogLevel-Warn "Warn") (otherwise "Error"))))
                (,realLogTxt (format nil ,@body))
                (,outTxt (format nil "~a~a~a" ,prxLogInfo ,realLogTxt #\Newline)))
           (write-line ,outTxt *standard-output*)))))


(defmacro wtLogInfo (&body body) `(gvLogOut gLogLevel-Info ,@body))
(defmacro wtLogKey (&body body) `(gvLogOut gLogLevel-Key ,@body))
(defmacro wtLogWarn (&body body) `(gvLogOut gLogLevel-Warn ,@body))
(defmacro wtLogErr (&body body) `(gvLogOut gLogLevel-Error ,@body))
;; (wtLogInfo "this is a test")


(defun keccak256Hash (inputAry &key)
  (let ((retArray (make-array `,(length inputAry) :element-type '(unsigned-byte 8) :initial-contents `,inputAry)) (digester (ironclad:make-digest :keccak/256)))
    (ironclad:update-digest digester retArray)
    (ironclad:produce-digest digester)))

(defun CreateMethodId-n0x (MethodDescript) (subseq (ironclad:byte-array-to-hex-string (keccak256Hash (ironclad:ascii-string-to-byte-array MethodDescript))) 0 8))
(defun CreateMethodId (MethodDescript) (concatenate 'string "0x" (CreateMethodId-n0x MethodDescript)))

(defun htppClientAccess (reqUrl &key (proxy nil) (cnTimeout 11) (reqHeaders nil) (postFields nil) (retBodyIsStr t))
  (handler-case
      (let (retInt pHeader pBody BodyLen headerStr bodyStr (pProxy (cffi:null-pointer))
                   (pReqHeaders (cffi:null-pointer)) (pPostFields (cffi:null-pointer)) (PostFieldsLen 0) (retVal nil) (tcpNoDelay 1))
        (cffi:with-foreign-object (OutStt '(:struct curlWrap-OutStt))
          (cffi:with-foreign-string (pReqUrl reqUrl)
            (if proxy (setf pProxy (cffi:foreign-string-alloc proxy)))
            (if reqHeaders (setf pReqHeaders (cffi:foreign-string-alloc reqHeaders)))
            (when postFields
              (if (stringp postFields) (setf postFields (sb-ext:string-to-octets postFields :external-format :utf-8)))
              (setf PostFieldsLen (length postFields))
              (setf pPostFields (cffi:foreign-alloc :uint8 :count PostFieldsLen))
              (cffi:lisp-array-to-foreign postFields pPostFields `(:array :uint8 ,PostFieldsLen)))
            (unwind-protect
                 (progn
                   (setf retInt (curl-Wrap-Impl pReqUrl pProxy cnTimeout tcpNoDelay pReqHeaders pPostFields PostFieldsLen OutStt))
                   (if proxy (cffi:foreign-string-free pProxy))
                   (if postFields (cffi:foreign-free pPostFields))
                   (if reqHeaders (cffi:foreign-string-free pReqHeaders))))

            (setf pHeader (cffi:foreign-slot-value OutStt '(:struct curlWrap-OutStt) 'pHeader)
                  pBody (cffi:foreign-slot-value OutStt '(:struct curlWrap-OutStt) 'pBody) BodyLen (cffi:foreign-slot-value OutStt '(:struct curlWrap-OutStt) 'BodyLen))))
        (when (> retInt 0)
          (if retBodyIsStr
              (setf bodyStr (cffi:foreign-string-to-lisp pBody))
              (setf bodyStr (cffi:foreign-array-to-lisp pBody `(:array :uint8 ,BodyLen) :element-type 'UNSIGNED-BYTE)))
          (setf headerStr (cffi:foreign-string-to-lisp pHeader))
          (setf retVal (list retInt headerStr bodyStr)))
        (FreeMallocPoint pHeader)
        (FreeMallocPoint pBody)
        (return-from htppClientAccess retVal))
    (error (cond) (progn (wtLogWarn "url:~a, AcsErrInfo:~a" reqUrl cond) (return-from htppClientAccess nil))))
  nil)
