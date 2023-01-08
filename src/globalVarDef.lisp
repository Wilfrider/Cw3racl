(in-package :cl-web3)

(uffi:def-enum gLogLevel ((:Debug 0) :Info :Key :Warn :Error) :separator-string "-")
(defparameter *gcur-LogLevel* gLogLevel-Debug)

(defparameter gCurUsrHomeDir (sb-unix:uid-homedir (sb-unix:unix-getuid)))

(defparameter gTimestampOffset (* 3600 8))
