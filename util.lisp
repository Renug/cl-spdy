(deftype ub16 () '(unsigned-byte 16))
(deftype ub32 () '(unsigned-byte 32))
(defun htonl (long)
  #+little-endian
  (logior (ash (logand (the ub32 long) #x000000FF) 24)
          (ash (logand (the ub32 long) #x0000FF00) 8)
          (ash (logand (the ub32 long) #x00FF0000) -8)
          (ash (logand (the ub32 long) #xFF000000) -24))
  #+big-endian long)

(defun htons (short)
  #+little-endian
  (logior (ash (logand (the ub16 short) #x00FF) 8)
          (ash (logand (the ub16 short) #xFF00) -8))
  #+big-endian short)

(defun ntohs (short)
  (htons short))

(defun ntohl (long)
  (htonl long))
;;====================================================================================================
(defun serialization (&rest lst)
  "serialization like '(value1 value2) to a array"
  (let* ((bits 0)
         (retval (apply #'logior (mapcar #'(lambda (val) 
                                             (let* ((val-len (cdr val))
                                                    (retval (ash (ldb (byte val-len 0) 
                                                                      (if (<= val-len 16) 
                                                                          (htons (car val))
                                                                        (htonl (car val))))
                                                                 bits)))
                                               (incf bits val-len)
                                               retval)) lst))))
    retval))
