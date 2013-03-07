;(use-package 'usocket)
;;====================================================================================================
(defparameter *control-frame-type-code* '((SYN_STREAM . 1)
                                          (SYN_REPLY . 2)
                                          (RST_STREAM . 3)
                                          (SETTINGS . 4)
                                          (NOOP . 5)
                                          (PING . 6)
                                          (GOAWAY . 7)
                                          (HEADERS . 8)
                                          (WINDOW_UPDATE . 9)
                                          (CREDENTIAL . #b0000000000001011)))

(defparameter *rst-stream-status-code* '((PROTOCOL_ERROR . 1)
                                   (INVALID_STREAM . 2)
                                   (REFUSED_STREAM . 3)
                                   (UNSUPPORTED_VERSION . 4)
                                   (CANCEL . 5)
                                   (INTERNAL_ERROR . 6)
                                   (FLOW_CONTROL_ERROR . 7)
                                   (STREAM_IN_USE . 8)
                                   (STREAM_ALREADY_CLOSED . 9)
                                   (INVALID_CREDENTIALS . 10)
                                   (FRAME_TOO_LARGE . 11)))

(defparameter *goway-status-code* '((OK . 0)
                              (PROTOCOL_ERROR . 1)
                              (INTERNAL_ERROR . 11)))

(defparameter *setting-flag-code* '((FLAG_SETTINGS_CLEAR_SETTINGS . #x1)))

(defparameter *setting-id-flag-code* '((FLAG_SETTINGS_PERSIST_VALUE . #x1)
                                 (FLAG_SETTINGS_PERSISTED . #x2)))

(defparameter *setting-id-code* '((SETTINGS_UPLOAD_BANDWIDTH . 1)
                            (SETTINGS_DOWNLOAD_BANDWIDTH . 2)
                            (SETTINGS_ROUND_TRIP_TIME . 3)
                            (SETTINGS_MAX_CONCURRENT_STREAMS . 4)
                            (SETTINGS_CURRENT_CWND . 5)
                            (SETTINGS_DOWNLOAD_RETRANS_RATE . 6)
                            (SETTINGS_INITIAL_WINDOW_SIZE . 7)
                            (SETTINGS_CLIENT_CERTIFICATE_VECTOR_SIZE . 8)))

(defparameter *control-frame-flag-code* '((FLAG_FIN . #x01)
                                          (FLAG_SETTINGS_CLEAR_SETTINGS . #x1)
                                          (FLAG_UNIDIRECTIONAL . #x02)
                                          (NONE . #x0)))
;;====================================================================================================
(defun append-vector (vector1 vector2)
  (concatenate 'vector vector1 vector2))
;;====================================================================================================

(defmacro symbol-to-code (symbol-code-alist symbol)
  `(cdr (assoc ,symbol ,symbol-code-alist)))


(defun control-frame-type-to-code (frame-type)
  (symbol-to-code  *control-frame-type-code* frame-type))

(defun control-frame-flag-symbol-to-code (symbol)
  (symbol-to-code *control-frame-flag-code* symbol))

(defun goaway-frame-flag-to-code (symbol)
  (symbol-to-code *goway-status-code* symbol))

(defun number-to-vector (len value)
  (reverse (loop with array = (make-array len)
                 for i from (- len 1) downto 0 
                 do (setf (aref array i) (ldb (byte 8 (* 8 i)) value))
                 finally (return array))))
;;====================================================================================================

(defgeneric frame-serialization (frame) 
  (:documentation "convert a control frame to a list"))

(defmethod frame-serialization (frame)
  nil)

(defun frame-deserialize (stream)
  (let ((first-ub32 (loop with value = 0
                          with count = 0
                          for item in (reverse (loop repeat 4 collect (read-byte stream))) do
                          (progn
                            (setf value (logior value (ash item count)))
                            (incf count 8))
                          finally (return value))))
    (if (= 1 (ldb (byte 1 31) first-ub32))
        (let ((frame (mk-control-frame))
              (second-ub32 (loop with value = 0
                                 with count = 0
                                 for item in (reverse (loop repeat 4 collect (read-byte stream)))
                                 do (progn
                                      (setf value (logior value (ash item count)))
                                      (incf count 8))
                                 finally (return value))))
          (with-slots (version flags frame-type length-in-byte control-frame-data) frame
            (setf version (ldb (byte 15 16) first-ub32))
            (setf frame-type (car (rassoc (ldb (byte 16 0) first-ub32) *control-frame-type-code*)))
            (setf length-in-byte (ldb (byte 24 0) second-ub32))
            (setf flags (ldb (byte 8 24) second-ub32))
            (when (< 0 length-in-byte)
              (let* ((arr (loop with arr = (make-array 10 :adjustable t :fill-pointer 0)
                                repeat length-in-byte
                                do (vector-push (read-byte stream) arr)
                                finally (return arr)))
                     (data-stream (mk-data-input-stream arr)))
              (setf control-frame-data (mk-control-frame-data data-stream frame-type))))
          frame))
      (let ((frame (mk-data-frame))
            (secode-ub32 (loop with value = 0
                               with count = 0
                               for item in (reverse (loop repeat 4 collect (read-byte stream))) 
                               do (progn
                                    (setf value (logior value (ash item count)))
                                    (incf count 8))
                               finally (return value))))
        (with-slots (stream-id flags length-in-byte data) frame
          (setf stream-id (ldb (byte 31 0) first-ub32))
          (setf flags (ldb (byte 8 24) secode-ub32))
          (setf length-in-byte (ldb (byte 24 0) secode-ub32) secode-ub32)
          (setf data (loop with array = (make-array length-in-byte :fill-pointer 0)
                           repeat length-in-byte 
                           do (vector-push (read-byte stream) array)
                           finally (return array))))
        frame))))

(defun nv-deserialize (stream count-of-nv-pair)
  (loop with retval = nil
        repeat count-of-nv-pair
        do (let* ((name-len (loop with value = 0
                                  with count = 0
                                  for item in (reverse (loop repeat 4 collect (read-byte stream))) 
                                  do (progn
                                       (setf value (logior value (ash item count)))
                                       (incf count 8))
                                  finally (return value)))
                  (name (loop with retval = (make-array name-len 
                                                        :element-type 'character 
                                                        :fill-pointer 0)
                              repeat name-len
                              do (vector-push (code-char (read-byte stream)) retval)
                              finally (return retval)))
                  (value-len (loop with value = 0
                                   with count = 0
                                   for item in (reverse (loop repeat 4 collect (read-byte stream))) 
                                   do (progn
                                        (setf value (logior value (ash item count)))
                                        (incf count 8))
                                   finally (return value)))
                  (value (loop with retval = (make-array name-len 
                                                         :element-type 'character 
                                                         :fill-pointer 0)
                               repeat name-len
                               do (vector-push (code-char (read-byte stream)) retval)
                               finally (return retval))))
             (setf retval (append retval (list (cons name value)))))
        finally (return retval)))

(defun iv-deserialize (stream count-of-nv-pair)
  (loop with retval = nil
        repeat count-of-nv-pair
        do (let* ((id (loop with value = 0
                                  with count = 0
                                  for item in (reverse (loop repeat 4 collect (read-byte stream))) 
                                  do (progn
                                       (setf value (logior value (ash item count)))
                                       (incf count 8))
                                  finally (return value)))
                  (value (loop with value = 0
                                  with count = 0
                                  for item in (reverse (loop repeat 4 collect (read-byte stream))) 
                                  do (progn
                                       (setf value (logior value (ash item count)))
                                       (incf count 8))
                                  finally (return value))))
             (setf retval (append retval (list (cons id value)))))
        finally (return retval)))
;;====================================================================================================
(defclass control-frame ()
  ((version :initform 3
            :type integer
            :initarg :version
            :accessor version)
   (flags :initform 'FLAG_FIN
          :initarg :flags
          :accessor flags)
   (frame-type :initform nil
               :initarg :frame-type
               :accessor frame-type)
   (length-in-byte :initform 0
                   :initarg :length-in-byte
                   :accessor length-in-byte)
   (control-frame-data :initform nil
                       :initarg :control-frame-data
                       :accessor control-frame-data)))

(defun mk-control-frame (&key (flags 'FLAG_FIN) (frame-type ) length-in-byte (version 3) (control-frame-data nil))
  (make-instance 'control-frame
                 :version version
                 :flags flags
                 :frame-type frame-type
                 :length-in-byte length-in-byte
                 :control-frame-data control-frame-data))

(defmethod frame-serialization ((frame control-frame))
  (with-slots (version flags length-in-byte frame-type control-frame-data) frame
    (let ((value (serialization (cons length-in-byte 24)
                                (cons (control-frame-flag-symbol-to-code flags) 8)
                                (cons (control-frame-type-to-code frame-type) 16)
                                (cons (logior (ash 1 15) (ldb (byte 15 0) version)) 16))))
      (append-vector (number-to-vector 8 value)
                     (frame-serialization control-frame-data)))))

;;====================================================================================================

(defun name-value-pair-to-list (nv-pair)
  (coerce 
   (mapcan #'(lambda (item)
               (let ((length-of-car (length (car item)))
                     (length-of-cdr (length (cdr item))))
                 (append (loop for i from 3 downto 0 collect (ldb (byte 8 (* 3 i)) length-of-car))
                         (loop for var across (car item) collect (char-code var))
                         (loop for i from 3 downto 0 collect (ldb (byte 8 (* 3 i)) length-of-cdr))
                         (loop for var across (cdr item) collect (char-code var)))))

           nv-pair) 'vector))


(defun id-value-pairs-to-list (id-value-pair)
  (when id-value-pair
    (let* ((first-item (car id-value-pair))
           (value (serialization (cons (cdr first-item) 32)
                                 (cons (car first-item) 32))))
      (append-vector (number-to-vector 8 value)
                     (id-value-pairs-to-list (cdr id-value-pair))))))
;;====================================================================================================

(defclass header-control-frame-data ()
  ((stream-id :initform 0
              :initarg :stream-id
              :accessor stream-id)
   (name-value-pair :initform nil
                    :initarg :name-value-pair
                    :accessor name-value-pair)))

(defun mk-header-control-frame-data (stream-id &key (name-value-pair nil))
  (make-instance 'header-control-frame-data
                 :stream-id stream-id
                 :name-value-pair name-value-pair))

(defun mk-header-control-frame (stream-id &key (flags 'FLAG_FIN)(name-value-pair nil))
  (let ((frame-data (mk-header-control-frame-data stream-id
                                                  :name-value-pair name-value-pair)))
    (mk-control-frame :flags (control-frame-flag-symbol-to-code flags)
                      :frame-type 'HEADERS
                      :control-frame-data frame-data
                      :length-in-byte (length (frame-serialization frame-data)))))

(defmethod mk-control-frame-data (stream (frame-type (eql 'HEADERS)))
  (let* ((stream-id (read-32bit-integer stream))
         (data-stream (mk-data-input-stream 
                       (zlib:uncompress (subseq (data-array stream) (read-position stream)))))
         (count-of-nv-pair (read-32bit-integer data-stream))
         (nv-pair (nv-deserialize data-stream count-of-nv-pair)))
  (mk-header-control-frame-data stream-id nv-pair)))

(defmethod frame-serialization ((frame header-control-frame-data))
  (with-slots (stream-id  name-value-pair) frame
    (let ((value (serialization (cons (length name-value-pair) 32)
                                (cons (ldb (byte 31 0) stream-id) 32))))
      (append-vector (number-to-vector 8 value)
                     (name-value-pair-to-list name-value-pair)))))

;;====================================================================================================
(defclass rst-stream-control-frame-data ()
  ((stream-id :initform 0
              :initarg :stream-id
              :accessor stream-id)
   (status-code :initform 0
                :initarg :status-code
                :accessor status-code)))

(defun mk-rst-stream-control-frame-data (stream-id &key (status-code 'PROTOCOL_ERROR))
  (make-instance 'rst-stream-control-frame-data 
                 :stream-id stream-id
                 :status-code status-code))

(defun mk-rst-stream-control-frame (stream-id status-code)
  (mk-control-frame :flags 0
                    :length-in-byte 8
                    :frame-type 'RST_STREAM
                    :control-frame-data (mk-rst-stream-control-frame-data
                                         stream-id
                                         :status-code status-code)))

(defmethod mk-control-frame-data (stream (frame-type (eql 'RST_STREAM)))
  (let* ((stream-id (read-32bit-integer stream))
         (status-code (car (rassoc (read-32bit-integer stream) *rst-stream-status-code*))))
    (mk-rst-stream-control-frame-data stream-id :status-code status-code)))

(defmethod frame-serialization ((frame rst-stream-control-frame-data))
  (with-slots (stream-id  status-code) frame
    (let ((value (serialization (cons status-code 32)
                                (cons stream-id 32)
                                (cons 0 1))))
      (number-to-vector 8 value))))
;;====================================================================================================
(defclass syn-reply-control-frame-data ()
  ((stream-id :initform 0
              :initarg :stream-id
              :accessor stream-id)
   (name-value-pair :initform nil
                    :initarg :name-value-pair
                    :accessor name-value-pair)))

(defun mk-syn-reply-control-frame-data (stream-id &key (name-value-pair nil))
  (make-instance 'syn-reply-control-frame-data 
                 :stream-id stream-id 
                 :name-value-pair name-value-pair))

(defun mk-syn-reply-control-frame (stream-id &key (flags 'FLAG_FIN) (name-value-pair nil))
  (let ((frame-data (mk-syn-reply-control-frame-data
                     :stream-id stream-id
                     :name-value-pair name-value-pair)))
    (mk-control-frame :flags (control-frame-flag-symbol-to-code flags)
                      :frame-type 'SYN_REPLY
                      :control-frame-data frame-data
                      :length-in-byte (length (frame-serialization frame-data)))))
(defun read-32bit-integer (stream)
  (loop with value = 0
        with count = 0
        for item in (reverse (loop repeat 4 collect (read-byte stream))) 
        do (progn
             (setf value (logior value (ash item count)))
             (incf count 8))
        finally (return value)))

(defmethod mk-control-frame-data (stream (frame-type (eql 'SYN_REPLY)))
  (let* ((stream-id (read-32bit-integer stream))
         (data-stream (mk-data-input-stream 
                       (zlib:uncompress (subseq (data-array stream) (read-position stream)))))
         (count-of-nv-pair (read-32bit-integer data-stream))
         (nv-pair (nv-deserialize data-stream count-of-nv-pair)))
    (mk-syn-reply-control-frame-data stream-id :name-value-pair nv-pair)))

(defmethod frame-serialization ((frame syn-reply-control-frame-data))
  (with-slots (stream-id name-value-pair) frame
    (let ((value (serialization (cons (length name-value-pair) 32)
                                (cons (ldb (byte 31 0) stream-id) 32))))
      (append-vector (number-to-vector 8 value)
                     (name-value-pair-to-list name-value-pair)))))
;;====================================================================================================
(defclass syn-stream-control-frame-data ()
  ((stream-id :initform 0
              :initarg :stream-id
              :accessor stream-id)
   (priority :initform 0
             :initarg :priority
             :accessor priority)
   (associated-to-stream-id :initform 0
                            :initarg :associated-to-stream-id
                            :accessor associated-to-stream-id)
   (slot :initform 0
         :initarg :slot
         :accessor slot)
   (name-value-pair :initform nil
                    :initarg :name-value-pair
                    :accessor name-value-pair)))

(defun mk-syn-stream-control-frame-data (stream-id 
                                         priority 
                                         &key 
                                         (associated-to-stream-id 0) 
                                         (slot 0) 
                                         (name-value-pair nil))
  (make-instance 'syn-stream-control-frame-data
                 :stream-id stream-id
                 :priority priority
                 :associated-to-stream-id associated-to-stream-id
                 :name-value-pair name-value-pair
                 :slot slot))


(defun mk-syn-stream-control-frame (stream-id priority &key 
                                              (flags 'FLAG_FIN)
                                              (associated-to-stream-id 0) 
                                              (slot 0) 
                                              (name-value-pair nil))
  (let ((frame-data (mk-syn-stream-control-frame-data stream-id 
                                                      priority
                                                      :associated-to-stream-id associated-to-stream-id
                                                      :slot slot
                                                      :name-value-pair name-value-pair)))
    (mk-control-frame :flags flags
                      :frame-type 'SYN_STREAM
                      :control-frame-data frame-data
                      :length-in-byte (length (frame-serialization frame-data)))))



(defmethod mk-control-frame-data (stream (frame-type (eql 'SYN_STREAM)))
  (let* ((stream-id (read-32bit-integer stream))
         (associated-to-stream-id (read-32bit-integer stream))
         (priority (ash (read-byte stream) -5))
         (slot (read-byte stream))
         (data-stream (mk-data-input-stream 
                       (zlib:uncompress (subseq (data-array stream) (read-position stream)))))
         (count-of-nv-pair (read-32bit-integer data-stream))
         (nv-pair (nv-deserialize data-stream count-of-nv-pair)))
    (mk-syn-stream-control-frame stream-id priority 
                                 :associated-to-stream-id associated-to-stream-id
                                 :slot slot
                                 :name-value-pair name-value-pair)))
    

(defmethod frame-serialization ((frame syn-stream-control-frame-data))
  (with-slots (stream-id priority associated-to-stream-id name-value-pair slot) frame
    (let ((value (serialization (cons (length name-value-pair) 32)
                                (cons slot 8)
                                (cons (ash (ldb (byte 8 0) priority) 5) 8)
                                (cons (ldb (byte 31 0) associated-to-stream-id) 32)
                                (cons (ldb (byte 31 0) stream-id) 32))))
      (append-vector (number-to-vector 14 value)
                     (name-value-pair-to-list name-value-pair)))))
;;====================================================================================================
(defclass goway-control-frame-data ()
  ((last-good-stream-id :initarg :last-good-stream-id
                        :initform 0
                        :accessor last-good-stream-id)
   (status-code :initarg :status-code
                :initform 'OK
                :accessor status-code)))

(defun mk-goway-control-frame-data (last-good-stream-id &key (status-code 'OK))
  (make-instance 'goway-control-frame-data 
                 :last-good-stream-id last-good-stream-id
                 :status-code status-code))

(defun mk-goway-control-frame (last-good-stream-id &key (status-code 'OK))
  (mk-control-frame
   :flags 'NONE
   :frame-type 'GOAWAY
   :length-in-byte 8
   :control-frame-data (mk-goway-control-frame-data last-good-stream-id 
                                                    :status-code status-code)))

(defmethod mk-control-frame-data (stream (frame-type (eql 'GOAWAY)))
  (let ((last-good-stream-id (read-32bit-integer stream))
        (status-code (read-32bit-integer stream)))
    (mk-goway-control-frame-data last-good-stream-id 
                                 :status-code 
                                 (car (rassoc status-code *goway-status-code*)))))

(defmethod frame-serialization ((frame goway-control-frame-data))
  (with-slots (last-good-stream-id status-code) frame
    (let ((value (serialization (cons (goaway-frame-flag-to-code status-code) 32)
                                (cons (ldb (byte 31 0) last-good-stream-id) 32))))
      (number-to-vector 8 value))))
;;====================================================================================================
(defclass ping-control-frame-data ()
  ((id :initarg :id
       :initform 0
       :accessor id)))

(defun mk-ping-control-frame-data (id)
  (make-instance 'ping-control-frame-data 
                 :id id))

(defun mk-ping-control-frame (id)
  (mk-control-frame :flags 'NONE
                    :length-in-byte 4
                    :frame-type 'PING
                    :control-frame-data (mk-ping-control-frame-data id)))

(defmethod mk-control-frame-data (stream (frame-type (eql 'PING)))
  (let ((id (read-32bit-integer stream)))
    (mk-ping-control-frame-data id)))

(defmethod frame-serialization ((frame ping-control-frame-data))
  (number-to-vector 4 (id frame)))
;;====================================================================================================
(defclass window-update-control-frame-data ()
  ((stream-id :initform 0
              :initarg :stream-id
              :accessor stream-id)
   (delta-window-size :initform 0
                      :initarg :delta-window-size
                      :accessor delta-window-size)))

(defun mk-window-update-control-frame-data (stream-id delta-window-size)
  (make-instance 'window-update-control-frame-data 
                 :stream-id stream-id
                 :delta-window-size delta-window-size))

(defun mk-window-update-control-frame (stream-id delta-window-size)
  (mk-control-frame
   :flags 'NONE
   :length-in-byte 8
   :frame-type 'WINDOW_UPDATE
   :control-frame-data (mk-window-update-control-frame-data stream-id
                                                            delta-window-size)))

(defmethod mk-control-frame-data (stream (frame-type (eql 'WINDOW_UPDATE)))
  (let ((stream-id (read-32bit-integer stream))
        (delta-window-size (read-32bit-integer stream)))
    (mk-window-update-control-frame-data stream-id delta-window-size)))

(defmethod frame-serialization ((frame window-update-control-frame-data))
  (with-slots (stream-id delta-window-size) frame
    (let ((value (serialization (cons (ldb (byte 31 0) delta-window-size) 32)
                                (cons (ldb (byte 31 0) stream-id) 32))))
      (number-to-vector 8 value))))
;;====================================================================================================      

(defclass setting-control-frame-data ()
  ((id-value-pair :initarg :id-value-pair
                  :initform nil
                  :accessor id-value-pair)))

(defun mk-setting-control-frame-data (id-value-pair)
  (make-instance 'setting-control-frame-data 
                 :id-value-pair id-value-pair))

(defun mk-setting-control-frame (id-value-pair &key (flags 'FLAG_SETTINGS_CLEAR_SETTINGS))
  (let ((frame-data (mk-setting-control-frame-data id-value-pair)))
    (mk-control-frame
     :flags flags
     :length-in-byte (length (frame-serialization frame-data))
     :frame-type 'SETTINGS
     :control-frame-data frame-data)))

(defmethod mk-control-frame-data (stream (frame-type (eql 'SETTINGS)))
  (let* ((data-stream (mk-data-input-stream 
                       (zlib:uncompress (subseq (data-array stream) (read-position stream)))))
         (count-of-nv-pair (read-32bit-integer data-stream)))
    (mk-setting-control-frame-data (iv-deserialize stream count-of-nv-pair))))

(defun comprised-flag-and-id (flag id)
  (logior (ash (ldb (byte 8 0) flag) 24) 
          (ldb (byte 24 0) id)))

(defmethod frame-serialization ((frame setting-control-frame-data))
  (with-slots (id-value-pair) frame
    (let ((length-of-id-value-pair (length id-value-pair)))
      (append-vector (number-to-vector 4 length-of-id-value-pair)
                     (id-value-pairs-to-list id-value-pair)))))
;;====================================================================================================
(defclass credential-control-frame-data ()
  ((slots :initarg :slots
          :initform 0
          :accessor slots)
   (proof-length :initarg :proof-length
                 :initform 0
                 :accessor proof-length)
   (proof :initarg :proof
          :initform 0
          :accessor proof)
   (length-certificate-pair :initform nil
                            :initarg :length-certificate-pair
                            :accessor length-certificate-pair)))

(defun mk-credential-control-frame-data (slots proof-length proof &key (length-certificate-pair nil))
  (make-instance 'credential-control-frame-data 
                 :slots slots
                 :proof-length proof-length
                 :proof proof
                 :length-certificate-pair length-certificate-pair))
;;not need now
;;(defmethod mk-control-frame-data (stream (frame-type (eql 'CREDENTIAL)))
;;  (mk-credential-control-frame-data 20 20 20))

(defmethod frame-serialization ((frame credential-control-frame-data))
  (with-slots (slots proof-length proof length-certificate-pair) frame
    (let ((value (logior (ldb (byte 32 0) proof)
                         (ash (ldb (byte 32 0) proof-length) 32)
                         (ash (ldb (byte 16 0) slots) 64))))
      (append-vector (number-to-vector 8 value)
                     (id-value-pairs-to-list length-certificate-pair)))))
;;====================================================================================================




            
            

