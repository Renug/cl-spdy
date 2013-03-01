;(use-package 'usocket)
;;====================================================================================================
(defvar *control-frame-type-code* '((SYN_STREAM . 1)
                                    (SYN_REPLY . 2)
                                    (RST_STREAM . 3)
                                    (SETTINGS . 4)
                                    (NOOP . 5)
                                    (PING . 6)
                                    (GOAWAY . 7)
                                    (HEADERS . 8)
                                    (WINDOW_UPDATE . 9)
                                    (CREDENTIAL . #b0000000000001011)))

(defvar *rst-stream-status-code* '((PROTOCOL_ERROR . 1)
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

(defvar *goway-status-code* '((OK . 0)
                              (PROTOCOL_ERROR . 1)
                              (INTERNAL_ERROR . 11)))

(defvar *setting-flag-code* '((FLAG_SETTINGS_CLEAR_SETTINGS . #x1)))

(defvar *setting-id-flag-code* '((FLAG_SETTINGS_PERSIST_VALUE . #x1)
                                 (FLAG_SETTINGS_PERSISTED . #x2)))

(defvar *setting-id-code* '((SETTINGS_UPLOAD_BANDWIDTH . 1)
                            (SETTINGS_DOWNLOAD_BANDWIDTH . 2)
                            (SETTINGS_ROUND_TRIP_TIME . 3)
                            (SETTINGS_MAX_CONCURRENT_STREAMS . 4)
                            (SETTINGS_CURRENT_CWND . 5)
                            (SETTINGS_DOWNLOAD_RETRANS_RATE . 6)
                            (SETTINGS_INITIAL_WINDOW_SIZE . 7)
                            (SETTINGS_CLIENT_CERTIFICATE_VECTOR_SIZE . 8)))

(defvar *control-frame-flag-code* '((FLAG_FIN . #x01)
                                    (FLAG_UNIDIRECTIONAL . #x02)))
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

(defun number-to-vector (len value)
  (reverse (loop with array = (make-array len)
                 for i from (- len 1) downto 0 
                 do (setf (aref array i) (ldb (byte 8 (* 8 i)) value))
                 finally (return array))))
;;====================================================================================================

(defgeneric frame-to-list (frame) 
  (:documentation "convert a control frame to a list"))

(defmethod frame-to-list (frame)
  nil)
;;====================================================================================================
(defclass control-frame ()
  ((version :initform 2
            :initarg :version
            :accessor version)
   (flags :initform 0
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

(defun mk-control-frame (version flags frame-type length-in-byte &key (control-frame-data nil)) 
  (make-instance 'control-frame
                 :version version
                 :flags flags
                 :frame-type frame-type
                 :length-in-byte length-in-byte
                 :control-frame-data control-frame-data))

(defmethod frame-to-list ((frame control-frame))
  (with-slots (version flags length-in-byte frame-type control-frame-data) frame
    (let ((value (logior (ash 1 63) 
                         (ash (ldb (byte 15 0) version) 48)
                         (ash (ldb (byte 16 0) (control-frame-type-to-code frame-type)) 32)
                         (ash (ldb (byte 8 0) flags) 24)
                         (ldb (byte 24 0) length-in-byte))))
      (append-vector (number-to-vector 8 value)
                     (frame-to-list control-frame-data)))))
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
           (value (logior (ldb (byte 32 0) (cdr first-item))
                          (ash (ldb (byte 32 0) (car first-item)) 32))))
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

(defmethod frame-to-list ((frame header-control-frame-data))
  (with-slots (stream-id  name-value-pair) frame
    (let ((value (logior (ash 0 63)
                         (ash (ldb (byte 31 0) stream-id) 32)
                         (ldb (byte 32 0) (length name-value-pair)))))
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

(defmethod frame-to-list ((frame rst-stream-control-frame-data))
  (with-slots (stream-id  status-code) frame
    (let ((value (logior (ash 0 63)
                         (ash (ldb (byte 31 0) stream-id) 32)
                         (ldb (byte 32 0) status-code))))
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

(defmethod frame-to-list ((frame syn-reply-control-frame-data))
  (with-slots (stream-id name-value-pair) frame
    (let ((value (logior (ash 0 63)
                         (ash (ldb (byte 31 0) stream-id) 32)
                         (ldb (byte 32 0) (length name-value-pair)))))
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

(defmethod frame-to-list ((frame syn-stream-control-frame-data))
  (with-slots (stream-id priority associated-to-stream-id name-value-pair slot) frame
    (let ((value (logior (ldb (byte 32 0) (length name-value-pair))
                         (ash (ldb (byte 8 0) slot) 32)
                         (ash (ldb (byte 8 0) (ash (ldb (byte 8 0) priority) 5)) 40)
                         (ash (ldb (byte 31 0) associated-to-stream-id) 48)
                         (ash (ldb (byte 1 0) 0) 79)
                         (ash (ldb (byte 31 0) stream-id) 80)
                         (ash (ldb (byte 1 0) 0) 111))))
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

(defmethod frame-to-list ((frame goway-control-frame-data))
  (with-slots (last-good-stream-id status-code) frame
    (let ((value (logior (ldb (byte 32 0) (symbol-to-code *goway-status-code* status-code))
                         (ash (ldb (byte 31 0) last-good-stream-id) 32)
                         (ash 0 63))))
      (number-to-vector 8 value))))
;;====================================================================================================
(defclass ping-control-frame-data ()
  ((id :initarg :id
       :initform 0
       :accessor id)))

(defun mk-ping-control-frame-data (id)
  (make-instance 'ping-control-frame-data 
                 :id id))

(defmethod frame-to-list ((frame ping-control-frame-data))
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

(defmethod frame-to-list ((frame window-update-control-frame-data))
  (with-slots (stream-id delta-window-size) frame
    (let ((value (logior (ldb (byte 31 0) delta-window-size)
                         (ash 0 31)
                         (ash (ldb (byte 31 0) stream-id) 32)
                         (ash 0 63))))
      (number-to-vector 8 value))))
;;====================================================================================================      

(defclass setting-control-frame-data ()
  ((id-value-pair :initarg :id-value-pair
                  :initform nil
                  :accessor id-value-pair)))

(defun mk-setting-control-frame-data (id-value-pair)
  (make-instance 'setting-control-frame-data 
                 :id-value-pair id-value-pair))

(defun comprised-flag-and-id (flag id)
  (logior (ash (ldb (byte 8 0) flag) 24) 
          (ldb (byte 24 0) id)))

(defmethod frame-to-list ((frame setting-control-frame-data))
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

(defmethod frame-to-list ((frame credential-control-frame-data))
  (with-slots (slots proof-length proof length-certificate-pair) frame
    (let ((value (logior (ldb (byte 32 0) proof)
                         (ash (ldb (byte 32 0) proof-length) 32)
                         (ash (ldb (byte 16 0) slots) 64))))
      (append-vector (number-to-vector 8 value)
                     (id-value-pairs-to-list length-certificate-pair)))))
;;====================================================================================================




            
            

