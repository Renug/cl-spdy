
(defclass spdy-data ()
  (data :initform nil
        :initarg :data
        :accessor data)
  (cursor :initform 0
          :initarg :cursor
          :accessor cursor)
  (needed :initform 0
          :initarg :needed
          :accessor needed))

(defclass spdy-data-frame ()
  ((stream-id :initform 0
              :initarg :stream-id
              :accessor stream-id)
   (flags :initform 0
          :initarg :flags
          :accessor flags)
   (length-in-byte :initform 0
                   :initarg :length-in-byte
                   :accessor length-in-byte)
   (data :initform nil
         :initarg :data
         :accessor data)))
 
(defun data-flags-symbol-to-code (symbol)
  (case symbol
    ('FLAG_FIN #x01)
    ('FLAG_COMPRESS #x02)))

(defun mk-data-frame (&key stream-id length-in-byte data (flags 'FLAG_FIN))
  (make-instance 'spdy-data-frame 
                 :stream-id stream-id
                 :length-in-byte length-in-byte
                 :data data
                 :flags flags))