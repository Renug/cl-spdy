(defparameter *stream-id-count* 0)


(defparameter *is-client* t)

(defparameter *server-stream-ids* (make-array 10 
                                              :element-type '(unsigned-byte 32)
                                              :adjustable t
                                              :initial-element 0))

(defparameter *client-stream-ids* (make-array 10 
                                              :element-type '(unsigned-byte 32)
                                              :adjustable t
                                              :initial-element 0))

(defun add-id-to-ids-array (id)
  (assert (or (= 0 id) 
              (member id *client-stream-ids*) 
              (member id *server-stream-ids*)))
  (if (oddp id)
      (vector-push id *client-stream-ids*)
    (vector-push id *server-stream-ids*))
  id)

(defun generate-server-stream-id ()
  (if (= 0 (length *server-stream-ids*))
      (add-id-to-ids-array 2) 
    (add-id-to-ids-array (+ (aref *server-stream-ids* 
                                  (- (length *server-stream-ids*) 1)) 2))))
      
(defun generate-client-stream-id ()
  (if (= 0 (length *client-stream-ids*))
      (add-id-to-ids-array 1)
    (add-id-to-ids-array (+ 2 (aref *client-stream-ids* 
                                    (- (length *server-stream-ids*) 1))))))

(defun generate-stream-id ()
  (if *is-client*
      (generate-server-stream-id)
    (generate-client-stream-id)))

(defclass spdy-stream ()
  ((stream-id :initarg :stream-id
              :initform 0
              :accessor stream-id)
   (associated-to-stream-id :initarg :associated-to-stream-id
                            :initform 0
                            :accessor associated-to-stream-id)
   (priority :initarg :priority
             :initform 0
             :accessor priority)
   (unidirectional :type boolean
                   :initarg :unidirectional
                   :initform 0
                   :accessor unidirectional)
   (input-stream :initarg :input-stream
                 :type data-input-stream
                 :accessor input-stream)
   (output-stream :initarg :output-stream
                  :accessor output-stream)
   (input-frames :initarg :input-frames
                 :initform (make-array 10 :adjustable t))
   (output-frames :initarg :output-frames
                  :initform (make-array 10 :adjustable t)
                  :accessor output-frames)))

(defun mk-spdy-stream (stream-id priority &key 
                                 (associated-to-stream-id 0) 
                                 (unidirectional 0)
                                 (input-stream nil input-stream-p)
                                 (output-stream nil output-stream-p))
  (assert (or input-stream-p output-stream-p))
  (make-instance 'spdy-stream 
                 :stream-id stream-id
                 :priority priority
                 :associated-to-stream-id associated-to-stream-id
                 :unidirectional unidirectional
                 :input-stream input-stream
                 :output-stream output-stream))

(defmethod flush-frames ((stream spdy-stream))
  (with-slots (output-frames output-stream) stream
    (loop for frame across output-frames do
          (let ((result-array (frame-to-list frame)))
            (loop for byte across result-array do
                  (write-byte byte output-stream))))
    (loop repeat (array-total-size output-frames) do
          (vector-pop output-frames))))


