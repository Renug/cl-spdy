
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
                  :type data-output-stream
                  :accessor output-stream)
   (frames :initarg :frames
           :initform #()
           :type vector)))

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


