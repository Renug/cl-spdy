
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
                   :accessor unidirectional)))

