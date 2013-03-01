

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *binary-input-stream*
    #+sbcl 'sb-gray:fundamental-binary-input-stream
    #+ccl 'gray:fundamental-binary-input-stream
    #+allegro 'excl:fundamental-binary-input-stream
    #+lispworks 'stream:fundamental-binary-input-stream)
  (defvar *binary-output-stream*
    #+sbcl 'sb-gray:fundamental-binary-output-stream
    #+ccl 'gray:fundamental-binary-output-stream
    #+allegro 'excl:fundamental-binary-output-stream
    #+lispworks 'stream:fundamental-binary-output-stream)
  (defvar *stream-read-byte-function*
    #+sbcl 'sb-gray:stream-read-byte
    #+ccl 'gray:stream-read-byte
    #+allegro 'excl:stream-read-byte
    #+lispworks 'stream:stream-read-byte)
  (defvar *stream-write-byte-function*
    #+sbcl 'sb-gray:stream-write-byte
    #+ccl 'gray:stream-write-byte
    #+allegro 'excl:stream-write-byte
    #+lispworks 'stream:stream-write-byte))


(defclass data-input-stream (#.*binary-input-stream*)
  ((data-array :initform #()
               :type vector
               :reader data-array
               :initarg :data-array)
   (read-position :initform 0
		  :reader read-position)))

(defun mk-data-input-stream (data-array)
  (assert data-array)
  (make-instance 'data-input-stream :data-array data-array))


(defclass data-output-stream (#.*binary-output-stream*)
  ((data-array :initform #()
               :type vector
               :accessor data-array
               :initarg :data-array)))

(defun mk-data-output-stream (data-array)
  (assert data-array)
  (make-instance 'data-output-stream :data-array data-array))

(defmethod #.*stream-read-byte-function* ((streams data-input-stream))
  (with-slots (data-array read-position) streams
    (if (>= read-position (length data-array))
	(error 'end-of-file :stream streams)
	(progn 
	  (incf read-position)
	  (aref data-array (1- read-position))))))

(defmethod #.*stream-write-byte-function* ((streams data-output-stream) byte-value)
  (with-slots (data-array) streams
    (setf data-array (append-vector data-array (vector byte-value)))))
