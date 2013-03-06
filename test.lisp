(use-package :REGRESSION-TEST)

(REGRESSION-TEST:rem-all-tests)
;;====================================================================================================
(REGRESSION-TEST:deftest test-frame-serialization.1
                         (let* ((retval 
                                 (frame-serialization 
                                  (mk-syn-stream-control-frame 
                                   20 7
                                   :associated-to-stream-id 30
                                   :name-value-pair '(("method" . "GET")
                                                      ("method" . "POST"))))))
                           (equalp retval (vector 
                                           #x80 #x03 #x00 #x01 
                                           #x01 #x00 #x00 #x31
                                           0 0 0 20 
                                           0 0 0 30 
                                           224 0 
                                           0 0 0 2 
                                           0 0 0 6 
                                           109 101 116 104 111 100 
                                           0 0 0 3 
                                           71 69 84 
                                           0 0 0 6 
                                           109 101 116 104 111 100 
                                           0 0 0 4 
                                           80 79 83 84)))
                                                                         
                         t)

;;====================================================================================================
(REGRESSION-TEST:deftest test-frame-serialization.2
                         (let* ((retval (frame-serialization (mk-goway-control-frame 20))))
                           (equalp retval (vector
                                           #x80 #x03 #x00 #x07 
                                           #x00 #x00 #x00 #x08
                                           0 0 0 20 0 0 0 0)))
                         t)
;;====================================================================================================
(REGRESSION-TEST:deftest test-frame-serialization.3
                         (let* ((retval (frame-serialization (mk-ping-control-frame 20))))
                           (equalp retval (vector
                                           #x80 #x03 #x00 #x06 
                                           #x00 #x00 #x00 #x04
                                           0 0 0 20)))
                         t)
;;====================================================================================================
(REGRESSION-TEST:deftest test-frame-serialization.4
                         (let* ((retval (frame-serialization (mk-window-update-control-frame 20 100))))
                           (equalp retval (vector 
                                           #x80 #x03 #x00 #x09 
                                           #x00 #x00 #x00 #x08
                                           0 0 0 20 0 0 0 100)))
                         t)
;;====================================================================================================
(REGRESSION-TEST:deftest test-frame-serialization.5
                         (let* ((retval (frame-serialization 
                                         (mk-setting-control-frame 
                                          (list (cons (comprised-flag-and-id 20 20) 
                                                      #xff)
                                                (cons (comprised-flag-and-id 30 30) 
                                                      #xfff))))))
                                                                            
                           (equalp retval (vector
                                           #x80 #x03 #x00 #x04 
                                           #x01 #x00 #x00 #x14
                                           #x00 #x00 #x00 #x02 
                                           #x14 #x00 #x00 #x14 
                                           #x00 #x00 #x00 #xff
                                           #x1e #x00 #x00 #x1e 
                                           #x00 #x00 #x0f #xff)))
                         t)
;;====================================================================================================
(REGRESSION-TEST:deftest test-data-stream.1
                         (let* ((retval 
                                 (frame-serialization 
                                  (mk-syn-stream-control-frame 
                                   20 7
                                   :associated-to-stream-id 30
                                   :name-value-pair '(("method" . "GET")
                                                      ("method" . "POST"))))))
                           (equalp (loop with str = (mk-data-input-stream retval)
                                         repeat (length retval)
                                         collect (read-byte str))
                                   (coerce retval 'list)))
                         t)
;;====================================================================================================
(REGRESSION-TEST:deftest test-data-stream.2
                         (let* ((retval (frame-serialization (mk-goway-control-frame 20))))
                           (equalp (loop with str = (mk-data-input-stream retval)
                                         repeat (length retval)
                                         collect (read-byte str))
                                   (coerce retval 'list)))
                         t)
;;====================================================================================================
(REGRESSION-TEST:deftest test-data-stream.3
                         (let* ((retval (frame-serialization (mk-ping-control-frame 20))))
                           (equalp (loop with str = (mk-data-input-stream retval)
                                         repeat (length retval)
                                         collect (read-byte str))
                                   (coerce retval 'list)))
                         t)
;;====================================================================================================
(REGRESSION-TEST:deftest test-data-stream.4
                         (let* ((retval (frame-serialization 
                                         (mk-window-update-control-frame 20 100))))
                           (equalp (loop with str = (mk-data-input-stream retval)
                                         repeat (length retval)
                                         collect (read-byte str))
                                   (coerce retval 'list)))
                         t)
;;====================================================================================================
(REGRESSION-TEST:deftest test-data-stream.5
                         (let* ((retval (frame-serialization 
                                         (mk-setting-control-frame 
                                          (list (cons (comprised-flag-and-id 20 20) 
                                                      #xff)
                                                (cons (comprised-flag-and-id 30 30) 
                                                      #xfff))))))
                           (equalp (loop with str = (mk-data-input-stream retval)
                                         repeat (length retval)
                                         collect (read-byte str))
                                   (coerce retval 'list)))
                         t)
;;====================================================================================================
(REGRESSION-TEST:deftest test-data-stream.6
                         (let ((retval (loop for i from 0 to 20 collect i))
                               (str (mk-data-output-stream #())))
                           (mapcar #'(lambda (item) (write-byte item str)) retval)
                           (equalp (data-array str) (coerce retval 'vector)))
                         t)

;;(do-tests)
(REGRESSION-TEST:do-tests)