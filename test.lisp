(use-package :REGRESSION-TEST)

(REGRESSION-TEST:rem-all-tests)

(deftest test-frame-to-list.1
         (let* ((retval (frame-to-list (mk-syn-stream-control-frame-data 20 7
                                                                         :associated-to-stream-id 30
                                                                         :name-value-pair '(("method" . "GET")
                                                                                            ("method" . "POST"))))))
           (equalp retval #(0 0 0 20 
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


(deftest test-frame-to-list.2
         (let* ((retval (frame-to-list (mk-goway-control-frame-data 20))))
           (equalp retval #(0 0 0 20 0 0 0 0)))
         t)

(deftest test-frame-to-list.3
         (let* ((retval (frame-to-list (mk-ping-control-frame-data 20))))
           (equalp retval #(0 0 0 20)))
         t)

(deftest test-frame-to-list.4
         (let* ((retval (frame-to-list (mk-window-update-control-frame-data 20 100))))
           (equalp retval #(0 0 0 20 0 0 0 100)))
         t)

(deftest test-frame-to-list.5
         (let* ((retval (frame-to-list (mk-setting-control-frame-data (list (cons (comprised-flag-and-id 20 20) 
                                                                                  #xff)
                                                                            (cons (comprised-flag-and-id 30 30) 
                                                                                  #xfff))))))
                                                                            
           (equalp retval #(#x00 #x00 #x00 #x02 
                                 #x14 #x00 #x00 #x14 
                                 #x00 #x00 #x00 #xff
                                 #x1e #x00 #x00 #x1e 
                                 #x00 #x00 #x0f #xff)))
         t)

(deftest test-data-stream.1
         (let* ((retval (frame-to-list (mk-syn-stream-control-frame-data 20 7
                                                                         :associated-to-stream-id 30
                                                                         :name-value-pair '(("method" . "GET")
                                                                                            ("method" . "POST"))))))
           (equalp (loop with str = (mk-data-input-stream retval)
                         repeat (length retval)
                         collect (read-byte str))
                   (coerce retval 'list)))
         t)

(deftest test-data-stream.2
         (let* ((retval (frame-to-list (mk-goway-control-frame-data 20))))
           (equalp (loop with str = (mk-data-input-stream retval)
                         repeat (length retval)
                         collect (read-byte str))
                   (coerce retval 'list)))
         t)

(deftest test-data-stream.3
         (let* ((retval (frame-to-list (mk-ping-control-frame-data 20))))
           (equalp (loop with str = (mk-data-input-stream retval)
                         repeat (length retval)
                         collect (read-byte str))
                   (coerce retval 'list)))
         t)

(deftest test-data-stream.4
         (let* ((retval (frame-to-list (mk-window-update-control-frame-data 20 100))))
           (equalp (loop with str = (mk-data-input-stream retval)
                         repeat (length retval)
                         collect (read-byte str))
                   (coerce retval 'list)))
         t)

(deftest test-data-stream.5
         (let* ((retval (frame-to-list (mk-setting-control-frame-data (list (cons (comprised-flag-and-id 20 20) 
                                                                                  #xff)
                                                                            (cons (comprised-flag-and-id 30 30) 
                                                                                  #xfff))))))
           (equalp (loop with str = (mk-data-input-stream retval)
                         repeat (length retval)
                         collect (read-byte str))
                   (coerce retval 'list)))
         t)

(deftest test-data-stream.6
         (let ((retval (loop for i from 0 to 20 collect i))
               (str (mk-data-output-stream #())))
           (mapcar #'(lambda (item) (write-byte item str)) retval)
           (equalp (data-array str) (coerce retval 'vector)))
         t)

;;(do-tests)
(REGRESSION-TEST:do-tests)