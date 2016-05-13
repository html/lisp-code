(declaim (optimize speed))

(defun cpu-test (n)
  (let ((a 0))
    (dotimes (i (expt 2 n) a)
      (setf a (logxor a
                      (* i 5)
                      (+ a i))))))

(sb-sprof:with-profiling 
  (:mode :cpu :loop nil)
  (cpu-test 27)
  #+l(sb-sprof:with-sampling (t)
       (cpu-test 26)))

(setf *inspect* 
  (let ((*standard-output* (make-string-output-stream)))
    (sb-sprof:report :type :graph)))

(setf *inspect-2* (root-caller *inspect*))

(car 
  (last 
    (slot-value 
      (let ((*standard-output* (make-string-output-stream)))
        (sb-sprof:report :type :graph))
      'sb-sprof::vertices)))

(print-node 
  (nth 50 
       (slot-value 
         (let ((*standard-output* (make-string-output-stream)))
           (sb-sprof:report :type :graph))
         'sb-sprof::vertices)))

(defun print-node (node &optional (deepness 0))
  (loop for i from 0 to deepness do 
        (format t " "))
  (format t "~A~%" node)
  (loop for i in (slot-value node 'sb-sprof::edges)
        do (print-node (slot-value i 'sb-sprof::vertex) (+ deepness 1))))



(loop for i in (slot-value 
                 (let ((*standard-output* (make-string-output-stream)))
                   (sb-sprof:report :type :graph))
                 'sb-sprof::vertices)
      collect (slot-value i 'sb-sprof::root))

(defun root-caller (call-graph)
  (loop for i in (slot-value call-graph 'sb-sprof::vertices) do 
        (unless (slot-value i 'sb-sprof::callers)
          (return-from root-caller i))))

(sb-sprof:report :type :graph)
(sb-sprof:reset)
(sb-profile:profile)
(sb-profile:report)
(sb-profile:reset)

(sb-profile:profile "VEG" "WEBLOCKS")

(loop for i in (list-all-packages) do 
      (progn 
        (print i)
        (sleep 1)
        (eval `(sb-profile:profile ,(package-name i)))))

(defmacro weblocks::with-profiling (&body body)
  `(sb-sprof:with-profiling 
     (:mode :cpu :loop nil :reset t)
     ,@body))
