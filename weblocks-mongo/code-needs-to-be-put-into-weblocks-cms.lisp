(defmethod model-class-from-description ((store-type (eql :mongo)) i)
  (eval
    `(progn 
       (defclass ,(keyword->symbol (getf i :name)) ()
         ((,(keyword->symbol :id))
          ,@(loop for j in (getf i :fields) collect 
                  (append 
                    (list 
                      (keyword->symbol (getf j :name))

                      :initarg (alexandria:make-keyword (string-upcase (getf j :name)))
                      :initform nil
                      :accessor (intern (string-upcase (format nil "~A-~A" (getf i :name)  (getf j :name))) *models-package*))
                    (cond 
                      ((find (getf j :type) (list :string :integer))
                       (list :type (intern (string-upcase (getf j :type)))))
                      (t nil)))))
         (:metaclass weblocks-mongo::mongo-metaclass)))))


