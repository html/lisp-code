(defmethod model-class-from-description ((store-type (eql :manardb)) i)
  (eval
    `(progn 
       (manardb::defmmclass ,(keyword->symbol (getf i :name)) ()
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
                      (t nil))))
          (weblocks-cms-last-used-time :initform nil)
          (persistent-p :initform nil))
         #+l(:metaclass manardb:mm-metaclass))

       (defmethod initialize-instance :after ((obj ,(keyword->symbol (getf i :name))) &rest args)
         (setf (slot-value obj 'weblocks-cms-last-used-time) (get-universal-time)))

       )))


