; This is code to evaluate on first init

; Define mongo store
(defstore *mongo-store* :mongo 
          :name "test" 
          :model-classes (list) #+l(list 'weblocks-cms::field-description 
                               'weblocks-cms::model-description 
                               'weblocks-cms::page
                               'weblocks-cms::product
                               'weblocks-cms::synonym
                               'weblocks-cms::template
                               'weblocks-cms::user)
          :integer-ids t)

; Init mongo store
(weblocks-stores:open-stores) 


; Define mongo classes

(in-package :weblocks-cms)

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

(in-package :game-republic) ; Or whatever package you have, just exit weblocks-cms package

(defun create-classes-for-mongo-models ()
  "For each data class of Weblocks CMS schema create mongo analog class with -debug suffix"
  (loop for i-orig in (weblocks-cms::available-schemes-data weblocks-cms::*current-schema*) do 
        (let ((i (copy-tree i-orig)))
          (setf (getf i :name) (intern (format nil "~A-BACKUP" (getf i :name)) "KEYWORD"))
          (weblocks-cms::model-class-from-description :mongo i))))

(create-classes-for-mongo-models)

; End of code to evaluate on first init

(defun display-progress (percent-completed)

  (when (> percent-completed 1)
    (error "Wrong percent, should be from 0 to 1"))

  (let* ((tips-count 80)
         (top-bottom-symbol "_")
         (space-symbol " ")
         (percent-actual (* percent-completed (* tips-count tips-count)))
         (percent-div (ceiling percent-actual tips-count)))
    (format t " ~{~A~}~%" (loop for i from 1 to tips-count collect (if (> i percent-div) space-symbol top-bottom-symbol)))
    (format t "[~{~A~}~{~A~}]~%" 
            (loop for i from 1 to percent-div collect (if (> i percent-div) space-symbol top-bottom-symbol))
            (if (= percent-div tips-count)
              (list "")
              (list* "|" (loop for i from percent-div to (- tips-count 2) collect space-symbol))))))

(defun map-object-fields (model-symbol lambda)
  "Maps all slots for MODEL-SYMBOL class using Weblocks CMS schema"
  (let ((model-scheme-data 
          (copy-tree 
            (loop for i in (weblocks-cms::available-schemes-data weblocks-cms::*current-schema*) 
                  do
                  (when (equal model-symbol (intern (string-upcase (getf i :name)) "WEBLOCKS-CMS"))
                    (return i))))))

    (loop for i in (getf model-scheme-data :fields) do 
          (let ((slot-name (intern (string-upcase (getf i :name)) :WEBLOCKS-CMS)))
            (funcall lambda slot-name)))))

(defun migrate-data-for-model-to-mongo (model-symbol &optional display-callback)
  "Copies data from table related to MODEL-SYMBOL to similar table <MODEL-SYMBOL>-BACKUP"
  (let* ((model-scheme-data 
           (copy-tree 
             (loop for i in (weblocks-cms::available-schemes-data weblocks-cms::*current-schema*) 
                   do
                   (when (equal model-symbol (intern (string-upcase (getf i :name)) "WEBLOCKS-CMS"))
                     (return i)))))
         (model-backup-symbol (intern (format nil "~A-BACKUP" (getf model-scheme-data :name)) "WEBLOCKS-CMS")))

    (setf (getf model-scheme-data :name) (intern (format nil "~A-BACKUP" (getf model-scheme-data :name)) "KEYWORD"))
    (weblocks-cms::model-class-from-description :mongo model-scheme-data)

    (pushnew model-backup-symbol (slot-value *mongo-store* 'weblocks-mongo::model-classes))

    (flet ((display-total-progress (model-name item-position total-items item-id)
             ; Clearing screen
             (if display-callback 
               (funcall display-callback)
               (format t "~A[H~@*~A[J" #\escape))
             (format t "~%Processing item #~A (~A of ~A)~%" item-id item-position total-items)
             (display-progress (if (zerop total-items) 0 (/ item-position total-items)))))

      (let ((all-items-count (length (all-of model-symbol :store *game-republic-store*))))
        (if (zerop all-items-count)
          (display-total-progress (format nil ":~A" model-symbol)
                                  0 0 0)
          (loop for item in (all-of model-symbol :store *game-republic-store*)
                for items-count from 1 ;to 100
                do 
                (let ((new-item (make-instance model-backup-symbol)))
                  (setf (object-id new-item) (object-id item))
                  
                  (map-object-fields 
                    model-symbol 
                    (lambda (field)
                      (setf (slot-value new-item field) (slot-value item field))))

                  (persist-object *mongo-store* new-item))
                (display-total-progress (format nil ":~A" model-symbol) items-count all-items-count (object-id item))))))))

(defun migrate-all-data-from-prevalence-to-mongo ()
  (let ((scheme-data (weblocks-cms::available-schemes-data weblocks-cms::*current-schema*))
        (model-symbol))

    (flet ((display-total-progress (model-name model-position total-models)
             ; Clearing screen
             (format t "~A[H~@*~A[J" #\escape)
             (format t "~%Processing model ~A ~A of ~A~%" model-name model-position total-models)
             (display-progress (/ model-position total-models))))

      (loop for i-orig in scheme-data 
            for j from 1 
            do
            (setf model-symbol (intern (string-upcase (getf i-orig :name)) "WEBLOCKS-CMS"))
            (migrate-data-for-model-to-mongo 
              model-symbol 
              (lambda ()
                (display-total-progress model-symbol j (length scheme-data))))))))

(defun data-values-equal (value-1 value-2)
  (and 
    (equal (type-of value-1) (type-of value-2))
    (etypecase value-1 
      (string (string= value-1 value-2))
      (integer (= value-1 value-2))
      (standard-object  
        (let ((id (object-id value-1)))
          (if id 
            (return-from data-values-equal 
                         (and (equal (type-of value-1)
                                     (type-of value-2))
                              (= id (object-id value-2))))
            (error "Not found ids for objects ~A ~A" value-1 value-2))))
      (null t)
      (boolean (equal value-1 value-2))
      (t (error "~A ~A ~A~%" (type-of value-1) value-1 value-2)))))

(defun test-data-migration-from-prevalence-to-mongo (model-symbol &optional display-callback)
  (let ((original-data (all-of model-symbol :store *game-republic-store*))
        (model-scheme-data 
          (copy-tree 
            (loop for i in (weblocks-cms::available-schemes-data weblocks-cms::*current-schema*) 
                  do
                  (when (equal model-symbol (intern (string-upcase (getf i :name)) "WEBLOCKS-CMS"))
                    (return i)))))
        (mongo-model-symbol (intern (format nil "~A-BACKUP" model-symbol) "WEBLOCKS-CMS")))

    (assert (equal (length original-data) (weblocks-utils:count-of mongo-model-symbol :store *mongo-store*)))

    (flet ((display-total-progress (model-name item-position total-items item-id)
             ; Clearing screen
             (if display-callback 
               (funcall display-callback)
               (format t "~A[H~@*~A[J" #\escape))
             (format t "~%Processing item #~A (~A of ~A)~%" item-id item-position total-items)
             (display-progress (if (zerop total-items) 0 (/ item-position total-items)))))

      ; Across all records
      (loop for i in original-data 
            for j from 1 do 
            (let ((analog-model-object (weblocks-stores:find-persistent-object-by-id *mongo-store* mongo-model-symbol (object-id i))))
              ; Across all slots
              (map-object-fields 
                model-symbol 
                (lambda (field)
                  (data-values-equal (slot-value i field) (slot-value analog-model-object field))))

              (display-total-progress (format nil ":~A" model-symbol) j (length original-data) (object-id i)))))))

#|

(weblocks-utils:find-by-values 
            'weblocks-cms::message 
            :status :ready-to-send 
            :type :smtp-mail
            :send-time nil
            :order-by (cons 'weblocks-cms::updated-at :asc))

(persist-object *mongo-store* (first-by-values 'weblocks-cms::message :id 0))

(loop for i in (all-of 'weblocks-cms::message :store *game-republic-store*)
      do 
      (let ((message i #+l(first-by-values 'weblocks-cms::message :id 0 :store *game-republic-store*)))
        (change-class message 'mongo-message)
        (persist-object *mongo-store* message)

        ))

(length (weblocks-utils:all-of 'weblocks-cms::town-backup :store *mongo-store*))
(weblocks-utils:delete-all 'weblocks-cms::town-backup :store *mongo-store*)
(loop for i in (weblocks-utils:all-of 'weblocks-cms::town-backup :store *mongo-store*) do 
      (delete-one i :store *mongo-store*))
|#
