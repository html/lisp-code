(weblocks-utils:find-by-values 
            'weblocks-cms::message 
            :status :ready-to-send 
            :type :smtp-mail
            :send-time nil
            :order-by (cons 'weblocks-cms::updated-at :asc))

(weblocks-cms-mailings::send-message (first-by-values 'weblocks-cms::message :id 9387))
(persist-object *mongo-store* (first-by-values 'weblocks-cms::message :id 0))

(loop for i in (all-of 'weblocks-cms::message :store *game-republic-store*)
      do 
      (let ((message i #+l(first-by-values 'weblocks-cms::message :id 0 :store *game-republic-store*)))
        (change-class message 'mongo-message)
        (persist-object *mongo-store* message)

        ))

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

(defun create-classes-for-mongo-models ()
  (loop for i-orig in (weblocks-cms::available-schemes-data weblocks-cms::*current-schema*) do 
        (setf i (copy-tree i-orig))
        (setf (getf i :name) (intern (format nil "~A-BACKUP" (getf i :name)) "KEYWORD"))
        (weblocks-cms::model-class-from-description :mongo i)))

(create-classes-for-mongo-models)

(defun do-job ()
  (let ((scheme-data (weblocks-cms::available-schemes-data weblocks-cms::*current-schema*))
        (model-symbol)
        (model-backup-symbol)
        (i))

    (flet ((display-total-progress (model-name model-position total-models item-position total-items item-id)
             ; Clearing screen
             (format t "~A[H~@*~A[J" #\escape)
             (format t "~%Processing model ~A ~A of ~A~%" model-name model-position total-models)
             (display-progress (/ model-position total-models))
             (format t "~%Processing item #~A (~A of ~A)~%" item-id item-position total-items)
             (display-progress (if (zerop total-items) 0 (/ item-position total-items)))))

      (loop for i-orig in scheme-data 
            for j from 1 
            when (> j 2)
            do
            (setf i (copy-tree i-orig))

            (setf model-symbol (intern (string-upcase (getf i :name)) "WEBLOCKS-CMS"))
            (setf model-backup-symbol (intern (format nil "~A-BACKUP" (getf i :name)) "WEBLOCKS-CMS"))

            (setf (getf i :name) (intern (format nil "~A-BACKUP" (getf i :name)) "KEYWORD"))
            (weblocks-cms::model-class-from-description :mongo i)
            (pushnew model-backup-symbol (slot-value *mongo-store* 'weblocks-mongo::model-classes))

            (let ((all-items-count (length (all-of model-symbol :store *game-republic-store*))))
              (if (zerop all-items-count)
                (display-total-progress (format nil ":~A" (getf i :name))
                                        j (length scheme-data)
                                        0 0 0)
                (loop for item in (all-of model-symbol :store *game-republic-store*)
                      for items-count from 1 ;to 100
                      do 
                      (change-class item model-backup-symbol)
                      (persist-object *mongo-store* item)
                      (display-total-progress (format nil ":~A" model-symbol) #+l(intern (getf i :name) "KEYWORD")
                                              j (length scheme-data)
                                              items-count all-items-count (object-id item)))))))))

(length (weblocks-utils:all-of 'weblocks-cms::town-backup :store *mongo-store*))
(weblocks-utils:delete-all 'weblocks-cms::town-backup :store *mongo-store*)
(loop for i in (weblocks-utils:all-of 'weblocks-cms::town-backup :store *mongo-store*) do 
      (delete-one i :store *mongo-store*))
