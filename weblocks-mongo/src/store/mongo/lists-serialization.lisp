(in-package :weblocks-mongo)

(defun serialize-values-except-docs (item)
  (if (and (atom item) 
           (not (typep item 'cl-mongo:document))) (maybe-serialize-lisp-value item) item))

(defun list->mongo-array (list)
  (let ((result))
    (flet ((debug-and-add-to-result (item)
             (setf item (serialize-values-except-docs item))

             (push item result)
             #+l(print item)))
      (loop while list 
            do
            (let ((car (car list)))
              (setf list (cdr list))
              (if (consp car)
                (debug-and-add-to-result (list->mongo-array car))
                (if (or (consp list) (not list))
                  (debug-and-add-to-result car)
                  (progn 
                    (debug-and-add-to-result 
                      (weblocks-mongo::alist->cl-mongo-doc 
                        `(("type" . "cons")
                          ("car" . ,(serialize-values-except-docs car))
                          ("cdr" . ,(serialize-values-except-docs list)))))
                    (setf list nil))))))

      (reverse result))))

(defun mongo-array->list (list)
  (let ((result))
    (flet ((debug-and-add-to-result (item)
             (setf result (append result item ))
             #+l(print item)))
      (loop while list 
            do
            (let ((car (car list)))
              (setf list (cdr list))
              (if (and 
                    (typep car 'cl-mongo:document)
                    (string= (cl-mongo:get-element "type" car) "cons"))
                (debug-and-add-to-result (cons (maybe-unserialize-lisp-value (cl-mongo:get-element "car" car)) 
                                               (maybe-unserialize-lisp-value (cl-mongo:get-element "cdr" car))))
                (if (atom car)
                  (debug-and-add-to-result (cons (maybe-unserialize-lisp-value car) nil))
                  (debug-and-add-to-result (cons (mongo-array->list car) nil))))))
      result)))

(let ((lists-cases 
        (list 
          '(1 . 2)
          '(1 2 . 3)
          '(1 (2 3))
          '(1 (2 . 3) 4))))
  (loop for i in lists-cases do 
        (unless (equal i (mongo-array->list (list->mongo-array i)))
          (format t "Failed for ~A~%~%~A~%~%~A~%" i 
                  (weblocks-mongo::maybe-unserialize-lisp-value (weblocks-mongo::maybe-serialize-lisp-value i))
                  (with-output-to-string (s) (describe (weblocks-mongo::maybe-serialize-lisp-value i) s))
                  )
          (return))))


(defparameter *cases* (list 
                  '(:NAME . "Исаак Шварцвальд")
                  '((:NAME . "Исаак Шварцвальд") (:URL . "http://vk.com/id1629813") (:PHOTO-200-URL . "http://cs618817.vk.me/v618817813/add0/x33Cxbb0Dk0.jpg")
                                    (:PHOTO-100-URL . "http://cs618817.vk.me/v618817813/add1/P_NU4HTdhCk.jpg") (:PHOTO-50-URL . "http://cs618817.vk.me/v618817813/add2/z8RtSVH12dc.jpg"))
                  '(((:NAME . "Исаак Шварцвальд") (:URL . "http://vk.com/id1629813") (:PHOTO-200-URL . "http://cs618817.vk.me/v618817813/add0/x33Cxbb0Dk0.jpg")
                                                  (:PHOTO-100-URL . "http://cs618817.vk.me/v618817813/add1/P_NU4HTdhCk.jpg") (:PHOTO-50-URL . "http://cs618817.vk.me/v618817813/add2/z8RtSVH12dc.jpg"))
                    ((:NAME . "Тимофей Лисицев") (:URL . "http://vk.com/id2725666") (:PHOTO-200-URL . "http://cs614621.vk.me/v614621666/13421/qavAcoUyVS0.jpg")
                                                 (:PHOTO-100-URL . "http://cs614621.vk.me/v614621666/13422/LmzXoSVQ1xQ.jpg") (:PHOTO-50-URL . "http://cs614621.vk.me/v614621666/13423/AncyqjVBoxc.jpg")))
                  (cons 3 (cons 1 2))
                  :vkontakte-friends-not-registered
                  (let ((hash (make-hash-table)))
                    (setf (gethash :vkontakte-friends-not-registered  hash)
                          '(((:NAME . "Исаак Шварцвальд") (:URL . "http://vk.com/id1629813") (:PHOTO-200-URL . "http://cs618817.vk.me/v618817813/add0/x33Cxbb0Dk0.jpg")
                                                          (:PHOTO-100-URL . "http://cs618817.vk.me/v618817813/add1/P_NU4HTdhCk.jpg") (:PHOTO-50-URL . "http://cs618817.vk.me/v618817813/add2/z8RtSVH12dc.jpg"))
                            ((:NAME . "Тимофей Лисицев") (:URL . "http://vk.com/id2725666") (:PHOTO-200-URL . "http://cs614621.vk.me/v614621666/13421/qavAcoUyVS0.jpg")
                                                         (:PHOTO-100-URL . "http://cs614621.vk.me/v614621666/13422/LmzXoSVQ1xQ.jpg") (:PHOTO-50-URL . "http://cs614621.vk.me/v614621666/13423/AncyqjVBoxc.jpg"))))
                    hash)
                  '(:ALL :REGISTERED-USERS :VK-FRIENDS :NOT-REGISTERED)
                  ))

(loop for i in *cases* do 
      (let ((reserialized (weblocks-mongo::maybe-unserialize-lisp-value (weblocks-mongo::maybe-serialize-lisp-value i))))
        (when (typep reserialized 'hash-table)
          (setf reserialized (alexandria:hash-table-alist reserialized))
          (setf i (alexandria:hash-table-alist i)))

        (unless (equal i reserialized)
          (format t "Failed for ~A~%~%~A~%~%~A~%" i 
                  (weblocks-mongo::maybe-unserialize-lisp-value (weblocks-mongo::maybe-serialize-lisp-value i))
                  (with-output-to-string (s) (describe (weblocks-mongo::maybe-serialize-lisp-value i) s)))
          (return))))
