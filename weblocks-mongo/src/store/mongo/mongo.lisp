
(defpackage #:weblocks-mongo
  (:use :cl :weblocks-stores)
  (:documentation
   "A driver for weblocks backend store API that connects to MongoDb using cl-mongo."))

(in-package :weblocks-mongo)

(weblocks-stores:register-store-type :mongo)

(defclass mongo-store ()
  ((name :initarg :name)
   (model-classes :initarg :model-classes :accessor list-model-classes)
   (use-auto-increment-ids :initarg :integer-ids)
   (objects-cache :initform (make-hash-table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialization/finalization ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod open-store ((store-type (eql :mongo)) &rest args)
  (cl-mongo:db.use (getf args :name))
  (setf *default-store* (apply #'make-instance (list* 'mongo-store args))))

(defmethod close-store ((store mongo-store))
  (when (eq *default-store* store)
    (setf *default-store* nil)))

(defmethod clean-store ((store mongo-store))
  (cl-mongo:db.eval 
    "db.getCollectionNames().forEach(function(c) {
    if(c != 'system.indexes') { 
        db.getCollection(c).drop();
    }
  });"))

;;;;;;;;;;;;;;;;;;;;
;;; Transactions ;;;
;;;;;;;;;;;;;;;;;;;;
(defmethod begin-transaction ((store mongo-store))
  ; Not supported for mongo 
  )

(defmethod commit-transaction ((store mongo-store))
  ; Not supported for mongo
  )

(defmethod rollback-transaction ((store mongo-store))
  ; Not supported for mongo
  )


(defun find-mongo-doc-by-id (cls-type id)
  (first (cl-mongo:docs 
           (cl-mongo:db.find cls-type 
                             (cl-mongo:kv "_id" id)))))

(defun alist->cl-mongo-doc (alist)
  (let ((doc (cl-mongo:make-document)))
    (loop for (key . val) in alist do 
          (cl-mongo:add-element key val doc))
    doc))

(defun lisp-value-serialized (value)
  (alist->cl-mongo-doc 
    `(("type" . "lisp-code")
      ("value" . ,value))))

(defun link-to-object (obj)
  (alist->cl-mongo-doc 
    `(("type" . "object-link")
      ("object-type" . ,(write-to-string (type-of obj)))
      ("value" . ,(slot-value obj 'id)))))

(defun serialize-hash (hash)
  (alist->cl-mongo-doc 
    `(("type" . "hash-table")
      ("value" .  ,(loop for (key . value) in (alexandria:hash-table-alist hash)
                         append (list (maybe-serialize-lisp-value key) (maybe-serialize-lisp-value value)))))))

(defun maybe-serialize-lisp-value (value)
  (cond 
    ((consp value) (list->mongo-array value))
    ((and (typep value 'standard-object) (object-id value))
     (link-to-object value))
    ((and value (symbolp value))
     (alist->cl-mongo-doc 
       `(("type" . "symbol")
         ("value" . ,(format nil "~A::~A" (string-upcase (package-name (symbol-package value))) (string-upcase value))))))
    ((typep value 'hash-table) (serialize-hash value))
    (t value)))

(defun unserialize-lisp-value (value)
  (cond 
    ((string= "cons" (cl-mongo:get-element "type" value))
     (cons (maybe-unserialize-lisp-value (cl-mongo:get-element "car" value)) 
           (maybe-unserialize-lisp-value (cl-mongo:get-element "cdr" value))))
    ((string= "symbol" (cl-mongo:get-element "type" value))
     (let ((parts (ppcre:split ":+" (cl-mongo:get-element "value" value))))
       (destructuring-bind (package symbol) (mapcar #'string-upcase parts)
         (intern symbol package))))
    ((string= "object-link" (cl-mongo:get-element "type" value))
     (find-persistent-object-by-id 
       *default-store* 
       (read-from-string (cl-mongo:get-element "object-type" value))
       (cl-mongo:get-element "value" value)))
    ((string= "hash-table" (cl-mongo:get-element "type" value))
     (alexandria:alist-hash-table 
       (loop for (key value) on (cl-mongo:get-element "value" value) by #'cddr 
             collect 
             (cons (maybe-unserialize-lisp-value key) (maybe-unserialize-lisp-value value)))))
    (t (error "Cannot unserialize ~A~%" value))
    ))

(defun maybe-unserialize-lisp-value (value)
  (cond 
    ((typep value 'cl-mongo:document)
     (unserialize-lisp-value value))
    ((consp value)
     (mongo-array->list value))
    (t value)))

(load #p"/home/oz/projects/lisp-projects/game-republic/lib/weblocks-stores/src/store/mongo/lists-serialization.lisp")

(defun get-next-sequence-id (class-name)
  (cl-mongo:get-element "retval"
                          (first (cl-mongo:docs 
                                   (cl-mongo:db.eval 
                                     (format nil
      "return (function(name) {
   var ret = db.counters.findAndModify(
          {
            query: { _id: name },
            update: { $inc: { seq: 1 } },
            new: true,
            upsert: true
          }
   );

   return ret.seq;
})(\"~A\");"
      class-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating and deleting persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod persist-object ((store mongo-store) object &key)
  ;; Note, we persist new objects in three steps, this should be
  ;; optimized into a single query later
  (let* ((pobj object)
         (string-type (type->mongo-str (type-of pobj)))
         (obj (or 
                (and (object-id pobj)
                     (find-mongo-doc-by-id string-type (object-id pobj)))
                (cl-mongo:make-document))))

    (loop for i in (c2mop:class-slots (class-of pobj))
          unless (equal 'id (c2mop:slot-definition-name i))
          do
          (when (slot-boundp pobj (c2mop:slot-definition-name i))
            (cl-mongo:add-element 
              (string-downcase (c2mop:slot-definition-name i))
              (maybe-serialize-lisp-value (slot-value pobj (c2mop:slot-definition-name i)))
              obj)))

    (if (and (not (object-id pobj))
             (slot-value store 'use-auto-increment-ids))
      (setf (slot-value obj 'cl-mongo::_id) (get-next-sequence-id string-type))
      (setf (slot-value obj 'cl-mongo::_id) (slot-value pobj 'id)))

    (cl-mongo:db.save string-type obj)

    (setf (get 'loaded-from-db pobj) t)
    (setf (gethash (slot-value obj 'cl-mongo::_id) (objects-of-class-cache store (type-of pobj))) pobj)
    
    pobj))

(defmethod delete-persistent-object ((store mongo-store) object)
  (delete-persistent-object-by-id store (type-of object) (object-id object)))

(defmethod delete-persistent-object-by-id ((store mongo-store) class-name object-id)
  (cl-mongo:db.delete (type->mongo-str class-name) 
                      (find-mongo-doc-by-id (type->mongo-str class-name) (mongo-id->bson-id object-id))))

;;;;;;;;;;;;;
;;; Utils ;;;
;;;;;;;;;;;;;

(defun range-to-offset (range)
  "Converts the 'range' argument to SQL OFFSET."
  (when range
    (car range)))

(defun range-to-limit (range)
  "Converts the 'range' argument to SQL LIMIT."
  (when range
    (- (cdr range) (car range))))

(defun type->mongo-str (type)
  (ppcre:regex-replace-all "-backup" 
                           (string-downcase (format nil "~A::~A" (package-name (symbol-package type)) type))
                           ""))

(defun mongo-id->bson-id (id)
  (if (numberp id)
    id
    (if (typep id 'cl-mongo::bson-oid)
      id
      (cl-mongo::make-bson-oid 
        :oid (if (stringp id) (mongoid:oid id) id)))))

(defun objects-of-class-cache (store type)
  (setf type (intern (ppcre:regex-replace-all "-BACKUP" (string-upcase type) "") (symbol-package type)))
  (or (gethash type (slot-value store 'objects-cache))
      (setf (gethash type (slot-value store 'objects-cache)) 
            (make-hash-table))))

(defun mongo-doc->object (store type doc)
  (let ((cls-cache (objects-of-class-cache store type)))
    (or 
      (and 
        (slot-value doc 'cl-mongo::_id)
        (gethash (slot-value doc 'cl-mongo::_id) cls-cache))
      (let ((obj (make-instance type))
            (elements-hash (cl-mongo::elements doc))
            (slots (c2mop:class-slots (find-class type))))
        (setf (object-id obj) (slot-value doc 'cl-mongo::_id))

        (loop for i in slots 
              unless (string= "id" (string-downcase (c2mop:slot-definition-name i)))
              do
              (setf (slot-value obj (c2mop:slot-definition-name i)) 
                    (maybe-unserialize-lisp-value 
                      (gethash 
                        (string-downcase (c2mop:slot-definition-name i))
                        elements-hash))))

        (setf (get 'loaded-from-db obj) t)

        (setf (gethash (slot-value doc 'cl-mongo::_id) cls-cache) obj)

        obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Querying persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod find-persistent-object-by-id ((store mongo-store) class-name object-id)
  (let ((doc (find-mongo-doc-by-id (type->mongo-str class-name) (mongo-id->bson-id object-id))))
    (when doc 
      (mongo-doc->object store class-name doc))))

(defmethod find-persistent-objects ((store mongo-store) class-name &key
                                                        order-by range filter &allow-other-keys)
  (when (and filter (functionp filter)) 
    (return-from 
      find-persistent-objects 
      (weblocks-memory:range-objects-in-memory 
        (weblocks-memory:order-objects-in-memory 
          (remove-if-not filter (find-persistent-objects store class-name))
          order-by)
        range)))

  (let ((find-args (list (type->mongo-str class-name) :all)))
    (if range 
      (setf find-args (append find-args (list :skip (range-to-offset range)
                                              :limit (range-to-limit range))))
      (setf find-args (append find-args (list :limit (+ 100 (count-persistent-objects store class-name))))))

    (when order-by 
      (setf find-args (append find-args (list :field (string-downcase (car order-by)) :asc (eql (cdr order-by) :asc)))))

    (loop for i in (cl-mongo:docs (if order-by
                                    (eval `(cl-mongo:db.sort ,@find-args))
                                    (apply #'cl-mongo:db.find find-args)))
          collect (mongo-doc->object store class-name i))))

(defmethod count-persistent-objects ((store mongo-store) class-name
                                                         &rest args
                                                         &key filter
                                                         &allow-other-keys 
                                                         )
  (when filter 
    (return-from count-persistent-objects (length (apply #'find-persistent-objects store class-name args))))

  (ceiling (cl-mongo:get-element "n" (first (cl-mongo:docs (cl-mongo:db.count (type->mongo-str class-name) :all))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLSQL/Weblocks Oddities ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+l(defmethod class-visible-slots-impl
    ((cls (eql (find-class 'clsql-sys::standard-db-object))) &key &allow-other-keys)
  "Hide slots on `standard-db-object'."
  '())

(defmethod class-visible-slots-impl :around (cls &key readablep writablep) 
  (remove-if 
    (lambda (item)
      (or 
        (not item)
        (string= 
          (package-name 
            (symbol-package (c2mop:slot-definition-name item)))
          "CLSQL-SYS")))
    (call-next-method)))

(defmethod object-id :around (obj)
  (let ((parent (call-next-method)))
    (if (typep parent 'cl-mongo::bson-oid)
      (mongoid:oid-str (cl-mongo::_id parent))
      parent)))

(defclass mongo-metaclass (standard-class)
  ())

(defmethod c2mop:validate-superclass ((class mongo-metaclass) (super standard-class))
  "Persistent classes may inherit from ordinary classes."
  t)

(defmethod c2mop:validate-superclass ((class standard-class) (super mongo-metaclass))
  "Ordinary classes may NOT inherit from persistent classes."
  nil)

(defmethod c2mop:slot-value-using-class :around ((cls mongo-metaclass) obj obj3)
  (let ((value (call-next-method)))
    value))

(defmethod (setf c2mop:slot-value-using-class) :around (value (cls mongo-metaclass) obj obj3)
  (if (get 'loaded-from-db obj)
    (progn 
      (format t "Update value of ~A to ~A~%" obj3 value)
      (persist-object *default-store* obj))
    (call-next-method)))
