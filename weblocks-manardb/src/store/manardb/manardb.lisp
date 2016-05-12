
(defpackage #:weblocks-manardb
  (:use :cl :metabang.utilities :weblocks-memory :weblocks-stores)
  (:documentation
   "A driver for weblocks backend store API that connects to Manardb."))

(in-package :weblocks-manardb)

(weblocks-stores:register-store-type :manardb)

(defclass manardb-store ()
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialization/finalization ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod open-store ((store-type (eql :manardb)) &rest args)
  (let* ((store (make-instance 'manardb-store)))
    (setf *default-store* store)))

(defmethod close-store ((store manardb-store))
  )

(defmethod clean-store ((store manardb-store))
  (error "Not implemented"))

;;;;;;;;;;;;;;;;;;;;
;;; Transactions ;;;
;;;;;;;;;;;;;;;;;;;;
(defmethod begin-transaction ((store manardb-store))
  ; No support for composable transactions in manardb yet
  nil)

(defmethod commit-transaction ((store manardb-store))
  ; No support for composable transactions in manardb yet
  nil)

(defmethod rollback-transaction ((store manardb-store))
  ; No support for composable transactions in manardb yet
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating and deleting persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod persist-object ((store manardb-store) object &key (txn-p t))
  (setf (slot-value object 'weblocks-cms::persistent-p) t)
  object)

(defmethod delete-persistent-object ((store manardb-store) object)
  (let ((object-class (class-of object)))
    (error "Not implemented ~A" object-class)))

(defmethod delete-persistent-object-by-id ((store manardb-store) class-name object-id)
  (error "Not implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Querying persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod find-persistent-object-by-id ((store manardb-store) class-name object-id)
  (error "Not implemented"))

(defmethod find-persistent-objects ((store manardb-store) class-name 
                                    &key (filter nil) order-by range slot
                                         (value nil value-given)
                                         (test #'equal))
  "The slot and value keys must appear together.  If they appear, a
filter will be applied (before the filter passed, if any) that
requires all objects to have the given value in the given slot."
  (cond 
    ((and 
       (not filter)
       (not order-by)
       (not range))
     (remove-if-not 
       (lambda (item)
         (slot-value item 'weblocks-cms::persistent-p))
       (manardb:retrieve-all-instances class-name)))
    (t (error "Not implemented")))
  
  #+l(range-objects-in-memory
   (order-objects-in-memory
    (let* ((seq (query store 'tx-find-persistent-objects-prevalence class-name))
           (seq2 (cond
                   ((and seq slot value-given)
                    (remove-if-not
                      (lambda (obj)
                        (cond
                          ((not (slot-exists-p obj slot))
                           (warn "FIND-PERSISTENT-OBJECTS: object ~A doesn't have the slot ~A" obj slot))
                          ((not (slot-boundp obj slot))
                           (warn "FIND-PERSISTENT-OBJECTS: slot ~A of object ~A is not bound" obj slot))
                          (t (funcall test (slot-value obj slot) value))))
                      seq))
                   (t seq)))
           (seq3 (cond
                   ((and seq2
                         (functionp filter))
                    (remove-if-not filter seq2))
                   (t seq2))))
      seq3)
    order-by)
   range))

(defmethod count-persistent-objects ((store manardb-store) class-name &rest args)
  (error "Not implemented")
  (length (apply #'find-persistent-objects store class-name args)))

(defmethod list-model-classes ((store manardb-store))
  (error "Not implemented")
  #+l(loop for i being the hash-keys of (cl-prevalence::get-root-objects store)
        collect i))

(defmethod delete-model-class ((store manardb-store) class-name)
  (error "Not implemented"))

(defmethod store-type ((store manardb-store))
  :manardb)
