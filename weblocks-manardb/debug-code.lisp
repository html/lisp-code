(defstore *manardb-store* :manardb)
(weblocks-stores:open-stores)
(setf weblocks-stores:*default-store* *veg-store*)

(loop for i in (weblocks-cms::available-schemes-data weblocks-cms::*current-schema*) do
      (setf (getf i :name) (intern (format nil "~A-BACKUP" (getf i :name)) "KEYWORD"))
      (weblocks-cms::model-class-from-description 
        :manardb i))

(manardb:open-all-mmaps)

(defun copy-all-instances (from to)
  (loop for i in (all-of from) do 
        (let ((copy (make-instance to)))
          (loop for j in (weblocks-stores:class-visible-slots from) do 
                (setf (slot-value copy (c2mop:slot-definition-name j))
                      (slot-value i (c2mop:slot-definition-name j))))
          (weblocks-stores:persist-object *manardb-store* copy))
        ))
(copy-all-instances 'weblocks-cms::veg-resource 'weblocks-cms::veg-resource-backup)
(copy-all-instances 'weblocks-cms::label 'weblocks-cms::label-backup)

(loop for i in '(WEBLOCKS-CMS::LOG-RECORD WEBLOCKS-CMS::VEG-RESOURCE 
                                          WEBLOCKS-CMS::SYNONYM
                                          ;WEBLOCKS-CMS:MODEL-DESCRIPTION 
                                          WEBLOCKS-CMS::USER WEBLOCKS-CMS::PRODUCT
                                          WEBLOCKS-CMS::LABEL 
                                          ;WEBLOCKS-CMS:FIELD-DESCRIPTION 
                                          WEBLOCKS-CMS::TEMPLATE
                                          WEBLOCKS-CMS::PAGE)
      do
      (copy-all-instances i (intern (format nil "~A-BACKUP" i) (symbol-package i))))

(defvar *obj* (make-instance 'weblocks-cms::veg-resource-backup))
(slot-value (first (all-of 'weblocks-cms::veg-resource-backup :store *manardb-store*)) 'id)
(describe (first (all-of 'weblocks-cms::veg-resource-backup :store *manardb-store*)))
(describe (first (manardb:retrieve-all-instances 'weblocks-cms::veg-resource-backup-persistent)))
(c2mop:slot-definition-name (second (weblocks-stores:class-visible-slots 'weblocks-cms::veg-resource-backup)))
(c2mop:slot-definition-name (first (weblocks-stores:class-visible-slots 'weblocks-cms::veg-resource)))

(manardb:rewrite-gc 
  (manardb:retrieve-all-instances 'weblocks-cms::veg-resource-backup-persistent)
  :verbose t)

(manardb:rewrite-gc 
  nil
  :verbose t)

(setf (slot-value *temp* 'weblocks-cms::title) "!!!@@@")
(describe *temp*)

(manardb:print-all-mmaps)
(persist-object *manardb-store* (make-instance 'weblocks-cms::veg-resource-backup))
(all-of 'weblocks-cms::veg-resource-backup :store *manardb-store*)
(all-of 'weblocks-cms::label-backup :store *manardb-store*)
(mapcar #'describe (manardb:retrieve-all-instances 'weblocks-cms::label-backup))
