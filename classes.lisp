
(in-package #:structured-sim)


(defclass plc-datatype (clog-table-row) ()
  (:documentation "Base class for PLC Datatypes"))

(defclass int-datatype (plc-datatype) ()
  (:documentation "Represents an Int datatype"))

(defclass string-datatype (plc-datatype) ()
  (:documentation "Represents a String datatype"))

(defgeneric make-plc-tag (plc-datatype)
  (:documentation "Create a plc tag"))


(defmethod create-string-tag ((obj clog-obj) &key content)
  (format t "HTML-ID: ~A" (first content))
  (let* ((new-row (create-table-row obj))
         (name-row (create-table-column new-row :content (first content)))
         (type-row (create-table-column new-row :content (second content)))
         (value-row (create-table-column new-row :content (third content) :html-id (first content)))
         (button-row (create-button (create-table-column new-row) :content "Delete")))
    (format t "~A~%" content)
    (set-on-click button-row (lambda (x)
                               (remhash (text-value name-row) *tags*)
                               (destroy new-row)))
    (if (string= (second content) "Bool")
        (progn
          (setf (background-color value-row) "#ff4040")
          (set-on-click value-row (lambda (x)
                                    (if (string= "False" (text-value x))
                                        (progn
                                          (setf (text-value x) "True")
                                          (setf (background-color x) "#5bb450"))
                                        (progn
                                          (setf (background-color x) "#ff4040")
                                          (setf (text-value x) "False")))))))
    (change-class new-row 'string-datatype)))

(defgeneric update-display (plc-datatype new-value)
  (:documentation "Update display for PLC tag"))

(defmethod update-display ((string-tag string-datatype) new-value)
  (let* ((cols (list-of-children string-tag))
         (second-col (first cols)))
    (setf (text-value second-col) new-value)))

(defun create-plc-tag (tablebody tag)
  (cond
    ((string= (second tag) "String")
     (create-string-tag tablebody :content tag))
    (t (create-string-tag tablebody :content tag))))
