
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
  (let* ((new-row (create-table-row obj)))
    (loop for x in content do
          (create-table-column new-row :content x))
    (create-button (create-table-column new-row) :content "Delete")
    (change-class new-row 'string-datatype)))



(defmethod update-display ((string-tag string-datatype) new-value)
  (let* ((cols (children string-tag))
         (second-col (nth 2 cols)))
    (setf (text-value second-col) new-value)))
