;;;; structured-sim.lisp

(in-package #:structured-sim)


(defun styles (&rest args)
  (apply #'str:concat args))


(defun on-new-window (body)
  (let ((running nil)
        (stopped t)
        (tags nil)
        (code nil))
    (clog-web:clog-web-initialize body)
    (setf (title (html-document body)) "Structured Sim")
    (add-class body "w3-blue-grey")
    (create-div body :class "w3-panel w3-light-blue"
                     :style +flex-col+
                     :content (spinneret:with-html-string
                                (:h1 "PLC Simulator")
                                (:h3 "With Structured Text")))
    (with-clog-create body
        (div (:style +flex-row+)

             (div (:style +flex-col+)
                  (p (:content "Create Tags Here"))
                  (div (:style (styles +flex-row+ "width:100%;margin-top:0px;justify-content:flex-start;") :class "w3-container")
                       (table (:style "width:100%;" :class "w3-table w3-border w3-bordered")
                              (table-body (:bind tablebody)
                                          (table-row ()
                                                     (table-heading (:content "Tag Name"))
                                                     (table-heading (:content "Tag Type"))
                                                     (table-heading (:content "Value"))
                                                     (table-heading (:content "Action")))
                                          (table-row ()
                                                     (table-column ()
                                                                   (form-element (:text :bind tag-name)))
                                                     (table-column ()
                                                                   (select (:bind type-select)
                                                                     (option (:value "Bool" :content "Bool"))
                                                                     (option (:value "Int" :content "Int"))
                                                                     (option (:value "Real" :content "Real"))
                                                                     (option (:value "String" :content "String[255]"))
                                                                     (option (:value "ArrayInt" :content "Array[0..255] OF Int"))
                                                                     (option (:value "ArrayReal" :content "Array[0..255] OF Real"))
                                                                     (option (:value "ArrayBool" :content "Array[0..255] OF Bool"))))
                                                     (table-column ())
                                                     (table-column ()
                                                                   (button (:bind add-button :content "Add"))))))))

             ;(div (:class "w3-panel w3-grey"))

             (div (:bind editor-d :style (styles +flex-col+ "width:45%;height:100%;"))
                  (div (:style (styles "gap:10px;" +flex-row+))
                       (button (:bind stop-button :style "background-color:orange;" :content "Stop"))
                       (button (:bind run-button :style "background-color:#ccc;" :content "Run"))))
             (div (:style (styles +flex-col+ "height:100%;margin-left:auto;"))
                  (label (:content "Debug Messages"))
                  (text-area (:rows 30 :columns 20))))


      (flet ((start-running (obj)
               (setf (background-color stop-button) "#ccc")
               (setf (background-color run-button) "green"))
             (stop-running (obj)
               (setf (background-color stop-button) "orange")
               (setf (background-color run-button) "#ccc")))
        (set-on-click stop-button #'stop-running)
        (set-on-click run-button #'start-running)
        (set-on-click add-button
                      (lambda (obj)
                        (when (not (str:emptyp (value tag-name)))
                          (with-clog-create tablebody
                              (table-row (:bind tag-row)
                                         (table-column (:content (value tag-name)))
                                         (table-column (:content (value type-select)))
                                         (table-column (:content (alexandria:switch ((value type-select) :test #'string=)
                                                                   ("Bool" "False")
                                                                   ("Int" "0")
                                                                   ("Real" "0.0")
                                                                   ("String" "''")
                                                                   ("Array" "[]")
                                                                   (t "Undefined"))))
                                         (table-column ()
                                                       (button (:bind del-button :content "Delete"))))
                            (set-on-click del-button (lambda (x) (destroy tag-row)))))))
                      (create-code-editor body editor-d)))))



(defun start-app ()
  (initialize 'on-new-window))


(defun create-code-editor (body container-div)
  (let ((editor-div (create-div container-div :html-id "editor")))
    (load-script (html-document body)
                 "https://cdnjs.cloudflare.com/ajax/libs/ace/1.32.6/ace.js")

    (setf (style editor-div "height") "500px")
    (setf (style editor-div "width") "100%")
    (setf (style editor-div "border") "1px solid #ccc")
    (js-execute body
                (format nil
                   "setTimeout(function() {
                   var editor = ace.edit('editor');
                   editor.setTheme('ace/theme/github');
                   editor.session.setMode('ace/mode/pascal');
                   editor.setFontSize(14);
                   editor.setValue('~a');
                   editor.setOptions({
                     showLineNumbers: true,
                     showGutter: true,
                     wrap: false
                   });

                   window.clogEditor = editor;
                 }, 100);" (escape-js-string (nth (random (length *code-samples*)) *code-samples*))))
    editor-div))

(defun escape-js-string (str)
  (str:replace-all
   "'" "\\'"
   (str:replace-all
    (string #\Newline) "\\n"
    (str:replace-all "\\" "\\\\" str))))
