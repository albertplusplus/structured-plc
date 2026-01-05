;;;; structured-sim.lisp

(in-package #:structured-sim)


(defun styles (&rest args)
  (apply #'str:concat args))


(defun on-new-window (body)
  (let* ((running nil)
         (tags nil)
         (stopped t)
         (code nil)
         (sample (alexandria:random-elt *code-samples*))
         (code (cdr (assoc 'code sample)))
         (vars (cdr (assoc 'tags sample))))
    (clog-web:clog-web-initialize body)
    (setf (title (html-document body)) "Structured Sim")
    (add-class body "w3-blue-grey")
    (create-div body :class "w3-panel w3-light-blue"
                     :style +flex-col+
                     :content (spinneret:with-html-string
                                (:h1 "PLC Simulator Using Structured Text (SCL)")))
    (with-clog-create body
        (div (:style +flex-col+)

             (div (:style +flex-col+)
                  (p (:content "Create Tags Here"))
                  (div (:style (styles +flex-row+ "width:100%;margin-top:0px;justify-content:flex-start;margin-bottom:30px;") :class "w3-container")
                       (table (:style "width:100%;" :class "w3-table w3-border w3-bordered")
                              (table-body (:bind tablebody)
                                          (table-row (:class "w3-light-blue")
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
                                                                     (option (:value "String" :content "String[30]"))
                                                                     (option (:value "ArrayInt" :content "Array[0..30] OF Int"))
                                                                     (option (:value "ArrayReal" :content "Array[0..30] OF Real"))
                                                                     (option (:value "ArrayBool" :content "Array[0..30] OF Bool"))))
                                                     (table-column ())
                                                     (table-column ()
                                                                   (button (:bind add-button :content "Add"))))))))

             ;(div (:class "w3-panel w3-grey"))

             (div (:style (styles +flex-row+ "width:80%;height:100%;"))

                  (div (:bind editor-d :style (styles +flex-col+ "flex:1;width:50%;height:100%;")))

                     ;  (div (:style (styles "gap:10px;" +flex-row+))
                      ;      (button (:bind stop-button :style "background-color:orange;" :content "Stop"))
                       ;     (button (:bind run-button :style "background-color:#ccc;" :content "Run"))))
                  (div (:style (styles +flex-col+ "height:100%;margin-left:auto;"))

                  (div (:style (styles +flex-row+ "gap:10px;"))
                       (button (:bind stop-button
                                 :style "background-color:orange;"
                                 :content "Stop"))
                       (button (:bind run-button
                                 :style "background-color:#ccc;"
                                 :content "Run")))
                       (text-area (:rows 30 :columns 20 :value "Debug info" :style "margin-left:3px;")))))


      (flet ((start-running (obj)
               (setf (background-color stop-button) "#ccc")
               (setf (background-color run-button) "green")
               (format t "~A~%" (lex (js-query body "window.clogEditor.getValue()"))))
             (stop-running (obj)
               (setf (background-color stop-button) "orange")
               (setf (background-color run-button) "#ccc"))
             (add-tags (tag)
             ;  (with-clog-create tablebody
                   (create-string-tag tablebody :content tag)))
                   ;(table-row (:bind tag-row)
                              ;(table-column (:content (first tag)))
                              ;(table-column (:content (second tag)))
                              ;(table-column (:content (third tag)))

                             ; (table-column ()
                             ;               (button (:bind del-button :content "delete"))))
                ; (set-on-click del-button (lambda (x) (destroy tag-row))))))
        (loop for tag in vars do
              (add-tags tag))
        (set-on-click stop-button #'stop-running)
        (set-on-click run-button #'start-running)
        (set-on-click add-button
                      (lambda (obj)
                        (when (not (str:emptyp (value tag-name)))
                          (add-tags (list (value tag-name)
                                          (value type-select)
                                          (alexandria:switch ((value type-select) :test #'string=)
                                            ("Bool" "False")
                                            ("Int" "0")
                                            ("Real" "0")
                                            ("String" "''")
                                            ("Array" "[]")))))))
                       ;   (with-clog-create tablebody
                       ;       (table-row (:bind tag-row)
                       ;                  (table-column (:content (value tag-name)))
                       ;                  (table-column (:content (value type-select)))
                       ;                  (table-column (:content (alexandria:switch ((value type-select) :test #'string=)
                       ;                                            ("Bool" "False")
                       ;                                            ("Int" "0")
                       ;                                            ("Real" "0.0")
                       ;                                            ("String" "''")
                       ;                                            ("Array" "[]")
                       ;                                            (t "Undefined"))))
                       ;                  (table-column ()
                       ;                                (button (:bind del-button :content "Delete"))))
                       ;     (set-on-click del-button (lambda (x) (destroy tag-row)))))))
                      (create-code-editor body editor-d code)))))



(defun start-app ()
  (initialize 'on-new-window))


(defun create-code-editor (body container-div code)
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
                 }, 100);" (escape-js-string code)))
    editor-div))

(defun escape-js-string (str)
  (str:replace-all
   "'" "\\'"
   (str:replace-all
    (string #\Newline) "\\n"
    (str:replace-all "\\" "\\\\" str))))
