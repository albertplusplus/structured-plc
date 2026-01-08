;;;; structured-sim.lisp

(in-package #:structured-sim)

(defparameter +foregounrd+ "w3-teal")
(defparameter +background+ "w3-blue-grey")
(defparameter +foregound-border+ "w3-border-light-blue")
(defparameter +light-blue+ "#DCE2F0")
(defparameter +grey+ "#50586C")

(defun add-semicolon (s)
  (format nil "~A~A" s (if (str:ends-with? ";" s) "" ";")))

(defun back-color (col)
  (add-semicolon (format nil "background-color: ~A" col)))

(defun fore-color (col)
  (add-semicolon (format nil "color: ~A" col)))

(defun styles (&rest args)
  (apply #'str:concat args))

(defun on-about (body)
  (clog-web-initialize body)
  (add-class body +background+)
  (let* ((menu (create-web-menu-bar body))
         (sim (create-web-menu-item menu :content "Simulator" :link "/"))
         (about (create-web-menu-item menu :content "About Me" :link "/about"))
         (_div (create-div body :class (format nil "~A ~A" "w3-panel" +foreground+)
                     :style +flex-col+
                     :content (spinneret:with-html-string (:h1 "About Me"))))
         (about-div (create-div body :style "display:flex;flex-direction:column;width:80%;align-items:center;")))
    (create-div about-div :style "align-items:left;width:90%;"
                          :content (spinneret:with-html-string
                                     (:h1 (:u "Who Am I"))
                                     (:p "My name is Albert Bear. I work as an Automation Engineer at Amazon. Previously I worked at Siemens where
                                          I was a Manufacturing Systems Engineer.")
                                     (:h1 (:u "Contacting Me"))
                                     (:p "Feel free to contact me on " (:a :href "https://www.linkedin.com" "LinkedIn") "or on email at " (:u "albertbear94@gmail.com"))))))


(defun on-new-window (body)
 (clog-web-initialize body)
  (let* ((menu (create-web-menu-bar body))
         (sim (create-web-menu-item menu :content "Simulator" :link "/"))
         (about (create-web-menu-item menu :content "About Me" :link "/about"))

         (running nil)
         (tags nil)
         (stopped t)
         (code nil)
         (sample (alexandria:random-elt *code-samples*))
         (code (cdr (assoc 'code sample)))
         (vars (cdr (assoc 'tags sample))))
    (clog-web:clog-web-initialize body)
    (setf (title (html-document body)) "Structured Sim")
   ; (add-class body +background+)
    (setf (style body "background-color") +light-blue+)
    (create-div body :class  "w3-panel"
                     :style (styles +flex-col+ (back-color +grey+))
                     :content (spinneret:with-html-string
                                (:h1 :style (fore-color +light-blue+) "PLC Simulator Using Structured Text")))
    (with-clog-create body
        (div (:style +flex-col+)

             (div (:style +flex-col+)
                  (p (:content "Create Tags Here"))
                  (div (:style (styles +flex-row+ "width:100%;margin-top:0px;justify-content:flex-start;margin-bottom:30px;") :class "w3-container")
                       (table (:style "width:100%;" :class "w3-table w3-border w3-bordered")
                              (table-body (:bind tablebody)
                                          (table-row (:class "w3-border" :style (back-color +grey+))
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

             (div (:class "w3-border w3-round" :style (styles +flex-row+ "width:80%;height:100%;border-width: 3px !important;" (format nil "border-color:~A !important;" +grey+)))

                  (div (:bind editor-d :style (styles +flex-col+ "flex:1;width:50%;height:100%;")))

                     ;  (div (:style (styles "gap:10px;" +flex-row+))
                      ;      (button (:bind stop-button :style "background-color:orange;" :content "Stop"))
                       ;     (button (:bind run-button :style "background-color:#ccc;" :content "Run"))))
                  (div (:style (styles +flex-col+ "height:100%;margin-left:auto;"))
                       (div (:style (styles +flex-row+ "gap:10px;width:100%;margin-left:3px;"))
                            (button (:bind stop-button
                                      :style "background-color:orange;width:30%;"
                                      :content "Stop"))
                            (button (:bind run-button
                                      :style "background-color:#ccc;width:30%;"
                                      :content "Run")))
                       (text-area (:bind debug-text-area :rows 25 :columns 25 :value (format nil "Debug info will be printed here~%") :style "margin-left:3px;"))))
             (div (:style "height:10vh")))


      (flet ((start-running (obj)
               (when stopped
                 (setf (background-color stop-button) "#ccc")
                 (setf (background-color run-button) "green")
                 (setf stopped nil)
                 (print-to-debug debug-text-area "Simulation Started")
                 (format t "~A~%" (lex (js-query body "window.clogEditor.getValue()")))))
             (stop-running (obj)
               (setf stopped t)
               (setf (background-color stop-button) "orange")
               (setf (background-color run-button) "#ccc")
               (print-to-debug debug-text-area "Simulation Stopped"))
             (add-tags (tag)
             ;  (with-clog-create tablebody
               (cond
                 ((string= (second tag) "Bool")
                  (create-string-tag tablebody :content tag))
                 ((string= (second tag) "Int")
                  (create-string-tag tablebody :content tag))
                 ((string= (second tag) "Real")
                  (create-string-tag tablebody :content tag))
                 ((string= (second tag) "String")
                  (create-string-tag tablebody :content tag))
                 ((string= (second tag) "ArrayBool")
                  (create-string-tag tablebody :content tag))
                 ((string= (second tag) "ArrayInt")
                  (create-string-tag tablebody :content tag))
                 ((string= (second tag) "ArrayInt")
                  (create-string-tag tablebody :content tag))
                 (t (error "Datatype Not Supported")))))
                  ;(create-string-tag tablebody :content tag)))
                   ;(table-row (:bind tag-row)
                              ;(table-column (:content (first tag)))
                              ;(table-column (:content (second tag)))
                              ;(table-column (:content (third tag)))

                             ; (table-column ()
                             ;               (button (:bind del-button :content "delete"))))
                ; (set-on-click del-button (lambda (x) (destroy tag-row))))))
        (loop for tag in vars do
              (add-tags tag))
        (disable-resize debug-text-area)
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
  (initialize 'on-new-window)
  (set-on-new-window 'on-about :path "/about"))


(defun create-code-editor (body container-div code)
  (let ((editor-div (create-div container-div :html-id "editor")))
    (load-script (html-document body)
                 "https://cdnjs.cloudflare.com/ajax/libs/ace/1.32.6/ace.js")

    (setf (style editor-div "height") "600px")
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

(defun print-to-debug (dbug line)
  (let ((txt (text-value dbug)))
    (setf (text-value dbug)
          (concatenate 'string txt (format nil "~A~%" line)))))

(defun escape-js-string (str)
  (str:replace-all
   "'" "\\'"
   (str:replace-all
    (string #\Newline) "\\n"
    (str:replace-all "\\" "\\\\" str))))
