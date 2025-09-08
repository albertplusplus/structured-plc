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
    (create-div body :class "w3-panel w3-sand"
                     :style +flex-col+
                     :content (spinneret:with-html-string
                                (:h1 "PLC Simulator")
                                (:h2 "Using Structured Text")))
    (with-clog-create body
        (div (:style +flex-row+)

             (div (:style (styles +flex-top-col+ "margin-right:10%;"))
                  (p (:content "Create Tags Here"))
                  (div (:style "width:100%;" :class "w3-white w3-container")
                       (table (:class "w3-table")
                              (table-body ()
                                          (table-row ()
                                                     (table-heading (:content "Tag Name"))
                                                     (table-heading (:content "Tag Type")))
                                          (table-row ()
                                                     (table-column (:content "bMotor"))
                                                     (table-column (:content "Bool"))))))
                  (form (:bind f1)
                        (select (:bind selobj :label (create-label f1 :content "Tag Type: ")))
                        (br ())
                        (form-element (:text :label (create-label f1 :content "Tag Name: ")))))

             (div (:class "w3-panel w3-grey"))


             (div (:bind editor-d :style (styles +flex-col+ "margin-right:0%;width:50%;height:100%;"))
                  (div (:style (styles "gap:10px;" +flex-row+))
                       (button (:bind stop-button :style "background-color:orange;" :content "Stop"))
                       (button (:bind run-button :style "background-color:#ccc;" :content "Run")))))
      (flet ((start-running (obj)
               (setf (background-color stop-button) "#ccc")
               (setf (background-color run-button) "green"))
             (stop-running (obj)
               (setf (background-color stop-button) "orange")
               (setf (background-color run-button) "#ccc")))
        (add-select-options selobj '("Input" "Output" "InOut" "Static" "Temp" "Constant"))
        (set-on-click stop-button #'stop-running)
        (set-on-click run-button #'start-running)
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
