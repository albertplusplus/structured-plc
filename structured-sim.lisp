;;;; structured-sim.lisp

(in-package #:structured-sim)


(defun styles (&rest args)
  (apply #'str:concat args))

(defun on-new-window (body)
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
                (form (:bind f1)
                      (select (:bind selobj :label (create-label f1 :content "Tag Type: ")))
                      (br ())
                      (form-element (:text :label (create-label f1 :content "Tag Name: ")))))

           (div (:class "w3-panel w3-grey"))


           (div (:bind editor-d :style (styles +flex-col+ "margin-right:0%;width:50%;height:100%;"))
                (div (:content (spinneret:with-html-string
                                 (:button "Simulate"))))))
    (add-select-options selobj '("Input" "Output" "InOut" "Static" "Temp" "Constant"))
    (create-code-editor body editor-d)))



(defun start-app ()
  (initialize 'on-new-window))


(defun create-code-editor (body container-div)
  (let ((editor-div (create-div container-div :html-id "editor")))
    (load-script (html-document body)
                 "https://cdnjs.cloudflare.com/ajax/libs/ace/1.32.6/ace.js")

    (setf (style editor-div "height") "800px")
    (setf (style editor-div "width") "100%")
    (setf (style editor-div "border") "1px solid #ccc")
    (js-execute body
                "setTimeout(function() {
                   var editor = ace.edit('editor');
                   editor.setTheme('ace/theme/github');
                   editor.session.setMode('ace/mode/pascal');
                   editor.setFontSize(14);
                   editor.setOptions({
                     showLineNumbers: true,
                     showGutter: true,
                     wrap: false
                   });

                   window.clogEditor = editor;
                 }, 100);")
    editor-div))
