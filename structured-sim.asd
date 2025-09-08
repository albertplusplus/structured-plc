;;;; structured-sim.asd

(asdf:defsystem #:structured-sim
  :description "Describe structured-sim here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:clog #:str #:spinneret #:parenscript #:cl-ppcre #:alexandria)
  :components ((:file "package")
               (:file "styles")
               (:file "code-samples")
               (:file "structured-sim")))
