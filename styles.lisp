(in-package #:structured-sim)


(alexandria:define-constant +flex-col+
  "display:flex;flex-direction:column;justify-content:center;align-items:center;"
  :test #'string=)

(alexandria:define-constant +flex-row+
  "display:flex;flex-direction:row;justify-content:flex-start;align-items:flex-start;"
  :test #'string=)

(defparameter *line-nums*
  "background: url(http://i.imgur.com/2cOaJ.png);
 background-attachment: local;
 background-repeat: no-repeat;
 padding-left: 35px;
 padding-top: 12px;
border: 1px solid #ccc;
 border-color:#ccf;
 resize:none;
 line-height:15px;")
