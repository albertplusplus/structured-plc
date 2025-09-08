(in-package :structured-sim)


(defparameter *code-samples*
  (list "outMotor := bPushButton AND NOT bFault;
IF outMotor THEN
  stackLightGreen := TRUE;
  mqttMessage := 'Running';
END_IF;"))
