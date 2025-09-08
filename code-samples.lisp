(in-package :structured-sim)


(defparameter *code-samples*
  (list "
// Set motor on if button is pressed and there is no fault
outMotor := (outMotor OR bPushButton) AND NOT bFault;

// Turn on stacklight and set mqtt message
IF outMotor THEN
  stackLightGreen := TRUE;
  mqttMessage := 'Running';
END_IF;"))
