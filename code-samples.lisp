(in-package :structured-sim)


(defparameter *code-samples*
  '(

    ((tags .
      (("outMotor" "Bool" "False")
       ("bPushButton" "Bool" "False")
       ("bFault" "Bool" "False")
       ("stackLightGreen" "Bool" "False")
       ("mqttMessage" "String" "''")))
     (code .
"
// Set motor on if button is pressed and there is no fault
outMotor := (outMotor OR bPushButton) AND NOT bFault;

// Turn on stacklight and set mqtt message
IF outMotor THEN
  stackLightGreen := TRUE;
  mqttMessage := 'Running';
ELSE
  stackLightGreen := FALSE;
  mqttMessage := 'Idle';
END_IF;"))


    ((tags .
      (("Bytes" "ArrayBool" "[]")))
     (code .
"
// Initialize Array
Bytes[0] := TRUE;
"))
    ))
