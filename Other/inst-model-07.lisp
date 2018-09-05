;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Andrea Stocco
;;;
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : inst-model-07.lisp
;;; Version     : 0.7
;;; 
;;; Description : Model for the INST task
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; ----- History -----
;;;
;;; 2010-10-27  : * Fixed a problem that prevented compilation of 
;;;             :   'retrieve-operation' and 'prepare-calculation-' (either
;;;             :   -unary or -binary).  Had to substitute ?imaginal queries
;;;             :   with ?imaginal-action queries, and get rid of ?goal buffer
;;;             :   queries.  Now compilation occurs just fine.
;;; 
;;; 2010-10-20  : * Back to work (after a long hiatus and move to Seattle). 
;;;             : * Added 'type' to operations (binary/unary).
;;;             : * Simplified the procedure of instantiating an operation;
;;;             :   now it's based on the operation type (binary/unary).
;;;             : * Got rid of the math buffer.  Now calculations are done
;;;             :   by retrieving arithmetic facts.
;;;             : * Enabled production compilation. 
;;;
;;; 2010-05-12  : * Model correctly performs the experiment. Not fitted to 
;;;             :   data yet.
;;;             : * Added task field to 'serial' and 'operation' chunks to
;;;             :   properly associate an operation to a specific task.  The
;;;             :   'task' variable is carried over to the execution phase.
;;;
;;; 2010-05-09  : * Model correctly encodes all instructions and their order.
;;;
;;; 2010-05-04  : * Model correctly encodes binary operations
;;;
;;; 2010-05-03  : * Model correctly executes calculations and executes
;;;             :   tasks (if they are provided in long-term memory).
;;;             : * Model correctly responds to probes.
;;;             : * Model correctly encodes unary operations from
;;;             :   instructions.
;;;
;;; 2010-04-30  : * Simplified visuo-motor actions. Added :auto-attend, added
;;;             :   reliance on visual instead of goal information to know
;;;             :   the state. 
;;;             : * Model correctly encodes the inputs (x and y). 
;;;
;;; 2010-03-09  : * Simple visuo-motor transitions are working. The model just
;;;             :   presses random keys at each step.
;;;
;;; 2010-03-03  : * Back to work.  Changed the code to avoid too many encodings
;;;             :   during breaks.
;;;
;;; 2010-01-21  : * File created.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(clear-all)
(define-model inst

(sgp :trace-detail            medium 
     :show-focus              t 
     :esc                     t 
     :lf                      0.9 
     :mas                     1.6 
     :ga                      1.0 
     :imaginal-activation     1.0 

     ;; Productions
     :epl                     t 
     :pct                     nil
     :ul                      t

     ;; Perceptual params
     :auto-attend             t 
     :visual-finst-span       10.0)

(chunk-type arithmetic-fact operation arg1 arg2 result)
(chunk-type operation task type operator arg1 arg2 position)
(chunk-type mapping letter operation type argument)   
(chunk-type scratchpad x y result position state task)
(chunk-type serial position task)

(add-dm (x isa chunk) (y isa chunk) (* isa chunk) (+ isa chunk)
	(/ isa chunk) (unary isa chunk) (binary isa chunk))

(add-dm (a isa mapping 
	   letter    A
	   operation *
	   type      binary
	   argument  nil)
	(b isa mapping
	   letter    B
	   operation /
	   type      binary
	   argument  nil)
	(c isa       mapping
	   letter    C
	   operation *
	   type      unary
	   argument  2)
	(d isa       mapping
	   letter    D
	   operation +
	   type      unary
	   argument  1)
	(e isa       mapping
	   letter    E
	   operation /
	   type      unary
	   argument  3))


;;; Visual states
;;; (NOTE should be added to the device to keep model code clean)
;;;
(add-dm (screen isa chunk)
	(instructions isa chunk)
	(inputs isa chunk)
	(probe isa chunk)
	(probe-responded isa chunk)
	(fixation isa chunk)
	(fixation1 isa chunk)
	(fixation2 isa chunk)
	(blank isa chunk)
	(feedback isa chunk)
	(operation isa chunk)
	(variable isa chunk))

(load (translate-logical-pathname "INST:inst-arithmetic-facts.lisp"))

;;; -------------------------------------------------------------- ;;;
;;; BASIC VISUAL ACTIONS
;;; -------------------------------------------------------------- ;;;
;;; Just encodes the screen when the model iss not looking at 
;;; anything in particular.
;;; -------------------------------------------------------------- ;;;

;; This production should never fire --- it's here to make the 
;; model work when code is changed and bugs are introduced.
;;
#|
(p look-at-the-screen
   "Looks at the screen if visual pathways are empty"
  ?visual-location>
    buffer       empty
    state        free
  ?visual>
    state        free
    buffer       empty
  ?manual>
    preparation  free
    execution    free
==>
  +visual-location>
    isa          visual-location
    kind         screen
)
|#

(p encode-the-screen
   "Encodes the screen if it's in visual-location"
  =visual-location>
    isa          visual-location
    kind         screen
  ?visual>
    state        free
    buffer       empty
==>
  +visual>
    isa          move-attention
    screen-pos  =visual-location
)

;;; -------------------------------------------------------------- ;;;
;;; INSTRUCTION PHASE
;;; -------------------------------------------------------------- ;;;
;;; During the instruction phase the model reads the screen and 
;;; encodes the necessary operations that are to be performed in
;;; the next phase.
;;; -------------------------------------------------------------- ;;;

(p look-at-instructions 
   "When instructions are detected, they are first encoded as a whole"
  ?imaginal>
    state        free
    buffer       empty
  =visual>
    isa          visual-object
    status       screen
    value        instructions
==>
  +visual-location>
    isa          visual-location
    kind         instructions
)


(p prepare-for-encoding 
   "Prepares to encode operations when the instructions are on"
  ?imaginal>
    state        free
    buffer       empty
  =visual>
    isa          visual-object
    status       instructions
    value       =TASK
==>
  +imaginal>
    isa          serial
    position     1
    task        =TASK
  +visual-location>
    isa          visual-location
    kind         operation
    screen-x     highest
    :attended    nil
)


(p find-mapping
   "Finds a mapping between a letter and the corresponding operation"
  ?retrieval>
    state        free
    buffer       empty
  ?goal>
    state        free
    buffer       empty
  =imaginal>
    isa          serial
  =visual>
    isa          visual-object
    status       operation
    value       =LETTER
==>
  =imaginal>
  =visual>
  +retrieval>
    isa          mapping
    letter      =LETTER
)


(p encode-unary-operation
   "Encodes a unary operation"
  =retrieval>
    isa          mapping
    operation   =OP
    argument    =ARG
    type         unary
  =imaginal>
    isa          serial
    position    =POS
    task        =TASK
  =visual>
    isa          visual-object
    status       operation
==>
  =imaginal>
  =visual>
  +goal>
    isa          operation
    type         unary
    task        =TASK
    operator    =OP
    arg2        =ARG
    position    =POS
)

(p find-variable-for-unary-operation
   "Determines which variable (X or Y) an operation applies to"
  ?visual>
    state        free
  =visual>
    isa          visual-object
    status       operation
    value       =LETTER
  =goal>
    isa          operation
    arg1         nil
  - arg2         nil
==>
  +visual-location>
    isa          visual-location
    kind         variable
  > screen-x     current
    screen-x     lowest
)


(p encode-variable-for-unary-operation
   "Encodes which variable an operation applies to"
  ?visual>
    state        free
  =visual>
    isa          visual-object
    status       variable
    value       =VARIABLE
  =goal>
    isa          operation
    arg1         nil
  - arg2         nil
==>
  =visual>
  =goal>
    arg1        =VARIABLE
)


;;; --------------------------------------------------------------
;;; In case of binary operations, one needs to look at both
;;; variables (to make sure to get them in order).
;;; --------------------------------------------------------------  

(p encode-binary-operation
  =retrieval>
    isa          mapping
    operation   =OP
    type         binary
  =imaginal>
    isa          serial
    position    =POS
    task        =TASK
  =visual>
    isa          visual-object
    status       operation
==>
  =imaginal>
  =visual>
  +goal>
    isa          operation
    type         binary
    operator    =OP
    position    =POS
    task        =TASK

    ;; One could possibly set X and Y right now 
    ;; since there are no Y/X trials.

    ;arg1 x
    ;arg2 y
)


(p find-first-variable-for-binary-operation
   "Finds the first variable for a bin operation (A or B)"
  ?visual>
    state        free
  =visual>
    isa          visual-object
    status       operation
    value       =LETTER
  =goal>
    isa          operation
    arg1         nil
    arg2         nil
==>
  +visual-location>
    isa          visual-location
    kind         variable
  > screen-x     current
    screen-x     lowest
)


(p encode-first-variable-for-binary-operation
   "Stores the first variable as ARG1 in the goal buffer"
  ?visual>
    state        free
  =visual>
    isa          visual-object
    status       variable
    value       =VARIABLE
  =goal>
    isa          operation
    arg1         nil
    arg2         nil
==>
  =goal>
    arg1        =VARIABLE
)


(p find-second-variable-for-binary-operation
   "Finds the second variable for a binayr operation (A or B)"
  ?visual>
    state        free
  ?retrieval>
    state        free
    buffer       empty
  =goal>
    isa          operation
  - arg1         nil
    arg2         nil
==>
  +visual-location>
    isa          visual-location
    kind         variable
  > screen-x     current
    screen-x     lowest
)


(p encode-second-variable-for-binary-operation
   "Stores the second variable (x or y) as ARG2 in the goal buffer"
  ?visual>
    state        free
  =visual>
    isa          visual-object
    status       variable
    value       =VARIABLE
  =goal>
    isa          operation
  - arg1         nil
    arg2         nil
==>
  =visual>
  =goal>
    arg2        =VARIABLE
)


;;; --------------------------------------------------------------
;;; Updates the position and proceeds.
;;; --------------------------------------------------------------

(p update-position
   "When an operation has been created, it is saved in memory and 
the model prepares for the next position in the series"
  ?imaginal>
    state        free
  =imaginal>
    isa          serial
  - position     3
  =goal>
    isa          operation
  - arg1         nil
  - arg2         nil
==>
  -goal>
  =imaginal>
  +imaginal-action>
    isa          generic-action
    action       inst-update-position
)


(p find-operation
   "Finds the next unattended operation on the screen (from right 
to left)"
  ?imaginal>
    state        free
  ?goal>
    state        free
    buffer       empty
  =imaginal>
    isa          serial
  =visual>
    isa          visual-object
    status       variable
;  - status       operation
;  - status       screen
==>
  =imaginal>
  +visual-location>
    isa          visual-location
    kind         operation
    screen-x     highest
    :attended    nil
)
  

;;; --------------------------------------------------------------
;;; Press a key when done
;;; --------------------------------------------------------------

(p go-through-instructions
   "Presses a key after instructions have been encoded"
  ?imaginal>
    state        free
  =imaginal>
    isa          serial
    position     3
  =goal>
    isa          operation
  - arg1         nil
  - arg2         nil
  ?manual>
    preparation  free
    execution    free
==>
  -goal>
  -visual>
  -visual-location>
  =imaginal>
  +manual>
    isa          press-key
    key          "2"
)


;;; -------------------------------------------------------------- ;;;
;;; EXECUTION PHASE
;;; -------------------------------------------------------------- ;;;
;;; During execution, the model looks at the two numbers X and Y 
;;; on the screen and stores them in the scratchpad. It then 
;;; retrieves the three operations in order and applies them to
;;; the intermediate results.
;;; -------------------------------------------------------------- ;;; 

(p begin-encoding-inputs
   "Begins encoding X and Y (creates the scratchpad)"
  ?imaginal>
    state        free
  =imaginal>
    isa          serial
    task        =TASK
  =visual>
    isa          visual-object
    status       screen
    value        inputs
==>
  =visual>
  +imaginal>
    isa          scratchpad
    task        =TASK
    position     1

  ;; A new goal is set here. The presence of this goal early on will
  ;; ensure that retrieve-operation and prepare-calculation-binary
  ;; (or -unary) will be compiled.

  +goal>
    isa          operation
)


(p find-x
   "Finds the X input value on the screen"
  ?visual>
    state        free
  =visual>
    isa          visual-object
    status       screen
    value        inputs
  =imaginal>
    isa          scratchpad
    x            nil
==>
  =visual>
  =imaginal>
  +visual-location>
    isa          visual-location
    kind         x
)


(p encode-x
   "Encodes the X in the scratchpad" 
  ?imaginal>
    state        free
  =imaginal>
    isa          scratchpad
    x            nil
  =visual>
    isa          visual-object
    status       x
    value       =X
==>
  =visual>
  =imaginal>
    x           =X
)


(p find-y
   "Finds the Y input value on the screen" 
  ?visual>
    state        free
  =visual>
    isa          visual-object
    status       x
  =imaginal>
    isa          scratchpad
  - x            nil
    y            nil
==>
  =imaginal>
  +visual-location>
    isa          visual-location
    kind         y
)


(p encode-y
   "Copies the Y value in the mental scratchpad"
  ?imaginal>
    state        free
  =imaginal>
    isa          scratchpad
    y            nil
  =visual>
    isa          visual-object
    status       y
    value       =Y
==>
  +visual>
    isa          clear
  =imaginal>
    y           =Y
)


;;; --------------------------------------------------------------
;;; Preparing and performing mental calculations
;;; -------------------------------------------------------------- 

(p retrieve-operation
   "Retrieves an operation to perform on inputs"
  ?retrieval>
    state        free
    buffer       empty
  ?imaginal-action>
    state        free
  =goal>
    isa          operation
    arg1         nil
    arg2         nil
  =imaginal>
    isa          scratchpad
    x           =X
    y           =Y
    position    =POS
    task        =TASK
    result       nil
==>
  =imaginal>
  +retrieval>
    isa          operation
    position    =POS
    task        =TASK
)

(p* prepare-calculation-binary
    "Prepares a calculation by finding the first argument in the 
imaginal buffer, then copying it"
  ?retrieval>
    state        free  
  =retrieval>
    isa          operation
    type         binary
    arg1        =ARG1
    arg2        =ARG2
    operator    =OP
  =imaginal>
    isa          scratchpad
   =ARG1        =INPUT1
   =ARG2        =INPUT2
==>
  =imaginal>
  +goal>
    isa          operation
    arg1        =ARG1
  +retrieval>
    isa          arithmetic-fact
    operation   =OP
    arg1        =INPUT1
    arg2        =INPUT2
)


(p* prepare-calculation-unary
    "Prepares a calculation by finding the first argument in the 
imaginal buffer, then copying it"
  ?retrieval>
    state        free
  =retrieval>
    isa          operation
    type         unary
    arg1        =ARG1
    arg2        =ARG2
    operator    =OP
  =imaginal>
    isa          scratchpad
   =ARG1        =INPUT
==>
  =imaginal>
  +goal>
    isa          operation
    arg1        =ARG1
  +retrieval>
    isa          arithmetic-fact
    operation   =OP
    arg1        =INPUT
    arg2        =ARG2
)


(p* update-scratchpad
    "After performing a calculation, updates the scratchpad with 
the result"
  =retrieval>
    isa          arithmetic-fact
    result      =RES
  =goal>
    isa          operation
    arg1        =ARG1
  =imaginal>
    isa          scratchpad
   =ARG1        =INPUT
==>
  =goal>
  =imaginal>
   =ARG1        =RES
)


(p next-position
   "Updates the mental position in the series of operations"
  ?retrieval>
    buffer       empty
    state        free
  ?imaginal>
    state        free
  =goal>
    isa          operation
    arg1        =NOT-NIL
  =imaginal>
    isa          scratchpad
  - position     3
    result       nil
==>
  =imaginal>
  +imaginal-action>
    isa          generic-action
    action       inst-update-position

  ;; Updates a new goal.  This will make compilation possible
  ;; for retrieve-operation and prepare-calculation-binary/unary.

  +goal>
    isa          operation
)


(p calculation-done
   "Detects when all the calculations are completed"
  ?retrieval>
    buffer       empty
    state        free
  ?imaginal>
    state        free
  =goal>
    isa          operation
    arg1        =NOT-NIL
  =imaginal>
    isa          scratchpad
    x           =RES
    position     3
==>
 -goal>
 =imaginal>
    result      =RES
 +visual-location>
    isa          visual-location
    kind         screen
)


;;; --------------------------------------------------------------
;;; When it's done, just press a button to proceed
;;; --------------------------------------------------------------

(p go-through-inputs
   "When all calculations are completed, just presses a key"
  ?visual>
    state        free
  ?imaginal>
    state        free
  =visual>
    isa          visual-object
    status       screen
    value        inputs
  =imaginal>
    isa          scratchpad
  - result       nil
  ?manual>
    preparation  free
    execution    free
==>
  -visual>
  -visual-location>
  =imaginal>
  +manual>
    isa          press-key
    key          "2"
)


;;; --------------------------------------------------------------
;;; PROBE PHASE
;;; --------------------------------------------------------------

(p attend-probe
   "Attends the number probe when the screen changes"
  ?visual>
    state        free
  ?manual>
    state        free
    preparation  free
  =visual>
    isa          visual-object
    status       screen
    value        probe
==>
  +visual-location>
    isa          visual-location
    kind         probe
)


(p answer-yes-to-probe
   "If the probe matches the result, answer '2'"
  =visual>
    isa          visual-object
    status       probe
    value       =VAL
  =imaginal>
    isa          scratchpad
    result      =VAL
  ?manual>
    preparation  free
    execution    free
==>
  =visual>
  +manual>
    isa          press-key
    key          "2"
  +visual-location>
    isa          visual-location
    kind         screen
)


(p answer-no-to-probe
   "If the probe does not match the result, answer '3'"
  =visual>
    isa          visual-object
    status       probe
    value       =VAL
  =imaginal>
    isa          scratchpad
  - result      =VAL
  ?manual>
    preparation  free
    execution    free
==>
  =visual>
  +manual>
    isa          press-key
    key          "3"
  +visual-location>
    isa          visual-location
    kind         screen
)

;;; -------------------------------------------------------------- ;;;
;;; FEEDBACK PHASE
;;; -------------------------------------------------------------- ;;;


(p attend-feedback
   "Attends the feedback when the screen changes"
  ?visual>
    state        free
  ?manual>
    state        free
    preparation  free
  =visual>
    isa          visual-object
    status       screen
    value        feedback
==>
  +visual-location>
    isa          visual-location
    kind         feedback
   :attended     nil
)

(p notice-success
   "Encodes a success when the feedback was positive"
  =visual>
    isa          visual-object
    status       feedback
    value        t
==>
  +visual-location>
    isa          visual-location
    kind         screen
)


(p notice-failure
   "Encodes a success when the feedback was positive"
  =visual>
    isa          visual-object
    status       feedback
    value        nil
==>
  +visual-location>
    isa          visual-location
    kind         screen
)


;;; -------------------------------------------------------------- ;;;
;;; INITIALIZE
;;; -------------------------------------------------------------- ;;;

(install-device (make-instance 'task-manager))
(setf (trials (current-device))
      (scramble (load-trials (translate-logical-pathname "INST:trials.txt"))))
(reset-manager (current-device))

(spp notice-success :reward 10)
(spp notice-failure :reward 0)

) ;; End of define-model
