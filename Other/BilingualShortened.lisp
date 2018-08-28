(clear-all)

#+(or :clisp :sbcl :openmcl) (setf (logical-pathname-translations "INST")
				   `(("**;*.*" ,(namestring (merge-pathnames "**/*.*" *load-truename*)))))

(define-model bilingual-devel

(sgp :trace-detail            low 
     :show-focus              t 
     :esc                     t 
     :le                      0.94
     :nu                      0.1
     :er                      t
     :imaginal-delay          0.17
     :ans                     0.01
     ;;:imaginal-activation     2
     ;;:mas                     1
     :time-noise              0.02
     ;; Productions
     :epl                     t
     :ul                      t
     :ult		      t
     :tt                      2
     :pct                     t
     :alpha                   0.2
     :bll                     0.5
     :lf                      1
     ;; Perceptual params
     :auto-attend             t 
     :visual-finst-span       10.0
     ;;:rt 0.4
     )

;;; Arithmetic facts
(load (translate-logical-pathname "INST:inst-arithmetic-facts.lisp"))

(sdp-fct `(,(no-output (sdm isa arithmetic-fact))
	    :creation-time -10000000 :references 2800))

;;; RITL chunks
(chunk-type ritl-task kind task1 task2 task3)
(chunk-type ritl-result kind x y result task task1 task2 task3)
(chunk-type phase step task)

;;; Arithmetic chunks
(chunk-type operation task type argument1 operator argument2 position)
;(sgp :buffer-trace t :buffer-trace-step .025 :traced-buffers (production retrieval goal visual-location imaginal))

;;; Visual
(chunk-type (ritl-screen (:include visual-object))
	    kind nature)
(chunk-type (ritl-rule (:include visual-object))
	    task1 task2 task3 kind)
(chunk-type (ritl-inputs (:include visual-object))
	    x y kind)
(chunk-type (ritl-probe (:include visual-object))
	    kind probe)

(add-dm (ritl-screen isa chunk)
	(rule isa chunk)
	(ritl-location isa chunk)
	(pause1 isa chunk)
	(pause2 isa chunk)
	(pause3 isa chunk)
	(done isa chunk)
	(probe isa chunk))


;;; Declarative memory
(add-dm (x isa chunk) (y isa chunk)
	;(* isa chunk) (+ isa chunk)
	;(/ isa chunk)
	(unary isa chunk) (binary isa chunk))

;;; Operations
(add-dm (double isa operation task double argument1 x operator * argument2 2 type unary)
    (half isa operation task half argument1 x operator / argument2 2 type unary)
    (third isa operation task third argument1 x operator / argument2 3 type unary)
    (triple isa operation task triple argument1 x operator * argument2 3 type unary)
    (increment isa operation task increment argument1 x operator + argument2 1 type unary)
    (decrement isa operation task decrement argument1 x operator - argument2 1 type unary))

(add-dm (add isa operation task add argument1 x operator + argument2 y type binary)
    (subtract isa operation task subtract argument1 x operator - argument2 y type binary)
    (times isa operation task times argument1 x operator * argument2 y type binary)
    (divide isa operation task divide argument1 x operator / argument2 y type binary))

;; Set all operations to high levels of activation
(sdp-fct `(,(no-output (sdm type unary)) :creation-time -10000000 :references 2500))
(sdp-fct `(,(no-output (sdm type binary)) :creation-time -10000000 :references 2500))

;;; ENCODING


(p look-at-screen
 ?imaginal>
    state        free
    buffer       empty

    ?retrieval>
    state free
    buffer empty

   ?visual>
   state free
   buffer empty

     ?manual>
     preparation free
     processor free
     execution free

    ?temporal>
    state free
    buffer empty
 ==>
 +visual-location>
    kind ritl-location)
    
;;; ENCODING

(p initiate-encoding 
   "Prepares to encode operations when the instructions are on"
  ?imaginal>
    state        free
    buffer       empty

    ?retrieval>
    state free
    buffer empty

   ?visual-location>
   state free

   ?visual>
   state free

     ?manual>
     preparation free
     processor free
     execution free
  ?goal>
    state free
    buffer empty

    ?temporal>
    state free
    buffer empty
  =visual>
    value rule  
==>
  +visual-location>
     kind ritl-location
     :attended nil
  +goal>
    isa phase
    step encoding
    
    )


(p encode-instructions
   "encode instructions and start waiting"
  ?imaginal>
    state        free
    buffer       empty
   
  =goal>
    isa phase
    step encoding

  =visual>
    isa ritl-rule
    task1 =first
    task2 =second
    task3 =third

    ==>
    
  +imaginal>
    isa   ritl-task
    kind  ritl-task
    task1 =first
    task2 =second
    task3 =third

   =goal>
   isa phase
   step encoding-complete-1

   =visual>
   )

(p clear-imaginal-1
   ?imaginal>
   state free
   buffer full

   =goal>
   isa phase
   step encoding-complete-1


   ==>
   =goal>
   step imaginal-cleared

   -imaginal>

  )

(p retrieve-instructions-1
   ?imaginal>
   state        free
   buffer       empty
   
   =goal>
   isa phase
   step IMAGINAL-CLEARED

   =visual>
   isa ritl-rule
   task1 =first
   task2 =second
   task3 =third

   ?temporal>
     state free
   
   ==>

   +retrieval>
   isa   ritl-task
   kind ritl-task
   task1 =first
   task2 =second
   task3 =third

   =visual>
   =goal>
     step remembered-1
   +temporal>
     isa time
     ticks 0
   )

(p instructions-fast
   ?imaginal>
   state        free
   buffer       empty

   =visual>
   isa ritl-rule
   task1 =first
   task2 =second
   task3 =third

   =retrieval>
   isa   ritl-task
   kind ritl-task
   task1 =first
   task2 =second
   task3 =third

   =goal>
   isa phase
   step remembered-1

   =temporal>
     isa time
   <= ticks 6
   
   ==>

   =retrieval>
   =visual>
   =goal>
     step moveon
   -temporal>
   )

(p instructions-slow
   ?imaginal>
   state        free
   buffer       empty

   =visual>
   isa ritl-rule
   task1 =first
   task2 =second
   task3 =third

   =retrieval>
   isa   ritl-task
   kind ritl-task
   task1 =first
   task2 =second
   task3 =third
   
   =goal>
   isa phase
   step remembered-1

   =temporal>
     isa time
   >= ticks 6
   ==>

   =visual>
   =goal>
     step encoding
   -temporal>
   )


;;; --------------------------------------------------------------
;;; Press a key when done
;;; --------------------------------------------------------------

(p go-through-instructions
   "Presses a key after instructions have been encoded"

   =retrieval>
   isa   ritl-task
   kind ritl-task
   task1 =first
   task2 =second
   task3 =third

   ?visual>
   state free

   ?imaginal>
   state free
   buffer empty

  ?manual>
    preparation  free
    processor free
    execution    free

  =goal>
    isa phase
    step moveon

    ==>

   +retrieval>
    isa operation
    task =first
    type unary

   +goal>
    isa phase
    step execution-x
    task =first

   +imaginal>
     isa   ritl-result
     task1 =first
     task2 =second
     task3 =third

  +manual>
    isa          press-key
    key          "2"
)

;;; EXECUTION

;;; Calculate x

(p retrieve-arithmetic-fact-unary-x
   =goal>
   isa phase
   step execution-x
   
   =imaginal>
   isa   ritl-task
   task1 =first

   =visual>
   isa  ritl-inputs
   x  =x

   =retrieval>
   isa  operation
   operator =op
   argument2 =arg2
   type unary

==>
  
 =visual>

  =imaginal>
   
   +retrieval>
   isa arithmetic-fact
   operation =op
   arg1 =x
   arg2 =arg2
   
   +goal>
   isa phase
   step update-pad-x
   )

(p update-scratchpad-x-start-y
   =goal>
   isa phase
   step update-pad-x

   =retrieval>
   isa arithmetic-fact
   result =ans

   =imaginal>
   isa  ritl-task
   task2 =second

   ?retrieval>
   state free
   
   ==>
      
  *imaginal>
    isa ritl-task
    x  =ans

    +goal>
    isa phase
    step execution-y

   +retrieval>
   isa  operation
   task =second
   type unary
   

  )

(p retrieve-arithmetic-fact-unary-y
   =imaginal>
   isa   ritl-task
   task2 =second
   ;;- x          nil
   y            nil

   =retrieval>
   isa  operation
   operator =op
   argument2 =arg2
   task =second
   type unary

   =visual>
   isa  ritl-inputs
   y  =y

   =goal>
   isa  phase
   step execution-y

   ==>
   
   =imaginal>
   
   +retrieval>
   isa arithmetic-fact
   operation =op
   arg1 =y
   arg2 =arg2
   
   +goal>
   isa phase
   step update-scratchpad-y
   )

(p update-scratchpad-y-start-binary
   ?retrieval>
   state free

   =imaginal>
   isa ritl-task
   task3 =third
   ;;- x   nil
   y nil
   
   =retrieval>
   isa arithmetic-fact
   result =ans
   
   =goal>
   isa phase
   step update-scratchpad-y
   
   ==>
   
   *imaginal>
   isa ritl-task
   y =ans

   +goal>
   isa phase
   step execution-binary

   +retrieval>
   isa operation
   task =third
   type binary

   )


(p retrieve-arithmetic-fact-binary
   
   =imaginal>
   isa ritl-task
   x =x
   y =y
   task3 =third

   =retrieval>
   isa  operation
   operator =op
   type binary
   task =third
   
   ==>
   
   =imaginal>
   
   +retrieval>
   isa arithmetic-fact
   operation =op
   arg1 =x
   arg2 =y
   
   +goal>
   isa phase
   step update-scratchpad-binary

   )


(p update-scratchpad-binary
   =goal>
   isa phase
   step update-scratchpad-binary

   =retrieval>
   isa arithmetic-fact
   result =ans

   =imaginal>
   isa ritl-task
   ;;- x   nil
   ;;- y   nil

   ==>
   
   *imaginal>
   isa ritl-task
   result  =ans

   +goal>
   isa phase
   step done
   
   )

;;; --------------------------------------------------------------
;;; When it's done, just press a button to proceed
;;; --------------------------------------------------------------
(p go-through-results
   "Presses a key after instructions have been encoded"

   ?visual>
   state free

;;   ?imaginal>
;;   buffer full
;;   state free

   =imaginal>
   isa ritl-task
   result =ans

   ?manual>
   preparation  free
   processor free
   execution    free

   =goal>
   isa phase
   step done

   ==>

   =goal>
   isa phase
   step answer

   =imaginal>

   +manual>
   isa          press-key
   key          "2"
   )



;;; RESPONSE


(p answer-yes
  =visual>
    isa          ritl-probe
    probe       =VAL
    
  =imaginal>
  isa         ritl-task
    result      =VAL
    
   ?manual>
    preparation  free
    execution    free

    =goal>
    isa phase
    step answer

==>
  +manual>
    isa          press-key
    key          "2"

    -goal>
    -imaginal>
 )

(p answer-no
  =visual>
    isa         ritl-probe
    probe       =VAL
    
  =imaginal>
  isa         ritl-task
  - result      =VAL
    
  ?manual>
    preparation  free
    execution    free

    =goal>
    isa phase
    step answer

    
 ==>

  +manual>
    isa          press-key
    key          "3"

    -goal>
    -imaginal>
)

)

