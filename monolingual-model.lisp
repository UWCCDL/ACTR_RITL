(clear-all)

#+(or :clisp :sbcl :openmcl) (setf (logical-pathname-translations "INST")
				   `(("**;*.*" ,(namestring (merge-pathnames "**/*.*" *load-truename*)))))

(define-model monolingual-devel

(sgp :trace-detail            low 
     :show-focus              t 
     :esc                     t 
     :le                      0.9
     :nu                      8  ;; modified from zero
     :er                      t
     :imaginal-delay          0.15
     :ans                     0.03
     ;:imaginal-activation     10
     ;:mas                     2
     :time-noise              0.02
     ;; Productions
     :epl                     t
     :ul                      t
     :ult		      t
     :tt                      2
     :pct                     t
     :alpha                   0.5
     :bll                     0.5
     :lf                      1
     ;; Perceptual params
     :auto-attend             t 
     :visual-finst-span       10.0
     :save-buffer-trace t
     :traced-buffers (imaginal visual retrieval manual production goal)
     )

;;; ------------------------------------------------------------------
;;; TASK KNOWLEDGE
;;; ------------------------------------------------------------------

;;; Arithmetic facts
(load (translate-logical-pathname "INST:inst-arithmetic-facts.lisp"))

(sdp-fct `(,(no-output (sdm isa arithmetic-fact))
			:creation-time -10000000 :references 2800))

;;; RITL chunks
(chunk-type ritl-task
			kind task1 task2 task3)
(chunk-type ritl-result
			kind x y result task task1 task2 task3)
(chunk-type phase
			step task)

;;; Arithmetic chunks
(chunk-type operation
			task type argument1 operator argument2 position)

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
(add-dm (double isa operation
				task double
				argument1 x
				operator *
				argument2 2
				type unary)
		(half isa operation
			  task half
			  argument1 x
			  operator /
			  argument2 2
			  type unary)
		(third isa operation
			   task third
			   argument1 x
			   operator /
			   argument2 3
			   type unary)
		(triple isa operation
				task triple
				argument1 x
				operator *
				argument2 3
				type unary)
		(increment isa operation
				   task increment
				   argument1 x
				   operator +
				   argument2 1
				   type unary)
		(decrement isa operation
				   task decrement
				   argument1 x
				   operator -
				   argument2 1
				   type unary))

(add-dm (add isa operation
			 task add
			 argument1 x
			 operator +
			 argument2 y
			 type binary)
		(subtract isa operation
				  task subtract
				  argument1 x
				  operator -
				  argument2 y
				  type binary)
		(times isa operation
			   task times
			   argument1 x
			   operator *
			   argument2 y
			   type binary)
		(divide isa operation
				task divide
				argument1 x
				operator /
				argument2 y
				type binary))

;;; Avoids warnings

(add-dm (encoding isa chunk)
		(encoding-complete-1 isa chunk)
		(imaginal-cleared isa chunk)
		(remembered-1 isa chunk)
		(ritl-task isa chunk)
		(moveon isa chunk)
		(execution-x isa chunk)
		(execution-y isa chunk)
		(update-scratchpad-y isa chunk)
		(execution-binary isa chunk)
		(update-scratchpad-binary isa chunk)
		(answer isa chunk))

;; Set all operations to high levels of activation
(sdp-fct `(,(no-output (sdm type unary))
			:creation-time -10000000 :references 2500))
(sdp-fct `(,(no-output (sdm type binary))
			:creation-time -10000000 :references 2500))

;;; ------------------------------------------------------------------
;;; TASK CONTROL
;;; ------------------------------------------------------------------

(p look-at-screen
   "Basic production that focuses attention on the screen (if it's ever lost)"
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
     kind ritl-location
)
    
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
     step encoding  
)


(p encode-instructions
   "encode instructions and start waiting"
   ?imaginal>
     state        free
     buffer       empty
   
   =goal>
     step encoding

   =visual>
     task1 =first
	 task2 =second
	 task3 =third
==>
   +imaginal>
     kind  ritl-task
	 task1 =first
	 task2 =second
	 task3 =third

   =goal>
     step encoding-complete

   =visual>
)

(p clear-imaginal
   ?imaginal>
     state free
     buffer full

   =goal>
     step encoding-complete
==>
   =goal>
     step imaginal-cleared

   -imaginal>
)

(p retrieve-instructions
   ?imaginal>
     state        free
     buffer       empty
   
   =goal>
     step IMAGINAL-CLEARED

   =visual>
     task1 =first
	 task2 =second
	 task3 =third

   ?temporal>
     state free
==>
   +retrieval>
     kind ritl-task
	 task1 =first
	 task2 =second
	 task3 =third

   =visual>

   =goal>
     step remembered
   +temporal>
     ticks 0
)


(p instructions-fast
   ?imaginal>
     state        free
	 buffer       empty

   =visual>
     task1 =first
	 task2 =second
	 task3 =third

   =retrieval>
     kind ritl-task
	 task1 =first
	 task2 =second
	 task3 =third

   =goal>
     step remembered

   =temporal>
     <= ticks 7   
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
     task1 =first
	 task2 =second
	 task3 =third

   =retrieval>
     kind ritl-task
	 task1 =first
	 task2 =second
	 task3 =third
   
   =goal>
     step remembered

   =temporal>
     >= ticks 7
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
     kind  ritl-task
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
    step moveon

    ==>

  +retrieval>
    task =first
    type unary

  +goal>
    step execution-x
    task =first

  +imaginal>
    task1 =first
	task2 =second
	task3 =third
	
  +manual>
    cmd         press-key
    key          "2"
)

;;; EXECUTION

;;; Calculate-x


(p retrieve-arithmetic-fact-unary-x
   =goal>
     step execution-x
   
   =imaginal>
     task1 =first

   =visual>
     x  =x

   =retrieval>
     operator =op
     argument2 =arg2
     type unary
==>
   =visual>

   =imaginal>
   
   +retrieval>
     operation =op
	 arg1 =x
	 arg2 =arg2
   
   =goal>
)


(p update-scratchpad-x
   =goal>
     step execution-x
   
   =imaginal>

   =retrieval>
     result =ans
 ==>	 
   *imaginal>
     x =ans

   =goal>
     step retrieve-task-y
)


(p retrieve-task-y
   =imaginal>
     task1 =first
	 task2 =second
	 task3 =third

   ?imaginal>
     state free

   ?retrieval>
     state free

   =goal>
     step retrieve-task-y
 ==>
   =imaginal>

   +retrieval>
     kind ritl-task
	 task1 =first
	 task2 =second
	 task3 =third

   =goal>
     step execution-y
)


(p calculate-y
   =goal>
     step execution-y
   
   =imaginal>
     y nil

   =visual>
     y  =y

   =retrieval>
     task2 =second
==>
   =goal>
   =visual>

   *imaginal>
     task =second

   +retrieval>
     task =second
     type unary
)


(p retrieve-arithmetic-fact-unary-y
   =imaginal>
     task2 =second
	 y nil
   
   =goal>
     step execution-y

   =retrieval>
     operator =op
	 argument2 =arg2
	 task =second
	 type unary

   =visual>
     y =y
==>
   +retrieval>
     operation =op
	 arg1 =y
	 arg2 =arg2

   =imaginal>

   +goal>
     step update-scratchpad-y
)


(p update-scratchpad-y
   =goal>
     step update-scratchpad-y

   =retrieval>
     result =ans

   =imaginal>
     y nil
	 task2 =second
	 task1 =first
	 task3 =third
==>
   *imaginal>
     y =ans

   =goal>
     step retrieve-task-binary
)


(p retrieve-task-binary
   =imaginal>
     task1 =first
     task2 =second
     task3 =third

   ?imaginal>
     state free
   
   =goal>
     step retrieve-task-binary

   ?retrieval>
     state free    
     
==>
   =imaginal>

   +retrieval>
     kind ritl-task
     task1 =first
     task2 =second
     task3 =third

   =goal>
     step execution-binary
   )


(p calculate-binary
   =goal>
     step execution-binary

   =imaginal>
     result nil
   
   =retrieval>
     kind ritl-task
	 task3 =third

==>
   *imaginal>
     task =third

   =goal>

   +retrieval>
     task =third
     type binary
   )

(p retrieve-arithmetic-fact-binary
   =imaginal>
     x =x
	 y =y
	 result nil
   
   =goal>
     step execution-binary

   =retrieval>
     operator =op
	 argument2 =arg2
	 task =third
	 type binary

==>
   +retrieval>
     operation =op
	 arg1 =x
	 arg2 =y

   =imaginal>

   +goal>
     step update-scratchpad-binary
)

(p update-scratchpad-binary
   =goal>
     step update-scratchpad-binary

   =retrieval>
     result =ans

   ?imaginal>
     state free

   =imaginal>
     result nil
==>
   *imaginal>
     result  =ans

   +goal>
     step done
)

;;; --------------------------------------------------------------
;;; When it's done, just press a button to proceed
;;; --------------------------------------------------------------
(p go-through-results
   "Presses a key after the response has been calculated"

   ?visual>
     state free

   ?imaginal>
     buffer full
	 state free

   =imaginal>
     result =ans

   ?manual>
     preparation  free
     processor free
	 execution    free

   =goal>
     step done
==>
   =goal>
     step answer

   =imaginal>

   +manual>
     cmd          press-key
	 key          "2"
)


;;; RESPONSE


(p answer-yes
   =visual>
     probe       =VAL
   
   =imaginal>
     result      =VAL
   
   ?manual>
     preparation  free
     execution    free

   =goal>
     step answer
==>
   +manual>
     cmd          press-key
	 key          "2"

   -goal>
   -imaginal>
)


(p answer-no
   =visual>
     probe       =VAL
   
   =imaginal>
     - result      =VAL
   
   ?manual>
     preparation  free
	 execution    free

   =goal>
     step answer
==>
   +manual>
     cmd          press-key
     key          "3"

   -goal>
   -imaginal>
)

) ;; End of define-model
