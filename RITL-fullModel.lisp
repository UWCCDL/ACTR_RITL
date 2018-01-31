(clear-all)

#+(or :clisp :sbcl :openmcl) (setf (logical-pathname-translations "INST")
				   `(("**;*.*" ,(namestring (merge-pathnames "**/*.*" *load-truename*)))))

(define-model encoding

(sgp :trace-detail            high 
     :show-focus              t 
     :esc                     t 
     :lf                      0.9 
     :mas                     1.6 
     :imaginal-activation     1.0 

     ;; Productions
     :epl                     t 
     :ul                      t
     :ult			      t

     ;; Perceptual params
     :auto-attend             t 
     :visual-finst-span       10.0)

;;; Arithmetic facts
(load (translate-logical-pathname "INST:inst-arithmetic-facts.lisp"))

;;; RITL chunks
(chunk-type ritl-instructions kind task1 task2  task3)
(chunk-type phase step)

;;; Arithmetic chunks
(chunk-type operation task type argument1 operator argument2 position)
(chunk-type scratchpad x y result position state)

;;; Visual
(chunk-type (ritl-screen (:include visual-object))
	    kind nature)

(chunk-type (ritl-rule (:include visual-object))
	    task1 task2 task3)

(chunk-type (ritl-inputs (:include visual-object))
	    x y)

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
(add-dm (x isa chunk) (y isa chunk) (* isa chunk) (+ isa chunk)
	(/ isa chunk) (unary isa chunk) (binary isa chunk))
; (add-dm (inst isa ritl-rule task1 double task2 half task3 add)) Is this still necesarry?
; (add-dm (stimulus isa ritl-stimulus x 5 y 6))


;;; Operations
(add-dm (double isa operation task double argument1 x operator * argument2 2 type unary))
(add-dm (half isa operation task half argument1 x operator / argument2 2 type unary))
(add-dm (add isa operation task add argument1 x operator + argument2 y type binary))
(add-dm (third isa operation task third argument1 x operator / argument2 3 type unary))


;;; ENCODING

(p prepare-encoding 
   "Prepares to encode operations when the instructions are on"
  ?retrieval>
    state        free
    buffer       empty

   ?visual>
     state free
     buffer empty
   ?manual>
     preparation free
     processor free
     execution free
  ?goal>
    state free
    buffer empty
==>
  +visual-location>
     kind ritl-location
     :attended nil
  +goal>
    isa phase
    step encoding
)

(p encode-instructions
  ?retrieval>
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

  +retrieval>
    isa   ritl-rule
    task1 =first
    task2 =second
    task3 =third

    +goal>
    isa phase
    step encoding-done

)

;;; --------------------------------------------------------------
;;; Press a key when done
;;; --------------------------------------------------------------

(p go-through-instructions
   "Presses a key after instructions have been encoded"

  ?visual>
     state free

  ?retrieval>
  state        free

  =retrieval>
  isa ritl-rule

  ?manual>
    preparation  free
    processor free
    execution    free

  =goal>
    isa phase
    step encoding-done

==>

  -goal>

  =retrieval>

  +manual>
    isa          press-key
    key          "2"
)

;;; EXECUTION

(p prepare-execution
   ?goal>
    state free
    buffer empty
   
   ?visual>
    state free
   
   ?manual>
    preparation free
    processor free
    execution free

   =retrieval>
    isa   ritl-rule
    task1 =first
    task2 =second
    task3 =third   

   ==>
   
   +goal>
   isa phase
   step setup-calculation
   
   =retrieval>

   )

;;; Calculate x

(p calculate-x

   =goal>
   isa phase
   step setup-calculation

   ?imaginal>
   state free
   buffer empty

   =visual>
   kind ritl-inputs
   x =x

   =retrieval>
   isa    ritl-rule
   task1   =first

 ==>
 
   =retrieval>
 
   @goal> =first
 
   +imaginal>
   isa scratchpad
   position 1

   =visual>

   )

(p retrieve-arithmetic-fact-unary-x
   =imaginal>
   isa   scratchpad
   x     nil
   position 1

   ?retrieval>
   state free

   =visual>
   isa  ritl-inputs
   x  =x

   =goal>
   isa  operation
   operator =op
   argument2 =arg2
   type unary

==>
   
  =imaginal>

  =visual>
   
   +retrieval>
   isa arithmetic-fact
   operation =op
   arg1 =x
   arg2 =arg2
   
   +goal>
   isa phase
   step update-pad
   )

(p update-scratchpad-x
   =goal>
   isa phase
   step update-pad

   =retrieval>
   isa arithmetic-fact
   result =ans

   =imaginal>
   isa  scratchpad
   position 1
   
   ==>
      
  =imaginal>
    isa scratchpad
    x  =ans

    -goal>
    
   +retrieval>
   isa ritl-rule
   kind ritl-rule
  )

(p calculate-y

   =goal>
   isa phase
   step setup-calculation
   
   =imaginal>
   isa          scratchpad
   - x          nil
   y            nil
   position    1

   =visual>
   isa  ritl-inputs
   y       =y

   =retrieval>
   isa    ritl-rule
   task2    =second

   ==>
   =visual>
   @goal> =second
   =retrieval>
   =imaginal>
   position 2
   )

(p retrieve-arithmetic-fact-unary-y
   =imaginal>
   isa   scratchpad
   y     nil
   position 2

   ?retrieval>
   state free

   =visual>
   isa  ritl-inputs
   y  =y

   =goal>
   isa  operation
   operator =op
   argument2 =arg2
   type unary

   ==>
   
   =imaginal>
   
   =visual>
   
   +retrieval>
   isa arithmetic-fact
   operation =op
   arg1 =y
   arg2 =arg2
   
   +goal>
   isa phase
   step update-pad
   )

(p update-scratchpad-y
   =imaginal>
   isa  scratchpad
   y    nil
   position 2

   =visual>
   isa  ritl-inputs
   y  =y

   =retrieval>
   isa arithmetic-fact
   result =ans
   =goal>
   isa phase
   step update-pad
   
   ==>
   
   =visual>
   
   =imaginal>
   isa scratchpad
   y =ans

   +retrieval>
   isa ritl-rule
   kind ritl-rule
   
   -goal>
   )


(p calculate-binary

   =imaginal>
   isa          scratchpad
   - x            nil
   - y            nil
   position     2

   =retrieval>
   isa    ritl-rule
   task3   =third

   ==>
   
   =retrieval>
   
   @goal> =third
   
   =imaginal>
   position 3
   )



(p retrieve-arithmetic-fact-binary
   =imaginal>
   isa scratchpad
   x  =x
   y  =y
   position  3

   =retrieval>
   isa    ritl-rule
   task3   =third

   =goal>
   isa  operation
   operator =op
   type binary

   ==>
   
   =imaginal>
   
   +retrieval>
   isa arithmetic-fact
   operation =op
   arg1 =x
   arg2 =y
   
   +goal>
   isa phase
   step update-pad

   )


(p update-scratchpad-binary
   =goal>
   isa phase
   step update-pad

   =retrieval>
   isa arithmetic-fact
   result =ans

   =imaginal>
   isa scratchpad
   - x   nil
   - y   nil
   result  nil

   ==>
   
   =imaginal>
   isa scratchpad
   result  =ans
   position    3

   -goal>
   -retrieval>
   )

;;; --------------------------------------------------------------
;;; When it's done, just press a button to proceed
;;; --------------------------------------------------------------

(p go-through-inputs
   "When all calculations are completed, just presses a key"

   ?retrieval>
   buffer       empty
   state        free
   
   ?imaginal>
   state        free
   
   =imaginal>
   isa          scratchpad
   result       =RES
   position     3
   

   ?visual>
   state free

   ?manual>
   preparation  free
   processor free
   execution    free

   ==>
   
   =imaginal>
   
   +manual>
   isa          press-key
   key          "2"
   )

;;; RESPONSE

(p prepare-response
  ?visual>
    state free

  ?retrieval>
    state        free
    buffer       empty

  ?goal>
    state free
    buffer empty

    ?imaginal>
    state busy
    buffer full

  =imaginal>
    result =ans
  
 ==>
   
  +visual-location>
    kind ritl-location
    :attended nil
   
  +goal>
    isa phase
    step respond
    
  =imaginal>
)

(p answer-yes
  =visual>
    isa          ritl-probe
    probe       =VAL
    
  =imaginal>
    isa          scratchpad
    result      =VAL
    
   ?manual>
    preparation  free
    execution    free
==>
  +manual>
    isa          press-key
    key          "2"
 )

(p answer-no
  =visual>
    isa         ritl-probe
    probe       =VAL
    
  =imaginal>
    isa          scratchpad
    - result      =VAL
    
  ?manual>
    preparation  free
    execution    free
    
 ==>

  +manual>
    isa          press-key
    key          "3"
)

)

