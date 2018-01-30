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
(add-dm (add isa operation task add argument1 x operator + argument2 y type binary position 3))

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
    kind ritl-rule
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
   
   +visual-location>
   kind ritl-location
   :attended nil
   
   +goal>
   isa phase
   step execute-x

   =retrieval>
   )

;;; Calculate x

(p calculate-x

   =goal>
   isa phase
   step execute-x

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
   
   =visual>
 
   =retrieval>
 
   @goal> =first
 
   +imaginal>
   isa scratchpad
   position 1

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
   
  -retrieval>
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
    
  =visual>

  +manual>
    isa          press-key
    key          "3"
)

)

