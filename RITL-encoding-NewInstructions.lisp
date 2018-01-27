(clear-all)

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

;;; RITL chunks
(chunk-type ritl-instructions task1 task2 task3)
(chunk-type ritl-stimulus x y)
(chunk-type do-encoding step)

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
; (add-dm (inst isa ritl-rule task1 double task2 half task3 add))

;;; Operations
(add-dm (double-1 isa operation task double argument1 x operator * argument2 2 type unary))
(add-dm (half-1 isa operation task half argument1 x operator / argument2 2 type unary))
(add-dm (add-1 isa operation task add argument1 x operator + argument2 y type binary position 3))

;;; Instruction

(p prepare-for-encoding 
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
    isa do-encoding
    step encoding
)

(p encode-instructions-1
  ?retrieval>
    state        free
    buffer       empty
   
  =goal>
    isa do-encoding
    step encoding

  =visual>
    kind ritl-rule
    task1 =first
    task2 =second
    task3 =third

==>

  +retrieval>
    isa   ritl-instructions
    task1 =first
    task2 =second
    task3 =third

  -goal>

)

;;; --------------------------------------------------------------
;;; Press a key when done
;;; --------------------------------------------------------------

(p go-through-instructions
   "Presses a key after instructions have been encoded"

   =visual>
   - kind ritl-screen
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

  ?goal>
    state free
    buffer empty
  
==>
  -visual>
  =retrieval>
  +manual>
    isa          press-key
    key          "2"
)

)
