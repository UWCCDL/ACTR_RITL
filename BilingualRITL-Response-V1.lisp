(clear-all)

(define-model response

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

;;; RITL-chunks
(chunk-type ritl-probe value)
(chunk-type scratchpad x y result position state task)

(define-chunks ritl-instructions first second third)

;;; Declarative memory
(add-dm
  (probe isa ritl-probe value 10)
  (scratchpad isa scratchpad result 10)
)

;;; Normally, we remember the contents of the imaginal buffer and detect the probe at the screen but now...
(set-buffer-chunk 'imaginal 'scratchpad)
(set-buffer-chunk 'visual 'probe)


(p answer-yes
  =visual>
    isa          ritl-probe
    value       =VAL
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

)
)
