(clear-all)

(define-model response-monkey

(sgp :trace-detail            high 
     :auto-attend             t)

;;; RITL chunks

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

;;; Just encodes visual objects and reponds

(p encode-anything
   ?visual>
     state free
     buffer empty
   ?manual>
     preparation free
     processor free
     execution free
==>
+visual-location>
     kind ritl-location
     :attended nil
)

(p respond-to-anything
   =visual>
   - kind ritl-screen
   ?visual>
     state free
   ?manual>
     preparation free
     processor free
     execution free
==>
   +manual>
     isa punch
     finger index
     hand left
)

)  
