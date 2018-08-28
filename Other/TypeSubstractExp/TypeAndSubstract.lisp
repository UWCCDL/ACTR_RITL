;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                               ;;;
;;;  Subtraction & Typing Model                   ;;;
;;;                                               ;;;
;;;  Jelmer Borst (jpborst@ai.rug.nl)             ;;;
;;;  Department of Artificial Intelligence        ;;;
;;;  University of Groningen                      ;;;
;;;  The Netherlands                              ;;;
;;;                                               ;;;
;;;  20090116                                     ;;;
;;;                                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;;  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *model-pathname* ;pathname to outputfile, default "pp-test"
  (merge-pathnames 
   (make-pathname :name "model" :directory '(:relative "modeloutput") :type "dat")  
   (current-pathname)))

;;; Code to run the experiment with the model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun m ()
  (Reset-experiment)
  (reset)
  (threads-reset)
  (setf *output-pathname* *model-pathname*)
  (setf *pp* "model1")
  (setf *model* t)
  (setf *realtime* nil)
  (setf *exp-part* "real")
  (setf *points* 200)
  (setf *trial* 0)
;  (setf *current-levels* *difficulty-levels*)
     ;set correct params
  (setf *current-levels*
        '(
          ;(easy none)
          ;(hard none)
          ;(none easy)
          ;(none hard)
          (easy easy)
          (hard easy)
          (easy hard)
          (hard hard)
;(hard hard)
;(hard hard)
          (hard hard)))
  (e "real"))

(defun mX (n &optional (pp 1))
  
  (setf *output-pathname* *model-pathname*)
  (setf *model* t)
  (setf *realtime* nil)
  (setf *exp-part* "real")
  
  (dotimes (i n)
    (setf *points* 200)
    (setf *trial* 0)
    (reset-experiment)
    (reset)
    (threads-reset)
    (setf *current-levels* *difficulty-levels*)
    (setf *pp* (format nil "model~a" (+ i pp)))
    (e "real")
    (gc-if-needed)
    (sleep 30)))

(defun mDiffParam (n x)
  (dotimes (i x)
    (eval (read-from-string (format nil "(sgp :lf ~a)" (+ .5 (/ i 10)))))
    (mX n (+ (* i n) 1))))

(defun reset-experiment ()
  (setf *typing-stimuli-hard*
        (randomize '("everything"
                     "understand"
                     "absolutely"
                     "interested" 
                     "appreciate" 
                     "completely"
                     "difference" 
                     "girlfriend"
                     "definitely" 
                     "especially" 
                     "government"
                     "sweetheart"
                     "department"
                     "experience"
                     "impossible"
                     "ridiculous" 
                     "everywhere"
                     "apparently" 
                     "restaurant"
                     "themselves"
                     "downstairs"
                     "incredible"
                     "personally"
                     "television"
                     )))
  (setf *typing-stimuli-easy*
        (randomize '("eiynehgvtr"
                     "nstaudrden"
                     "buaetlyslo"
                     "etstirened" 
                     "cpaitperae"
                     "oetlpylecm"
                     "enfdiecfer"
                     "frgrniedli"
                     "tenediylif"
                     "sceleailpy"
                     "vgotmnenre"
                     "tareweshte"
                     "emprtednta"
                     "iencxerepe" 
                     "pslosiembi" 
                     "riudclsoui"
                     "vereweyehr"
                     "parnlytape"
                     "anutrarest"
                     "lvesehtems"
                     "andswtrois"
                     "dceleinbri"
                     "rsypnlaleo"
                     "elstevioin")))

  (setf *subtraction-stimuli-easy*
    (get-easy-stims 24))

  (setf *subtraction-stimuli-hard*
        (get-hard-stims 24))

  (setf *difficulty-levels* '())

  (do ()
      ((= (length *difficulty-levels*) 36))
    (setf *difficulty-levels* nil)
    (let* ((lst '(1 2 3 4))
           (dif-list (randomize lst))
           (blocks 3) ;nr of blocks
           (out-lst nil))
      (dotimes (i (- blocks 1))
        (do ((new-lst (randomize lst) (randomize lst) )) 
            ((equalp (first new-lst) (nth (- (length dif-list) 1) dif-list)))
          (setf out-lst new-lst))
        (setf dif-list (append dif-list out-lst)))
      (dolist (i dif-list)
        (case i
          (1 ;;change lists to determine length of condition
             (setf *difficulty-levels* (append *difficulty-levels* '((easy easy) (easy easy) (easy easy))))) ; (easy easy)))))
          (2
           (setf *difficulty-levels* (append *difficulty-levels* '((easy hard) (easy hard) (easy hard))))) ; (easy hard)))))
          (3
           (setf *difficulty-levels* (append *difficulty-levels* '((hard easy) (hard easy) (hard easy))))) ; (hard easy)))))
          (4
           (setf *difficulty-levels* (append *difficulty-levels* '((hard hard) (hard hard) (hard hard)))))))))) ; (hard hard)))))))))


;;; Modeling hacks
;;;;;;;;;;;;;;;;;;;;;;;

(defun find-location-x (s)
  "Hack to find a button with string s"   ;now really only BUTTONS!!
  (let ((answer nil)
        (l (visicon-chunks (get-module :vision))))
    (while (and (null answer) l)
        (if (and
             (equal (chunk-slot-value-fct (first l) 'kind) 'BUTTON) 
             (equal (string-upcase s)(string-upcase (chunk-real-visual-value (first l)))))
            (setf answer (first l))
          (setf l (Rest l))))
      (if answer (chunk-slot-value-fct answer 'screen-x)
        (progn (format t "~%*** Warning: find-location-x called with ~S which is not on the screen ***~%" s)
          nil))))
  
(defun find-location-y (s)
  "Hack to find a button with string s"   ;now really only BUTTONS!!
  (let ((answer nil)
        (l (visicon-chunks (get-module :vision))))
    (while (and (null answer) l)
        (if (and
             (equal (chunk-slot-value-fct (first l) 'kind) 'BUTTON) 
             (equal (string-upcase s)(string-upcase (chunk-real-visual-value (first l)))))
            (setf answer (first l))
          (setf l (Rest l))))
      (if answer (chunk-slot-value-fct answer 'screen-y)
        (progn (format t "~%*** Warning: find-location-y called with ~S which is not on the screen ***~%" s)
          nil))))

(defun text-not-attended ()
  (awhen (get-module :vision)  ;; Test that there is a vision module
         (update-new it)
         (check-finsts it)) 
  (let ((l (visicon-chunks (get-module :vision))))
    (dolist (element l)
      (when (and 
             (equalp (chunk-slot-value-fct element 'kind) 'TEXT)
             (not (equalp (feat-attended element (get-module :vision)) t)))
        (return t))))) 

(defun look-right ()
  (if (> (svref (eye-loc (get-module :vision)) 0) 512) t nil))

;;; Disable buffer stuffing
(defmethod stuff-visloc-buffer ((vis-mod vision-module)) nil)

   

(defun calc-similarities (a b)
  (let ((result nil))
    (when (and (numberp a) (numberp b)) ;order facts & problem states
      (let ((a (if (not (numberp a)) (parse-integer a :junk-allowed t) a))
            (b (if (not (numberp b)) (parse-integer b :junk-allowed t) b)))
        (when (and a b)
        ;  (setf result (* .8 (- (sqr (- a b))))))
          (setf result(* (max .8 (- 1 (/ a 10))) (- (sqr (- a b))))))))
    
    (when (not (and (numberp a) (numberp b))) ;math facts
      (let ((a (if (not (numberp a)) (parse-integer a :junk-allowed t) a))
            (b (if (not (numberp b)) (parse-integer b :junk-allowed t) b)))
        (when (and a b)
          (setf result (* (max .5 (- 1 (/ a 20))) (- (sqr (- a b))))))
        (if (or (and (> a 10) (< b 11)) (and (< a 10) (> b 11))) (setf result -100))))
    result))
  




;;; Model
;;;;;;;;;;;;;;;;;

(clear-all)

(define-model subtractionTyping

(threads-reset)

(sgp :esc t
     :er t
     :egs 0 ;.5
     :bll .5
     :rt -4.0
     :lf .3 
     :ans .1 ;.1 ;.1 ;.1 ;nil ;.1 ; EDIT!
     :ol t ;3 ; t;3 ;nil ;3
     :crt nil
     :cst nil
     :mp 1 ;1 ;partial matching
     :ms 0 ;maximum similarity
     :md -1  ;maximum difference
     :sim-hook calc-similarities


     :needs-mouse nil
     :show-focus nil
    
     :optimize-visual t  ; was nil NT nil
     :randomize-time nil
     :visual-movement-tolerance 30 ;.5 ;20 ;20 ;1 ;.5 ;20
     :process-cursor nil
     :trace-detail low ; medium ;medium
     :v nil ;Macintosh HD:Users:Jelmer:Desktop:trace.txt
     :ga 1
     :act nil
     :declarative-finst-span 3
     :mas nil
     :BUFFER-TRACE nil
     :SAVE-BUFFER-TRACE nil
     :NCNAR nil


     :save-dm-history nil ;save dm history for history tool
     )

(set-hand-location left 21 5)
(set-hand-location right 28 2)

;;; General
;;;;;;;;;;;;;;;

;;DM

(chunk-type typing state last id step)
(chunk-type subtraction type state last id step)

(chunk-type (button (:include visual-object)) tag)
;(chunk-type (text (:include visual-object)) x y)

(add-dm
 (start isa chunk)
 (enter isa chunk)
 (enter-easy isa chunk)
 (enter-hard isa chunk)
 (next-letter isa chunk)
 (easy isa chunk)
 (next-number isa chunk)
 (restore isa chunk)

 (button isa chunk)
 (grey isa chunk)


 (typing-goal isa typing state start)
 (subtraction-goal isa subtraction type easy state start)
)


;;; Typing
;;;;;;;;;;;;;;;

;;DM

(chunk-type typing-word word character pos step id)
(chunk-type order identity next)
(chunk-type carry-subtraction carry termB step id)

(add-dm
 (a isa order identity 0 next 1)
 (b isa order identity 1 next 2)
 (c isa order identity 2 next 3)
 (d isa order identity 3 next 4)
 (e isa order identity 4 next 5)
 (f isa order identity 5 next 6)
 (g isa order identity 6 next 7)
 (h isa order identity 7 next 8)
 (i isa order identity 8 next 9)
 (j isa order identity 9 next 10)
 (k isa order identity 10 next 11)
 )

(sdp a :creation-time -1e12 :references 400000)
(sdp b :creation-time -1e12 :references 400000)
(sdp c :creation-time -1e12 :references 400000)
(sdp d :creation-time -1e12 :references 400000)
(sdp e :creation-time -1e12 :references 400000)
(sdp f :creation-time -1e12 :references 400000)
(sdp g :creation-time -1e12 :references 400000)
(sdp h :creation-time -1e12 :references 400000)
(sdp i :creation-time -1e12 :references 400000)
(sdp j :creation-time -1e12 :references 400000)
(sdp k :creation-time -1e12 :references 400000)

;;Production Rules

(p clear-retrieval
   !eval! (not (busy *exp*))
   ?retrieval>
     buffer full
==>
  -retrieval>
)

(p new-goals
   !eval! (not (busy *exp*))
   =goal>
     isa typing
     last =something
   ?visual>
     state free
==>
  +visual>
    isa clear
  -goal>
  +goal>
    isa typing
    state start
)
(p new-goals3
   !eval! (not (busy *exp*))
   =goal>
     isa typing
     id =something
   ?visual>
     state free
==>
  +visual>
    isa clear
  -goal>
  +goal>
    isa typing
    state start
)

(p new-goals2
   !eval! (not (busy *exp*))
   =goal>
     isa subtraction
     last =something
   ?visual>
     state free
==>
  +visual>
    isa clear
  -goal>
  +goal>
    isa subtraction
    type easy
    state start
)

(p clear-visual-location
  !eval! (not (busy *exp*))
   ?visual-location>
     state error
==>
  -visual-location>
)

;;TYPING

(p typing-start
   !eval! (typing-stimulus *exp*)
   !eval! (typing-enabled *exp*)
   =goal>
     isa typing
     state start
   ?visual-location>
     buffer empty
   - state error
   ?visual>
     buffer empty
     state free
   ?manual>
     state free
   ?imaginal> ;perhaps delete for bilingual
     state free
   ?retrieval>
     state free
==>
  =goal>
    state enter
  +visual-location>
    isa visual-location
    kind text
    > screen-x 500
    < screen-x 1000
    screen-y 308
)

(p typing-start-but-no-letter-help
   !eval! (typing-stimulus *exp*)
   !eval! (typing-enabled *exp*)
   =goal>
     isa typing
     state enter
   ?visual-location>
     state error
==>
  =goal>
    state next-letter
)

(p typing-start-with-visual
    !eval! (typing-stimulus *exp*)
   !eval! (typing-enabled *exp*)
   =goal>
     isa typing
     state start
   =visual>
     isa text
  !eval! (look-right)

==>
   =goal>
    state enter
  +visual-location>
    isa visual-location
    kind text
    > screen-x 512
    < screen-x 1000
    screen-y lowest
)


(p attend-stimulus
   !eval! (typing-enabled *exp*)
   =goal>
     isa typing
     state enter
   =visual-location>
     isa visual-location
     kind text
   > screen-x 510
   ?visual>
     state free
     buffer empty

==>
  +visual>
    isa move-attention
    screen-pos =visual-location
)

(p empty-typing-goal
   !eval! (not (typing-enabled *exp*))
   =goal>
     isa typing
     state enter
  ?visual>
     buffer empty
     state free
==>
  =goal>
    state start
)
   
(p stimulus-attended-but-not-enabled
   !eval! (not (typing-enabled *exp*))
   !eval! (look-right)
   =goal>
     isa typing
     state enter
   ?visual>
     state free
   =visual>
     isa text

==>
  =goal>
    state start
)
;;;easy typing

(p recode
   !eval! (typing-enabled *exp*)
   =goal>
     isa typing
     state enter
     last =txt
   ?visual>
     state free
   =visual>
     isa text
     screen-pos =pos
     value =txt
   !eval! (equalp (length =txt) 1)
==>
  +visual>
    isa move-attention
    screen-pos =pos
)

(p ah-its-hard
   !eval! (typing-enabled *exp*)
   !eval! (look-right)
   =goal>
     isa typing
     state enter
   - last =txt
   ?visual>
     state free
   =visual>
     isa text
     value =txt
   !eval! (equalp (length =txt) 0)
==>
  =goal>
    state next-letter
)

(p direct-click-initiate
   !eval! (typing-enabled *exp*)
   !eval! (look-right)
   =goal>
     isa typing
     state enter
   - last =txt
   ?visual>
     state free
   =visual>
     isa text
     value =txt
   !eval! (equalp (length =txt) 1)
==>
  =goal>
    state enter-easy
   !bind! =x (find-location-x =txt)
   !bind! =y (find-location-y =txt)
   +visual-location>
     isa visual-location
     kind button
     screen-x =x
     screen-y =y
)

(p direct-click-find
   !eval! (typing-enabled *exp*)
   =goal>
     isa typing
     state enter-easy
   =visual-location>
     isa visual-location
     kind button
   ?visual>
     state free
     buffer empty
   ?manual>
     state free
==>
  +visual>
    isa move-attention
    screen-pos =visual-location
  +manual>
    isa move-cursor
    loc =visual-location
)

(p direct-click-click
   !eval! (typing-enabled *exp*)
  =goal>
    isa typing
    state enter-easy
  ?visual>
   state free
  =visual>
    isa button
    value =txt
  ?manual>
    state free
 ==>
   =goal>
     state start
     last =txt
   +manual>
     isa click-mouse

)



;;;hard typing

(p hard-click-initiate
   !eval! (typing-enabled *exp*)
   =goal>
     isa typing
     state enter
   ?visual>
   state free
   =visual>
     isa text
     value =txt
   !eval! (> (length =txt) 1)
==>
  !bind! =id *trial*
  !bind! =step *typ_step*
  =goal>
    state enter-hard
    id =id
    step =step
  !bind! =nextletter (subseq =txt 0 1)
  !bind! =x (find-location-x =nextletter)
  !bind! =y (find-location-y =nextletter)
   +visual-location>
     isa visual-location
     kind button
     screen-x =x
     screen-y =y
  +imaginal>
    isa typing-word
    word =txt
    character =nextletter
    pos 1
    step =step
    id =id
)

(p hard-click-find
   !eval! (typing-enabled *exp*)
   =goal>
     isa typing
     state enter-hard
   =visual-location>
     isa visual-location
     kind button
   ?visual>
     state free
     buffer empty
   ?manual>
     state free
==>
  +visual>
    isa move-attention
    screen-pos =visual-location
  +manual>
    isa move-cursor
    loc =visual-location
)

(p hard-click-click
   !eval! (typing-enabled *exp*)
  =goal>
    isa typing
    state enter-hard
  ?visual>
   state free
  =visual>
    isa button
  ?manual>
    state free
  =imaginal>
    isa typing-word
    word =txt
    pos =pos
  ?visual-location>
    buffer empty
  ?retrieval>
    state free
    buffer empty
 ==>
   +manual>
     isa click-mouse
   =imaginal>
  
=visual>
  =goal>
    state next-number
)

(p next-letter-after-number
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa typing
     state next-number
  
   ?manual>
     preparation free
     processor free
     ;execution free
   =imaginal>
     isa typing-word
     word =wrd
     character =chr
     pos =pos
   ?visual>
     state free
==>
  +visual>
    isa clear
  !bind! =id *trial*
  !bind! =step *typ_step*
   +imaginal>
     isa typing-word
     word =wrd
     character =chr
     pos =pos
     step =step

   =goal>
     state start
     id =id
     step =step
)

(p start-after-subtraction-with-imaginal
   !eval! (typing-enabled *exp*)
   !eval! (typing-stimulus *exp*)
   =goal>
     isa typing
     state next-letter
   =imaginal>
     isa typing-word
     word =txt
     pos =pos
   ?visual>
     state free
     buffer empty
   ?retrieval>
    state free
    buffer empty
 ==>
   =imaginal>
   +retrieval>
     isa order
     identity =pos
)

(p start-after-subtraction-with-no-imaginal
   !eval! (typing-enabled *exp*)
   !eval! (typing-stimulus *exp*)
   =goal>
     isa typing
     state next-letter
     id =id
     step =step
  ?imaginal> ; perhaps remove in bilingual
    buffer empty
    state free
   ?visual>
     state free
     buffer empty
   ?retrieval>
    state free
    buffer empty
 ==>
   =goal>
     state restore
   +retrieval>
     isa typing-word
     id =id
     step =step

)

(p start-after-subtraction-with-wrong-imaginal
   !eval! (typing-enabled *exp*)
   !eval! (typing-stimulus *exp*)
   =goal>
     isa typing
     state next-letter
     id =id
     step =step
   =imaginal>
     isa carry-subtraction
   ?visual>
     state free
     buffer empty
   ?retrieval>
    state free
    buffer empty
   ?imaginal> ;perhaps remove in bilingual model
     state free
 ==>



   =goal>
     state restore
   +retrieval>
     isa typing-word
     id =id
     step =step

)

(p typing-restore-imaginal
   =goal>
     isa typing
     state restore
   =retrieval>
     isa typing-word
     word =wrd
     character =char
     pos =pos
   ?imaginal> ;perhaps remove in bilingual
     state free
==>
  !bind! =id *trial*
  !bind! =step *typ_step*
  =goal>
   id =id
   step =step
   +imaginal>
     isa typing-word
     word =wrd
     character =char
     pos =pos
     id =id
     step =step
   =goal>
     state next-letter
)

(p next-letter
   !eval! (typing-stimulus *exp*)
   !eval! (typing-enabled *exp*)
   =goal>
     isa typing
     state next-letter
   =retrieval>
     isa order
     identity =pos
     next =nxt
   =imaginal>
     isa typing-word
     word =txt
     pos =sth
     id =id
     step =step
==>
  !bind! =nextletter (if (< =pos 10) (subseq =txt =pos =nxt) (subseq =txt 9 10))
  !bind! =x (find-location-x =nextletter)
  !bind! =y (find-location-y =nextletter)
  =goal>
    state enter-hard
  =imaginal> 
    word =txt
    character =nextletter
    pos =nxt
    id =id
    step =step
 

   +visual-location>
     isa visual-location
     kind button
     screen-x =x
     screen-y =y
)



;;; Subtraction
;;;;;;;;;;;;;;;;;;;

;;DM

(chunk-type sub-fact termA termB diff)
(chunk-type add-fact addA addB add)


(add-dm
 (reading isa chunk)
 (respond isa chunk)
 (find-next isa chunk)
 (find-new-letter isa chunk)
 (busy-typing isa chunk)
 (retrieving isa chunk)
 (wait-for-carry isa chunk)
 (hard isa chunk)
 (read-b-again isa chunk)
)

;;11 - 0tot9
(dotimes (j 11)
  (dotimes (i (+ 10))
    (let ((name (intern (string-upcase (concatenate 'string "sub" (write-to-string j) "-" (write-to-string i)))))
          (a (write-to-string j))
          (b (write-to-string i))
          (diff (write-to-string (- j i))))
      (eval `(add-dm (,name isa sub-fact termA ,a termB ,b diff ,diff)))
      (eval `(sdp ,name :creation-time -1e12 :references 15000000)))))

;;19 - 0tot9
(dotimes (j 9)
  (dotimes (i (+ 10))
    (let* ((j (+ j 11))
           (name (intern (string-upcase (concatenate 'string "sub" (write-to-string j) "-" (write-to-string i)))))
           (a (write-to-string j))
           (b (write-to-string i))
           (diff (write-to-string (- j i))))
      (eval `(add-dm (,name isa sub-fact termA ,a termB ,b diff ,diff)))
      (eval `(sdp ,name :creation-time -1e12 :references 150000)))))

(dotimes (j 10)
  (let ((name (intern (string-upcase (concatenate 'string "add" (write-to-string j) "+" (write-to-string 10)))))
        (a (write-to-string j))
        (b (write-to-string 10))
        (addi (write-to-string (+ j 10))))
    (eval `(add-dm (,name isa add-fact addA ,a addB ,b add ,addi)))
    (eval `(sdp ,name :creation-time -1e12 :references 1000000))))

;;Production Rules

(p subtraction-start
     !eval! (subtraction-stimulus *exp*)
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state start
   ?visual-location>
     buffer empty
   ?visual>
     buffer empty
     state free
   ?imaginal> ;perhaps delete for bilingual (maybe)
     state free
   ?retrieval>
     state free
==>
  =goal>
    state reading
  +visual-location>
    isa visual-location
    kind text
    screen-x 250
    screen-y 295
)

(p read-term-A
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state reading
   =visual-location>
     isa visual-location
     kind text
     screen-y 295
     screen-x =x
   ?visual>
      state free
      buffer empty
==>
  +visual>
    isa move-attention
    screen-pos =visual-location
  +visual-location>
    isa visual-location
    kind text
    screen-x =x
    screen-y 335
)

(p read-term-B
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state reading
     type easy
   ?visual>
   state free
=visual>
     isa text
     value =txt
   =visual-location>
     isa visual-location
     screen-y 335
   
==>
   +visual>
     isa move-attention
     screen-pos =visual-location
   =goal>
     state retrieving
     last =txt
)

(p start-subtraction-retrieval
   !eval! (subtraction-enabled *exp*)
  =goal>
    isa subtraction
    state retrieving
    type easy
    last =termA
  ?visual>
   state free
=visual>
    isa text
    value =termB
  ?retrieval>
    state free
    buffer empty
==>
  =visual>
  +retrieval>
    isa sub-fact
    termA =termA
    termB =termB
  =goal>
    state respond
)

(p subtraction-response-0
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state respond
   =retrieval>
     isa sub-fact
     termA =a
     termB =b
     diff "0"
   ?manual>
     state free
   ?visual>
   state free
=visual>
     isa text
==>
  =visual>
  =goal>
    state find-next
  +manual>
    isa peck-recoil
    hand left
    finger ring
    r 1
    theta 1.57 ;== 0
  
)

(p subtraction-response-1
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state respond
   =retrieval>
     isa sub-fact
     termA =a
     termB =b
     diff "1"
   ?manual>
     state free
   ?visual>
   state free
=visual>
     isa text
==>
  =visual>
  =goal>
    state find-next
  +manual>
    isa punch
    hand left
    finger ring
    ;== 1

)

(p subtraction-response-2
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state respond
   =retrieval>
     isa sub-fact
     termA =a
     termB =b
     diff "2"
   ?manual>
     state free
   ?visual>
   state free
=visual>
     isa text
==>
  =visual>
  =goal>
    state find-next
  +manual>
    isa punch
    hand left
    finger middle
    ;== 2

)
    
(p subtraction-response-3
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state respond
   =retrieval>
     isa sub-fact
     termA =a
     termB =b
     diff "3"
   ?manual>
     state free
   ?visual>
   state free
=visual>
     isa text
==>
  =visual>
  =goal>
    state find-next
  +manual>
    isa punch
    hand left
    finger index
    ;== 2

)  

(p subtraction-response-4
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state respond
   =retrieval>
     isa sub-fact
     termA =a
     termB =b
     diff "4"
   ?manual>
     state free
   ?visual>
   state free
=visual>
     isa text
==>
  =visual>
  =goal>
    state find-next
  +manual>
    isa peck-recoil
    hand left
    finger ring
    r 1
    theta 4.71

)    

(p subtraction-response-5
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state respond
   =retrieval>
     isa sub-fact
     termA =a
     termB =b
     diff "5"
   ?manual>
     state free
   ?visual>
   state free
=visual>
     isa text
==>
  =visual>
  =goal>
    state find-next
  +manual>
    isa peck-recoil
    hand left
    finger middle
    r 1
    theta 4.71

)

(p subtraction-response-6
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state respond
   =retrieval>
     isa sub-fact
     termA =a
     termB =b
     diff "6"
   ?manual>
     state free
   ?visual>
   state free
=visual>
     isa text
==>
  =visual>
  =goal>
    state find-next
  +manual>
    isa peck-recoil
    hand left
    finger index
    r 1
    theta 4.71

)

(p subtraction-response-7
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state respond
   =retrieval>
     isa sub-fact
     termA =a
     termB =b
     diff "7"
   ?manual>
     state free
   ?visual>
   state free
=visual>
     isa text
==>
  =visual>
  =goal>
    state find-next
  +manual>
    isa peck-recoil
    hand left
    finger ring
    r 2
    theta 4.71 ;== 7

)

(p subtraction-response-8
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state respond
   =retrieval>
     isa sub-fact
     termA =a
     termB =b
     diff "8"
   ?manual>
     state free
   ?visual>
   state free
=visual>
     isa text
==>
  =visual>
  =goal>
    state find-next
  +manual>
    isa peck-recoil
    hand left
    finger middle
    r 2
    theta 4.71 ;== 8

)

(p subtraction-response-9
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state respond
   =retrieval>
     isa sub-fact
     termA =a
     termB =b
     diff "9"
   ?manual>
     state free
   ?visual>
   state free
=visual>
     isa text
==>
  =visual>
  =goal>
    state find-next
  +manual>
    isa peck-recoil
    hand left
    finger index
    r 2
    theta 4.71 ;== 9
)

(p subtraction-response-10-but-thats-impossible-so-respond-9
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state respond
   =retrieval>
     isa sub-fact
     termA =a
     termB =b
     diff "10"
   ?manual>
     state free
   ?visual>
   state free
=visual>
     isa text
==>
  =visual>
  =goal>
    state find-next
  +manual>
    isa peck-recoil
    hand left
    finger index
    r 2
    theta 4.71 ;== 9
)
 
(p subtraction-see-response-easy
   =goal>
     isa subtraction
     state find-next
     type easy
   ?manual>
     state free
   ?visual-location>
   - buffer full
   - state error
   ?visual>
   state free
=visual>
     isa text
==>
   =goal>
     state busy-typing

)
(p subtraction-see-response-hard
   =goal>
     isa subtraction
     state find-next
     type hard
   ?manual>
     state free
   ?visual-location>
   - buffer full
   - state error
   ?visual>
   state free
=visual>
     isa text
   =imaginal>
     isa carry-subtraction
     carry =cry
     termB =tB
     step =step
==>
  !bind! =id *trial* 
  !bind! =step *sub_step*
   +imaginal>
     isa carry-subtraction
     carry =cry
     termB =tB
     step =step
     id =id
   =goal>
     state busy-typing
     id =id
     step =step

)

(p subtraction-see-response-but-disabled
   !eval! (not (subtraction-enabled *exp*))
  =goal>
     isa subtraction
     state busy-typing
==>
  =goal>
    state find-new-letter
)

(p subtraction-see-response-and-finished
 
  =goal>
     isa subtraction
     state find-new-letter
  =visual-location>
    isa visual-location
    screen-x 25
    screen-y 375
==>
  =goal>
    state start
    type easy
)


(p subtraction-find-current-letter-easy
   !eval! (subtraction-enabled *exp*)

 =goal>
     isa subtraction
     state find-new-letter
     type easy
  
   ?visual-location>
   - buffer full
   - state error
   ?visual>
     state free
     buffer empty
   ?imaginal> ;perhaps delete for bilingual
     state free
   ?retrieval>
     state free
==>
   +visual>  
     isa clear 
   +visual-location>
     isa visual-location
     screen-x lowest
     screen-y 375
)

(p subtraction-find-current-letter-hard-imaginal
   !eval! (subtraction-enabled *exp*)
   !eval! (busy *exp*)

 =goal>
     isa subtraction
     state find-new-letter
     type hard
  
   ?visual-location>
   - buffer full
   - state error
   ?visual>
     state free
     buffer empty
   =imaginal>
     isa carry-subtraction
   ?imaginal> ;perhaps delete for bilingual
     state free
   ?retrieval>
     state free
==>
   !bind! =step *sub_step*
   =goal>
     step =step
  =imaginal>
    step =step
   +visual>  
     isa clear 
   +visual-location>
     isa visual-location
     screen-x lowest
  
     screen-y 375
)

(p subtraction-find-current-letter-hard-no-imaginal
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state find-new-letter
     type hard
     id =id
     step =step
  
   ?visual-location>
   - buffer full
   - state error
   ?visual>
     state free
     buffer empty
   ?imaginal>  ;perhaps delete for bilingual
     buffer empty
     state free
   ?retrieval>
     state free
     buffer empty


==>
   +visual>  
     isa clear 
   +visual-location>
     isa visual-location
     screen-x lowest
     screen-y 375
   +retrieval>
     isa carry-subtraction
     id =id
     step =step
)

(p subtraction-find-current-letter-hard-wrong-imaginal
   !eval! (subtraction-enabled *exp*)
  =goal>
     isa subtraction
     state find-new-letter
     type hard
     id =id
     step =step
   ?visual-location>
   - buffer full
   - state error
   ?visual>
     state free
     buffer empty
   =imaginal>
    isa typing-word
   ?retrieval>
     state free
     buffer empty
   ?imaginal> ;perhaps delete for bilingual
     state free
 
==>

   +visual>  
     isa clear 
   +visual-location>
     isa visual-location
     screen-x lowest
     screen-y 375
   +retrieval>
     isa carry-subtraction
     id =id
     step =step
)

(p subtraction-restore-imaginal
   =goal>
     isa subtraction
     type hard
   =retrieval>
    isa carry-subtraction
    carry =cry
    termB =b
==>
   !bind! =id *trial*
   !bind! =step *sub_step*
   =goal>
     id =id
     step =step
   +imaginal>
     isa carry-subtraction
     carry =cry
     termB =b
     step =step
     id =id
)


(p subtraction-find-next
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state find-new-letter
   =visual-location>
     isa visual-location
     screen-y 375
     screen-x =x
   ?imaginal>  ;perhaps delete for bilingual
     state free
   ?retrieval>
     state free
==>
  !bind! =new-x (- =x 25)
  =goal>
    state reading
  +visual-location>
    isa visual-location
    kind text
    screen-x =new-x
    screen-y 295
)

(p subtraction-wow-it-is-hard
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state respond
     type easy
   =retrieval>
     isa sub-fact
     termA =a
     termB =b
     diff =diff
  !eval! (< (parse-integer =diff) 0)
==>
   !bind! =id *trial*
   !bind! =step *sub_step*
   =goal>
     state add-tens
     type hard
     id =id
     step =step
     last =a
   +imaginal>
     isa carry-subtraction
     carry 22
     termB =b
     step =step
     id =id
  +visual-location>
     isa visual-location
     kind text
     screen-y 295
     screen-x current
  !eval! (format t "help it's hard!")
)

(p subtraction-hard-addition
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state respond
     type hard
   =retrieval>
     isa sub-fact
     termA =a
     termB =b
     diff =diff
   !eval! (< (parse-integer =diff) 0)
   =imaginal>
     isa carry-subtraction
   ?visual>
   state free
=visual>
     isa text
     screen-pos =pos
==>
 !bind! =id *trial* 
   !bind! =step *sub_step*
   =goal>
    last =a
    id =id
    step =step
    state add-tens  
  +imaginal>
    isa carry-subtraction
     carry 22
     termB =b
     id =id
     step =step
  +visual-location>
     isa visual-location
     kind text
     screen-y 295
     screen-x current
)

(p add-tens-look-up
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state add-tens
     type hard
     last =a
   ?imaginal> ;perhaps delete for bilingual
     state free
     buffer full
   =visual-location>
     isa visual-location
     kind text
     screen-y 295
     screen-x =x
   ?visual>
     state free
==>
  +visual>
    isa move-attention
    screen-pos =visual-location
  =imaginal>
)

(p addition-done-time-for-subtraction
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state add-tens
     type hard
     last =a
  ?visual>
   state free
=visual>
    isa text
   =imaginal>
     isa carry-subtraction
     termB =b
     carry =carry
     id =id
     step =step
   ?visual>
     state free
==>

  !bind! =string-add (write-to-string (+ (values (parse-integer =a)) 10))
  +imaginal>
    isa carry-subtraction
    termB =b
    carry 1
    id =id
    step =step
  =goal>
    state read-b-again
    last =string-add
  +visual-location>
    isa visual-location
    screen-x current
    screen-y 335

)

(p read-b-again-subtraction-hard
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state read-b-again
     type hard
    =visual-location>
     isa visual-location
     kind text
     screen-y 335
   ?imaginal> ;perhaps delete for bilingual
     state free
   ?visual>
     state free
==>
  +visual>
    isa move-attention
    screen-pos =visual-location
  =goal>
    state retrieving
)

(p start-subtraction-retrieval-hard
   !eval! (subtraction-enabled *exp*)
  =goal>
    isa subtraction
    state retrieving
    type hard
    last =termA
  ?visual>
   state free
=visual>
    isa text
    value =termB
 ?retrieval>
    state free
    buffer empty
  =imaginal>
    isa carry-subtraction
  ?visual>
    state free
==>

  =visual>
  =imaginal>

  +retrieval>
    isa sub-fact
    termA =termA
    termB =termB
  =goal>
    state respond
)

(p read-term-B-hard-no-carry
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state reading
     type hard
   ?visual>
   state free
=visual>
     isa text
     value =txt
   =visual-location>
     isa visual-location
     screen-y 335
   =imaginal>
     isa carry-subtraction
     carry 0
   ?visual>
     state free
==>
   =imaginal>
   +visual>
     isa move-attention
     screen-pos =visual-location
   =goal>
     state retrieving
     last =txt
)

(p read-term-B-hard-with-carry
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state reading
     type hard
   ?visual>
   state free
=visual>
     isa text
     value =txt
   =visual-location>
     isa visual-location
     screen-y 335
   =imaginal>
     isa carry-subtraction
     carry 1
   ?visual>
     state free
   ?retrieval>
     state free
     buffer empty
==>
   =imaginal>
   -visual-location>
   +retrieval>
     isa sub-fact
     termA =txt
     termB "1"
   =goal>
     state wait-for-carry  
)

(p process-carry
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state wait-for-carry
     type hard
   =retrieval>
     isa sub-fact
     termA =a
     diff =diff
   =imaginal>
     isa carry-subtraction
==>

  =imaginal>
    carry 0
  =goal>
    state read-b-again
    last =diff
  +visual-location>
    isa visual-location
    kind text
    screen-x current
    screen-y 335
  
)

(p subtraction-end
   !eval! (subtraction-enabled *exp*)
   =goal>
     isa subtraction
     state find-next
   ?visual-location>
     state error
==>
  =goal>
    state start
    type easy
   +visual>
     isa clear
)

(p subtraction-end-diff-state
   !eval! (subtraction-enabled *exp*)
   !eval! (busy *exp*)
   =goal>
     isa subtraction
     state find-new-letter
   ?visual-location>
     state error
==>
  =goal>
    state start
    type easy
   +visual>
     isa clear
)

(goal-focus subtraction-goal)
(goal-focus typing-goal)
)

