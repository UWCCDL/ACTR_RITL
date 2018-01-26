;;; ------------------------------------------------------------------
;;; DEVICE FOR 2AFC TASK
;;; ------------------------------------------------------------------
;;;
;;; This file contains a simple implementation of a two-alternative
;;; forced chice (2AFC) task to compare ACT-R vs. DDM parameters.
;;;
;;; ------------------------------------------------------------------
;;;
;;; (C) 2018, Andrea Stocco,
;;;     University of Washington
;;;     stocco@uw.edu
;;;
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;; ACT-R functions and parameters
;;; ------------------------------------------------------------------

(defparameter *verbose* nil "Flag for verbose output (for debugging") 

(defparameter *utility-learning-enabled* nil "No learning for now")

;;; ------------------------------------------------------------------
;;; UTILITIES
;;; ------------------------------------------------------------------

(defun act-r-loaded? ()
  "Checks whether ACTR is loaded"
  (member :act-r *features*))

(defun pick (lst)
  "Picks up an element from a list"
  (when  (listp lst)
    (elt lst (random (length lst)))))


(defun scramble (lst &optional (sofar nil))
  "Scrambles a list of different elements"
  (if (null lst)
      sofar
    (let ((picked (pick lst)))
      (scramble (remove picked lst) (cons picked sofar)))))

(defun scramble* (lst)
  "Scrambles any list of objects"
  (let ((l (length lst))
        (pos nil))
    (dotimes (i l)
      (push i pos))
    (mapcar #'(lambda (x) (elt lst x)) (scramble pos))))

(defun mean (&rest nums)
  "Mean of a set of numbers"
  (when (every #'numberp nums)
    (/ (reduce #'+ nums)
       (length nums))))

;;; ------------------------------------------------------------------
;;; RITL TASK
;;; ------------------------------------------------------------------

;; ---------------------------------------------------------------- ;;
;; Data structures and parameters for the task
;; ---------------------------------------------------------------- ;;

(defparameter *operators* '(double half
			    triple third
			    increase decrease
			    add minus
			    times divide))


(defparameter *operations* '((* x 2) (floor x 2)
			     (* x 3) (floor x 3)
			     (+ x 1) (- x 1)
			     (+ x y) (- x y)
			     (* x y) (floor x y)))

(defparameter *interpretations* (pairlis *operators* *operations*))

(defun splice (func inputs)
  "replaces Xs and Ys with numbers"
  (subst (second inputs) 'y (subst (first inputs) 'x func))) 

(defun apply-op (op &rest inputs)
  (let* ((expression (cdr (assoc op *interpretations*)))
	 (res (splice expression inputs)))
    (apply (first res) (rest res))))

(defparameter *test* '((double third add) (5 9) 13))

(defparameter *responses* '((f . yes) (j . no)) "Left hand = Yes, Right hand = No")

(defun ritl-rule (stim)
  (first stim))

(defun ritl-inputs (stim)
  (second stim))

(defun ritl-probe (stim)
  (third stim))

(defun ritl-rule? (rule)
  (and (= (length rule) 3)
       (every #'(lambda (x) (member x *operators*)) rule)))

(defun ritl-inputs? (ins)
  (and (= (length ins) 2)
       (every #'numberp ins)))

(defun ritl-probe? (num)
  (numberp num))

(defun ritl-stimulus? (stim)
  (and (= (length stim) 3)
       (ritl-rule? (ritl-rule stim))
       (ritl-inputs? (ritl-inputs  stim))
       (ritl-probe? (ritl-probe stim))))


(defun stimulus-correct-response (stim)
  "The correct response needs to be calculated internally"
  (when (ritl-stimulus? stim)
    (let ((rule (ritl-rule stim))
	  (inputs (ritl-inputs stim)))
      (let* ((x (apply-op (first rule) (first inputs)))
	     (y (apply-op (second rule) (second inputs)))
	     (res (apply-op (third rule) x y)))
	(if (= res (ritl-probe stim))
	    'yes
	    'no)))))



;;; RITL TRIAL STRUCT FORMAT:
;;;
;;; 1 - RULE
;;; 2 - Rule Onset Time
;;; 3 - Rule Offset (response) Time
;;; 4 - Execution Onset Time
;;; 5 - Execution Offset (response) Time
;;; 6 - Probe Onset Time
;;; 7 - Probe Offset (response) Time
;;; 8 - Correct response
;;; 9 - Actual response
		 		 
(defun make-trial (stim)
  (when (ritl-stimulus? stim)
    (list stim 0 0 0 0 0 0 (stimulus-correct-response stim) nil)))

(defun trial-stimulus (trial)
  (nth 0 trial))

(defun set-trial-stimulus (trial stimulus)
  (when (ritl-stimulus? stimulus)
    (setf (nth 0 trial) stimulus)))

(defun trial-rule-onset-time (trial)
  (nth 1 trial))

(defun set-trial-rule-onset-time (trial tme)
  (setf (nth 1 trial) tme))

(defun trial-rule-response-time (trial)
  (nth 2 trial))

(defun set-trial-rule-response-time (trial tme)
  (setf (nth 2 trial) tme))

(defun trial-execution-onset-time (trial)
  (nth 3 trial))

(defun set-trial-execution-onset-time (trial tme)
  (setf (nth 3 trial) tme))

(defun trial-execution-response-time (trial)
  (nth 4 trial))

(defun set-trial-execution-response-time (trial tme)
  (setf (nth 4 trial) tme))

(defun trial-probe-onset-time (trial)
  (nth 5 trial))

(defun set-trial-probe-onset-time (trial tme)
  (setf (nth 5 trial) tme))

(defun trial-probe-response-time (trial)
  (nth 6 trial))

(defun set-trial-probe-response-time (trial tme)
  (setf (nth 6 trial) tme))

(defun trial-correct-response (trial)
  (nth 7 trial))

(defun set-trial-correct-response (trial response)
  (setf (nth 7 trial) response))

(defun trial-actual-response (trial)
  (nth 8 trial))

(defun set-trial-actual-response (trial response)
  (setf (nth 8 trial) response))

(defun generate-stimuli (&optional (n 100))
;;  (let ((result nil))
;;    (dolist (stim *stimuli* result)
 ;;     (dotimes (i n)
  ;;	(push stim result)))))
  (let ((results nil))
    (dotimes (i n)
      (push *test* results))))

(defun generate-trials (stim-list)
  (declare (ignore stim-list))
  (mapcar #'make-trial (list *test*)))

(defun trial-rt (trial)
  (- (trial-response-time trial)
     (trial-onset-time trial)))

(defun trial-accuracy (trial)
  (if (equal (trial-correct-response trial)
	     (trial-actual-response trial))
      1
      0)) 

(defparameter *transitions* '((rule . pause1) (pause1 . execution)
			      (execution . pause2) (pause2 . probe)
			      (probe . pause3) (pause3 . rule)))

(defparameter *pauses* '(pause1 pause2 pause3 pause4))

(defun pause? (sym)
  (member sym *pauses*))

(defclass ritl-task ()
  ((phase :accessor task-phase
	  :initform nil)
   (index :accessor index
	  :initform nil)
   (trials :accessor trials
	   :initform (generate-trials (generate-stimuli)))
   (current-trial :accessor current-trial
		  :initform nil)
   (experiment-log :accessor experiment-log
		   :initform nil))
  (:documentation "A manager for the RITL task"))

(defmethod init ((task ritl-task))
  "Initializes the RITL task manager"
  (unless (null (trials task))
    (setf (index task) 0)
    (setf (experiment-log task) nil)
    (setf (trials task) (scramble* (trials task)))
    (setf (current-trial task)
	  (nth (index task) (trials task)))
    (setf (task-phase task) 'rule)))


(defmethod respond ((task ritl-task) key)
  "Records a response in the PSS task"
  (unless (null (current-trial task))
    (let* ((trial (current-trial task))
	   (response (cdr (assoc key *responses*))))
      (when (equal (task-phase task) 'probe)
	(set-trial-actual-response trial response))
      (when (act-r-loaded?)
	(schedule-event-relative 0 #'next :params (list task))))))
            

(defmethod next ((task ritl-task))
  "Moves to the next step in a RITL Task timeline"
  (let* ((current-phase (task-phase task))
	 (next-phase (cdr (assoc current-phase *transitions*))))
    
    (cond
      ;;; Rule
      ((equal current-phase 'rule)
       (when (act-r-loaded?)
	 (set-trial-rule-response-time (current-trial task)
				       (mp-time))
       	 (schedule-event-relative 1 'next
				  :params (list task))))

      ;;; Pause between Rule and Execution
      ((equal current-phase 'pause1)
       (when (act-r-loaded?)
	 (set-trial-execution-onset-time (current-trial task)
					 (mp-time))))

      ;;; Execution
      ((equal current-phase 'execution)
       (when (act-r-loaded?)
	 (set-trial-execution-response-time (current-trial task)
					    (mp-time))
       	 (schedule-event-relative 1 'next
				  :params (list task))))

      ;;; Pause between Execution and Probe
      ((equal current-phase 'pause2)
       (when (act-r-loaded?)
	 (set-trial-probe-onset-time (current-trial task)
				     (mp-time))))
      
      ;;; Probe
      ((equal current-phase 'probe)
       (when (act-r-loaded?)
	 (set-trial-probe-response-time (current-trial task)
					(mp-time))
	 (schedule-event-relative 1 'next
				  :params (list task)))
       (push (current-trial task) (experiment-log task))
       (setf (current-trial task) nil))

      ;;; Pause3
      ((equal current-phase 'pause3)
       (incf (index task))
       (cond ((>= (index task) (length (trials task)))
	      (setf next-phase 'done))
	     (t
	      (setf (current-trial task) (nth (index task)
					      (trials task)))
	      (when (act-r-loaded?)
		(set-trial-rule-onset-time (current-trial task)
					   (mp-time))
		(schedule-event-relative 1 'next
					 :params (list task)))))))
    
    (setf (task-phase task) next-phase))
	  

      
  (cond ((equal (task-phase task) 'stimulus)
	 (setf (task-phase task) 'pause)
	 (push (current-trial task) (experiment-log task))
	 (setf (current-trial task) nil)
	 (when (act-r-loaded?)
	   (schedule-event-relative 1 'next :params (list task))))
	((equal (task-phase task) 'pause)
	 (incf (index task))
	 (cond ((>= (index task) (length (trials task)))
		(setf (task-phase task) 'done))
	       (t
		(setf (task-phase task) 'stimulus)
		(setf (current-trial task) (nth (index task)
						(trials task)))
		(when (act-r-loaded?)
		  (set-trial-onset-time (current-trial task)
					(mp-time)))))))
  (when (act-r-loaded?) 
    (schedule-event-relative 0 'proc-display :params nil)))

;;; ------------------------------------------------------------------
;;; ACT-R DEVICE INTERFACE
;;; ------------------------------------------------------------------
;;; These functions turn the RITL-Task class into an ACT-R device
;;; ------------------------------------------------------------------

(defmethod device-handle-keypress ((task ritl-task) key)
  "Converts the key into a symbol and passes it on to the task manager"
  (respond task (intern (string-capitalize (format nil "~a" key)))))

			   
(defmethod device-handle-click ((task ritl-task))
  "Does nothing"
  (declare (ignore task))
  nil)

(defmethod device-move-cursor-to ((task ritl-task) pos)
  "Does nothing"
  (declare (ignore task))
  nil)


(defmethod get-mouse-coordinates ((task ritl-task))
  "Does nothing"
  (declare (ignore task))
  (vector 0 0))

(defmethod cursor-to-vis-loc ((task ritl-task))
  "Does nothing"
  (declare (ignore task))
  nil)

(defmethod build-vis-locs-for ((task ritl-task) vismod)
  (let ((results nil)
	(phase (task-phase task)))
    (push  `(isa visual-location 
		 kind ritl-location
		 value ,phase
		 color black
		 screen-x 0
		 screen-y 0
		 height 400 
		 width 400)
	   results)
    (define-chunks-fct results)))



(defmethod vis-loc-to-obj ((task ritl-task) vis-loc)
  "Transforms a visual-loc into a visual object"
  (let* ((new-chunk nil)
	 (phase (task-phase task))
	 (trial (current-trial task))
	 (stim (trial-stimulus trial)))
    (cond ((or (pause? phase) (equal 'done phase))
	   (setf new-chunk (create-screen-chunk phase vis-loc)))

	  ((equal phase 'rule)
	   (setf new-chunk (create-rule-chunk (ritl-rule stim) vis-loc)))

	  ((equal phase 'execution)
	   (setf new-chunk (create-execution-chunk (ritl-inputs stim) vis-loc)))

	  ((equal phase 'probe)
	   (setf new-chunk (create-probe-chunk (ritl-probe stim)  vis-loc))))
    
    (fill-default-vis-obj-slots new-chunk vis-loc)
    new-chunk))



(defmethod create-rule-chunk ((rule list) vis-loc)
  "Transforms a stimulus into a visual object"
  (first (define-chunks-fct 
	     `((isa ritl-rule
		    kind ritl-rule
		    phase rule
		    task1 ,(first rule)
		    task2 ,(second rule)
		    task3 ,(third rule))))))


(defmethod create-execution-chunk ((inputs list) vis-loc)
  "Transforms a stimulus into a visual object"
  (first (define-chunks-fct 
	     `((isa ritl-inputs
		    kind ritl-inputs
		    phase execution
		    x ,(first inputs)
		    y ,(second inputs))))))
		    

(defmethod create-probe-chunk ((probe number) vis-loc)
  "Transforms a stimulus into a visual object"
  (first (define-chunks-fct 
	     `((isa ritl-probe
		    kind ritl-probe
		    phase probe
		    probe ,probe)))))


(defmethod create-screen-chunk ((pause symbol) vis-loc)
  "Transforms a stimulus into a visual object"
  (first (define-chunks-fct 
	     `((isa ritl-screen
		    kind ritl-screen 
		    value ,pause)))))

;;; ------------------------------------------------------------------
;;; DATA FORMATTING
;;; ------------------------------------------------------------------

(defun ritl-reload (&optional (device (make-instance 'ritl-task)))
  "Reloads the current PSS model"
  (reload)
  (init device)
  (install-device device)
  (init device)
  (proc-display))
