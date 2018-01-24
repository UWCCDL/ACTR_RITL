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

(defun apply-op (op inputs)
  (let ((res (splice op inputs)))
    (apply (first res) (rest res))))

(defparameter *test* '((double third add) (5 9) 13))

(defparameter *responses* '((f . left) (j . right)))

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
    (let* (rule ( 

(defun make-trial (stim)
  (when (ritl-stimulus? stim)
    (list stim 0 0 (stimulus-correct-response stim) nil)))

(defun trial-stimulus (trial)
  (nth 0 trial))

(defun set-trial-stimulus (trial stimulus)
  (when (ritl-stimulus? stimulus)
    (setf (nth 0 trial) stimulus)))

(defun trial-onset-time (trial)
  (nth 1 trial))

(defun set-trial-onset-time (trial tme)
  (setf (nth 1 trial) tme))

(defun trial-response-time (trial)
  (nth 2 trial))

(defun set-trial-response-time (trial tme)
  (setf (nth 2 trial) tme))

(defun trial-correct-response (trial)
  (nth 3 trial))

(defun set-trial-correct-response (trial response)
  (setf (nth 3 trial) response))

(defun trial-actual-response (trial)
  (nth 4 trial))

(defun set-trial-actual-response (trial response)
  (setf (nth 4 trial) response))

(defun generate-stimuli (&optional (n 100))
  (let ((result nil))
    (dolist (stim *stimuli* result)
      (dotimes (i n)
	(push stim result)))))

(defun generate-trials (stim-list)
  (mapcar #'make-trial stim-list))

(defun trial-rt (trial)
  (- (trial-response-time trial)
     (trial-onset-time trial)))

(defun trial-accuracy (trial)
  (if (equal (trial-correct-response trial)
	     (trial-actual-response trial))
      1
      0)) 

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
    (setf (task-phase task) 'stimulus)))


(defmethod respond ((task ritl-task) key)
  "Records a response in the PSS task"
  (unless (null (current-trial task))
    (let* ((trial (current-trial task))
	   (response (cdr (assoc key *responses*))))
      (set-trial-actual-response trial response)
      (when (act-r-loaded?)
	(set-trial-response-time (current-trial task)
				 (mp-time))
	(when *utility-learning-enabled*
	  (if (= 1 (trial-accuracy (current-trial task)))
	      (trigger-reward 1)
	      (trigger-reward -1)))
	(schedule-event-relative 0 #'next :params (list task))))))
            

(defmethod next ((task ritl-task))
  "Moves to the next step in a RITL Task timeline"
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
  (if (equalp (task-phase task) 'stimulus)
      (build-vis-locs-for (trial-stimulus (current-trial task))
			  vismod)
      (build-vis-locs-for (task-phase task)
			  vismod)))

(defmethod build-vis-locs-for ((trial list) vismod)
  (let ((results nil))
    (push  `(isa ritl-location 
		 kind ritl-stimulus
		 value stimulus
		 color black
		 screen-x 0
		 screen-y 0
		 height 400 
		 width 400
		 ,@trial)
	   results)
    (define-chunks-fct results)))

(defmethod build-vis-locs-for ((phase symbol) vismod)
  (let ((results nil))
    (push  `(isa ritl-location 
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
  (let ((new-chunk nil)
	(phase (task-phase task)))
    (if (equal phase 'stimulus)
	(setf new-chunk (vis-loc-to-obj (current-trial task) vis-loc))
	(setf new-chunk (vis-loc-to-obj phase vis-loc)))
    (fill-default-vis-obj-slots new-chunk vis-loc)
    new-chunk))


(defmethod vis-loc-to-obj ((stimulus list) vis-loc)
  "Transforms a stimulus into a visual object"
  (first (define-chunks-fct 
	     `((isa ritl-object
		    kind ritl-stimulus 
		    value ,(trial-stimulus stimulus)
		    )))))

(defmethod vis-loc-to-obj ((phase symbol) vis-loc)
  "Transforms a stimulus into a visual object"
  (first (define-chunks-fct 
	     `((isa ritl-object
		    kind ritl-screen 
		    value ,phase
		    )))))

;;; ------------------------------------------------------------------
;;; DATA FORMATTING
;;; ------------------------------------------------------------------

(defun ritl-reload (&optional (device (make-instance 'ritl-task)))
  "Reloads the current PSS model"
  (reload)
  (install-device device)
  (init device)
  (proc-display))
