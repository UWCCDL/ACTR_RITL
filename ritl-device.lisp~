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
;;; 2AFC TASK
;;; ------------------------------------------------------------------

;; ---------------------------------------------------------------- ;;
;; Data structures and parameters for the task
;; ---------------------------------------------------------------- ;;

(defparameter *stimuli* '(correct incorrect))

(defparameter *rules* '((correct . left) (incorrect . right)))

(defparameter *responses* '((f . left) (j . right)))

(defun stimulus? (stim)
  (member stim *stimuli*))

(defun stimulus-correct-response (stim)
  "The correct answers is always that associated with the 'correct' stimulus (in this case, always left)"
  (when (stimulus? stim)
    (cdr (assoc 'correct *rules*))))

(defun make-trial (stim)
  (when (2afc-stimulus? stim)
    (list stim 0 0 (stimulus-correct-response stim) nil)))

(defun trial-stimulus (trial)
  (nth 0 trial))

(defun set-trial-stimulus (trial stimulus)
  (when (2afc-stimulus? stimulus)
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

(defclass 2afc-task ()
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
  (:documentation "A manager for the 2AFC task"))

(defmethod init ((task 2afc-task))
  "Initializes the 2AFC task manager"
  (unless (null (trials task))
    (setf (index task) 0)
    (setf (experiment-log task) nil)
    (setf (trials task) (scramble* (trials task)))
    (setf (current-trial task)
	  (nth (index task) (trials task)))
    (setf (task-phase task) 'stimulus)))


(defmethod respond ((task 2afc-task) key)
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
            

(defmethod next ((task 2afc-task))
  "Moves to the next step in a 2AFC Task timeline"
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
;;; These functions turn the 2AFC-Task class into an ACT-R device
;;; ------------------------------------------------------------------

(defmethod device-handle-keypress ((task 2afc-task) key)
  "Converts the key into a symbol and passes it on to the task manager"
  (respond task (intern (string-capitalize (format nil "~a" key)))))

			   
(defmethod device-handle-click ((task 2afc-task))
  "Does nothing"
  (declare (ignore task))
  nil)

(defmethod device-move-cursor-to ((task 2afc-task) pos)
  "Does nothing"
  (declare (ignore task))
  nil)


(defmethod get-mouse-coordinates ((task 2afc-task))
  "Does nothing"
  (declare (ignore task))
  (vector 0 0))

(defmethod cursor-to-vis-loc ((task 2afc-task))
  "Does nothing"
  (declare (ignore task))
  nil)

(defmethod build-vis-locs-for ((task 2afc-task) vismod)
  (if (equalp (task-phase task) 'stimulus)
      (build-vis-locs-for (trial-stimulus (current-trial task))
			  vismod)
      (build-vis-locs-for (task-phase task)
			  vismod)))

(defmethod build-vis-locs-for ((trial list) vismod)
  (let ((results nil))
    (push  `(isa 2afc-location 
		 kind 2afc-stimulus
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
    (push  `(isa 2afc-location 
		 kind 2afc-location
		 value ,phase
		 color black
		 screen-x 0
		 screen-y 0
		 height 400 
		 width 400)
	   results)
    (define-chunks-fct results)))


(defmethod vis-loc-to-obj ((task 2afc-task) vis-loc)
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
	     `((isa 2afc-object
		    kind 2afc-stimulus 
		    value ,(trial-stimulus stimulus)
		    )))))

(defmethod vis-loc-to-obj ((phase symbol) vis-loc)
  "Transforms a stimulus into a visual object"
  (first (define-chunks-fct 
	     `((isa 2afc-object
		    kind 2afc-screen 
		    value ,phase
		    )))))

;;; ------------------------------------------------------------------
;;; DATA FORMATTING
;;; ------------------------------------------------------------------

(defun format-results (device &optional (stream t))
  (dolist (trial (reverse (experiment-log device)))
    (format stream "~A,~A,~A~%"
	    (trial-stimulus trial)
	    (trial-rt trial)
	    (trial-accuracy trial))))
	    

(defun 2afc-reload (&optional (device (make-instance '2afc-task)))
  "Reloads the current PSS model"
  (reload)
  (install-device device)
  (init device)
  (proc-display))

