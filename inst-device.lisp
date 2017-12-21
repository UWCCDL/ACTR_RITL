;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Andrea Stocco
;;;
;;; Address     : Department of Psychology 
;;;             : University of Washington
;;;             : Seattle, WA 91195
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : inst-device.lisp
;;; Version     : 0.1
;;; 
;;; Description : Device, task, and additional modules for the INST task model.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; ----- History -----
;;;
;;; 2010-10-19  : * Fixed a bug that caused a function to remove items from
;;;             :   the exp window ieven when the window was NIL or invisible.
;;;             : * Fixed the internal log of latency and accuracy for each
;;;             :   trial.
;;;
;;; 2010-05-13  : * Added optional experimental window.
;;;
;;; 2010-05-12  : * Added 'floor' function when performing math calculations
;;;             :   in the math module---this ensures correct integer values 
;;;             :   in divisions.
;;;             : * Added a new visual-location object that encodes the entire
;;;             :   instructions (eg, "DAxCy").
;;;
;;; 2010-05-03  : * Fixed some bugs and added no-output macros.
;;; 
;;; 2010-04-30  : * Extended the device interface to distinguish between 
;;;             :   responded and non-responded probes (as it happend in the
;;;             :   real experiment
;;; 
;;; 2010-04-27  : * Math module up and running
;;;
;;; 2010-04-24  : * Added a skeleton of a math module to do arithmetic.
;;;
;;; 2010-01-21  : * File created.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#+:lispworks (setf (logical-pathname-translations "INST")
		   (list (list "**;*.*" 
			       (concatenate 'string
					    (format nil "~A" (make-pathname
							      :host 
							      (pathname-host *load-truename*)
							      :directory 
							      (pathname-directory 
							       *load-truename*))) 
                             "**/*.*"))))

#+(or :clisp :sbcl :openmcl) (setf (logical-pathname-translations "INST")
				   `(("**;*.*" ,(namestring (merge-pathnames "**/*.*" *load-truename*)))))


;;; -------------------------------------------------------------- ;;;
;;; UTILS
;;; -------------------------------------------------------------- ;;;

(unless (boundp '*whitespace*)
  (defconstant *whitespace* '(#\space #\newline #\tab) "White space"))

(unless (boundp '*null-string*)
  (defconstant *null-string* "" "The null string"))

(defun tokenize (str &optional (blank *whitespace*))
  "Divides a string into a list of words"
  (let ((tokens '())
        (token '()))
  (dotimes (i (length str) tokens)
    (if (member (char str i) blank)
        (unless (null token)
          (push token tokens)
          (setf token '()))
      (push (char str i) token)))
  (unless (null token)
    (push token tokens))
  (reverse (mapcar #'(lambda (x) (concatenate 'string (reverse x))) tokens))))


;;; PROCESS-STRING
;;; 
;;; Splits a task string into a list
;;;
;;; E.g. (process-string "BUxUy")
;;; --> ("B" "U" "x" "U" "y")
;;;
(defun process-string (str)
  "Creates a list of instructions from a string"
  (let ((res nil)) 
  (dotimes (i (length str) (reverse res))
    (push (subseq str i (1+ i)) res))))


;; PICK
;;
;; Picks an element at random from a list
;;
(defun pick (lst)
  "Picks up an element from a list"
  (when  (listp lst)
    (elt lst (random (length lst)))))


;; SCRAMBLE
;;
;; Scrambles the elements of a list.  Basically, it keeps picking up
;; elements at random and removing them from the list.
;; 
;; This version does not  work when the same element is repeated more 
;; than once.  
;;
;; e.g.  (scramble '(1 2 3 4 4))
;; -->   (4 2 3 1)
;;
(defun scramble (lst &optional (sofar nil))
  "Scrambles a list of different elements"
  (if (null lst)
      sofar
    (let ((picked (pick lst)))
      (scramble (remove picked lst) (cons picked sofar)))))


;; SCRAMBLE* 
;;
;; Scrambles the elements of a list.  It is an optimized version of
;; SCRAMBLE, and works in every condition.  It works by creating a
;; list of the positions of the elements, e.g. a list (0 1 2...N),
;; where N is the length of the list.  Then, it scrambles the 
;; positions (and not the elements).  Finally, it maps each position
;; with the corresponding elements.  Repeated elements do not 
;; constitute a problem.
;;
;; e.g.  (scramble* '(1 2 3 4 4))
;; -->   (4 3 4 1 2)
;;
(defun scramble* (lst)
  "Scrambles any list of objects"
  (let ((l (length lst))
        (pos nil))
    (dotimes (i l)
      (push i pos))
    (mapcar #'(lambda (x) (elt lst x)) (scramble pos))))


;;; -------------------------------------------------------------- ;;;
;;; TRIALS
;;; -------------------------------------------------------------- ;;;

;;; A Trial in the INST experiment
;;; (definition taken from the INS code)
;;;
(defstruct trial
  (task nil)
  (operators nil)
  (length 0 :type integer)
  (function nil)
  (structure nil)
  (delay-x 0 :type integer)
  (delay-y 0 :type integer)
  (total-delay 0 :type integer)
  (max-delay 0 :type integer)
  (asymmetry 0 :type integer)
  (nesting 0 :type integer)
  (instructions nil)
  (x 0 :type integer)
  (y 0 :type integer)
  (inputs nil :type list)
  (result 0)
  (intermediate-results nil :type list)
  (foil 0 :type integer)
  (probe 0 :type integer)
  (type nil)
  (correct 2 :type integer)
  (practiced nil)
  (complexity nil)
  (input-string *null-string*))

(defun load-trials (filename)
  "Loads a set of trials from a text file"
  (with-open-file (file filename :direction :input)
    (let ((res '()))
      (do ((line (read-line file) (read-line file nil 'eof)))
	  ((eql line 'eof) (reverse res))
	(let ((tokens (tokenize line '(#\Tab))))
	  (when (trial-list? tokens)
	    (push (create-trial-from-list tokens) res))))
      res)))


(defun trial-list? (lst)
  "Checks whether a list represents a trial"
  (and (= (length lst)  22)
       (every #'numberp (mapcar #'read-from-string (subseq lst 5 17)))))

(defun create-trial-from-list (lst)
  "Creates a trial from a valid list of elements"
  (make-trial :instructions (nth 0 lst)
	      :function (read-from-string (nth 1 lst))
	      :intermediate-results (read-from-string (nth 2 lst))
	      :operators (read-from-string (nth 3 lst))
	      :structure (nth 4 lst)
	      :length (read-from-string (nth 5 lst))
	      :delay-x (read-from-string (nth 6 lst))
	      :delay-y (read-from-string (nth 7 lst))
	      :total-delay (read-from-string (nth 8 lst))
	      :max-delay (read-from-string (nth 9 lst))
	      :asymmetry (read-from-string (nth 10 lst))
	      :nesting (read-from-string (nth 11 lst))
	      :x (read-from-string (nth 12 lst))
	      :y (read-from-string (nth 13 lst))
	      :result (read-from-string (nth 14 lst))
	      :foil (read-from-string (nth 15 lst))
	      :probe (read-from-string (nth 16 lst))
	      :type (read-from-string (nth 17 lst))
	      :correct (read-from-string (nth 18 lst))
	      :practiced (read-from-string (nth 19 lst))
	      :input-string (nth 20 lst)
	      :complexity (read-from-string (nth 21 lst))))


;;; -------------------------------------------------------------- ;;;
;;; TASK MANAGER
;;; -------------------------------------------------------------- ;;;


;;; -------------------------------------------------------------- ;;;
;;; EXPERIMENTAL WINDOW
;;; -------------------------------------------------------------- ;;;

(defparameter *experimental-window-visible?* nil)

(defparameter *window* nil)

(defparameter *reference-time* 0)

(defclass task-manager ()
  ((trials :accessor trials
	   :initform nil)
   (state :accessor state
	  :initform nil)
   (current-trial :accessor current-trial
		  :initform nil)
   (current-trial-index :accessor current-trial-index
			:initform nil)
   (current-trial-log :accessor current-trial-log
		      :initform nil)
   (experiment-log :accessor experiment-log
		   :initform nil))
  (:documentation "A manager for the INST task"))


(defmethod reset-manager ((tm task-manager))
  (setf (current-trial-index tm) 0)  ;;
  (if (trials tm)
      (progn
	(setf (state tm) 'fixation1)
	(setf (current-trial tm) (nth (current-trial-index tm) (trials tm))))
      (setf (state tm) nil))
  (setf (experiment-log tm) nil)
  (setf (current-trial-log tm) 
	(list (trial-complexity (current-trial tm))
	      (trial-practiced (current-trial tm))
	      (trial-instructions (current-trial tm))
	      (current-trial-index tm)))
  (when *experimental-window-visible?*
    (setf *window*
	  (open-exp-window "INST Experiment"                                      				
			   :visible t
			   :width 400
			   :height 300
			   :x 300
			   :y 300)))
  (schedule-task-update tm))


(defun act-r-loaded? ()
  "Cheap hack to check whether ACTR is loaded"
  (and (fboundp 'run-n-events)
       (fboundp 'start-environment)))

(defun can-schedule-event? ()
  "An event can be scheduled if ACT-R is laoded and there is a model"
  (and (act-r-loaded?)
       (current-model)))

(defparameter *state-transitions* '((fixation1 . instructions)
				    (instructions . blank1)
				    (blank1 . fixation2)
				    (fixation2 . inputs)
				    (inputs . blank3)
				    (blank3 . probe)
				    (probe . feedback)
				    (feedback . blank4)))
				    

(defparameter *blanks* '(blank1 blank2 blank3 blank4))

(defparameter *fixations* '(fixation1 fixation2))

(defparameter *wait-states* '(instructions inputs))

(defmethod last-trial? ((tm task-manager))
  "Checks whether the current trial is also the last one"
  (and (not (null (trials tm)))
       (equal (current-trial tm)
	      (first (last (trials tm)))))) 	

(defmethod update-window ((tm task-manager))
  (let ((state (state tm)))
    (when *experimental-window-visible?*
      (remove-all-items-from-rpm-window *window*)
      (cond ((equal state 'fixation1)
	     (add-text-to-exp-window :text "+"
				     :width 10
				     :x 195
				     :y 145))
	    ((equal state 'fixation2)
	     (add-text-to-exp-window :text "*"
				     :width 10
				     :height 10
				     :x 195
				     :y 145))
	    ((or (equal state 'probe)
		 (equal state 'probe-responded))
	     (add-text-to-exp-window :text (format nil "~A" (trial-probe (current-trial tm)))
				     :width 10
				     :height 10
				     :x 195
				     :y 145))
	    ((equal state 'inputs)
	     (add-text-to-exp-window :text (format nil "~A" (trial-x (current-trial tm)))
				     :width 10
				     :x 150
				     :y 145)
	     (add-text-to-exp-window :text (format nil "~A" (trial-y (current-trial tm)))
				     :width 10
				     :x 250
				     :y 145))
	    ((equal state 'instructions)
	     (let ((elements (mapcar 'read-from-string (process-string (trial-instructions (current-trial tm)))))
		   (x 145))
	       (loop for e in elements
		  do
		    (add-text-to-exp-window :text (format nil "~A" e)
					    :width 10
					    :x x
					    :y 145)
		    (incf x 20))))
	   
	    (t
	     nil)))))

;; Will work for most platform but will likely offset the fixation ring.
;;
(defmethod device-update-attended-loc ((tm task-manager) xyloc)
 "Updates the attention focus on the window"
 (when *window*
   (device-update-attended-loc *window* xyloc))) 

;; THIS IS UI-SPECIFIC WILL NOT WORK ON ANY PLATFORM
;;
;(defmethod device-update-attended-loc ((tm task-manager) xyloc);
;  "Updates the attention focus on the window"
;  (when *window*
;    (device-update-a-named-object *window* 'focus-ring xyloc)))

(defmethod next-trial ((tm task-manager))
  "Moves to the next trial (if possible)"
  (unless (null (trials tm))
    (setf (experiment-log tm)
	  (append (experiment-log tm) (list (reverse (current-trial-log tm)))))

    (cond ((not (last-trial? tm))
	   (incf (current-trial-index tm))
	   (setf (current-trial tm)
		 (nth (current-trial-index tm) (trials tm)))
	   (setf (current-trial-log tm)
		 (list (trial-complexity (current-trial tm))
		       (trial-practiced (current-trial tm))
		       (trial-instructions (current-trial tm))
		       (current-trial-index tm) ))
	   (setf (state tm) 'fixation1)
	   (schedule-task-update tm))
	  
	  ;; If it's the last trial, just stop ACT-R
	  ((last-trial? tm)
	   (setf (current-trial-log tm) nil)
	   (when (act-r-loaded?)
	     (schedule-break-relative 0 :details "Experiment is finished"))))))

(defun schedule-task-update (tm)
  "Schedules the next update of the trial manager"
  (when (act-r-loaded?)
    (proc-display)
    (update-window tm)
    (unless (member (state tm) *wait-states*)
      (let ((duration 0))
	(cond ((member (state tm) *blanks*)
	       (setf duration (+ 2 (* 2 (random 3)))))
	      ((member (state tm) *fixations*)
	       (setf duration 2))
	      ((equal (state tm) 'probe)
	       (setf duration 2))
	      ((equal (state tm) 'feedback)
	       (setf duration 2)))
	(schedule-event-relative duration #'next :params (list tm))))))

(compile (defun inst-report (str) (declare (ignore str)) nil))

(defmethod next ((tm task-manager) &key (verbose t))
  "Moves on to the next state"
  (when (act-r-loaded?)
    (setf *reference-time* (mp-time)))
  (let* ((current-state (state tm))
	 (next-state (cdr (assoc current-state *state-transitions*))))
    (when verbose
      (let ((str (format nil "FROM ~a TO ~a, TRIAL ~a" current-state next-state (current-trial-index tm))))
	(schedule-event-relative 0 'inst-report :params `(,str) :output 'low :module 'device)))
    ;(update-window tm)
    (cond (next-state
	   (setf (state tm) next-state)
	   (schedule-task-update tm))

	  ;; If there is no next state, we are at the end
	  ;; of a trial.
	  ((null next-state)
	   (next-trial tm)))))
	   	  

(defmethod record-latency ((tm task-manager))
  "Records the model's latency"
  (when (act-r-loaded?)  ;; Only if we are running a model
    (let ((current-time (mp-time)))
      (push (- current-time *reference-time*) (current-trial-log tm)))))

(defmethod submit-keypress ((tm task-manager) val)
  "Handle's a model's keypress"
  (let ((state (state tm)))
    (cond ((member state *wait-states*)
	   (record-latency tm)
	   (next tm))
	  ((equal state 'probe)
	   (record-latency tm)
	   (submit-response tm val))
	  (t
	   nil))))

(defmethod submit-response ((tm task-manager) val)
  "Submits a Y/N response to the task manager"
  (let* ((trial (current-trial tm))
	 (correct (= val (trial-correct trial))))
    (push correct (current-trial-log tm))
    (when (act-r-loaded?)
      (proc-display))))
    

(defmethod trial-responded? ((tm task-manager))
  (let* ((current (current-trial-log tm))
	 (response (first current)))
    (and (> (length current) 1)
	 (or (equal t response)
	     (equal nil response)))))
	   
;;; -------------------------------------------------------------- ;;;
;;; DATA ANALYSIS
;;; -------------------------------------------------------------- ;;;
;;; These are functions for processing a model's experimental log.
;;; -------------------------------------------------------------- ;;;

(defun mean (&rest nums)
  (when (every #'numberp nums)
    (/ (reduce #'+ nums)
       (length nums))))

(defun trial-log? (trial-log)
  (and (listp trial-log)
       (every #'atom (list (nth 2 trial-log)
			   (nth 3 trial-log)
			   (nth 7 trial-log)))
       (every #'numberp (cons (nth 0 trial-log)
			      (subseq  trial-log 4 7)))))
	      
(defun trial-practiced? (trial-log)
  (when (trial-log? trial-log)
    (nth 2 trial-log)))

(defun trial-correct? (trial-log)
  (first (reverse trial-log)))

(defun stats (log)
  "Short report on model's performance"
  (when (every #'trial-log? log)
    (let* ((correct (remove-if-not #'trial-correct? log)) 
	   (practiced (remove-if-not #'trial-practiced? correct))
	   (novel (remove-if #'trial-practiced? correct))
	   (ip (apply #'mean (mapcar #'(lambda (x) (nth 4 x)) practiced)))
	   (in (apply #'mean (mapcar #'(lambda (x) (nth 4 x)) novel)))
	   (xp (apply #'mean (mapcar #'(lambda (x) (nth 5 x)) practiced)))
	   (xn (apply #'mean (mapcar #'(lambda (x) (nth 5 x)) novel))))
      `(:accuracy ,(/ (length correct) 80) :ip ,ip :in ,in :xp ,xp :xn ,xn))))

;;; -------------------------------------------------------------- ;;;
;;; DEVICE INTERFACE
;;; -------------------------------------------------------------- ;;;
;;; The task manager doubles as an ACT-R device. This makes it 
;;; always accessible by calling '(current-device)'. To be a valid
;;; device the task manager has to implement a number of methods
;;; -------------------------------------------------------------- ;;;

(defmethod device-handle-keypress ((device task-manager) key)
  "Converts the key into a symbol and passes it on to the task manager"
  (let ((val (read-from-string (format nil "~a" key))))
    (submit-keypress device val)))
			   
(defmethod device-handle-click ((device task-manager))
  "Does nothing"
  (declare (ignore device))
  nil)

(defmethod device-move-cursor-to ((device task-manager) pos)
  "Does nothing"
  (declare (ignore device))
  nil)

(defconstant *mouse-pos* (vector 0 0) "A fixed mouse position at (0,0)")

(defmethod get-mouse-coordinates ((device task-manager))
  "Does nothing"
  (declare (ignore device))
  *mouse-pos*)

(defmethod cursor-to-vis-loc ((device task-manager))
  (declare (ignore device))
  nil)

(defun build-vis-loc-for-fixations (tm)
  "Creates a visual-loc for a fixation with either '*' or '+' @ 195, 145"
  (let ((fixation nil))
    (case (state tm)
      (fixation1 (setf fixation '+))
      (fixation2 (setf fixation '*))
      (otherwise nil))
    (list `(isa visual-location 
		kind fixation
		value ,fixation
		screen-x 195
		screen-y 145
		height 10
		width 10))))

(defun build-vis-loc-for-instructions (tm)
  "Creates five visual locations for the five letters of the intructions"
  (let* ((elements (mapcar 'read-from-string (process-string (trial-instructions (current-trial tm)))))
	 (task (trial-instructions (current-trial tm)))
	 (x 155))
    (let ((locations (loop for e in elements
			collect
			  (let ((kind 'operation))
			    (when (member e '(x y))
			      (setf kind 'variable))
			    `(isa visual-location 
				  kind ,kind 
				  value ,e 
				  screen-y 145 
				  screen-x ,x 
				  height 10 
				  width 10))
			do
			  (incf x 20)))
	  (instructions `(isa visual-location
			      kind instructions
			      value ,task
			      screen-y 145
			      screen-x 155
			      height 10
			      width 50)))
      (push instructions locations))))
      
(defun build-vis-loc-for-inputs (tm)
  "Creates two vis-locs @ 150,145 (x) and 250,145 (y)"
  (let* ((trial (current-trial tm))
	 (x (trial-x trial))
	 (y (trial-y trial)))
    (list `(isa visual-location
		kind x
		value ,x
		screen-x 150
		screen-y 145
		height 10
		width 10)
	  `(isa visual-location
		kind y
		value ,y
		screen-x 250
		screen-y 145
		height 10
		width 10))))

(defun build-vis-loc-for-probe (tm)
  (list `(isa visual-location
	      kind probe
	      value ,(trial-probe (current-trial tm))
	      screen-x 195
	      screen-y 145
	      height 10
	      width 10)))

(defun build-vis-loc-for-feedback (tm)
  (let ((correct (first (current-trial-log tm))))
    (list `(isa visual-location
			 kind feedback
			 value ,correct
			 screen-x 195
			 screen-y 145
			 height 10
			 width 10))))

(defun build-vis-loc-for-screen (tm)
  (let ((state (state tm)))
    (cond ((member state *blanks*)
	   (setf state 'blank))
	  ((and (equal state 'probe)
		(trial-responded? tm))
	   (setf state 'probe-responded)))
    (list `(isa visual-location 
		kind screen 
		value ,state 
		screen-x 0 
		screen-y 0 
		height 300 
		width 400))))

(defmethod build-vis-locs-for ((device task-manager) vismod)
  "Creates a list of visual locations"
  (let ((screen (build-vis-loc-for-screen device))
	(other-chunks nil))
  (case (state device)
    ((fixation1 fixation2) (setf other-chunks (build-vis-loc-for-fixations device)))
    (instructions (setf other-chunks (build-vis-loc-for-instructions device)))
    (feedback (setf other-chunks (build-vis-loc-for-feedback device)))
    (probe (setf other-chunks (build-vis-loc-for-probe device)))
    (inputs (setf other-chunks (build-vis-loc-for-inputs device)))
    (otherwise (setf other-chunks (build-vis-loc-for-screen device))))
  (funcall #'define-chunks-fct (append screen other-chunks))))


(defmethod vis-loc-to-obj ((device task-manager) vis-loc)
  "Transforms a visual-loc into a visual object"
  (let ((kind (chunk-slot-value-fct vis-loc 'kind))
	(value (chunk-slot-value-fct vis-loc 'value))
	(responded (trial-responded? device))
	(new-chunk nil))
    
    (cond ((equal kind 'screen)
	   (setf new-chunk (first (define-chunks-fct `((isa visual-object value ,value status ,kind))))))
	  (t
	   (setf new-chunk (first (define-chunks-fct `((isa visual-object value ,value status ,kind)))))))
    (fill-default-vis-obj-slots new-chunk vis-loc)
    new-chunk))

 
;;; -------------------------------------------------------------- ;;;
;;; MATH MODULE
;;; -------------------------------------------------------------- ;;;
;;; The math module is an elemetary ACT-R module that performs 
;;; simple math calculations.
;;; -------------------------------------------------------------- ;;; 


;;; A Math Module is just a simple struct with a slot that
;;; determines whether the module is busy.
(defstruct math-module busy)

(defun set-math-module-free (mod)
  (setf (math-module-busy mod) nil))

(defun set-math-module-busy (mod)
  (setf (math-module-busy mod) t))

(defun create-math-module (model-name)
  "Creates a math moduke (a simple struct)"
  (declare (ignore model-name))
  (make-math-module))

(defun delete-math-module (arg)
  "Deletes the math module (in fact, does nothing)"
  (declare (ignore arg)))

(defun reset-math-module (mod)
  "Resets the math module (and adds the chunk-type 'calculation')"  
  (declare (ignore mod))
  (chunk-type calculation operation arg1 arg2 result))

;;; MATH-MODULE-QUERY
;;; --------------------------------------------------------------
;;; A query that handles the basic state checks, i.e. state free,
;;; busy, error.  
;;; --------------------------------------------------------------
(defun math-module-query (mod buffer slot value)
  "Simple query function for the math module"
  (case slot
    (state
     (case value
       (error nil)
       (busy (math-module-busy mod))
       (free (not (math-module-busy mod)))
       (t (print-warning "Bad state query to ~s buffer" buffer))))
    (t (print-warning 
     "Invalid slot ~s in query to buffer ~s" query buffer))))


;;; MATH-BUFFER-READY?
;;; --------------------------------------------------------------
;;; Checks whether the math buffer is available for calculation.
;;; A calculation is possible when the chunk in the mat buffer
;;; is of type "calculation", and has one operation and both args 
;;; set to an admissible value (numberp and non-nil)
;;; --------------------------------------------------------------
(defun math-buffer-ready? ()
  "Checks  whether the math buffer is ready to perfom a calculation"
  (let ((contents (first (no-output (buffer-chunk-fct '(math))))))
    (and contents
	 (equal 'calculation (chunk-chunk-type-fct contents))
	 (chunk-slot-value-fct contents 'operation)
	 (chunk-slot-value-fct contents 'arg1)
	 (numberp (chunk-slot-value-fct contents 'arg1))
	 (chunk-slot-value-fct contents 'arg2)
	 (numberp (chunk-slot-value-fct contents 'arg1)))))
			  

(defun calculate-math-delay (op arg1 arg2)
  "Estimates the time needed to perform a math calculation"
  0.2)


;;; CHECK-MATH-BUFFER
;;; --------------------------------------------------------------
;;; If the math buffer is ready (see above), then perform the 
;;; required calculation
;;; --------------------------------------------------------------
(defun check-math-buffer (module)
  "Checks whether a calculation is possible in the Math buffer" 
  (when (math-buffer-ready?)
    (let* ((contents (first (no-output (buffer-chunk-fct '(math)))))
	   (op (chunk-slot-value-fct contents 'operation))
	   (arg1 (chunk-slot-value-fct contents 'arg1))
	   (arg2 (chunk-slot-value-fct contents 'arg2))
	   (result (floor (funcall op arg1 arg2)))
	   (delay (calculate-math-delay op arg1 arg2)))
      ;; Set the math module busy
      (set-math-module-busy module)
      (schedule-mod-buffer-chunk 'math `(result ,result) delay :module 'math :priority -100) 
      (schedule-event-relative delay 'set-math-module-free :params (list module) :priority -101))))

  

;;; MATH-MODULE-REQUEST
;;; --------------------------------------------------------------
;;; Handles requests to put chunks in the math module
;;; --------------------------------------------------------------
(defun math-module-request (module buffer spec)
  "Handles specifications for representations of calculations"
  (declare (ignore buffer))
  (if (math-module-busy module)
      (model-warning "Cannot handle request when busy")
      
      ;; If the module is free, just create a chunk in a goal-like
      ;; fashion
      (progn
	(goal-style-request module 'math spec 0)
	(schedule-event-relative 0.05 'check-math-buffer :params (list module)))))


;;; MATH-MODULE-MOD
;;; --------------------------------------------------------------
;;; Handles a modification request.
;;; --------------------------------------------------------------
(defun math-module-mod (module buffer mods)
  "Modifies a chunk in the math buffer" 
  (if (math-module-busy module)
      (model-warning "Cannot handle a modification request when busy")
      (progn
	(goal-style-mod-request module 'math mods 0)
	(schedule-event-relative 0.05 'check-math-buffer :params (list module)))))


;;; MATH MODULE DEFINITION
;;; --------------------------------------------------------------
;;; Finally, let's define the module
;;; --------------------------------------------------------------
(define-module-fct 'math '(math) nil    
   :version "1.0"
   :documentation "Simple module for doing quick math"
   :creation 'create-math-module
   :delete 'delete-math-module
   :reset 'reset-math-module 
   :request 'math-module-request
   :query 'math-module-query
   :buffer-mod 'math-module-mod
)


;;; --------------------------------------------------------------
;;; IMAGINAL MODULE
;;; --------------------------------------------------------------
;;; The imaginal module is extended by defining one additional
;;; type of generic-action, ie. 'inst-update-position'.
;;; This action simply updates the mental index of the sequence
;;; of operations that one must perform.
;;; --------------------------------------------------------------   

(defparameter *position-update-delay* 0.1)

(defun inst-update-position ()
  "Updates the serial position of an operator in the imaginal 'scratchpad' buffer"
  (let* ((scratchpad (first (no-output (buffer-chunk-fct '(imaginal)))))
	 (pos (chunk-slot-value-fct scratchpad 'position))
	 (newpos (1+ pos)))
    (schedule-mod-buffer-chunk 'imaginal `(position ,newpos) *position-update-delay* :module 'imaginal :priority -100)
    (schedule-event-relative *position-update-delay* 'set-imaginal-free :module 'imaginal :priority -101 :params nil))) 
    

