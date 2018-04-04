;;; ==================================================================
;;; 2AFC SIMULATIONS CODE
;;; ==================================================================

;;; Params hould be given in the form:
;;;
;;;   ((:PARAM_1 VAL) (:PARAM_2 VAL) ... (:PARAM_N VAL))
;;;
;;; Note that this is NOT the form in which params are accepted by
;;; ACT-R functions, like sgp and sgp-fct
;;;
;;; Usage:
;;; ------
;;;
;;; (simulate 100 :params '((:ga 2.0) (:le 0.6)) :start 10 :filename "~/Documents/ga2_le06.csv")
;;;
(defun simulate (n &key (params nil) (start 0) (filename nil))
  "A generic function to run the model N times. Returns a table of performance measures with the params"
  (let ((results nil)
	(colnames (append (list "idx")
			  (mapcar #'(lambda (x)
				      (string-downcase
				       (format nil "~A" x)))
				  (mapcar #'first params))
			  (list "stim" "response" "rt"))))
    (format t "Beginning simulations in hyperpoint ~{~,2f~^, ~}~%" (mapcar #'second params))
    (dotimes (i n (append (list colnames) (reverse results)))
      (let ((p (make-instance '2afc-task)))
	;;(suppress-warnings (reload))
	(2afc-reload p)
	(sgp :v nil
	     :style-warnings nil
	     :model-warnings nil)
	
	;; Applies the necessary parameters

	(when params
	  (sgp-fct (flatten params)))

	;; Do the simulations

	(run 1000000)

	;; Create a list of trial-by-trial performance
	;; (with the corresponding parameter values)
	
	(let* ((formatted (extract-results p))
	       (information (cons (+ start i)
				  (mapcar #'second params))))
	  (dolist (trial formatted)
	    (push (append information trial)
		  results)))))

    ;; IF a filename is given, write a CSV file

    (let* ((rev (reverse results))
	   (final-version (push colnames rev)))
      
      (if filename
	  (write-csv final-version filename)
	  final-version))))
	

(defun write-csv (table filename)
  "Writes a list of trials (formatted as in 'extract-results') in a CSV file"
  (with-open-file (out filename
		       :direction :output
		       :if-exists :append
		       :if-does-not-exist :create)
    (dolist (row table)
      (format out "~{~a~^,~}~%" row))
    (finish-output out)))
