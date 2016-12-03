;;; ------------------------------------------------------------------
;;; SIMON-SIMULATIONS.LISP
;;; ------------------------------------------------------------------
;;; A set of utilities for simulations and parameter fitting.  
;;; ------------------------------------------------------------------

;;; ----------------------------------------------------------------
;;; Running simulations
;;; ----------------------------------------------------------------

(defun decision-productions ()
  "Returns a sorted list of decision productions for a model, ('picks' first)"
  (let* ((prods (no-output (spp)))
	 (dos 
	  (remove-if-not #'(lambda (x) 
			     (string-equal (subseq (symbol-name x) 0 4) "proc")) 
			 prods))
	 (donts 
	  (remove-if-not #'(lambda (x) 
			     (string-equal (subseq (symbol-name x) 0 4) "dont")) 
			 prods)))
    (append (sort dos #'string< :key 'symbol-name)
	    (sort donts #'string< :key 'symbol-name))))

(defun decision-utilities (prods)
  "Returns a list of utilities associated with a list of productions"
  (mapcar #'(lambda (x) 
	      (caar (no-output (spp-fct (list x :u))))) 
	  prods))


(defun simulate-d2 (n vals &key (out t) (report t))
  " Generates a list of performances for varyig D2 values"
  (format out "~{~a~^, ~}~%" '("D2" "Con/ACC" "Con/RT" "In/ACC" "In/RT"))
  (dolist (v vals)
    (setf *d2* v)
    (let* ((res (simulate n :verbose nil :report report))
	   (nums (mapcar #'(lambda (x) (cons v x)) res)))
      (dolist (partial nums)
	(format out "~{~4,f~^, ~}~%" partial)))))


(defun simulate-d1-d2 (n vals &key (out t) (report t))
  "Generates a list of performances for varying D1 and D2 values"
  (format out "~{~a~^, ~}~%" '("D1" "D2" "Con/ACC" "Con/RT" "In/ACC" "In/RT"))
  (dolist (v1 vals)
    (dolist (v2 vals)
      (setf *d1* v1)
      (setf *d2* v2)
      (let* ((res (simulate n :verbose nil :report report))
	     (nums (mapcar #'(lambda (x) (append 
					  (list v1 v2) 
					  x)) 
			   res)))
	(dolist (partial nums)
	  (format out "~{~4,f~^, ~}~%" partial))))))

      (let* ((results (simulate n 
				:verbose nil 
				:report report)))
	(dolist (res results)
	  (let ((nums (mapcar #'float
			      (append (list v1 v2)
				      (apply #'append
					     (mapcar #'rest res))))))
	    (format out "~{~4,f~^, ~}~%" nums)))))))


(defun simulate (n &key (params nil) (verbose nil) (report t))
  "Simulates N runs of the model, and returns the results either as a list or as a report"
  (let ((results nil))
    (dotimes (i n (average-results results))
      (simon-reload :visicon nil)
      (when params
	;(sgp-fct (mapcan #'(lambda (x) (list (first x) (rest x))) params)))
	(sgp-fct (mapcan #'(lambda (x) x) params)))
      (sgp :v nil
	   :style-warnings nil
	   :model-warnings nil)
      (run 10000)
      (when verbose
	(when (= (mod i (round (/ n 10))) 0)
	  (let* ((c (round (/ (* 10 i) n)))
		 (empty (- 10 c))
		 (bar (make-string c :initial-element #\=))
		 (space (make-string empty :initial-element #\space))
		 (total (format nil "|~A~A| ~A#\%" bar space (* 10 c))))
	    
	    (format t total)
	    (finish-output))
	  (if *using-swank*
	      (format t "~%")
	      (dotimes (i 17)
		(write-char #\backspace)))))
      (push (append (analyze-log (experiment-log (current-device)))
		    (decision-utilities (decision-productions)))
	    results))
    (if report
	(list (average-results results))
	results)))


;;; -------------------------------------------------------------- ;;;
;;; PARAMETER SPACE PARTITIONING
;;; -------------------------------------------------------------- ;;;
;;; PSP analysis to examine how model parameters affect data
;;; patterns.
;;; Which are the data patterns of interest? 
;;; -------------------------------------------------------------- ;;;

 
;;; -------------------------------------------------------------- ;;;
;;; Model flexibility analysis
;;; -------------------------------------------------------------- ;;;
;;; Determines how much flexbility the model can produce, given
;;; parameters.
;;; -------------------------------------------------------------- ;;;



;;; -------------------------------------------------------------- ;;;
;;; Various attempts at efficiency
;;; -------------------------------------------------------------- ;;;

(defun incremental-average-results (avg current n)
  (if (null avg)
      current
      (let ((avg-cong-acc (second (first avg)))
	    (avg-cong-rt (third (first avg)))
	    (avg-incong-acc (second (second avg)))
	    (avg-incong-rt (third (second avg)))
	    (curr-cong-acc (second (first current)))
	    (curr-cong-rt (third (first current)))
	    (curr-incong-acc (second (second current)))
	    (curr-incong-rt (third (second current))))
	(list (list :congruent
		    (float (incremental-average avg-cong-acc curr-cong-acc n))
		    (float (incremental-average avg-cong-rt curr-cong-rt n)))
	      
	      (list :congruent
		    (float (incremental-average avg-incong-acc curr-incong-acc n))
		    (float (incremental-average avg-incong-rt curr-incong-rt n)))))))



(defun incremental-average (previous-mean current-value n)
  (+ previous-mean (/ (- current-value previous-mean) n)))


(defun inc-simulate (n)
  (let ((results nil))
    (dotimes (i n results)
      ;(print i)
      (simon4-reload :visicon nil)
      (sgp :v nil
	   :style-warnings nil
	   :model-warnings nil)
      (run 500)
    
      (setf results (incremental-average-results
		     results
		     (analyze-log (experiment-log (current-device)))
		     i)))))
					;(print results)

