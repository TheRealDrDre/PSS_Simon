;;;
;;; Searches through parameter space
;;;
;;; 3^N -1
;;;

;;; RESULTS 1(E= 61739): ((:ANS . 0.10000001) (:LE . 0.6750001) (:LF . 0) (:EGS . 0.1)
;;;  (:ALPHA . 0.040000007) (:IMAGINAL-ACTIVATION . 7.0) (:VISUAL-ACTIVATION . 0))
;;; RESULTS 2: (E= 62479) ((:ANS . 0.2) (:LE . 0.6750001) (:LF . 0.05) (:EGS . 0.3) (:ALPHA . 0.06999999)
;;; (:IMAGINAL-ACTIVATION . 8.0) (:VISUAL-ACTIVATION . 1.5))

;;; RESULTS 3 (E = 32)
;;; ((:ANS . 0.40000004) (:LE . 0.70000005) (:LF . 0.05) (:EGS . 0.2)
;;; (:ALPHA . 0.08) (:IMAGINAL-ACTIVATION . 10) (:VISUAL-ACTIVATION . 1.5))
;;;
;;; RESULTS 4 (E = 30.89)
;;; ((:ANS . 0.01) (:LE . 0.9250001) (:LF . 0.40000004) (:EGS . 0.05000001)
;;; (:ALPHA . 0.08) (:IMAGINAL-ACTIVATION . 10.0) (:VISUAL-ACTIVATION . 2.0))
;;;
;;; RESULTS 5 (E = 30.74)
;;; ((:ANS . 0.11) (:LE . 0.9000001) (:LF . 0.3) (:EGS . 0.10000001)
;;;  (:ALPHA . 0.06999999) (:IMAGINAL-ACTIVATION . 7.0) (:VISUAL-ACTIVATION . 7.5))

(defparameter *params* '((:ans 0.1 0.01 0.5 0.05)
			 (:le 0.75 0.1 1 0.025)
			 (:lf 0.3 0.05 2.0 0.05)
			 (:egs 0.2 0 1 0.05)
			 (:alpha 0.05 0.01 0.5 0.01)
			 (:imaginal-activation 3.0 0 10 0.5)
			 (:visual-activation 1.0 0 10 0.5)))
			 

(defparameter *results* '((:CONGRUENT 0.977 0.425093)
			  (:INCONGRUENT 0.882 0.494868)))

(defun same (x)
  "Returns the sam evalue"
  x)

(defun asinsqrt (x)
  "Normalizes accuracy data 0 < x < 1"
  (asin (sqrt x)))

(defun vectorize (res)
  "Transforms a structured results into a list of 4 comparable numbers"
  (mapcar #'(lambda (x) (* 1000 x))
	  (mapcar #'funcall '(asinsqrt
			      same
			      asinsqrt
			      same)
		  (apply #'append (mapcar #'rest res)))))
	

(defun energy (results &optional (comparison *results*))
  "Calculates the energy, as the distance from the ideal state"
  (let ((current (vectorize results))
	(milestone (vectorize comparison)))
    (/ (reduce #'+
	       (mapcar #'(lambda (x y) (expt (- x y) 2))
		       current milestone))
       2000)))


(defun param-list? (struct)
  "A list made of param name, start value, min value, max value, step size"
  (and (= (length struct) 5)
       (every #'numberp (rest struct))
       (keywordp (first struct))))

(defun param-name (lst)
  (first lst))

(defun param-start-value (lst)
  (second lst))

(defun param-min-value (lst)
  (third lst))

(defun param-max-value (lst)
  (fourth lst))

(defun param-step-size (lst)
  (fifth lst))

(defun 1d-point? (lst)
  (and (consp lst)
       (keywordp (first lst))
       (numberp (rest lst))))

(defun 1d-neighborhood (1d-point param-list)
  "Returns the neighbors of a 1D point"
  (let* ((dimension (first 1d-point))
	 (param (assoc dimension param-list))
	 (step (param-step-size param))
	 (v-min (param-min-value param))
	 (v-max (param-max-value param))
	 (current (rest 1d-point))
	 (low (max (- current step) v-min))
	 (high (min (+ current step) v-max)))
    (mapcar #'(lambda (x) (cons dimension x))
	    (list low current high))))

(defun hyperpoint? (lst)
  (and (> (length lst) 1)
       (every #'1d-point? lst)))

(defun create-hyperpoint (param-lists)
  (reverse (pairlis (mapcar #'param-name param-lists)
		    (mapcar #'param-start-value param-lists))))

(defun equal-hyperpoints (hp1 hp2)
  "Two hyperpoits are equal if they have the same values for each coordinate" 
  (let ((p1 (sort hp1 #'string-lessp :key #'first))
	(p2 (sort hp2 #'string-lessp :key #'first)))
    (equalp p1 p2)))


(defun cartesian-product (lst)
  (if (= (length lst) 1)
      (mapcar #'list (first lst))
      (let ((results nil)
	    (a (first lst))
	    (b (cartesian-product (rest lst))))
	(dolist (i a results)
	  (dolist (j b)
	    (push (push i j) results))))))  

(defun hyperneighborhood (hyperpoint param-lists)
  (remove-duplicates
   (cartesian-product (mapcar #'(lambda (x) (1d-neighborhood x param-lists))
			      hyperpoint))))

(defun evaluate-hyperpoint (hyperpoint &optional (n 100))
  (energy (simulate n :params hyperpoint :verbose nil)))

(defun simulated-annealing (param-lists &key (start-temp 10) (end-temp 0.001) (frac 0.95))
  (format t "Starting SA...~%")
  (force-output)
  (let* ((current (create-hyperpoint param-lists))
	 (e0 (evaluate-hyperpoint current))
	 (ctemp start-temp))
    
    (while (> ctemp end-temp)
      (format t "Temp = ~5,f, Energy = ~5,f~%" ctemp e0)
      (force-output)
      (let* ((next (pick (remove current (hyperneighborhood current param-lists))))
	     (e1 (evaluate-hyperpoint next))
	     (delta (- e1 e0)))
	(format t "  New energy = ~2,f~%" e1)
	(cond ((< delta 0)
	       (setf current next)
	       (setf e0 e1))
	      (t
	       (let ((rand (random 0.999999))
		     (metro (exp (/ (* -1 delta) ctemp))))
		 (format t "     Metro: ~6,f~%" metro)
		 (when (< rand metro)
		   (setf current next)
		   (setf e0 e1))))))
      (setf ctemp (* frac ctemp)))
    current))
