;;;
;;; Searches through parameter space
;;;
;;; 3^N - 1
;;;
(defparameter *params* '((:GA 1.0 0 10 0.5)
			 (:ALPHA 0.05 0 1 0.05)
			 (:D1 0 10 1)))

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

(defun hyper-coordinates (param-lists)
  (reverse (pairlis (mapcar #'param-name param-lists)
		    (mapcar #'param-start-value param-lists))))

(defun equal-hypercoordinates (lsts)
  (

(defun hyper-neighbours (param-lists)
  

(defun param-search (param-lists n data function)
  ;;; Searches through multiple parameters
  (current 
