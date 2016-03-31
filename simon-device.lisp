;;; ------------------------------------------------------------------
;;; RAPM-DEVICE.LISP
;;; ------------------------------------------------------------------
;;; A class that provide an ACT-R GUI interface for a modified
;;; version of Raven's Advanced Progressive Matrices
;;; ------------------------------------------------------------------


(defun act-r-loaded? ()
  "Cheap hack to check whether ACTR is loaded"
  (and (fboundp 'run-n-events)
       (fboundp 'start-environment)))


(defparameter *d1* 1)

(defparameter *d2* 1)

(defparameter *reward* 10)

(defparameter *ticks* 20)



(defun bg-reward-hook (production reward time)
  (declare (ignore time))
;;  (print production)
  (let* ((pname (symbol-name production))
	 (i (position #\* pname))
	 (start (subseq pname (1+ i) (+ 5 i))))

    (cond ((string-equal start "PICK")
	   (* *d1* reward))
	  ((string-equal start "DONT")
	   (* *d2* reward))
	  (t
	   nil))))


(defun simulate (n &optional (res-file "results.csv"))
  (with-open-file (file res-file
			   :direction :output
			   :if-exists :overwrite
			   :if-does-not-exist :create)
    (let ((names (list 'd2 'ticks 'alpha 'egs 'accuracy 'problem 'choice)))
      (format file "~{~a~^, ~}~%" names))

    (dolist (d2 '(1/4 1/2 3/4 1 5/4 3/2 7/4 2))
      (dolist (ticks '(10 15 20 25 30 35 40))
	(dolist (alpha '(0 2/10 4/10 6/10 8/10))
	  (dolist (egs '(0 1/10 2/10 3/10 4/10 5/10))
	    (format t "~{~a~^, ~}~%" (list 'd2 d2 'ans ans 'alpha alpha 'egs egs))
	    (dotimes (j n)
	      (rapm-reload)  ; Reload
	      (setf *d2* d2)
	      (sgp-fct `(:egs ,egs :ans ,ans :alpha alpha :v nil)) ; Sets the params
	      (run 20 :real-time nil)
	      (let* ((trial (first (experiment-log (current-device))))
		     (res (list d2 ans alpha egs
				(trial-accuracy trial)
				(trial-problem-rt trial)
				(trial-choice-rt trial))))
		(format file "~{~a~^, ~}~%" res)))))))))
