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

;;; A simon-trial of the form (shape circle position left)

(defun simon-trial? (lst)
  (and (consp lst)
       (evenp (length lst))))

(defmethod device-handle-keypress ((trial list) key)
  "Converts the key into a symbol and passes it on to the task manager"
  (declare (ignore trial key))
  nil)
			  

			   
(defmethod device-handle-click ((task list))
  "Does nothing"
  (declare (ignore task))
  nil)

(defmethod device-move-cursor-to ((task list) pos)
  "Does nothing"
  (declare (ignore task))
  nil)


(defmethod get-mouse-coordinates ((task list))
  "Does nothing"
  (declare (ignore task))
  (vector 0 0))

(defmethod cursor-to-vis-loc ((task list))
  "Does nothing"
  (declare (ignore task))
  nil)


(defmethod build-vis-locs-for ((trial list) vismod)
  (let ((results nil))
    (push  `(isa simon-stimulus-location 
		 kind simon-stimulus
		 value stimulus
		 color black
		 screen-x 0
		 screen-y 0
		 height 400 
		 width 400
		 ,@trial)
	   results)
    (define-chunks-fct results)))

(defmethod vis-loc-to-obj ((task list) vis-loc)
  "Transforms a visual-loc into a visual object"
  (let ((kind (chunk-slot-value-fct vis-loc 'kind))
	(new-chunk nil)
	(trial task))
    (cond ((equal kind 'simon-stimulus)
	   
	   ;; If the location was a cell

	   (setf new-chunk
		 (first (define-chunks-fct 
			    `((isa simon-stimulus
				   kind ,kind 
				   ,@trial 
				   )))))))
    (fill-default-vis-obj-slots new-chunk vis-loc)
    new-chunk))

