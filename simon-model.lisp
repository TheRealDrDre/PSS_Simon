;;; ================================================================
;;; SIMON TASK MODEL
;;; ================================================================
;;; (c) 2016, Andrea Stocco, University of Washington
;;;           stocco@uw.edu
;;; ================================================================
;;; This is an ACT-R model of the Simon task. It is based on ideas
;;; heavily borrowed from Marsha Lovett's (2005) NJAMOS model of
;;; the Stroop task. It also explcitly models the competition
;;; between direct and indirect pathways of the basal ganglia as two
;;; separate set of rules, "process" and "dont-process" rules. In
;;; turn, this idea is borrowed from my model of Frank's (2004)
;;; Probabilistic Stimulus Selection Task. The same result
;;; could possibily be achieved through other means, but this
;;; solution is simple, intutitive, and permits to model competitive
;;; dynamics of the BG without changing ACT-R.
;;; ================================================================

(clear-all)

(define-model competitive-simon

(sgp :er t
     :act nil
     :esc T
     :ans 0.5
     :auto-attend T
     :le 0.63
;     :lf 0.3
     :mas 4.0
     :ul T
     :egs 0.1
     :reward-hook bg-reward-hook
     :alpha 0.4
     :imaginal-activation 3.0
;     :imaginal-delay 0.1
;     :visual-activation 2.0
     ;:motor-burst-time 0.05
     :motor-feature-prep-time 0.01    
;     :motor-initiation-time 0.01
)

(chunk-type (simon-stimulus (:include visual-object))
	    kind shape color location)

(chunk-type (simon-screen (:include visual-object))
	    kind value)

(chunk-type (simon-stimulus-location (:include visual-location))
	    shape color location)

(chunk-type simon-rule kind has-motor-response shape hand dimension)

(chunk-type compatible-response has-motor-response hand location)


(chunk-type wm       ;; Working Memory. Simple imaginal chunk with 2 slots
	    state
	    value1
	    value2
	    checked)

(add-dm (simon-rule isa chunk)
	(simon-stimulus isa chunk)
	(simon-screen isa chunk)
	(stimulus isa chunk)
	(circle isa chunk)
	(square isa chunk)
	(shape isa chunk)
	(yes isa chunk)
	(no isa chunk)
	(proceed isa chunk)
	(process isa chunk)
	(zero isa chunk)
	(done isa chunk)
	(pause isa chunk)
	(screen isa chunk)

	;;; The Simon Task rules:
	
	(circle-left isa simon-rule
		     kind simon-rule
		     has-motor-response yes
		     hand left
		     shape circle
		     dimension shape)

	(square-right isa simon-rule
		      kind simon-rule
		      has-motor-response yes
		      hand right
		      shape square
		      dimension shape)
)

(p find-screen
   "Look at the screen (if you were not already looking at it)"
   ?visual>
     buffer empty
     state free
     
   ?visual-location>
     buffer empty
     state free
==>
   +visual-location>
     screen-x lowest
)  

(p prepare-wm
   "If there are no contents in WM, prepare contents"
   ?imaginal>
     buffer empty
     state free

   ?manual>
     preparation free
     processor free
     execution free  
==>
   +imaginal>
     isa wm
     state process
     checked no
)

;;; ----------------------------------------------------------------
;;; SELECTIVE ATTENTION
;;; ----------------------------------------------------------------
;;; These production compete for attention to shape and location of
;;; the stimulus
;;; ----------------------------------------------------------------

(p process-shape
   "Encodes the shape in WM"
   =visual>
     kind simon-stimulus
     shape =SHAPE
     
   =imaginal>
     state process
     value1 nil

   ?retrieval>
     state free
     buffer empty

==>
   =visual>
   =imaginal>
     value1 =SHAPE
)

(p dont-process-shape
   "Does not encode the shape (focuses on location as a side effect)"
   =visual>
     kind simon-stimulus
     location =POS
     
   =imaginal>
     state process
     value1 nil

   ?retrieval>
     state free
     buffer empty

==>
   =visual>
   =imaginal>
    value1 =POS
    ;value1 zero
   
)

(p process-location
   "Encodes the stimulus location in WM"
   =visual>
     kind simon-stimulus
     location =POS
     
   =imaginal>
     state process
     value2 nil

   ?retrieval>
     state free
     buffer empty

==>
   =visual>
   =imaginal>
     value2 =POS
)

(p dont-process-location
   "Does not encode the location (focuses on the shape as a side effect"
   =visual>
     kind simon-stimulus
     shape =SHAPE
     
   =imaginal>
     state process
     value2 nil

   ?retrieval>
     state free
     buffer empty

==>
   =visual>     
   =imaginal>
     value2 =SHAPE
)

;;; ----------------------------------------------------------------
;;; RESPONSE AND CHECK
;;; ----------------------------------------------------------------
;;; The more responds by harvesting the most active Simon rule.
;;; Thus, response is guided by spreading activation from WM.
;;; A one-time check routine is also granted.
;;; ----------------------------------------------------------------

(p retrieve-intended-response
   "Retrieves the relevant part of the Simon Task rule"
   =visual>
     kind simon-stimulus
     shape =SHAPE
     
   =imaginal>
     state process
   - value1 nil
   - value2 nil  
   
   ?retrieval>
     state free
     buffer empty
==>
   =visual>   ; Keep visual
   =imaginal> ; Keep WM
   
   +retrieval>
     kind simon-rule
     has-motor-response yes
)


;;; Check
;;; Last time to catch yourself making a mistake
(p check-pass
   "Makes sure the response is compatible with the rules"
   =visual>
     shape =SHAPE
   
   =retrieval>
     kind  simon-rule
     shape =SHAPE

   =imaginal>
     state process
     checked no
   
   ?imaginal>
     state free
==>
   =visual>
   =retrieval>
   =imaginal>
     ;value2 nil
     checked yes
 )

(p check-detect-problem
   "If there is a problem, redo the retrieval once"
   =visual>
     shape =SHAPE
   
   =retrieval>
     kind  simon-rule
   - shape =SHAPE

   =imaginal>
     state process
     checked no
   
   ?imaginal>
     state free
 ==>
   =visual>
   -retrieval>
   =imaginal>
     value1 nil
     ;value2 nil
     checked yes
 )

 
(p respond
   "If we have a response and it has been check, we respond"
   =visual>
     kind simon-stimulus
     shape =SHAPE 

   =imaginal>
     state process
     checked yes

   =retrieval>
     kind simon-rule
     has-motor-response yes
     hand =HAND
     
   ?manual>
     preparation free
     processor free
     execution free
==>
  -imaginal>
  -retrieval>
  +manual>
     isa punch
     hand =HAND
     finger index
)

;;; -------------------------------------------------------------- ;;;
;;; SETUP
;;; -------------------------------------------------------------- ;;;

;;; Enabled for sims #3
;(spp check-pass :reward 1)

; Standard versions 
(spp check-detect-problem :reward -1)
;(spp process-shape :u 1 :fixed-utility t)
;(spp process-location :u 0.7 :fixed-utility t)

(spp-fct `((process-shape :u ,*bias*)))
(spp-fct `((process-location :u ,*bias*)))

)  ;;; End of the model

;;; -------------------------------------------------------------- ;;;
;;; Reload and restart
;;; -------------------------------------------------------------- ;;; 

(defun simon-reload (&key (visicon t))
  "Reloads model and resets both task and device"
  (reload)
  (install-device (make-instance 'simon-task))
  (init (current-device))
  (proc-display)
  (when visicon
    (print-visicon)))
