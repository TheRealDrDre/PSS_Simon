;;; PSS _ SImon task


;;; main idea, same as Lovett's NJAMOS
;;; Selective attention is competition among productions.

(clear-all)
(define-model pssimon

(sgp :er t :esc T :auto-attend T)

(chunk-type (simon-stimulus (:include visual-object))
	    kind shape color position)

(chunk-type (simon-stimulus-location (:include visual-object))
	    shape color position)


(chunk-type simon-rule kind has-motor-response shape hand)

(chunk-type compatible-response has-motor-response hand position)

(chunk-type wm kind value dimension)


(add-dm (circle-left isa simon-rule
		     kind simon-rule
		     has-motor-response yes
		     hand left
		     shape circle)

	(square-right isa simon-rule
		      kind simon-rule
		      has-motor-response yes
		      hand right
		      shape square)

	(compatible-response-right isa compatible-response
				   has-motor-response yes
				   hand right
				   position right)

	(compatible-response-left isa compatible-response
				  has-motor-response yes
				  hand left
				  position left)

	(stimulus1 isa simon-stimulus
		   shape circle
		   position right
		   color black
		   kind simon-stimulus)

	(wm1 isa wm
	     kind proceed)
)

(p find-stimulus
   ?visual>
     buffer empty
     state free
==>
   +visual-location>
     kind simon-stimulus
)  

(p prepare-wm
   ?imaginal>
     buffer empty
     state free
==>
   +imaginal>
     isa wm
     kind proceed
)  


(p process-shape
   =visual>
     kind simon-stimulus
     shape =SHAPE 
   =imaginal>
     kind proceed
     value nil 
 ==>
   =visual>    
   =imaginal>
     dimension shape
     value =SHAPE
)



(p find-response
   =imaginal>
     kind proceed
   - value nil
   ?retrieval>
     buffer empty
     state free
 ==>
   =imaginal>    
   +retrieval>
     has-motor-response yes
)   

(p respond
   =retrieval>
     has-motor-response yes
     hand =HAND
   ?manual>
     preparation free
     processor free
     execution free
==>
  +manual>
     isa punch
     hand =HAND
     finger index
)      
)
;(set-buffer-chunk 'visual 'stimulus1)
;(set-buffer-chunk 'imaginal 'wm1)

(install-device '(shape circle color black position right))
(proc-display)

(defun my-reload ()
  (clear-all)
  (reload)
  (install-device '(shape circle color black position right)))

