#!/usr/bin/env python

TMPLT = """
(load "/projects/actr/actr7/load-act-r.lisp")
(load "pss-device.lisp")
(defparameter *params* '((:alpha . %0.2f) (:EGS . %0.2f)))
(with-open-file (out "sims-positive-negative-alpha_%0.2f-egs_%0.2f-noncomp.txt" :direction :output 
		     :if-exists :overwrite :if-does-not-exist :create)
  (simulate-positive-negative-feedback "pss-model-noncompetitive.lisp"
				     250
				     (seq 0.0 1.01 1/10)
				     :neg-vals (seq 0.0 -1.01 -1/10)
				     :stream out
				     :report nil
				     :utilities t
				     :params *params*))
"""



#blocks = ((0, 0.5), (0.5, 1.0), (1.0, 1.5), (1.5, 2.0))

   
if __name__ == "__main__":
    i = 1
    for alpha in [x/10.0 for x in range(11)]:
        for egs in [x/10.0 for x in range(11)]:
            fout = open("sims-alpha_%0.2f-egs_%0.2f-noncomp.lisp" % (alpha, egs), 'w')
            s = TMPLT % (alpha, egs, alpha, egs)
            fout.write(s)
            fout.flush()
            fout.close()
            i = i + 1

    
