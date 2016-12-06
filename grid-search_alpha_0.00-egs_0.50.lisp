
(load "/projects/actr/actr7/load-act-r.lisp")
(load "simon-device.lisp")
(load "simon-model.lisp")
(load "simon-simulations.lisp")
(with-open-file (out "grid-search-alpha_0.00-lf_0.50.txt" :direction :output 
		     :if-exists :overwrite :if-does-not-exist :create)
  (simulate-psp 250
		out
                :alpha 0.00
                :lf 0.50))
		
