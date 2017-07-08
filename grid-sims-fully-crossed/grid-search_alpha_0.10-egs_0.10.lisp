
(load "/projects/actr/actr7/load-act-r.lisp")
(load "../simon-device-short.lisp")
(load "../model/simon-model.lisp")
(load "../model/simon-simulations.lisp")
(with-open-file (out "grid-search-alpha_0.10-egs_0.10.txt" :direction :output 
		     :if-exists :overwrite :if-does-not-exist :create)
  (simulate-psp2 200 out
                :alpha 0.10
                :egs 0.10))
(quit)
		
