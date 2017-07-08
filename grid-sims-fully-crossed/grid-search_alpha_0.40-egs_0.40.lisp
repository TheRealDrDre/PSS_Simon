
(load "/projects/actr/actr7/load-act-r.lisp")
(load "../simon-device-short.lisp")
(load "../model/simon-model.lisp")
(load "../model/simon-simulations.lisp")
(with-open-file (out "grid-search-alpha_0.40-egs_0.40.txt" :direction :output 
		     :if-exists :overwrite :if-does-not-exist :create)
  (simulate-psp2 200 out
                :alpha 0.40
                :egs 0.40))
(quit)
		
