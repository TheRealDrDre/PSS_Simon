#!/usr/bin/env python

TMPLT = """
(load "/projects/actr/actr7/load-act-r.lisp")
(load "simon-device.lisp")
(load "simon-model.lisp")
(load "simon-simulations.lisp")
(with-open-file (out "grid-search-alpha_%0.2f-lf_%0.2f.txt" :direction :output 
		     :if-exists :overwrite :if-does-not-exist :create)
  (simulate-psp 250
		out
                :alpha %0.2f
                :lf %0.2f))
		
"""



#blocks = ((0, 0.5), (0.5, 1.0), (1.0, 1.5), (1.5, 2.0))

   
if __name__ == "__main__":
    i = 1
    for alpha in [x/100.0 for x in range(0,101,25)]:
        for lf in [x/100.0 for x in range(0,101,25)]:
            fout = open("grid-search_alpha_%0.2f-egs_%0.2f.lisp" % (alpha, lf), 'w')
            s = TMPLT % (alpha, lf, alpha, lf)
            fout.write(s)
            fout.flush()
            fout.close()
            i = i + 1

    
