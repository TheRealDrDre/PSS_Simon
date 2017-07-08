#!/usr/bin/env python

TMPLT = """
(load "/projects/actr/actr7/load-act-r.lisp")
(load "../simon-device-short.lisp")
(load "../model/simon-model.lisp")
(load "../model/simon-simulations.lisp")
(with-open-file (out "grid-search-alpha_%0.2f-egs_%0.2f.txt" :direction :output 
		     :if-exists :overwrite :if-does-not-exist :create)
  (simulate-psp2 200 out
                :alpha %0.2f
                :egs %0.2f))
(quit)
		
"""
   
if __name__ == "__main__":
    i = 1
    for alpha in [x/100.0 for x in range(10,51,10)]:
        for egs in [x/100.0 for x in range(0,41,10)]:
            fout = open("grid-search_alpha_%0.2f-egs_%0.2f.lisp" % (alpha, egs), 'w')
            s = TMPLT % (alpha, egs, alpha, egs)
            fout.write(s)
            fout.flush()
            fout.close()
            i = i + 1
