# An ACT-R model of the Simon Task

This respoitory contains an ACT-R model of the Simon task, together
with a set of data that validates the model's assumptions.  Both model
and experimental results have been published in Stocco et al. (2017).

## The model 
The model itself borrows from Marsha Lovett's (2005) NJAMOS model of
the Stroop task. It also explcitly models the competition between
direct and indirect pathways of the basal ganglia as two separate set
of rules, "process" and "dont-process" rules.

In turn, this idea is borrowed from my model of Frank's (2004)
_Probabilistic Stimulus Selection_ (PSS) Task. The same result
could possibily be achieved through other means, but this
solution is simple, intutitive, and permits to model competitive
dynamics of the BG without changing ACT-R.  For an in-depth analysis
of why this particular approach is preferrable, see Stocco (2018).

### How to run this model

1. Open a Lisp interpreter and load ACT-R, version 7.x

2. Load the `simon-device.lisp` file first. This will load the
   experimental Simon task, as well as the reward learning system for
   `Process`/`Don't Process` productions, and a number of functions to
   interface the task with ACT-R and analyze the data.

3. (Optional) if you want to run simulations, load the
   `simon-simulations.lisp` file.

4. Finally, load the `simon-model.lisp` task.

5. Run the model using ACT-R commands, e.g. `(run 1000 :real-time t)`

## Reference

Experimental results and model discussion have been published in the
following paper:

Stocco, A., Murray, N. L., Yamasaki, B. L., Renno, T. J., Nguyen, J.,
& Prat, C. S. (2017). Individual differences in the Simon effect are
underpinned by differences in the  competitive dynamics in the basal
ganglia: An experimental verification and a computational
model. _Cognition_, _164_, 31-45.

