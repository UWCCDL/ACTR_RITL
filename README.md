# ACTR_RITL

## About this model

The model currently understands given instructions and performs one RITL trial. 
I assume that the model understands individual instructions (e.g. "add" means x+y). Performance may improve when a set of familiar instructions ("double"/"half"/"add") has been presented earlier.

## Running the model

* Open a Lisp interpreter and load ACT-R

* Load the device-file ritl-device.lisp. It presents the stimuli and interprets button presses

* Load the appropriate model: RITL-fullModel.lisp.

* Run the model using ACT-R commands, e.g. (run 20).

