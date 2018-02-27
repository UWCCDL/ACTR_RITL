# ACTR_RITL

## About this model

Model performs a RITL task with 40 trials. Stimuli remain the same. The two most recent models have either one chunk with all presented information (instructions and variables), or two separate chunks.

## Running the model

* Open a Lisp interpreter and load ACT-R

* Load the device-file ritl-device.lisp. It presents the stimuli and interprets button presses

* Load the appropriate model. The two most recent models are BilingualModelBilingual.lisp and BilingualModelMonolingual.lisp

* Run a reload using (ritl-reload)

* Run the model using ACT-R commands, e.g. (run 20).

