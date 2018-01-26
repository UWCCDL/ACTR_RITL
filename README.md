# ACTR_RITL

## About this model

The model currently consists of three separate parts for encoding instructions, executing calculations, and responding to a probe. They will be merged soon.
Currently, the model only understand instructions that have been entered explicitly to the Declarative Memory. I assume that the model understands individual instructions (e.g. "add" means x+y). Performance may improve when a set of familiar instructions ("double"/"half"/"add") has been presented earlier.

## Running the model

* Open a Lisp interpreter and load ACT-R

* Load the device-file ritl-device.lisp. It presents the stimuli and interprets button presses

* Load the appropriate model: BilingualRITL-Encoding-V1, BilingualRITL-Execution-V1, or BilingualRITL-Response-V1.

* Run the model using ACT-R commands, e.g. (run 10).



