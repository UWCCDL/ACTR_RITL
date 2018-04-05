#!/bin/bash
# ====================================================================
# run-sims.sh
# ====================================================================
# Runs Lisp simulations of an ACT-R model.
# It expects that all the files to run are named "simulations_*.lisp".
# ====================================================================

for file in simulations_*.lisp; do
    # Starts a nice thread
    nice sbcl --load ${file} &
done
