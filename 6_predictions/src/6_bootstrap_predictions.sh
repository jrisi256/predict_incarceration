#!/bin/bash

TOTAL_SEEDS=50
MAX_CONCURRENT=11

cd ~/Documents/dissertation

# seq generates a sequence of numbers from 1 to TOTAL_SEEDS.
# xargs takes one number at a time from the sequence.
# -P $MAX_CONCURRENT limits the number of running processes to MAX_CONCURRENT.
# {} catches the seed as the argument.
# The second argument in Rscript is the number of bootstrap samples to use in each instance.
seq 1 $TOTAL_SEEDS | xargs -P $MAX_CONCURRENT -I {} Rscript 6_predictions/src/6_bootstrap_predictions.R {} 1000