#!/bin/bash

TOTAL_SEEDS=2000
MAX_CONCURRENT=4
cd ~/Documents/dissertation

seq 1 $TOTAL_SEEDS | xargs -P $MAX_CONCURRENT -I {} Rscript 7_var_imp/src/9_calc_permutation_importance.R {}