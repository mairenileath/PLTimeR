#!/bin/bash --login
#$ -cwd
#$ -o ./logs
#$ -e ./logs
#$ -N SV_prop_estim
conda activate /mnt/bmh01-rds/UoOxford_David_W/shared/code/conda/R_env
Rscript ./03_brp_prop_20Kb.R
