#!/bin/bash

mapfile -t g_l < Groups_PPCG.txt

for j in ${g_l[@]}; do

echo '#!/bin/bash --login' > ./$j/job_$j.sh
echo '#$ -cwd' >> ./$j/job_$j.sh
echo '#$ -o ./logs' >> ./$j/job_$j.sh
echo '#$ -e ./logs' >> ./$j/job_$j.sh
echo "#$ -N job$i" >> ./$j/job_$j.sh
echo "conda activate /mnt/bmh01-rds/UoOxford_David_W/shared/code/conda/R_env" >> ./$j/job_$j.sh
echo "Rscript ./02_dist_CI_per_bin_count_Obs-Sim.R ${j}" >> ./$j/job_$j.sh

chmod u+x ./$j/job_$j.sh

qsub ./$j/job_$j.sh

done
