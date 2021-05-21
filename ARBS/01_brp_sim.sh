#!/bin/bash

mapfile -t g_l < Groups_PPCG.txt

for j in ${g_l[@]}; do

mkdir ./$j

mkdir ./$j/Per_chr

mapfile -t chr_l < Chromosomes_list.txt

for i in ${chr_l[@]}; do

echo '#!/bin/bash --login' > ./$j/Per_chr/job$i.sh
echo '#$ -cwd' >> ./$j/Per_chr/job$i.sh
echo '#$ -o ./logs' >> ./$j/Per_chr/job$i.sh
echo '#$ -e ./logs' >> ./$j/Per_chr/job$i.sh
echo "#$ -N job$i" >> ./$j/Per_chr/job$i.sh
echo "conda activate /mnt/bmh01-rds/UoOxford_David_W/shared/code/conda/R_env" >> ./$j/Per_chr/job$i.sh
echo "Rscript ./01_brp_sim.R ${j} ${i}" >> ./$j/Per_chr/job$i.sh

chmod u+x ./$j/Per_chr/job$i.sh

qsub ./$j/Per_chr/job$i.sh

done

done
