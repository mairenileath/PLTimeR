rm(list = ls())

library(data.table)
library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyr)
library(copynumber)
library(PlackettLuce)
library(reshape2)

args <- commandArgs(trailingOnly = TRUE)

tumour_type = args[1]
genome_path = args[2]
r_path = args[3]

source(paste0(r_path,"01_prepare_subclones_for_timing.R"))

hg_genome <- read.table(genome_path,header = TRUE)
chr_lengths = hg_genome$end
data_dir = "./subclones/"
output_dir = "./outputs/"
refsegs_dir = "./outputs/refsegs/"
landscape_dir = "./outputs/cnlandscape/"
allsegs_file = paste0("./outputs/",tumour_type,"_allsegs.txt")
annotated_segments_file = paste0("./outputs/",tumour_type,"_annotated_segments.txt")

#Collate the subclones data from Battenberg into a single file and add the ploidy of the sample.
# Inputs: subclone files
# Outputs: allsegs file
subclone_collation(data_dir, tumour_type, output_dir)

#Sets types of CNA
# Inputs: allsegs file
# Outputs: annotated_segments file
CNA_annotation(allsegs_file, tumour_type, output_dir)

#Prepare data for plotting of landscape of CNAs across the whole genome.
# Inputs: annotated_segments file
# Outputs: refsegs files
prepare_data_for_landscape(annotated_segments_file, tumour_type, chr_lengths, refsegs_dir)

#Plot the CNA data across the genome (all/clonal/subclonal aberrations)
# Inputs: annotated_segments file and refsegs files
# Outputs: landscape plots
plot_CN_landscape(annotated_segments_file, refsegs_dir, tumour_type, chr_lengths, landscape_dir)
