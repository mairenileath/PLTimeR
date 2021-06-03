#' Run the Timing Model pipeline
#' 
#' @param tumour_type
#' @param genome_path
#' @param r_path
#' @param data_dir
#' @param output_dir

timer = function(tumour_type, genome_path, r_path, data_dir, output_dir){
rm(list = ls())

library(data.table)
library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyr)
library(copynumber)
library(PlackettLuce)
library(reshape2)

source(paste0(r_path,"01_prepare_subclones_for_timing.R"))
source(paste0(r_path,"02_01_identify_enriched_regions.R"))

hg_genome <- read.table(genome_path,header = TRUE)
chr_lengths = hg_genome$end
refsegs_dir = paste0(output_dir, "refsegs/")
landscape_dir = paste0(output_dir, "/cnlandscape/")
allsegs_file = paste0(output_dir, tumour_type, "_allsegs.txt")
gain_dir = paste0(output_dir, "gain/")
hd_dir = paste0(output_dir,"hd/")
loh_dir = paste0(output_dir,"loh/")
annotated_segments_file = paste0(output_dir, tumour_type,"_annotated_segments.txt")

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
  

#To be run 1000 times
#Simulate random LOH, gains and HD to identify enriched events
# Inputs: annotated_segments file and refsegs files
# Outputs: simulations for gain, hd and loh
identify_enriched_regions(annotated_segments_file, refsegs_dir, gain_dir, loh_dir, hd_dir, tumour_type, chr_lengths, run)

}
