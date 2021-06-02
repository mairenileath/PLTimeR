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

source(paste0(r_path,"02_02_fdr_summary_function.R"))
source(paste0(r_path,"03_prepare_enriched_regions_for_ordering.R"))


hg_genome <- read.table(genome_path,header = TRUE)
chr_lengths = hg_genome$end
refsegs_dir = "./outputs/refsegs/"
output_dir = "./outputs/"
pvals_dir = "./outputs/pvals/"
enriched_dir = "./outputs/enriched/"
merged_dir = "./outputs/merged/"
gain_dir = "./outputs/gain/"
hd_dir = "./outputs/hd/"
loh_dir = "./outputs/loh/"
allsegs_file = paste0("./outputs/",tumour_type,"_allsegs.txt")
annotated_segments_file = paste0("./outputs/",tumour_type,"_annotated_segments.txt")

#Identify enriched events using the p-values
# Inputs: refsegs and simulations
# Outputs: files with enriched regions, the Bonferroni and FDR-corrected p-values
fdr_summary(refsegs_dir, gain_dir, tumour_type, "Gain", pvals_dir)
fdr_summary(refsegs_dir, loh_dir, tumour_type, "LOH", pvals_dir)
fdr_summary(refsegs_dir, hd_dir, tumour_type, "HD", pvals_dir)

#Remove artefacts from the enriched regions, eg. segments near telomere, centromere, HLA region, in a few samples
# Inputs: 
# Outputs: Enriched region files for LOH, HD, Gain, trimmed of artesfacts
remove_artefacts(annotated_segments_file, tumour_type, enriched_dir, pvals_dir, genome_path, 10000, 3)

#Merge segments overlapping the enriched regions
# Inputs: annotated segments file and enriched region files
# Outputs: merged enriched segments and the plots of the enriched regions are output
merge_enriched_regions(annotated_segments_file, tumour_type, enriched_dir, genome_path, enriched_dir)

#Check if any new breakpoints should be added to the enriched regions
# Inputs: enriched region files
# Outputs: merged enriched regions (separate files for LOH, HD and gain) and plots of the segments overlapping the enriched regions
multipcf_new_breakpoints(output_dir, tumour_type, enriched_dir)

#Process the enriched data into the format required for the ordering step with the Plackett-Luce model
# Inputs: annotated segments file and enriched region files
# Outputs: files with enriched regions (separate files for LOH, HD and gain) in the format for the ordering script
prepare_ordering_data(annotated_segments_file, tumour_type, enriched_dir, genome_path, merged_dir)
