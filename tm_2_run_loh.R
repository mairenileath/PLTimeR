rm(list = ls())

args <- commandArgs(trailingOnly = TRUE)

tumour_type = args[1]
genome_path = args[2]
r_path = args[3]

source(paste0(r_path,"02_01_identify_enriched_regions_gain.R"))
source(paste0(r_path,"02_01_identify_enriched_regions_loh.R"))
source(paste0(r_path,"02_01_identify_enriched_regions_hd.R"))

refsegs_dir = "./outputs/refsegs/"
loh_dir = "./outputs/loh/"
annotated_segments_file = paste0("./outputs/",tumour_type,"_annotated_segments.txt")

run = as.numeric(Sys.getenv("SGE_TASK_ID"))

#To be run 100 times
#Simulate random LOH, gains and HD to identify enriched events
# Inputs: annotated_segments file and refsegs files
# Outputs: simulations for gain, hd and loh

identify_enriched_regions_loh(annotated_segments_file, refsegs_dir, loh_dir, tumour_type, run)
