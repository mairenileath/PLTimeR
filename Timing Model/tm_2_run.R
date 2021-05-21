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

source(paste0(r_path,"02_01_identify_enriched_regions.R"))


hg_genome <- read.table(genome_path,header = TRUE)
chr_lengths = hg_genome$end
refsegs_dir = "./outputs/refsegs/"
gain_dir = "./outputs/gain/"
hd_dir = "./outputs/hd/"
loh_dir = "./outputs/loh/"
annotated_segments_file = paste0("./outputs/",tumour_type,"_annotated_segments.txt")

run = as.numeric(Sys.getenv("SGE_TASK_ID"))

#To be run 100 times
#Simulate random LOH, gains and HD to identify enriched events
# Inputs: annotated_segments file and refsegs files
# Outputs: simulations for gain, hd and loh

identify_enriched_regions(annotated_segments_file, refsegs_dir, gain_dir, loh_dir, hd_dir, tumour_type, chr_lengths, run)
