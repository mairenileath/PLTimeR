rm(list = ls())

library(data.table)
library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyr)
library(copynumber)
library(PlackettLuce)
library(reshape2)
library(PLMIX)

args <- commandArgs(trailingOnly = TRUE)

tumour_type = args[1]
model = args[2]
r_path = args[3]

source(paste0(r_path,"04_01_tree_building_functions.R"))
source(paste0(r_path,"04_02_order_events_across_cohort.R"))


merged_dir = "./outputs/merged/"
mixed_dir = "./outputs/mixed/"
pl_dir = "./outputs/pl_out/"
annotated_segments_file = paste0("./outputs/",tumour_type,"_annotated_segments.txt")

#Process the enriched data into the format required for the ordering step with the Plackett-Luce model
# Inputs: annotated_segs, mergedsegs
#if (model=='mixed') {

# Identify matrix of relationships between orderings.
# Outputs: matrix
#    order_events_across_chort(annotated_segments_file,merged_dir, tumour_type, FALSE, NULL, TRUE, "PLMIX", NULL, mixed_dir)
#} else {

# Generate ordering plot
# Outputs: outs matrix and plots
#    order_events_across_chort(annotated_segments_file,merged_dir, tumour_type, FALSE, NULL, FALSE, "PLMIX", NULL, pl_dir)
#}

if (model=='mixed') {
    order_events_across_chort(annotated_segments_file,merged_dir, tumour_type, FALSE, NULL, TRUE, "PLMIX", NULL, mixed_dir,TRUE)
} else if (model=='notmo') {
    order_events_across_chort(annotated_segments_file,merged_dir, tumour_type, FALSE, NULL, FALSE, "PLMIX", NULL, pl_dir,TRUE)
} else if (model=='lmixed') {
    order_events_across_chort(annotated_segments_file,merged_dir, tumour_type, FALSE, NULL, TRUE, "PLMIX", NULL, mixed_dir, FALSE)
} else if (model=='lnotmo') {
    order_events_across_chort(annotated_segments_file,merged_dir, tumour_type, FALSE, NULL, FALSE, "PlackettLuce", NULL, pl_dir,FALSE)
}






