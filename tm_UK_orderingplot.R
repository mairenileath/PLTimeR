rm(list = ls())

source("functions_subtyping.R")

A_out <- read.csv("PPCGUKA_wgd_ordering_plot_file.txt", sep = "\t")
B_out <- read.csv("PPCGUKB_wgd_ordering_plot_file.txt", sep = "\t")

plot_A=plot_tm_ordering(A_out, "UKA")
plot_B=plot_tm_ordering(B_out, "UKB")
combined_orderings <- grid.arrange(plot_A, plot_B, ncol=2)
combined_orderings
ggsave(paste0("tm_PPCGUK_combined_orderings.png"), plot=combined_orderings)
