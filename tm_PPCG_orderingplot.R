rm(list = ls())

source("functions_subtyping.R")

A_out <- read.csv("PPCGAl_wgd_ordering_plot_file.txt", sep = "\t")
B_out <- read.csv("PPCGBl_wgd_ordering_plot_file.txt", sep = "\t")
#C_out <- read.csv("PPCGCl_wgd_ordering_plot_file.txt", sep = "\t")

plot_A=plot_tm_ordering(A_out, "A")
plot_B=plot_tm_ordering(B_out, "B")
#plot_C=plot_tm_ordering(C_out, "C")


#plot_A <- plot_A + theme(legend.position = "none")
#plot_B <- plot_B + theme(legend.position = "none")

#plot_D=plot_tm_ordering(D_out, "D")
#combined_orderings <- grid.arrange(plot_A, plot_B, plot_C, plot_D, nrow=2, ncol=2)
#combined_orderings <- grid.arrange(plot_A, plot_B, plot_C, ncol=3)
#combined_orderings
ggsave(paste0("tm_PPCGl_A_orderings.png"), plot=plot_A)
ggsave(paste0("tm_PPCGl_B_orderings.png"), plot=plot_B)
#(paste0("tm_PPCGl_C_orderings.png"), plot=plot_C)

# combined_orderings1 <- grid.arrange(plot_A, plot_B, ncol=2)
# combined_orderings1
# ggsave(paste0("tm_PPCG_combined_orderings1.png"), plot=combined_orderings1)
# 
# combined_orderings2 <- grid.arrange(plot_C, plot_D, ncol=2)
# combined_orderings2
# ggsave(paste0("tm_PPCG_combined_orderings2.png"), plot=combined_orderings2)
