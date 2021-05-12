rm(list = ls())

source("functions_subtyping.R")

df_atef <- read.csv("Atef_ARBS.txt", sep=" ", header=FALSE)
df_atef <- df_atef[,c(1,2,5)]
names(df_atef) = c("sample","color","obs_sim_adj")

df_atef[df_atef$sample == "PPCG0109a_DNA",c("color")] <- "grey"
df_atef[df_atef$sample == "PPCG0109a_DNA",c("obs_sim_adj")] <- 0

df_ppcguk <- read.csv("PPCGUK_Per_group_20kb_brp_prop_all_chr.csv")
df_ppcguk <- df_ppcguk[,c(1,2,6)]
names(df_ppcguk) = c("sample","color","obs_sim_adj")

uk_red_count <- nrow(df_ppcguk[df_ppcguk$color == "red",])
uk_blue_count <- nrow(df_ppcguk[df_ppcguk$color == "blue",])
uk_grey_count <- nrow(df_ppcguk[df_ppcguk$color == "grey",])

df_ppcg <- read.csv("PPCG_Per_group_20kb_brp_prop_all_chr.csv")
df_ppcg <- df_ppcg[,c(1,2,6)]
names(df_ppcg) = c("sample","color","obs_sim_adj")

ppcg_red_count <- nrow(df_ppcg[df_ppcg$color == "red",])
ppcg_blue_count <- nrow(df_ppcg[df_ppcg$color == "blue",])
ppcg_grey_count <- nrow(df_ppcg[df_ppcg$color == "grey",])

plot_a <- plot_arbs_bars(df_atef,"Original UK Run - 158 samples")
plot_b <- plot_arbs_bars(df_ppcguk, "Our UK Run - 158 samples")
plot_c <- plot_arbs_bars(df_ppcg,"839 Samples")

#com_plot <- grid.arrange(plot_a, plot_b, plot_c, nrow=3)
com_plot <- grid.arrange(plot_a, plot_b, nrow=2)
#com_plot
ggsave(paste0("arbs_uk_comp_158.png"), plot=com_plot)

ggsave(paste0("arbs_uk_comp_839.png"), plot=plot_c)

