# CONGA - Chronological OrderiNg of Genomic Aberrations

Timing Model with contributions from Iliana Peneva, Naser Ansari-Pour, Ruxandra Tesloianu, and Máire Ní Leathlobhair.

functions_subtyping.R must be available to other R scripts.

## Timing Model

#### Function scripts 
01_prepare_subclones_for_timing.R

02_01_identify_enriched_regions_gain.R

02_01_identify_enriched_regions_hd.R

02_01_identify_enriched_regions_loh.R

02_01_identify_enriched_regions.R

02_02_fdr_summary_function.R

03_prepare_enriched_regions_for_ordering.R

04_01_tree_building_functions.R

04_02_order_events_across_cohort.R

05_distance_summary_functions.R

#### Get SCNA raw files from PPCGID
tm_cluster_prepare_subclone_list.R

#### Summaries
tm_CNA_counts.R

tm_get_cna_from_merge.R

#### Plot results
tm_ordering_plot_totals.R
