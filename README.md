# SubtypingMSc

Timing Model has changes from version provided by Iliana Peneva and Naser Ansari-Pour from the University of Oxford. 

functions_subtyping.R must be available to other R scripts.

## Timing Model

#### Function scripts (modified from Iliana's version)
01_prepare_subclones_for_timing.R

02_01_identify_enriched_regions_gain.R

02_01_identify_enriched_regions_hd.R

02_01_identify_enriched_regions_loh.R

02_01_identify_enriched_regions.R

02_02_fdr_summary_function.R

03_prepare_enriched_regions_for_ordering.R

04_01_tree_building_functions.R

04_02_order_events_across_cohort.R

#### Run sequence
tm_1_run.R

tm_2_run.R

tm_3_run.R

tm_4_run.R


#### Used for running with seperated SCNA types
tm_2_run_gain.R

tm_2_run_hd.R

tm_2_run_loh.R


#### Create distance matrix
tm_cluster_generate_distance_summary.R

#### View hierarchical cluster of distance summary
tm_cluster_visualise.R

#### Get distance summary groups
tm_group_cluster.R

#### Get SCNA raw files from PPCGID
tm_cluster_prepare_subclone_list.R

#### Summaries
tm_CNA_counts.R

tm_get_cna_from_merge.R

#### Plot results
tm_PPCG_orderingplot.R

tm_UK_orderingplot.R

tm_ordering_plot_totals.R

#### Feature proportion of method
cl_feature_proportion.R

#### Heatmap of method
cl_generic_heatmap.R

##### Summarise by region
cl_region.R

cl_region_aggregate.R
