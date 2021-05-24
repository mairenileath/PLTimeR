# SubtypingMSc
Code for prostate subtyping MSc project with David Wedge

ARBS contains minor change from version provided by Atef Sahli of the University of Manchester.

Timing Model has changes from version provided by Iliana Peneva and Naser Ansari-Pour from the University of Oxford. 

functions_subtyping.R must be available to other R scripts.

## ARBS

#### Running
01_brp_sim.sh

02_dist_CI_per_bin_count_Obs-Sim.sh

03_brp_prop_20Kb.sh

#### Plots for distribution of categories
ar_plot.R

#### Heatmap of summary measurements (requires summary measurement output PPCG.txt)
ar_heatmap.R

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


## Get summary measurements
Run each to generate summary.txt files

load_indel_ml_data.R
    
load_other_ml_data.R
    
load_pga_ml_data.R
    
load_scna_ml_data.R
    
load_snv_ml_data.R
    
load_sv_ml_data.R

Run merge_data.R to create cna_summary.txt

Clean data ahead of cluster analysis with ca_clean_data.R

## Clustering Summary measurements

### PCA
ca_pca.R

#### Comparing output of multiple runs
ca_pca_comparison.R

### t-SNE
ca_tsne.R

### UMAP
ca_umap.R

#### Comparing output of multiple runs
ca_umap_comparison.R

#### Compare silhouette method results from each run
ca_umap_silhouette.R

#### Aggregate replicates for clusterings
ca_umap_aggregate.R

#### Cluster aggregate umap
ca_umap_cluster.R

### SOM
ca_som.R

#### Comparing output of multiple runs
ca_som_comparison.R

### Autoencoder
ca_ae_run.R

#### Comparing output of multiple runs
ca_ae_comparison.R

#### Aggregate replicates for clusterings
ca_ae_aggregate.R

#### Cluster aggregate ae
ca_ae_cluster_distance.R

## Generate consensus of method

#### Summary measurement (ARI, Fisher's Exact Test, Confusion Matrix)
cl_check_evotypes.R

#### Combine Methods
cl_method_aggregate.R

#### Feature proportion of method
cl_feature_proportion.R

#### Heatmap of method
cl_generic_heatmap.R

##### Summarise by region
cl_region.R

cl_region_aggregate.R
