#' Run the Timing Model pipeline
#' 
#' @param tumour_type
#' @param genome_path
#' @param r_path Full path to folder with timing model scripts
#' @param data_dir Full path to folder with subclones files
#' @param output_dir Full path to a output folder
#' @param cna_type CNA event to be considered (Default: "all")
#' @param model "mixed","lmixed","notmo","lnotmo" (Default: "mixed")
#' @param minN (Default: 3)
#' @param min_region (Default: 10000)
#' @param skip_landscape  Provide TRUE when subclone collation and CNA landscape is already complete (Default: FALSE)
#' @param skip_simulations  Provide TRUE when random CNA simulation is already complete (Default: FALSE)
#' @param skip_enrichment  Provide TRUE when eniched CNA events have already been identified (Default: FALSE)
#' @param skip_ordering Provide TRUE when eniched CNA ordering is already complete (Default: FALSE)
#' @param ref_genome

conga = function(tumour_type, genome_path, r_path, data_dir, output_dir, cna_type="all", minN=3, min_region=10000, skip_landscape=F, skip_simulations=F, skip_enrichment=F, skip_ordering=F, model="mixed", ref_genome) {

  library(PlackettLuce)
  
  hg_genome <- read.table(genome_path,header = TRUE)
  chr_lengths = hg_genome$end
  refsegs_dir = paste0(output_dir,"refsegs/")
  landscape_dir = paste0(output_dir,"/cnlandscape/")
  if (!file.exists(landscape_dir)) { dir.create(landscape_dir) }
  gain_dir = paste0(output_dir, "gain/")
  if (!file.exists(gain_dir)) { dir.create(gain_dir) }
  hd_dir = paste0(output_dir,"hd/")
  if (!file.exists(hd_dir)) { dir.create(hd_dir) 
  loh_dir = paste0(output_dir,"loh/")
  if (!file.exists(loh_dir)) { dir.create(loh_dir) }
  pvals_dir = paste0(output_dir,"pvals/")
  enriched_dir = paste0(output_dir,"enriched/")
  merged_dir = paste0(output_dir,"merged/") 
  mixed_dir = paste0(output_dir,"mixed/")
  pl_dir = paste0(output_dir,"pl_out/")
  annotated_segments_file = paste0("./outputs/",tumour_type,"_annotated_segments.txt")
  annotated_segments_file = paste0(output_dir, tumour_type,"_annotated_segments.txt")
  allsegs_file = paste0(output_dir, tumour_type, "_allsegs.txt")
  
  if (!skip_landscape) {
    #Collate the subclones data from Battenberg into a single file and add the ploidy of the sample.
    # Inputs: subclone files
    # Outputs: allsegs file
    subclone_collation(data_dir, 
                       tumour_type, 
                       output_dir)

    #Sets types of CNA
    # Inputs: allsegs file
    # Outputs: annotated_segments file
    CNA_annotation(allsegs_file, 
                   tumour_type, 
                   output_dir)

    #Prepare data for plotting of landscape of CNAs across the whole genome.
    # Inputs: annotated_segments file
    # Outputs: refsegs files
    prepare_data_for_landscape(annotated_segments_file, 
                               tumour_type, 
                               chr_lengths, 
                               refsegs_dir)

    #Plot the CNA data across the genome (all/clonal/subclonal aberrations)
    # Inputs: annotated_segments file and refsegs files
    # Outputs: landscape plots
    plot_CN_landscape(annotated_segments_file, 
                      refsegs_dir, 
                      tumour_type, 
                      chr_lengths, 
                      landscape_dir)
  } 
  
  if (!skip_simulations) {
  
    #To be run 1000 times
    #Simulate random LOH, gains and HD to identify enriched events
    # Inputs: annotated_segments file and refsegs files
    # Outputs: simulations for gain, hd and loh
  
    if (cna_type=="all") {
      identify_enriched_regions(annotated_segments_file, 
                                refsegs_dir, 
                                gain_dir, 
                                loh_dir, 
                                hd_dir, 
                                tumour_type, 
                                chr_lengths, 
                                run)
      
    } else if (cna_type=="gain") {
      identify_enriched_regions_gain(annotated_segments_file, 
                                     refsegs_dir, 
                                     gain_dir, 
                                     tumour_type, 
                                     run)
    } else if (cna_type=="loh") {
      identify_enriched_regions_loh(annotated_segments_file, 
                                    refsegs_dir, 
                                    loh_dir, 
                                    tumour_type, 
                                    run)
    } else if (cna_type=="hd") {
      identify_enriched_regions_hd(annotated_segments_file, 
                                   refsegs_dir, 
                                   hd_dir, 
                                   tumour_type, 
                                   run)
    }
  }
  
 if (!skip_enrichment) { 
  #Identify enriched events using the p-values
  # Inputs: refsegs and simulations
  # Outputs: files with enriched regions, the Bonferroni and FDR-corrected p-values
  fdr_summary(refsegs_dir, 
              gain_dir, 
              tumour_type, 
              "Gain", 
              pvals_dir)
   
  fdr_summary(refsegs_dir, 
              loh_dir, 
              tumour_type, 
              "LOH", 
              pvals_dir)
   
  fdr_summary(refsegs_dir, 
              hd_dir, 
              tumour_type, 
              "HD", 
              pvals_dir)

  #Remove artefacts from the enriched regions, eg. segments near telomere, centromere, HLA region, in a few samples
  # Inputs: 
  # Outputs: Enriched region files for LOH, HD, Gain, trimmed of artesfacts
  remove_artefacts(annotated_segments_file, 
                   tumour_type, 
                   enriched_dir, 
                   pvals_dir, 
                   genome_path, 
                   min_region_choice=min_region, 
                   minN_choice=minN)

  #Merge segments overlapping the enriched regions
  # Inputs: annotated segments file and enriched region files
  # Outputs: merged enriched segments and the plots of the enriched regions are output
  merge_enriched_regions(annotated_segments_file, 
                         tumour_type, 
                         enriched_dir, 
                         genome_path, 
                         enriched_dir)
  
  #Check if any new breakpoints should be added to the enriched regions
  # Inputs: enriched region files
  # Outputs: merged enriched regions (separate files for LOH, HD and gain) and plots of the segments overlapping the enriched regions
  multipcf_new_breakpoints(output_dir, 
                           tumour_type, 
                           enriched_dir)
 } 
  
  if (!skip_ordering) {  
    #Process the enriched data into the format required for the ordering step with the Plackett-Luce model
    # Inputs: annotated segments file and enriched region files
    # Outputs: files with enriched regions (separate files for LOH, HD and gain) in the format for the ordering script
    prepare_ordering_data(annotated_segments_file, 
                          tumour_type, 
                          enriched_dir, 
                          genome_path, 
                          merged_dir)
  
    # Identify matrix of relationships between orderings and generate ordering plot
    # Outputs: outs matrix and plots
    if (model=='mixed') {
      order_events_across_chort(annotated_segments_file,
                                merged_dir, 
                                tumour_type, 
                                driver_mutations=FALSE, 
                                drivers_file=NULL, 
                                mixture_model=TRUE, 
                                model="PLMIX", 
                                clonal_driver_mutations=NULL, 
                                mixed_dir,
                                include_unobserved=TRUE)
    } else if (model=='notmo') {
      order_events_across_chort(annotated_segments_file,
                                merged_dir, 
                                tumour_type, 
                                driver_mutations=FALSE, 
                                drivers_file=NULL, 
                                mixture_model=FALSE, 
                                model="PLMIX", 
                                clonal_driver_mutations=NULL, 
                                pl_dir,
                                include_unobserved=TRUE)
    } else if (model=='lmixed') {
      order_events_across_chort(annotated_segments_file,
                                merged_dir, 
                                tumour_type, 
                                driver_mutations=FALSE, 
                                drivers_file=NULL, 
                                mixture_model=TRUE, 
                                model="PLMIX", 
                                clonal_driver_mutations=NULL, 
                                mixed_dir, 
                                include_unobserved=FALSE)
    } else if (model=='lnotmo') {
      order_events_across_chort(annotated_segments_file,
                                merged_dir, 
                                tumour_type, 
                                driver_mutations=FALSE, 
                                drivers_file=NULL, 
                                mixture_model=FALSE, 
                                model="PlackettLuce", 
                                clonal_driver_mutations=NULL, 
                                pl_dir,
                                include_unobserved=FALSE)
    } 
  }
}  
