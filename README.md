# PLTimeR

Plackett-Luce probabilistic timing model with contributions from Iliana Peneva, Naser Ansari-Pour, Ruxandra Tesloianu, Thomas Mitchell, Will Eagles, Dan Woodcock, Jiqiu Cheng, and Máire Ní Leathlobhair.

PLTimeR orders the occurrence of copy number events and driver mutations with respect to whole genome duplication by using the clonality information about these events. Information from individual tumours is combined with a Plackett-Luce model to determine the most likely ordering of events within a single tumour type.  

## Input Description 
PLTimer takes as input a driver mutations list. This list contains a separate line for each driver mutation in the cohort and lists can have multiple lines for one tumour if there are clonal and subclonal driver mutations. 

*startpos* and *endpos* are the coordinates of the mutation. 

*w.mean* is the CCF (frac1_A) of the enriched region weighted by the region length 
