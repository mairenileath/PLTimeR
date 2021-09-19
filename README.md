# PLTimeR

Plackett-Luce probabilistic timing model with contributions from Iliana Peneva, Naser Ansari-Pour, Ruxandra Tesloianu, Thomas Mitchell, and Máire Ní Leathlobhair.

## Input Description 
PLTimer takes as input a driver mutations list. This list contains a separate line for each driver mutation in the cohort and lists can have multiple lines for one tumour if there are clonal and subclonal driver mutations. 

*startpos* and *endpos* are the coordinates of the mutation. 

*w.mean* is the CCF (frac1_A) of the enriched region weighted by the region length 
