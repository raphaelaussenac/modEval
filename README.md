# Goal
Produce comparisons between inventory data and 4 forest models (4C, landclim, Salem and Samsara).


# Installation
The file 'plan.R' produces both pretrated data saved in data directory, then produces figures in plotEval directory
To launch, need package 'drake', then source('plan.R'), make(plan)
Command "clean()" force drake to redo every calculation
 


# Drake
To visualize the workflow : vis_drake_graph(plan) 

