# Urban tree deaths from invasive pests in the United States from 2020-2050
<i>Authors: Emma J. Hudgins, Frank H. Koch, Mark J. Ambrose, and Brian Leung</i>

In this paper, we estimate the number of additional urban trees expected to die in the United States in the next 30 years due to the impacts of all known invasive forest pests.

Urban trees are expected to be the target of the greatest economic impacts of forest pests, but previous impact estimates were at the country-level, and therefore lacked the necessary pest, tree, and spatiotemporal resolution to allow targeted management. In this study, we synthesized urban tree distributional models across 30,000 US communities with spread predictions for 57 pest species and host-specific estimates of tree death due to pest exposure. 

We estimated that an additional (i.e., above background mortality) 2.2 million street trees will die due to pests from 2020 to 2050, along with 249 million community trees and 46 million residential trees. 

Additionally, we used this framework to identify a set of risk factors for future high impact urban forest pests, where we predict the highest risk due to a novel wood borer of maple or oak entering via a port in the southern US. 

The predicted tree mortality/cost in each US community by tree genus is available in  "mortgrid.RDS/costgrid.RDS". For more finescale predictions, email emma.hudgins@mail.mcgill.ca

Read .RDS files into R using <i>readRDS().</i>


## Scripts

### Scripts using private data (private_treedata_code subfolder)  

1. '01_totaltree_genusspecific_models.R'- fits total abundance and genus-specific abundance models for small, medium and large street trees
2. '02_extrapolate_models.R' - predicts genus-specific tree abundance in all US communities using the models fit in the previous scripts
3. '03_nonstreet_tree_models.R' - script used to calculate trees across all land use types using a smaller dataset for whole-community trees, which follows a same model selection approach to 01-02 (see MS), and get combined with street trees in synthesis'
3. 'totaltree_clean.R' - helper script to fits total tree models for small, medium and large street trees for use in script 01.
4. 'treemod_funcs.R' - helper functions used to assist in fitting street tree models


### Scripts using public data  

1.'010_beta_mortality_stan.R' - R script calling STAN model (./stan/beta_mort.stan) and saving output using latin hypercube sampling to show theoretical validity, and then fitting to pest severity data
2. '02*_ files.R' - forecasts pest spread based on Hudgins et al. 2017;2020 for pests present for more and less than to years with only a single occurrence timepoint (see Hudgins et al. 2020), as well as for 4 pest species with historical spread data.
3. '030_tree_grid_public.R' - uses models produced in private folder to calculate trees in each grid cell in order to get matched to pest spread forecasts (which get converted back to community-level data in script 041)
4. '040_model_synthesis.R' - synthesizes four model predictions into tree mortality and cost estimates by community
	'041_eachcommunity.R' - extracts tree mortality and cost incurred by each US community in the most likely scenario
	'042_plotting.R' - reproduces important plots from the manuscript

### Derived data  

1. 'presences_time_noforce3.csv' - pest spread forecasts from Hudgins et al. (2020) Ecol. App.  
2. 'datanorm.csv' - pest data from Hudgins et al. 2017;2019  
3. 'grid_x.csv' - predicted trees in each grid cell, for street, residential (res), and non-residential (com) trees of each size class   
4. 'mortality_x.csv' - predicted mortality for each tree/grid cell/pest combination for each lag or the 
6. 'completepredictionset.rds' - predicted trees of 3 size classes of each genus, plus # expected to die due to each pest, in each community by PLACEFIPS code. Please email me if you are looking for a csv copy of this file.
7. 'hostgen_x.rds' - predicted asymptotic mortality for each pest/host combination

For any questions or suspected bugs, feel free to open an issue or email me at emma.hudgins@mail.mcgill.ca





