# Urban tree deaths from invasive pests in the United States from 2020-2050
<i>Authors: Emma J. Hudgins, Frank H. Koch, Mark J. Ambrose, and Brian Leung</i>

In this paper, we estimate the number of additional urban trees expected to die in the United States in the next 30 years due to the impacts of all known invasive forest pests.

Urban trees are expected to be the target of the greatest economic impacts of forest pests, but previous impact estimates were at the country-level, and therefore lacked the necessary pest, tree, and spatiotemporal resolution to allow targeted management. In this study, we synthesized urban tree distributional models across 30,000 US communities with spread predictions for 57 pest species and host-specific estimates of tree death due to pest exposure. 

We estimated that an additional (i.e., above background mortality) 4.2 million street trees will die due to pests from 2020 to 2050, along with 249 million community trees and 46 million residential trees. Predicted annualized costs to remove pest-killed trees are roughly $US 150M, while total urban pest costs could be an order of magnitude larger (~$4.8B). We expect that over 95% of damages will be caused by emerald ash borer (EAB) killing ash trees, and find a highly unequal community-level impact distribution, where <20% of all urban centres are predicted to bear roughly 90% of all impacts. Further, we define a high-impact zone spanning 555,000km2, made up of ~5000 communities including Minneapolis-St. Paul, Milwaukee WI and Indianapolis, IN, within which we predict >99% mortality of preferred hosts, and >50% mortality of all street trees.   

Additionally, we used this framework to identify a set of risk factors for future high impact urban forest pests, where we predict the highest risk due to a novel wood borer of maple or oak entering via a port in the southern US. 

The predicted tree mortality in each US community due to each pest is available in "complete_predictor_set.RDS"

Read this file into R using <i>readRDS().</i>


*Scripts*
1. 'small_tree_models.R'- fits small genus-level urban tree distributional models
2. 'medlarge_parallel.R' - fits medium and large genus-level urban tree distributional models
3. 'ch4_clean.R' - synthesizes four model predictions into tree mortality and cost estimates by community
4. 'totaltree_clean.R' - fits total tree models for small, medium and large urban trees
5. 'treemod_funcs.R' - common functions used across tree models
6. 'beta_mort.stan' - STAN model for host mortality
7. 'lhc_stan_beta.R' - R script calling STAN model and saving output


*Derived data*
1. 'presences_time_noforce3.csv' - pest spread forecasts from Hudgins et al. (2020) Ecol. App.
2. 'datanorm,csv' - pest data from Hudgins et al. 2017;2019
3. 'grid_*.csv' - predicted trees in each grid cell, for street, residential (res), and non-residential (com) trees
4. 'mostlikely_*.csv' - predicted mortality (or cost) for each tree/grid cell/pest combination
5. 'citydetails.rds' - details for all US communities by PLACEFIPS code (i column matches to grid cells via ID column match in 'grid_*.csv' files
6. 'completepredictionset.rds' - predicted trees of 3 size classes of each genus, plus # expected to die due to each pest, in each community by PLACEFIPS code.please email me if you wish to get a csv copy of this file.
7. 'hostgen_*.rds' - predicted asymptotice mortality for each pest/host combination

For any questions or issues, feel free to open an issue or email me at emma.hudgins@mail.mcgill.ca





