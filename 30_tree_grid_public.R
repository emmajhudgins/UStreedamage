### Tree abundance by tree size class and genus ###

## This code is licensed under the GPL v3.0 ##
## written by Emma J. Hudgins (2020) ##
## PhD Candidate, McGill University ##
## Montreal, QC, Canada ##

## For questions, e-mail emma.hudgins@mail.mcgill.ca ##
require(here) #load these scripts within the RProj file in order for the automatic selection of the correct working directory
setwd(here())
prop_areas_places<-read.csv('./data/prop_place_area_2019.csv') # area of each community within each grid cell used to redistributed tree abundance across grid by proportion of cell occupied
#pred_trees_huge<-readRDS('~/Desktop/OneDrive - McGill University/Grad/scripts/pred_trees_huge.rds') # predicted trees from tree models
pred_trees_huge<-readRDS('./data/predicted_trees_huge_public.RDS') # predicted trees from tree models, with observed data removed (running the script will produce slightly different result to manuscript for this reason)

trees_LU<-c(0.1646509, 1-(0.1646509)) #land use class means (community, residential)
#pred_trees_all<-readRDS('~/Desktop/OneDrive - McGill University/Grad/scripts/pred_trees_all.RDS') # predicted trees of all land uses from tree models

pred_trees_all<-readRDS('./data/predicted_trees_all_public.RDS') # predicted trees of all land uses from tree models, with observed data removed (running the script will produce slightly different result to manuscript for this reason)

#50x50km grid reallocations based on grid from Hudgins et al. 2017 (IDs matched to pest spread data)
grid<-read.csv('./data/countydatanorm_march.csv') # identical grid is used in emmajhudgins/GDKvsCustomized and Dryad repo for Hudgins et al. 2017
grid_small<-matrix(0, 3372, 48)
grid_med<-matrix(0,3372, 48)
grid_large<-matrix(0, 3372, 48)
for (i in 1: nrow(grid))
{
  ID<-grid$ID[i]
  sub_places<-subset(prop_areas_places, ID_2==ID)
  sub_trees<-subset(pred_trees_huge, PLACEFIPS%in%sub_places$PLACEFIPS)
  sub_trees[is.na(sub_trees)]<-0
  prop_expanded<-sub_places$prop[match(sub_trees$PLACEFIPS, sub_places$PLACEFIPS)]
  sub_trees[,13:15]<-sub_trees[,13:15]*prop_expanded
  sub_trees$pred_trees<-(sub_trees$pred_small+sub_trees$pred_med+sub_trees$pred_large)
  if (nrow(sub_trees)>0)
  {
    grid_small[i,]<-tapply(sub_trees$pred_small, sub_trees$genus, sum)
    grid_med[i,]<-tapply(sub_trees$pred_med, sub_trees$genus, sum)
    grid_large[i,]<-tapply(sub_trees$pred_large, sub_trees$genus, sum)
  }
}
saveRDS(grid_small, file="/output/grid_small.RDS")
saveRDS(grid_med, file="/output/grid_med.RDS")
saveRDS(grid_large, file="/output/grid_large.RDS")
