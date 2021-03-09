### Tree abundance by tree size class and genus ###

## This code is licensed under the GPL v3.0 ##
## written by Emma J. Hudgins (2020) ##
## PhD Candidate, McGill University ##
## Montreal, QC, Canada ##

## For questions, e-mail emma.hudgins@mail.mcgill.ca ##
require(here) #load these scripts within the RProj file in order for the automatic selection of the correct working directory
setwd(here())
prop_areas_places<-read.csv('./data/prop_place_area_2019.csv') # area of each community within each grid cell used to redistributed tree abundance across grid by proportion of cell occupied
pred_trees_huge<-readRDS('./data/predicted_trees_huge_public.RDS') # predicted trees from tree models, with observed data removed (running the script will produce slightly different result to manuscript for this reason)
trees_LU<-c(0.1646509, 1-(0.1646509)) #land use class means (community, residential)
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
saveRDS(grid_small, file="grid_small.RDS")
saveRDS(grid_med, file="grid_med.RDS")
saveRDS(grid_large, file="grid_large.RDS")

###non-street trees

#create 50x50km grid as above
grid_all_small<-matrix(0, 3372, 48)
grid_all_med<-matrix(0,3372, 48)
grid_all_large<-matrix(0, 3372, 48)
for (i in 1: nrow(grid))
{
  ID<-grid$ID[i]
  sub_places<-subset(prop_areas_places, ID_2==ID)
  sub_trees<-subset(pred_trees_all, PLACEFIPS%in%sub_places$PLACEFIPS)
  sub_trees[is.na(sub_trees)]<-0
  prop_expanded<-sub_places$prop[match(sub_trees$PLACEFIPS, sub_places$PLACEFIPS)]
  sub_trees[,15:17]<-sub_trees[,15:17]*prop_expanded
  sub_trees$pred_trees<-(sub_trees$pred_small+sub_trees$pred_med+sub_trees$pred_large)
  if (nrow(sub_trees)>0)
  {
    grid_all_small[i,]<-tapply(sub_trees$pred_small, sub_trees$SP, sum)
    grid_all_med[i,]<-tapply(sub_trees$pred_med, sub_trees$SP, sum)
    grid_all_large[i,]<-tapply(sub_trees$pred_large, sub_trees$SP, sum)
  }
}

#remove street tree predictions and reapportion remainder by land use class fractions observed empirically across communities where data was available
grid_res_small<-grid_all_small*trees_LU[1]-grid_small
grid_res_small[which(grid_res_small<0)]<-0 
grid_res_med<-grid_all_med*trees_LU[1]-grid_med
grid_res_med[which(grid_res_med<0)]<-0 
grid_res_large<-grid_all_large*trees_LU[1]-grid_large
grid_res_large[which(grid_res_large<0)]<-0 


grid_com_small<-grid_all_small*trees_LU[2]
grid_com_small[which(grid_com_small<0)]<-0 
grid_com_med<-grid_all_med*trees_LU[2]
grid_com_med[which(grid_com_med<0)]<-0 
grid_com_large<-grid_all_large*trees_LU[2]
grid_com_large[which(grid_com_large<0)]<-0 

saveRDS(grid_com_small, file="./output/grid_com_small.RDS")
saveRDS(grid_com_med, file="./output/grid_com_med.RDS")
saveRDS(grid_com_large, file="./output/grid_com_large.RDS")

saveRDS(grid_res_small, file="./output/grid_res_small.RDS")
saveRDS(grid_res_med, file="./output/grid_res_med.RDS")
saveRDS(grid_res_large, file="./output/grid_res_large.RDS")
