### Tree abundance by tree size class and genus ###

## This code is licensed under the GPL v3.0 ##
## written by Emma J. Hudgins (2020) ##
## PhD Candidate, McGill University ##
## Montreal, QC, Canada ##

## For questions, e-mail emma.hudgins@mail.mcgill.ca ##
require(here) #load these scripts within the RProj file in order for the automatic selection of the correct working directory
setwd(here())
prop_areas_places<-read.csv('prop_place_area_2019.csv') # area of each community within each grid cell used to redistributed tree abundance across grid by proportion of cell occupied
pred_trees_huge<-read.csv('predicted_alltrees_allUS.csv') # model predictions for street trees across comunities, genera, size classes
real_trees<-read.csv('street_tree_each_cor2.csv')
pred_trees_huge<-merge(pred_trees_huge, real_trees, by=c("PLACEFIPS", "genus"), all.x=T) # set observed tree counts to observations rather than predictions
pred_trees_huge<-pred_trees_huge[,c(1:4,8,26:29,57:62,119:121)] #subset to shrink dataset size
pred_trees_huge[which(is.na(pred_trees_huge$small)==F),]$pred_small<-pred_trees_huge[which(is.na(pred_trees_huge$small)==F),]$small
pred_trees_huge[which(is.na(pred_trees_huge$med)==F),]$pred_med<-pred_trees_huge[which(is.na(pred_trees_huge$med)==F),]$med
pred_trees_huge[which(is.na(pred_trees_huge$large)==F),]$pred_large<-pred_trees_huge[which(is.na(pred_trees_huge$large)==F),]$large

## Adjust tree counts to account for known preventative cutting in two communities (Mark Ambrose email communications from municipal governments, accounting for 5% overestimate due to non-street trees being incorporated into these estimates)
pred_trees_huge[319604,13:15]<-(59*0.95)*(pred_trees_huge[319604,13:15]/sum(pred_trees_huge[319604,13:15]))# Tinley Park
pred_trees_huge[which(pred_trees_huge$NAME.y.x=="Fort Wayne"&pred_trees_huge$genus=="fraxinus"),13:15]<-c(323 ,	 446, 	 62 )*0.95

#disallow negative predictions and set NA to 0
pred_trees_huge$pred_large[which(is.na(pred_trees_huge$pred_large)==T)]<-0
pred_trees_huge$pred_med[which(is.na(pred_trees_huge$pred_med)==T)]<-0
pred_trees_huge$pred_small[which(is.na(pred_trees_huge$pred_small)==T)]<-0
pred_trees_huge$pred_large_trees[which(is.na(pred_trees_huge$pred_large_trees)==T)]<-0
pred_trees_huge$pred_med_trees[which(is.na(pred_trees_huge$pred_med_trees)==T)]<-0
pred_trees_huge$pred_small_trees[which(is.na(pred_trees_huge$pred_small_trees)==T)]<-0

saveRDS(pred_trees_huge, file="pred_trees_huge.rds") # save cleaned file in more compact format

#50x50km grid reallocations based on grid from Hudgins et al. 2017 (IDs matched to pest spread data)
grid<-read.csv('countydatanorm_march.csv') # identical grid is used in emmajhudgins/GDKvsCustomized and Dryad repo for Hudgins et al. 2017
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
pred_trees_all<-read.csv('~/Downloads/predicted_trees_allUS_2019-apr.csv')
real_dat_total<-read.csv('~/Downloads/street_tree_cor.csv') # correct for real observations as above
pred_trees_all<-pred_trees_all[,c(1:3,7,25:28,32,57,70:75)]
pred_trees_all$genus<-pred_trees_all$SP
pred_trees_all<-merge(pred_trees_all, real_dat_total, by=c("PLACEFIPS", "genus"), all.x=T)
pred_trees_all<-pred_trees_all[,c(1:17,90:92)]
pred_trees_all[which(is.na(pred_trees_all$small_cor)==F),]$pred_small<-pred_trees_all[which(is.na(pred_trees_all$small_cor)==F),]$small
pred_trees_all[which(is.na(pred_trees_all$med_cor)==F),]$pred_med<-pred_trees_all[which(is.na(pred_trees_all$med_cor)==F),]$med
pred_trees_all[which(is.na(pred_trees_all$large_cor)==F),]$pred_large<-pred_trees_all[which(is.na(pred_trees_all$large_cor)==F),]$large

pred_trees_all$pred_large[which(is.na(pred_trees_all$pred_large)==T)]<-0
pred_trees_all$pred_med[which(is.na(pred_trees_all$pred_med)==T)]<-0
pred_trees_all$pred_small[which(is.na(pred_trees_all$pred_small)==T)]<-0
pred_trees_all$pred_large_trees[which(is.na(pred_trees_all$pred_large_trees)==T)]<-0
pred_trees_all$pred_med_trees[which(is.na(pred_trees_all$pred_med_trees)==T)]<-0
pred_trees_all$pred_small_trees[which(is.na(pred_trees_all$pred_small_trees)==T)]<-0

saveRDS(pred_trees_all, file="pred_trees_all") # save in more compact file format

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

#reapportion by land use class fractions observed empirically across communities where data was available
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

saveRDS(grid_com_small, file="grid_com_small.RDS")
saveRDS(grid_com_med, file="grid_com_med.RDS")
saveRDS(grid_com_large, file="grid_com_large.RDS")

saveRDS(grid_res_small, file="grid_res_small.RDS")
saveRDS(grid_res_med, file="grid_res_med.RDS")
saveRDS(grid_res_large, file="grid_res_large.RDS")
