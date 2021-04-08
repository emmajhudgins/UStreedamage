require(here)
library(reshape2)
setwd(here())
genus<-readRDS('./data/genera.RDS') # host tree genera
#pest data
data2<-read.csv('./data/datanorm.csv', stringsAsFactors = FALSE)
data2<-data2[-c(25,69),]
data2<-data2[-4,]
#row indices for pest guilds (pathogens removed)

def<-which(data2$Guild=="Defoliators") 
bore<-which(data2$Guild=="Borers")
suck<-which(data2$Guild=="Suckers")
bestguess_genus<-readRDS('./output/mean_mort.RDS')[,c(def,bore,suck),]
bestguess_cost_genus<-readRDS('./output/by_site_st_costs.RDS')[,c(def,bore,suck),]
pred_trees_huge<-readRDS('./output/pred_trees_huge_public.RDS') #private data removed so results will be dissimilar to MS
#pred_trees_huge<-readRDS('~/Desktop/OneDrive - McGill University/Grad/scripts/pred_trees_huge.rds') #private data

#pred_trees_huge<-readRDS('./output/pred_trees_huge_public.RDS') #private data removed so results will be dissimilar to MS
prop_areas_places<-read.csv('./data/prop_place_area_2019.csv')#community information with regards to 50x50km grid

# 50x50km grid spatial information
grid<-read.csv('./data/countydatanorm_march.csv')

costgrid<-data.frame()
mortgrid<-data.frame()
for (i in 1:3372)
{
  ID<-grid$ID[i]
  sub_places<-subset(prop_areas_places, ID_2==ID)
  if (nrow(sub_places)>0){
    sub_trees<-subset(pred_trees_huge, PLACEFIPS%in%sub_places$PLACEFIPS)
    sub_trees[is.na(sub_trees)]<-0
    if(nrow(sub_trees)>0){
      prop_expanded<-sub_places$propreal3[match(sub_trees$PLACEFIPS, sub_places$PLACEFIPS)]
      sub_trees[,13:15]<-sub_trees[,13:15]*prop_expanded
      sub_trees_mort<-cbind(sub_trees$PLACEFIPS,data.frame(prop_expanded*rep(colSums(bestguess_genus[i,,]),length(prop_expanded)/48),genus=genus))
      prop_city=1
      sub_trees_cost<-cbind(sub_trees$PLACEFIPS,data.frame(prop_expanded*rep(colSums(bestguess_cost_genus[i,,],length(prop_expanded)/48)),genus=genus))
      if (nrow(sub_places)>1 & nrow(sub_trees)>0)
      {
        prop_city<-tapply(rowSums(sub_trees[,13:15]), sub_trees$PLACEFIPS, sum)/sum(sub_trees[,13:15])
        sub_trees_cost[,2]<-sub_trees_cost[,2]*prop_city[match(sub_trees_cost$`sub_trees$PLACEFIPS`,names(prop_city))]
        sub_trees_mort[,2]<-sub_trees_mort[,2]*prop_city[match(sub_trees_mort$`sub_trees$PLACEFIPS`,names(prop_city))]
      }
      costgrid<-rbind(costgrid,melt(sub_trees_cost, id.vars = c("sub_trees$PLACEFIPS", "genus")))
      mortgrid<-rbind(mortgrid,melt(sub_trees_mort, id.vars = c("sub_trees$PLACEFIPS", "genus")))
    }
  }}
saveRDS(costgrid, './output/costgrid.RDS')
saveRDS(mortgrid, './output/mortgrid.RDS')
