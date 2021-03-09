###script to model tree abundance at city level for 3 size classes
### NOTE- in order to conform to data sharing agreements, this code cannot be executed, because tree abundance data are private

write=F #turn on to write output
fit=F # turn on to refit models
library(gbm)
library(mgcv)
library(dismo)
rich_dat<-read.csv('./data/genus_level_trees.csv') # street tree inventory data for individual genera  (tree abundances have been removed due to data sharing restrictions)
rich_dat<-subset(rich_dat, city2!="NEW YORK NY") # remove NYC for fitting as it is an outlier
data<-read.csv('./data/city_level_trees.csv') # street tree inventory data combined across genera (tree abundances have been due to data sharing restrictions)
allUS<-read.csv('./data/entire_predictors_fipsfix.csv') # predictor variables, important to add mising levels for ecological province

#clean up extraneous spaces
rich_dat$inc_1969<-as.numeric(as.character(gsub(",", "",rich_dat$inc_1969)))
rich_dat$inc_1969[which(is.na(rich_dat$inc_1969)==T)]<-mean(rich_dat$inc_1969, na.rm=T)
rich_dat$inc_1979<-as.numeric(as.character(gsub(",", "",rich_dat$inc_1979)))
rich_dat$inc_1979[which(is.na(rich_dat$inc_1979)==T)]<-mean(rich_dat$inc_1979, na.rm=T)
rich_dat$inc_1989<-as.numeric(as.character(gsub(",", "",rich_dat$inc_1989)))
rich_dat$inc_1989[which(is.na(rich_dat$inc_1989)==T)]<-mean(rich_dat$inc_1989, na.rm=T)

#set ecological province as factor
rich_dat$ecoprovmaj<-factor(rich_dat$ecoprovmaj, levels=levels(as.factor(allUS$ecoprovmaj)))
data$ecoprovmaj<-factor(data$ecoprovmaj, levels=levels(as.factor(allUS$ecoprovmaj)))
#set city fips code to numeric
data$PLACEFIPS<-as.numeric(as.character(data$PLACEFIPS))

#calculate total trees of all genera across size places by city fips code
spsum<-aggregate(small~city2, rich_dat, FUN=sum)
spsum2<-aggregate(med~city2, rich_dat, FUN=sum)
spsum3<-aggregate(large~city2, rich_dat, FUN=sum)
spsum4<-rich_dat[!duplicated(rich_dat$city2),]

#set up data frame with summed tree abundances
cordat<-cbind(spsum, spsum2[,2], spsum3[,2])
colnames(cordat)[2:4]<-c("small_cor", "med_cor", "large_cor")
#merge  tree abundances with site-level predictors
data<-merge(spsum4, cordat, by="city2")
cormat<-cor(data[,c(6:7,10:25,27,28,31:33,35:57)])
data<-data[,(which(colnames(data)%in%c("HOUSEUNITS", "fdmed1stmean","fdmedlastmean","gddmenmean", "pptmenmean", "tmpmaxmean", "tmpmaxsummean", "phzone", "inc_1969", "inc_1979", "bio14", "bio17", "bio18")==F))]
#clean up income data and fill in missing values with the mean
data$inc_1989<-as.numeric(as.character(gsub(",","",data$inc_1989)))
data$inc_1989[which(is.na(data$inc_1989)==T)]<-mean(data$inc_1989, na.rm=T)


##check for and remove highly correlated variables
colnames(data)
cormat<-cor(data[,c(6,9:17,19:20,23,25:44)])
data<-data[,(which(colnames(data)%in%c("phmnxtmpmean","bio1","bio3", "bio4", "bio5", "bio6","bio9")==F))]
colnames(data)
cormat<-cor(data[,c(6,9:16,18:19,22,24:37)])
data<-data[,(which(colnames(data)%in%c("phmnxtmpmean","bio7", "bio16")==F))]
colnames(data)
cormat<-cor(data[,c(6,9:16,18:19,22,24:35)])
cormat<-cor(cbind(data[,c(6,9:16,18:19,22,24:35)], as.numeric(as.character(data$ecoprovmaj))))
data<-data[,(which(colnames(data)%in%c("ycoord")==F))]
cormat<-cor(cbind(data[,c(6,9:16,18,21,23:34)], as.numeric(as.character(data$ecoprovmaj))))
data<-data[,(which(colnames(data)%in%c("bio19")==F))]
cormat<-cor(cbind(data[,c(6,9:16,18,21,23:33)], as.numeric(as.character(data$ecoprovmaj))))
data<-data[,(which(colnames(data)%in%c("xcoord")==F))]
cormat<-cor(cbind(data[,c(6,9:16,20,22:32)], as.numeric(as.character(data$ecoprovmaj))))
#check finalized variables have correlations below 0.8
max(abs(cormat[lower.tri(cormat) | upper.tri(cormat)]))

#create smoother terms based on finaized variables 
varz=c("s(bio2,k=3)","s(bio8,k=3)","s(bio10,k=3)","s(bio12,k=3)","s(bio13,k=3)","s(bio15,k=3)","s(bio11,k=3)","s(canopy,k=3)","s(ha,k=3)","s(pptmeasmean,k=3)","ecoprovmaj","s(frzfremedmean, k=3)","s(logdtrdnsmean, k=3)","s(POPULATION, k=3)","s(mindexmean,k=3)","s(Mean.YEAR,k=3)","s(Mean.Value,k=3)","s(inc_1989, k=3)", "s(coastdat.coast3, k=3)", "s(uselevmean, k=3)")


data$small<-round(data$small)
allUS$ecoprovmaj<-as.factor(allUS$ecoprovmaj)
data$ecoprovmaj<-factor(data$ecoprovmaj, levels=levels(allUS$ecoprovmaj))

#use integer tree abundances
data$small_cor<-round(data$small_cor)
data$med_cor<-round(data$med_cor)
data$large_cor<-round(data$large_cor)

#alternative naming of smoother variables to help associate with columns in dataframe
varz3<-gsub(",.*", ")", varz)
varz4<-gsub("s\\(", "",varz)
varz4<-gsub(",.*", "",varz4)

#fit total tree models by size class via backward selection gam using a stopping rule for model-level concurvity below 0.8 (gam equivalent of collinearity, see manuscript for more info)

#compare fit to boosted regression trees (BRTs have greater predictive performance)
if (fit==T)
{
data$log_small_cor<-log(data$small_cor+1)
tot_small_brt<-gbm.step(data=data, gbm.x=c(6,9:17,20, 22:32), gbm.y=53,n.folds=10, n.trees=30, family='gaussian', tree.complexity = 3, learning.rate=0.01, drop.unused.levels=F)
data$log_med_cor<-log(data$med_cor+1)
tot_med_brt<-gbm.step(data=data, gbm.x=c(6,9:17,20, 22:32), gbm.y=54,n.folds=10, n.trees=30, family='gaussian', tree.complexity = 3, learning.rate=0.005, drop.unused.levels=F)

data$log_large_cor<-log(data$large_cor+1)
tot_large_brt<-gbm.step(data=data, gbm.x=c(6,9:17,20, 22:32), gbm.y=55,n.folds=10, n.trees=30, family='gaussian', tree.complexity = 2, learning.rate=0.005, drop.unused.levels=F)
}
if (fit==F)
{

  tot_small_brt<-readRDS( file="./output/tot_small_brt.rds")
  tot_med_brt<-readRDS( file="./output/tot_med_brt.rds")
  tot_large_brt<-readRDS( file="./output/tot_large_brt.rds")
}

##merge output from models to spatial data
pred_small_trees<-exp(predict.gbm(tot_small_brt, n.trees=tot_small_brt$n.trees))
pred_med_trees<-exp(predict.gbm(tot_med_brt, n.trees=tot_med_brt$n.trees))
pred_large_trees<-exp(predict.gbm(tot_large_brt, n.trees=tot_large_brt$n.trees))

rich_dat$city<-as.character(rich_dat$city)
pred_each<-(cbind(as.character(data$city), pred_small_trees, pred_med_trees,pred_large_trees))
pred_each<-as.data.frame(pred_each)
pred_each[,2]<-as.numeric(as.character(pred_each[,2]))
pred_each[,3]<-as.numeric(as.character(pred_each[,3]))
pred_each[,4]<-as.numeric(as.character(pred_each[,4]))
colnames(pred_each)[1]<-"city"

wpred_data<-merge(rich_dat[,which(colnames(rich_dat)%in%colnames(data)==T)], pred_each, by="city")

if (write==T)
{
write.csv(wpred_data, file='./data/wpred_totaltreedata4.csv', row.names=F)
saveRDS(tot_small_brt, file="./data/tot_small_brt.rds")
saveRDS(tot_med_brt, file="./data/tot_med_brt.rds")
saveRDS(tot_large_brt, file="./data/tot_large_brt.rds")
}
