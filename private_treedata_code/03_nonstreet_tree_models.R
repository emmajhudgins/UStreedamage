rm(list=c(ls()))
library(gbm)
library(mgcv)
library(dismo)

###script provided for reference, but data are private###
### calculated ntotal tree abundance and genus specific abundance for all land use classes, extrapolatees to all US communities.

rich_dat<-read.csv('urb_incmiss_apr.csv')
data<-read.csv('urb_new_apr.csv')
dataold<-read.csv('toturb_2019-2.csv')
rich_dat[which(rich_dat$city=="Fairfax Co"), c("xcoord", "ycoord")]<-data[which(data$city=="Fairfax Co"),  c("xcoord", "ycoord")] # fill in missing coords
allUS<-read.csv('entire_predictors_fipsfix.csv')


##clean up data
nyc<-which(rich_dat$city=="NYC")
rich_dat$inc_1969<-as.numeric(as.character(rich_dat$inc_1969))
rich_dat$inc_1969[which(is.na(rich_dat$inc_1969)==T)]<-mean(rich_dat$inc_1969, na.rm=T)
rich_dat$inc_1979<-as.numeric(as.character(rich_dat$inc_1979))
rich_dat$inc_1979[which(is.na(rich_dat$inc_1979)==T)]<-mean(rich_dat$inc_1979, na.rm=T)
rich_dat$inc_1989<-as.numeric(as.character(rich_dat$inc_1989))
rich_dat$inc_1989[which(is.na(rich_dat$inc_1989)==T)]<-mean(rich_dat$inc_1989, na.rm=T)
rich_dat$ecoprovmaj<-as.factor(rich_dat$ecoprovmaj)
data$ecoprovmaj<-as.factor(data$ecoprovmaj)rich_dat$ecoprovmaj<-factor(rich_dat$ecoprovmaj, levels=levels(as.factor(allUS$ecoprovmaj)))
data$ecoprovmaj<-factor(data$ecoprovmaj, levels=levels(as.factor(allUS$ecoprovmaj)))
data$PLACEFIPS<-as.numeric(as.character(data$PLACEFIPS))
match<-which(allUS$PLACEFIPS==3651000)

#smoother terms
varz=c("s(cadinc, k=3)","s(cadgr,k=3)","s(xcoord,k=3)","s(ycoord,k=3)","s(bio1,k=3)","s(bio2,k=3)","s(bio3,k=3)","s(bio4,k=3)","s(bio12,k=3)","s(bio13,k=3)","s(bio15,k=3)","s(bio18,k=3)","s(bio19,k=3)","s(canopy,k=3)","s(phzone,k=3)","s(ha,k=3)","s(pptmeasmean,k=3)","s(pptmenmean,k=3)","s(tmpmaxmean,k=3)","s(gddmenmean,k=3)","ecoprovmaj","s(frzfremedmean, k=3)","s(fdmed1stmean, k=3)","s(fdmedlastmean, k=3)","s(logdtrdnsmean, k=3)","s(phmnxtmpmean, k=3)","s(HOUSEUNITS, k=3)","s(POPULATION, k=3)","s(mindexmean,k=3)","s(inc_1969, k=3)","s(Mean.YEAR,k=3)","s(Mean.Value,k=3)","s(inc_1979, k=3)","s(inc_1989, k=3)","s(tmpmaxsummean, k=3)", "s(coastdat.coast3, k=3)", "s(trait_value.diameter.at.breast.height..1.3.m., k=3)","s(trait_value.leaf.area , k=3)","s(trait_value.leaf.dry.mass, k=3)" ,"s(trait_value.seed.mass, k=3)", "s(trait_value.whole.plant.height, k=3)", "s(OrigValueStr.Leaf.length, k=3)", "s(OrigValueStr.Leaf.thickness, k=3)" ,"s(OrigValueStr.Leaf.width, k=3)", "s(OrigValueStr.Plant.growth.rate, k=3)","s(OrigValueStr.Plant.height.vegetative, k=3)" , "s(OrigValueStr.Plant.lifespan..longevity.,k=3)", "s(OrigValueStr.Stem.diameter, k=3)", "s(bygenus_pests_each_city[,1], k=2)","s(bygenus_pests_each_city[,3], k=3)","s(bygenus_pests_each_city[,5], k=2)","s(bygenus_pests_each_city[,6], k=2)","s(bygenus_pests_each_city[,7], k=2)","s(bygenus_pests_each_city[,9], k=2)","s(bygenus_pests_each_city[,10], k=3)","s(bygenus_pests_each_city[,11], k=2)","s(bygenus_pests_each_city[,13], k=2)","s(bygenus_pests_each_city[,14], k=2)","s(bygenus_pests_each_city[,15], k=2)", "s(data_year, k=3)")

data$inc_1969<-as.numeric(as.character(data$inc_1969))
data$inc_1969[which(is.na(data$inc_1969)==T)]<-mean(data$inc_1969, na.rm=T)
data$inc_1979<-as.numeric(as.character(data$inc_1979))
data$inc_1979[which(is.na(data$inc_1979)==T)]<-mean(data$inc_1979, na.rm=T)
data$inc_1989<-as.numeric(as.character(data$inc_1989))
data$inc_1989[which(is.na(data$inc_1989)==T)]<-mean(data$inc_1989, na.rm=T)
data$small<-round(data$small)
allUS$ecoprovmaj<-as.factor(allUS$ecoprovmaj)
data[41, colnames(allUS)[which(colnames(allUS)%in%colnames(data))]]<-allUS[match,colnames(allUS)[which(colnames(allUS)%in%colnames(data))]]

rich_dat[nyc,"phzone"]<-72
rich_dat[nyc,"ecoprovmaj"]<-221
data[41, "ecoprovmaj"]<-221
data$ecoprovmaj<-factor(data$ecoprovmaj, levels=levels(allUS$ecoprovmaj))


##total small tree abundance models
data$log_small<-log1p(data$small)

#manual forward selection with varz variables
tot_small_gam<-gam(as.formula(paste("small~",varz[21], varz[16],varz[35],varz[14],varz[31],varz[5],sep="+")), select=T, method="GCV.Cp", data=data, family="poisson", drop.unused.levels = F)

pred_small_trees<-predict.gam(tot_small_gam, type="response")

##total medium tree abundance models
data$med<-round(data$med)
#manual forward selection with varz variables
tot_med_gam<-gam(as.formula(paste("med~s(pred_small_trees,k=3)",varz[21],varz[12],varz[35],varz[26],varz[36],varz[20],varz[32],varz[22],sep="+")), select=T, method="GCV.Cp", data=data, family="poisson", drop.unused.levels = F)
pred_med_trees<-predict.gam(tot_med_gam, type="response")

#large models
#manual forward selection with varz variables
data$large<-round(data$large)
tot_large_gam<-gam(as.formula(paste("large~s(pred_small_trees, k=3)+s(pred_med_trees,k=3)",sep="+")), select=T, method="GCV.Cp", data=data, family="poisson", drop.unused.levels = F)

pred_large_trees<-predict.gam(tot_large_gam, type="response")

##Two-part models, fit brt or gam to binomial small tree presence, then log(n_small+1) gaussian model to subset where present, rescaling to get correct mean expectations (either by site or countrywide - by site seems to work worse for some reason)


#helper functions
r2mse<-function(obs, pred)
{
1-((sum((wpred_data[,obs]-(pred))^2))/(sum((wpred_data[,obs]-mean(wpred_data[,obs]))^2)))
}

plot_pred<-function(obs, pred)
{
  plot(wpred_data[,obs]~pred, xlab="Predicted # Trees", ylab="Observed # Trees")
  abline(0,1)
}

rescale_constant<-function(prednum, predexist,sizex)
{
  return(prednum*predexist*(sum(wpred_data[,sizex])/sum(prednum*predexist)))
}

preds_gam<-function(mod, size=NULL)
{
  if (is.null(size)==T)
  {
  preds<-(predict.gam(mod, newdata=wpred_data, type="response"))
  }
  if (is.null(size)==F)
  {
    preds<-rep(0,nrow(rich_dat))
    preds[which(wpred_data[,size]>0)]<-predict.gam(mod, type="response")
  }
  preds[which(is.na(preds)==T)]<-0
  return(preds)
}

preds_ziP<-function(mod)
{
  preds<-rep(0,nrow(rich_dat))
  preds<-predict.gam(mod, type="response", newdata=wpred_data)
  preds[which(is.na(preds)==T)]<-0
  return(preds)
}

preds_brt<-function(mod)
{
  return(predict.gbm(mod, newdata=wpred_data, n.trees=mod$n.trees, type="response"))
}

preds_brtzip<-function(mod, size)
{
  preds<-rep(0,nrow(rich_dat))
  if(size=="med"){preds[which(wpred_data$SP%in%wpred_data_med$SP)]<-predict.gbm(mod, newdata=wpred_data_med, n.trees=mod$n.trees, type="response")}
  if(size=="large"){preds[which(wpred_data$SP%in%wpred_data_large$SP)]<-predict.gbm(mod, newdata=wpred_data_large, n.trees=mod$n.trees, type="response")}
  return(preds)
}

## genus-specific models using same approach as street tree models
##data cleaning
rich_dat$inc_1969<-as.numeric(as.character(rich_dat$inc_1969))
rich_dat$inc_1969[which(is.na(rich_dat$inc_1969)==T)]<-mean(rich_dat$inc_1969, na.rm=T)
rich_dat$inc_1979<-as.numeric(as.character(rich_dat$inc_1979))
rich_dat$inc_1979[which(is.na(rich_dat$inc_1979)==T)]<-mean(rich_dat$inc_1979, na.rm=T)
rich_dat$inc_1989<-as.numeric(as.character(rich_dat$inc_1989))
rich_dat$inc_1989[which(is.na(rich_dat$inc_1989)==T)]<-mean(rich_dat$inc_1989, na.rm=T)
rich_dat$city<-as.character(rich_dat$city)
pred_each<-(cbind(as.character(data$city), pred_small_trees, pred_med_trees,pred_large_trees))
pred_each<-as.data.frame(pred_each)
pred_each[,2]<-as.numeric(as.character(pred_each[,2]))
pred_each[,3]<-as.numeric(as.character(pred_each[,3]))
pred_each[,4]<-as.numeric(as.character(pred_each[,4]))
colnames(pred_each)[1]<-"city"
rich_dat$small<-round(rich_dat$small,0)
data$PLACEFIPS[41]<-"NYC-other"
rich_dat$data_year<-data$data_year[match(rich_dat$PLACEFIPS, data$PLACEFIPS)]


rich_dat$city[which(rich_dat$city=="41")]<-"NYC"
wpred_data<-merge(rich_dat, pred_each, by="city")


wpred_data$ecoprovmaj<-factor(wpred_data$ecoprovmaj, levels=levels(data$ecoprovmaj))

#small tree models
#BRT outperforms GAM
wpred_data$is_small<-as.numeric(wpred_data$small>0)

is_small_brt<-gbm.step(data=wpred_data, gbm.x=c(2,11:22,40:78, 81:88, 90:105), gbm.y=106, n.folds=10, n.trees=30, family='bernoulli', tree.complexity = 3, learning.rate=0.05, drop.unused.levels=F)
rich_dat_small<-subset(wpred_data, is_small==1)
rich_dat_small$ecoprovmaj<-factor(rich_dat_small$ecoprovmaj, levels=levels(data$ecoprovmaj))
rich_dat_small$log_n_small<-log(rich_dat_small$small+1)
small_brt<-gbm.step(data=rich_dat_small, gbm.x=c(2,20,27:64, 67:74,81:92,93), gbm.y=97, n.folds=10, n.trees=30, family='gaussian', tree.complexity = 4, learning.rate=0.03)

pred_num<-exp(preds_brt(small_brt))
pred_exist<-preds_brt(is_small_brt)
final_pred<-rescale_constant(pred_num, pred_exist,"small")
ratio_small<-sum(wpred_data[,"small"])/sum(pred_num*pred_exist)
# r2mse("small", final_pred)
# plot_pred("small", final_pred)
wpred_data$pred_small<-final_pred


### Medium tree models (brt vs. gam)
# BRT outperforms GAM
# #remove spp not found as medium trees anywhere 
med_SP<-unique(as.character(rich_dat$SP))[which(tapply(rich_dat$med, rich_dat$SP,sum)>0)]
rich_dat_med<-subset(wpred_data, SP%in%med_SP)
rich_dat_med$log_med<-log(rich_dat_med$med+1)
 

large_SP<-unique(rich_dat$SP[which(rich_dat$large.x>0)])
rich_dat_large<-subset(rich_dat, SP%in%large_SP)
rich_dat_large$log_large<-log(rich_dat_large$large.x+1)

#BRT outperforms GAM for small trees
is_small_mod<-gam(as.formula(paste("is_small~SP+s(pred_small_trees, k=3)+",paste(varz[c(1:14, 16:20, 22:36)], collapse='+'), sep="+")), select=T, method="GCV.Cp", data=wpred_data, family=binomial)

is_small_brt<-gbm.step(data=rich_dat, gbm.x=c(2,35:75,79:80, 84:89), gbm.y=91, n.folds=10, n.trees=30, family='bernoulli', tree.complexity = 3, learning.rate=0.01) #fits gbm with optimal number of trees - tweak tree complexity to maximize cv correlation, adjust learning rate and tolerance to ensure at least 1000 trees
 rich_dat_small$log_n_small<-log(rich_dat_small$small.x+1)
small_brt<-gbm.step(data=rich_dat_small, gbm.x=c(2,4:14,34:74,78:79, 83:88), gbm.y=91, n.folds=10, n.trees=30, family='gaussian', tree.complexity = 3, learning.rate=0.0001)
 rich_dat_small<-subset(rich_dat, small.x>0)
small_mod2<-gam(as.formula(paste("log(small+1)~SP+",paste(varz[c(1:14,16:20,21,22:36)], collapse="+"),sep="")), select=T, method="GCV.Cp", data=rich_dat_small, family=gaussian)
#pred_num<-preds_gam(small_mod2)
#pred_exist<-preds_gam(is_small_mod)
#final_pred<-rescale_constant(pred_num, pred_exist,"small.x")
#final_pred<-rescale_city(pred_num, pred_exist, "small")
#r2mse("small.x", final_pred)
#plot_pred("small.x", final_pred)

## Medium 2-step models

is_med_mod<-gam(as.formula(paste("is_med~SP+s(pred_small, k=3)+s(pred_med_trees, k=3)+",paste(varz[c(1:14, 16:20, 22:48)], collapse='+'), sep="+")), select=T, method="GCV.Cp", data=wpred_data, family=binomial)


wpred_data$is_med<-wpred_data$medium>0

is_med_brt<-gbm.step(data=wpred_data, gbm.x=c(2,20,27:64, 67:74,81:92,93,94), gbm.y=98, n.folds=10, n.trees=30, family='bernoulli', tree.complexity = 5, learning.rate=0.005) #fits gbm with optimal number of trees - tweak tree complexity to maximize cv correlation, adjust learning rate and tolerance to ensure at least 1000 trees


rich_dat_med<-subset(wpred_data, medium>0)
rich_dat_med$ecoprovmaj<-factor(rich_dat_med$ecoprovmaj, levels=levels(data$ecoprovmaj))
rich_dat_med$log_n_med<-log(rich_dat_med$medium+1)

med_brt<-gbm.step(data=rich_dat_med, gbm.x=c(2,20,27:64, 67:74,81:92,93,94), gbm.y=99, n.folds=10, n.trees=30, family='gaussian', tree.complexity = 5, learning.rate=0.004)

#gam underperforms BRT for medium trees      
med_mod<-gam(as.formula(paste("log(med+1)~SP+s(pred_small,k=3)+s(pred_med_trees,k=3)+",paste(varz[c(1:14,16:20,22:36)], collapse="+"),sep="")), select=T, method="GCV.Cp", data=rich_dat_med, family=gaussian)
pred_num<-exp(preds_brt(med_brt))
pred_exist<-preds_brt(is_med_brt)
final_pred<-rescale_constant(pred_num, pred_exist,"medium")
ratio_med<-sum(wpred_data[,"medium"])/sum(pred_num*pred_exist)
# r2mse("medium", final_pred)
# plot_pred("medium", final_pred)
wpred_data$pred_med<-final_pred

#large tree models- GAM outperforms BRT

wpred_data$is_large<-as.numeric(wpred_data$large>1)

is_large_mod<-gam(as.formula(paste("is_large~s(pred_small,k=3)+s(pred_med,k=3)+s(pred_large_trees, k=3)+SP",paste(varz[c(3:20, 22:48)], collapse='+'), sep="+")), select=T, method="GCV.Cp", data=wpred_data, family=binomial, drop.unused.levels = F)
sum_ilm<-summary(is_large_mod)

var<-colnames(wpred_data)[c(2,35:75,79:80, 84:89,92,94)]
is_large_brt<-gbm.step(data=wpred_data, gbm.x=c(2,11:22,40:78, 81:88, 92,94,95), gbm.y=26, n.folds=10, n.trees=30, family='bernoulli', tree.complexity = 1, learning.rate=0.001) #fits gbm with optimal number of trees - tweak tree complexity to maximize cv correlation, adjust learning rate and tolerance to ensure at least 1000 trees

rich_dat_large<-subset(wpred_data, large>0)
rich_dat_large$ecoprovmaj<-factor(rich_dat_large$ecoprovmaj, levels=levels(data$ecoprovmaj))
rich_dat_large$log_n_large<-log(rich_dat_large$large+1)

large_brt<-gbm.step(data=rich_dat_large, gbm.x=c(2,11:22,40:78, 81:88, 92,94,95), gbm.y=96, bag.fraction = 0.95,n.folds=10, n.trees=30, family='gaussian', tree.complexity = 2, learning.rate=0.003) #brt underperforms GAM for large trees


#not enough data to use select function, manually performed fwd sel, and this was the resulting model

 large_mod<-gam(as.formula(paste("log_n_large~s(pred_med, k=3)+s(pred_large_trees, k=3)+s(pred_med, k=3)+s(pred_small, k=3)+SP",varz[18],sep="+")), select=T, method="GCV.Cp", data=rich_dat_large, family=gaussian)
  cat(i, large_mod$aic, "\n")

pred_num<-exp(preds_gam(large_mod, "large"))
#pred_num<-exp(preds_brt(large_brt))
pred_exist<-preds_gam(is_large_mod)
ratio_large<-sum(wpred_data[,"large"])/sum(pred_num*pred_exist)
final_pred<-rescale_constant(pred_num, pred_exist,"large")
# r2mse("large", final_pred)
# plot_pred("large", final_pred)


##predict on US
 
 allUS<-read.csv('entire_predictors.csv')
 acs<-read.csv('ACS_15_5YR_DP04_with_ann.csv')[,c(2,15,26)]

 colnames(acs)[1]<-"PLACEFIPS"
 allUS2<-merge(allUS, acs, by="PLACEFIPS")
r <- raster::getData("worldclim",var="bio",res=2.5)
coords<-cbind(allUS2$xcoord,allUS2$ycoord)
coords<-SpatialPoints(coords)
proj4string(coords)<-"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
coords<-spTransform(coords,CRS='+proj=longlat +ellps=WGS84')
values <- extract(r,coords)
allUS<-cbind(allUS2, values)
coast3<-read.csv('allcoast.csv') # distance to coast
allUS<-cbind(allUS,coast3)
genera<-as.character(unique(wpred_data$SP))
colnames(allUS)[c(4,5,54)]<-c("HOUSEUNITS", "POPULATION","coastdat.coast3")
sub<-wpred_data[!duplicated(wpred_data$SP),81:92]
canopy<-read.csv('censusvars_toadd.csv')[,c(1,3)]
colnames(canopy)[2]<-"canopy"
allUS<-merge(allUS, canopy, by="PLACEFIPS")
allUS$data_year<-rep(2005, nrow(allUS))
fix_cols<-c(4:5,8:21,23,25:26, 29:31, 33:55)
matchcol<-match(colnames(allUS)[fix_cols], colnames(wpred_data))
allUS$inc_1969<-as.numeric(gsub(",","",(as.character(allUS$inc_1969))))
allUS$inc_1979<-as.numeric(gsub(",","",(as.character(allUS$inc_1979))))
allUS$inc_1989<-as.numeric(gsub(",","",(as.character(allUS$inc_1989))))
allUS$Mean.Value<-as.numeric(as.character(allUS$Mean.Value))
allUS$Mean.YEAR<-as.numeric(as.character(allUS$Mean.YEAR))

allUS_small<-allUS
for (i in 1:length(fix_cols))
{
  allUS_small[which(allUS_small[,fix_cols[i]]>max(rich_dat_small[,matchcol[i]], na.rm=T)),fix_cols[i]]<-max(rich_dat_small[,matchcol[i]], na.rm=T)
  allUS_small[which(allUS_small[,fix_cols[i]]<min(rich_dat_small[,matchcol[i]], na.rm=T)),fix_cols[i]]<-min(rich_dat_small[,matchcol[i]], na.rm=T)
}

allUS_med<-allUS
for (i in 1:length(fix_cols))
{
  allUS_med[which(allUS_med[,fix_cols[i]]>max(rich_dat_med[,matchcol[i]], na.rm=T)),fix_cols[i]]<-max(rich_dat_med[,matchcol[i]], na.rm=T)
  allUS_med[which(allUS_med[,fix_cols[i]]<min(rich_dat_med[,matchcol[i]], na.rm=T)),fix_cols[i]]<-min(rich_dat_med[,matchcol[i]], na.rm=T)
}

allUS_large<-allUS
for (i in 1:length(fix_cols))
{
  allUS_large[which(allUS_large[,fix_cols[i]]>max(rich_dat_large[,matchcol[i]], na.rm=T)),fix_cols[i]]<-max(rich_dat_large[,matchcol[i]],na.rm=T)
  allUS_large[which(allUS_large[,fix_cols[i]]<min(rich_dat_large[,matchcol[i]], na.rm=T)),fix_cols[i]]<-min(rich_dat_large[,matchcol[i]], na.rm=T)
}


allUS_small$inc_1969[which(is.na(allUS_small$inc_1969)==T)]<-mean(allUS_small$inc_1969, na.rm=T)
allUS_small$inc_1979[which(is.na(allUS_small$inc_1979)==T)]<-mean(allUS_small$inc_1979, na.rm=T)
allUS_small$inc_1989[which(is.na(allUS_small$inc_1989)==T)]<-mean(allUS_small$inc_1989, na.rm=T)
allUS_small$Mean.Value[which(is.na(allUS_small$Mean.Value)==T)]<-mean(allUS_small$Mean.Value, na.rm=T)
allUS_small$Mean.YEAR[which(is.na(allUS_small$Mean.YEAR)==T)]<-mean(as.numeric(as.character((allUS_small$Mean.YEAR))), na.rm=T)


allUS_med$inc_1969[which(is.na(allUS_med$inc_1969)==T)]<-mean(allUS_med$inc_1969, na.rm=T)
allUS_med$inc_1979[which(is.na(allUS_med$inc_1979)==T)]<-mean(allUS_med$inc_1979, na.rm=T)
allUS_med$inc_1989[which(is.na(allUS_med$inc_1989)==T)]<-mean(allUS_med$inc_1989, na.rm=T)
allUS_med$Mean.Value[which(is.na(allUS_med$Mean.Value)==T)]<-mean(allUS_med$Mean.Value, na.rm=T)
allUS_med$Mean.YEAR[which(is.na(allUS_med$Mean.YEAR)==T)]<-mean(as.numeric(as.character((allUS_med$Mean.YEAR))), na.rm=T)

allUS_large$inc_1969[which(is.na(allUS_large$inc_1969)==T)]<-mean(allUS_large$inc_1969, na.rm=T)
allUS_large$inc_1979[which(is.na(allUS_large$inc_1979)==T)]<-mean(allUS_large$inc_1979, na.rm=T)
allUS_large$inc_1989[which(is.na(allUS_large$inc_1989)==T)]<-mean(allUS_large$inc_1989, na.rm=T)
allUS_large$Mean.Value[which(is.na(allUS_large$Mean.Value)==T)]<-mean(allUS_large$Mean.Value, na.rm=T)
allUS_large$Mean.YEAR[which(is.na(allUS_large$Mean.YEAR)==T)]<-mean(as.numeric(as.character((allUS_large$Mean.YEAR))), na.rm=T)
rm(canopy)
predicted_trees<-matrix(0, 1,75)
colnames(predicted_trees)<-c(colnames(allUS), "SP", colnames(sub), "pred_small_trees", "pred_med_trees", "pred_large_trees", "pred_small", "pred_med", "pred_large")
for (i in 1:nrow(allUS))
{
to_add_small<-cbind(allUS_small[rep(i, 48),],genera, sub)
to_add_small$ecoprovmaj<-as.factor(to_add_small$ecoprovmaj)
to_add_small$pred_small_trees<-predict.gam(tot_small_gam, type="response", newdata=to_add_small)
to_add_med<-cbind(allUS_med[rep(i, 48),],genera, sub)
to_add_small$pred_small_trees[which((to_add_small$pred_small_trees>max(rich_dat_small$pred_small_trees))==T)]<-max(rich_dat_small$pred_small_trees)
to_add_small$pred_small_trees[which((to_add_small$pred_small_trees<min(rich_dat_small$pred_small_trees))==T)]<-min(rich_dat_small$pred_small_trees)
to_add_med$ecoprovmaj<- as.factor(to_add_med$ecoprovmaj)
to_add_med$pred_small_trees<-to_add_small$pred_small_trees
to_add_med$pred_med_trees<-predict.gam(tot_med_gam, type="response", newdata=to_add_med)
to_add_med$pred_med_trees[which((to_add_med$pred_med_trees>max(rich_dat_med$pred_med_trees))==T)]<-max(rich_dat_med$pred_med_trees)
to_add_med$pred_med_trees[which((to_add_med$pred_med_trees<min(rich_dat_med$pred_med_trees))==T)]<-min(rich_dat_med$pred_med_trees)
to_add_large<-cbind(allUS_large[rep(i, 48),],genera, sub)
to_add_large$ecoprovmaj<- as.factor(to_add_large$ecoprovmaj)
to_add_large$pred_small_trees<-to_add_small$pred_small_trees
to_add_large$pred_med_trees<-to_add_med$pred_med_trees
to_add_large$pred_large_trees<-predict.gam(tot_large_gam, type="response", newdata=to_add_large)
to_add_large$pred_large_trees[which((to_add_large$pred_large_trees>max(rich_dat_large$pred_large_trees))==T)]<-max(rich_dat_large$pred_large_trees)
to_add_large$pred_large_trees[which((to_add_large$pred_large_trees<min(rich_dat_large$pred_large_trees))==T)]<-min(rich_dat_large$pred_large_trees)
colnames(to_add_small)[57]<-"SP"
colnames(to_add_med)[57]<-"SP"
colnames(to_add_large)[57]<-"SP"
small_is_add<-predict.gbm(is_small_brt,newdata=to_add_small, n.trees=is_small_brt$n.trees, type="response")
small_add<-exp(predict.gbm(small_brt,newdata=to_add_small, n.trees=small_brt$n.trees, type="response"))
to_add_small$pred_small<-small_add*small_is_add*ratio_small
to_add_med$pred_small<-to_add_small$pred_small
med_sub<-subset(to_add_med, SP%in%rich_dat_med$SP)
med_is_add<-predict.gbm(is_med_brt,newdata=med_sub, n.trees=is_med_brt$n.trees, type="response")
med_add<-exp(predict.gbm(med_brt,newdata=med_sub, n.trees=med_brt$n.trees, type="response"))
to_add_med$pred_med<-rep(0,48)
to_add_med$pred_med[which(genera%in%rich_dat_med$SP)]<-med_add*med_is_add*ratio_med
to_add_large$pred_small<-to_add_small$pred_small
to_add_large$pred_med<-to_add_med$pred_med
large_sub<-subset(to_add_large, SP%in%rich_dat_large$SP)
large_is_add<-predict.gam(is_large_mod,newdata=large_sub,type="response")
large_add<-exp(predict.gam(large_mod,newdata=large_sub, type="response"))
to_add_large$pred_large<-rep(0,48)
to_add_large$pred_large[which(genera%in%rich_dat_large$SP)]<-large_add*large_is_add*ratio_large
predicted_trees<-rbind(predicted_trees, to_add_large)
}
predicted_trees<-predicted_trees[2:nrow(predicted_trees),]
write.csv(predicted_trees, file="predicted_trees_allUS_2019-apr.csv", row.names=F)

saveRDS(large_mod, file="large_mod.rds")
saveRDS(is_large_mod, file="is_large_mod.rds")
saveRDS(small_brt, file="small_brt.rds")
saveRDS(is_small_brt, file="is_small_brt.rds")
saveRDS(med_brt, file="med_brt.rds")
saveRDS(is_med_brt, file="is_med_brt.rds")
saveRDS(tot_small_gam, file="tot_small_gam.rds")
saveRDS(tot_med_gam, file="tot_med_gam.rds")
saveRDS(tot_large_gam, file="tot_large_gam.rds")


wpred_data$pred_large<-final_pred
write.csv(wpred_data, file="model_predictions_cities_apr2019.csv", row.names=F)
