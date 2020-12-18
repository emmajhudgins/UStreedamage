rm(list=c(ls()))
write=F
fit=F
library(gbm)
library(mgcv)
library(dismo)
rich_dat<-read.csv('street_tree_each_cor2.csv')
rich_dat<-subset(rich_dat, city2!="NEW YORK NY")
data<-read.csv('street_tree_cor.csv')
allUS<-read.csv('entire_predictors_fipsfix.csv')

#rm_gen<-c("cinnamomum", "acacia", "citrus")
rich_dat$inc_1969<-as.numeric(as.character(gsub(",", "",rich_dat$inc_1969)))
rich_dat$inc_1969[which(is.na(rich_dat$inc_1969)==T)]<-mean(rich_dat$inc_1969, na.rm=T)
rich_dat$inc_1979<-as.numeric(as.character(gsub(",", "",rich_dat$inc_1979)))
rich_dat$inc_1979[which(is.na(rich_dat$inc_1979)==T)]<-mean(rich_dat$inc_1979, na.rm=T)
rich_dat$inc_1989<-as.numeric(as.character(gsub(",", "",rich_dat$inc_1989)))
rich_dat$inc_1989[which(is.na(rich_dat$inc_1989)==T)]<-mean(rich_dat$inc_1989, na.rm=T)


rich_dat$ecoprovmaj<-factor(rich_dat$ecoprovmaj, levels=levels(as.factor(allUS$ecoprovmaj)))
data$ecoprovmaj<-factor(data$ecoprovmaj, levels=levels(as.factor(allUS$ecoprovmaj)))
data$PLACEFIPS<-as.numeric(as.character(data$PLACEFIPS))
spsum<-aggregate(small~city2, rich_dat, FUN=sum)
spsum2<-aggregate(med~city2, rich_dat, FUN=sum)
spsum3<-aggregate(large~city2, rich_dat, FUN=sum)
spsum4<-rich_dat[!duplicated(rich_dat$city2),]
cordat<-cbind(spsum, spsum2[,2], spsum3[,2])
colnames(cordat)[2:4]<-c("small_cor", "med_cor", "large_cor")
data<-merge(spsum4, cordat, by="city2")
cormat<-cor(data[,c(6:7,10:25,27,28,31:33,35:57)])
data<-data[,(which(colnames(data)%in%c("HOUSEUNITS", "fdmed1stmean","fdmedlastmean","gddmenmean", "pptmenmean", "tmpmaxmean", "tmpmaxsummean", "phzone", "inc_1969", "inc_1979", "bio14", "bio17", "bio18")==F))]
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
max(abs(cormat[lower.tri(cormat) | upper.tri(cormat)]))
varz=c("s(bio2,k=3)","s(bio8,k=3)","s(bio10,k=3)","s(bio12,k=3)","s(bio13,k=3)","s(bio15,k=3)","s(bio11,k=3)","s(canopy,k=3)","s(ha,k=3)","s(pptmeasmean,k=3)","ecoprovmaj","s(frzfremedmean, k=3)","s(logdtrdnsmean, k=3)","s(POPULATION, k=3)","s(mindexmean,k=3)","s(Mean.YEAR,k=3)","s(Mean.Value,k=3)","s(inc_1989, k=3)", "s(coastdat.coast3, k=3)", "s(uselevmean, k=3)")

#fit total tree models by size class via fwd selection gam (too many predictors otherwise - be wary of categorical vars inflating coefficients to unrealistic values)
data$inc_1989<-as.numeric(as.character(gsub(",","",data$inc_1989)))
data$inc_1989[which(is.na(data$inc_1989)==T)]<-mean(data$inc_1989, na.rm=T)

data$small<-round(data$small)

allUS$ecoprovmaj<-as.factor(allUS$ecoprovmaj)

data$ecoprovmaj<-factor(data$ecoprovmaj, levels=levels(allUS$ecoprovmaj))

data$small_cor<-round(data$small_cor)
data$med_cor<-round(data$med_cor)
data$large_cor<-round(data$large_cor)
varz3<-gsub(",.*", ")", varz)
varz4<-gsub("s\\(", "",varz)
varz4<-gsub(",.*", "",varz4)
if (fit==T)
{
tot_small_gam<-gam(as.formula(paste("small_cor~",paste(varz[c(1:20)],collapse="+"), sep="")), select=T, method="GCV.Cp", data=data, family="poisson", drop.unused.levels = F)
conc<-concurvity(tot_small_gam)
while(max(concurvity(tot_small_gam)[1,2:(ncol(conc)-1)])>=0.8)
{
  conc<-concurvity(tot_small_gam)
  gform<-as.formula(paste("small_cor~",paste(varz[c(1:10, 12:20)[which(varz3[c(1:10, 12:20)]%in%colnames(conc)[c(2:ncol(conc))[-which.max(conc[1,2:ncol(conc)])]])]],collapse="+"), sep=""))
  tot_small_gam<-gam(gform, select=T, method="GCV.Cp", data=data, family="poisson", drop.unused.levels = F)
}
data$log_small_cor<-log(data$small_cor+1)
tot_small_brt<-gbm.step(data=data, gbm.x=c(6,9:17,20, 22:32), gbm.y=53,n.folds=10, n.trees=30, family='gaussian', tree.complexity = 3, learning.rate=0.01, drop.unused.levels=F)
plot(data$small_cor~exp(predict.gbm(tot_small_brt, n.trees=tot_small_brt$n.trees)))
tot_med_gam<-gam(as.formula(paste("med_cor~",paste(varz[c(1:20)],collapse="+"), sep="")), select=T, method="GCV.Cp", data=data, family="poisson", drop.unused.levels = F)
conc<-concurvity(tot_med_gam)
while(max(concurvity(tot_med_gam)[1,2:(ncol(conc)-1)])>=0.8)
{
  conc<-concurvity(tot_med_gam)
  gform<-as.formula(paste("med_cor~",paste(varz[c(1:10, 12:20)[which(varz3[c(1:10, 12:20)]%in%colnames(conc)[c(2:ncol(conc))[-which.max(conc[1,2:ncol(conc)])]])]],collapse="+"), sep=""))
  tot_med_gam<-gam(gform, select=T, method="GCV.Cp", data=data, family="poisson", drop.unused.levels = F)
}
data$log_med_cor<-log(data$med_cor+1)
tot_med_brt<-gbm.step(data=data, gbm.x=c(6,9:17,20, 22:32), gbm.y=54,n.folds=10, n.trees=30, family='gaussian', tree.complexity = 3, learning.rate=0.005, drop.unused.levels=F)
tot_large_gam<-gam(as.formula(paste("large_cor~", paste(varz[c(1:20)],collapse="+"), sep="")), select=T, method="GCV.Cp", data=data, family="poisson", drop.unused.levels = F)
conc<-concurvity(tot_large_gam)
while(max(concurvity(tot_large_gam)[1,2:(ncol(conc)-1)])>=0.8)
{
  conc<-concurvity(tot_large_gam)
  gform<-as.formula(paste("large_cor~",paste(varz[c(1:10, 12:20)[which(varz3[c(1:10, 12:20)]%in%colnames(conc)[c(2:ncol(conc))[-which.max(conc[1,2:ncol(conc)])]])]],collapse="+"), sep=""))
  tot_large_gam<-gam(gform, select=T, method="GCV.Cp", data=data, family="poisson", drop.unused.levels = F)
}
data$log_large_cor<-log(data$large_cor+1)
tot_large_brt<-gbm.step(data=data, gbm.x=c(6,9:17,20, 22:32), gbm.y=55,n.folds=10, n.trees=30, family='gaussian', tree.complexity = 2, learning.rate=0.005, drop.unused.levels=F)
}
if (fit==F)
{
  tot_small_gam<-readRDS( file="tot_small_gam_cor4.rds")
  tot_med_gam<-readRDS( file="tot_med_gam_cor4.rds")
  tot_large_gam<-readRDS( file="tot_large_gam_cor4.rds")
  tot_small_brt<-readRDS( file="tot_small_brt.rds")
  tot_med_brt<-readRDS( file="tot_med_brt.rds")
  tot_large_brt<-readRDS( file="tot_large_brt.rds")
}

# pred_small_trees<-predict.gam(tot_small_gam, type="response")
# pred_med_trees<-predict.gam(tot_med_gam, type="response")
# pred_large_trees<-predict.gam(tot_large_gam, type="response")
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
write.csv(wpred_data, file='wpred_totaltreedata4.csv', row.names=F)
saveRDS(tot_small_gam, file="tot_small_gam_cor4.rds")
saveRDS(tot_med_gam, file="tot_med_gam_cor4.rds")
saveRDS(tot_large_gam, file="tot_large_gam_cor4.rds")
saveRDS(tot_small_brt, file="tot_small_brt.rds")
saveRDS(tot_med_brt, file="tot_med_brt.rds")
saveRDS(tot_large_brt, file="tot_large_brt.rds")
}
