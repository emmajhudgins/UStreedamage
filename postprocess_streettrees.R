rm(list=c(ls()))
library(gbm)
library(mgcv)
library(dismo)
setwd('~/SASUniversityEdition/myfolders/To Emma Hudgins/old_sas_output/')
#rich_dat<-read.csv('2019_spp_by_site.csv')
setwd('~/Downloads/')
rich_dat<-read.csv('street_tree_each_cor2.csv')
data<-read.csv('street_tree_cor.csv')
#rich_dat[which(rich_dat$city=="Fairfax Co"), c("xcoord", "ycoord")]<-data[which(data$city=="Fairfax Co"),  c("xcoord", "ycoord")]
allUS<-read.csv('~/Downloads/entire_predictors_fipsfix.csv')
rm_gen<-c("cinnamomum", "acacia", "citrus")
#nyc<-which(rich_dat$city=="NYC")
rich_dat$inc_1969<-as.numeric(as.character(gsub(",", "",rich_dat$inc_1969)))
rich_dat$inc_1969[which(is.na(rich_dat$inc_1969)==T)]<-mean(rich_dat$inc_1969, na.rm=T)
rich_dat$inc_1979<-as.numeric(as.character(gsub(",", "",rich_dat$inc_1979)))
rich_dat$inc_1979[which(is.na(rich_dat$inc_1979)==T)]<-mean(rich_dat$inc_1979, na.rm=T)
rich_dat$inc_1989<-as.numeric(as.character(gsub(",", "",rich_dat$inc_1989)))
rich_dat$inc_1989[which(is.na(rich_dat$inc_1989)==T)]<-mean(rich_dat$inc_1989, na.rm=T)


rich_dat$ecoprovmaj<-as.factor(rich_dat$ecoprovmaj)
#rich_dat$phzone<-as.factor(rich_dat$phzone)
data$ecoprovmaj<-as.factor(data$ecoprovmaj)
#data$phzone<-as.factor(data$phzone)
rich_dat$ecoprovmaj<-factor(rich_dat$ecoprovmaj, levels=levels(as.factor(allUS$ecoprovmaj)))
#rich_dat$phzone<-factor(rich_dat$phzone, levels=levels(as.factor(allUS$phzone)))
data$ecoprovmaj<-factor(data$ecoprovmaj, levels=levels(as.factor(allUS$ecoprovmaj)))
#data$phzone<-factor(data$phzone, levels=levels(as.factor(allUS$phzone)))
data$PLACEFIPS<-as.numeric(as.character(data$PLACEFIPS))
#match<-which(allUS$PLACEFIPS==3651000)
wpred_data2<-subset(wpred_data, genus%in%rm_gen==F)
richdat2<-subset(rich_dat, genus%in%rm_gen==F)
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
# infested_time<-read.csv('~/Desktop/OneDrive - McGill University/Grad/scripts/infestation_time.csv')
# host_gen<-read.csv('~/Desktop/OneDrive - McGill University/Grad/scripts/host_sharing.csv')
# host_gen<-host_gen[-3,]
# matchedhosts<-match(unique(each_city_trees$SP),toupper(colnames(host_gen)))
# matchedhosts<-matchedhosts[which(is.na(matchedhosts)==F)]
# host_gen<-host_gen[,matchedhosts]
# host_infestation<-list()
# data$PLACEFIPS[c(9,41)]<-c("Brooklyn", "NYC-other")
# infested_time<-merge(data, infested_time, by="PLACEFIPS")
# for (i in 1:63)
# {
#   host_infestation[[i]]<-data.frame(n.rows=nrow(data))
#   host_infestation[[i]]<-matrix(as.numeric(rep(host_gen[i,],56)), byrow=T, nrow=56, ncol=48)*matrix(rep(as.numeric(subset(infested_time, infested_time[,67]==i)$time), 48), nrow=56, ncol=48)
# }
# mort<-read.csv('~/Documents/p_mortality_mar2019.csv', stringsAsFactors=F)
# mort<-mort[-3,]
# 
# bygenus_pests_each_city<-matrix(0,nrow(rich_dat), 16)
# 
# for (j in which(mort$guild=="borers"))
# {
#   if(mort$L.M.H.VH[j]=="L")
#   {
#     bygenus_pests_each_city[,1]<-bygenus_pests_each_city[,1]+ c(host_infestation[[j]])
#   }
#  if(mort$L.M.H.VH[j]=="M")
#   {
#     bygenus_pests_each_city[,2]<-bygenus_pests_each_city[,2]+c(host_infestation[[j]])
#   }
#   if(mort$L.M.H.VH[j]=="H")
#   {
#     bygenus_pests_each_city[,3]<-bygenus_pests_each_city[,3]+c(host_infestation[[j]])
#   }
#  if(mort$L.M.H.VH[j]=="VH")
#   {
#     bygenus_pests_each_city[,4]<-bygenus_pests_each_city[,4]+c(host_infestation[[j]])
#   }
# }
# for (o in which(mort$guild=="suckers"))
# {
#   if(mort$L.M.H.VH[o]=="L")
#   {
#     bygenus_pests_each_city[,5]<-bygenus_pests_each_city[,5]+c(host_infestation[[o]])
#   }
#  if(mort$L.M.H.VH[o]=="M")
#   {
#     bygenus_pests_each_city[,6]<-bygenus_pests_each_city[,6]+c(host_infestation[[o]])
#   }
#   if(mort$L.M.H.VH[o]=="H")
#   {
#     bygenus_pests_each_city[,7]<-bygenus_pests_each_city[,7]+c(host_infestation[[o]])
#   }
#   for (n in which(mort$L.M.H.VH[o]=="VH"))
#   {
#     bygenus_pests_each_city[,8]<-bygenus_pests_each_city[,8]+c(host_infestation[[o]])
#   }
# }
# for (p in which(mort$guild=="pathogens"))
# {
#   if(mort$L.M.H.VH[p]=="L")
#   {
#     bygenus_pests_each_city[,9]<-bygenus_pests_each_city[,9]+c(host_infestation[[p]])
#   }
#   if(mort$L.M.H.VH[p]=="M")
#   {
#     bygenus_pests_each_city[,10]<-bygenus_pests_each_city[,10]+c(host_infestation[[p]])
#   }
#   if(mort$L.M.H.VH[p]=="H")
#   {
#     bygenus_pests_each_city[,11]<-bygenus_pests_each_city[,11]+c(host_infestation[[p]])
#   }
#   if(mort$L.M.H.VH[p]=="VH")
#   {
#     bygenus_pests_each_city[,12]<-bygenus_pests_each_city[,12]+c(host_infestation[[p]])
#   }
# }
# for (q in which(mort$guild=="defoliators"))
# {
#   if(mort$L.M.H.VH[q]=="L")
#   {
#     bygenus_pests_each_city[,13]<-bygenus_pests_each_city[,13]+c(host_infestation[[q]])
#   }
#   if(mort$L.M.H.VH[q]=="M")
#   {
#     bygenus_pests_each_city[,14]<-bygenus_pests_each_city[,14]+c(host_infestation[[q]])
#   }
#   if(mort$L.M.H.VH[q]=="H")
#   {
#     bygenus_pests_each_city[,15]<-bygenus_pests_each_city[,15]+c(host_infestation[[q]])
#   }
#   if(mort$L.M.H.VH[q]=="VH")
#   {
#     bygenus_pests_each_city[,16]<-bygenus_pests_each_city[,16]+c(host_infestation[[q]])
#   }
# }

#varz=c("s(cadinc, k=3)","s(cadgr,k=3)","s(xcoord,k=3)","s(ycoord,k=3)","s(bio1,k=3)","s(bio2,k=3)","s(bio3,k=3)","s(bio4,k=3)","s(bio12,k=3)","s(bio13,k=3)","s(bio15,k=3)","s(bio18,k=3)","s(bio19,k=3)","s(canopy,k=3)","s(phzone,k=3)","s(ha,k=3)","s(pptmeasmean,k=3)","s(pptmenmean,k=3)","s(tmpmaxmean,k=3)","s(gddmenmean,k=3)","ecoprovmaj","s(frzfremedmean, k=3)","s(fdmed1stmean, k=3)","s(fdmedlastmean, k=3)","s(logdtrdnsmean, k=3)","s(phmnxtmpmean, k=3)","s(HOUSEUNITS, k=3)","s(POPULATION, k=3)","s(mindexmean,k=3)","s(inc_1969, k=3)","s(Mean.YEAR,k=3)","s(Mean.Value,k=3)","s(inc_1979, k=3)","s(inc_1989, k=3)","s(tmpmaxsummean, k=3)", "s(coastdat.coast3, k=3)", "s(trait_value.diameter.at.breast.height..1.3.m., k=3)","s(trait_value.leaf.area , k=3)","s(trait_value.leaf.dry.mass, k=3)" ,"s(trait_value.seed.mass, k=3)", "s(trait_value.whole.plant.height, k=3)", "s(OrigValueStr.Leaf.length, k=3)", "s(OrigValueStr.Leaf.thickness, k=3)" ,"s(OrigValueStr.Leaf.width, k=3)", "s(OrigValueStr.Plant.growth.rate, k=3)","s(OrigValueStr.Plant.height.vegetative, k=3)" , "s(OrigValueStr.Plant.lifespan..longevity.,k=3)", "s(OrigValueStr.Stem.diameter, k=3)", "s(bygenus_pests_each_city[,1], k=2)","s(bygenus_pests_each_city[,3], k=3)","s(bygenus_pests_each_city[,5], k=2)","s(bygenus_pests_each_city[,6], k=2)","s(bygenus_pests_each_city[,7], k=2)","s(bygenus_pests_each_city[,9], k=2)","s(bygenus_pests_each_city[,10], k=3)","s(bygenus_pests_each_city[,11], k=2)","s(bygenus_pests_each_city[,13], k=2)","s(bygenus_pests_each_city[,14], k=2)","s(bygenus_pests_each_city[,15], k=2)", "s(data_year, k=3)")

varz=c("s(xcoord,k=3)","s(ycoord,k=3)","s(bio2,k=3)","s(bio8,k=3)","s(bio10,k=3)","s(bio12,k=3)","s(bio13,k=3)","s(bio15,k=3)","s(bio11,k=3)","s(bio19,k=3)","s(canopy,k=3)","s(ha,k=3)","s(pptmeasmean,k=3)","ecoprovmaj","s(frzfremedmean, k=3)","s(logdtrdnsmean, k=3)","s(POPULATION, k=3)","s(mindexmean,k=3)","s(Mean.YEAR,k=3)","s(Mean.Value,k=3)","s(inc_1989, k=3)", "s(coastdat.coast3, k=3)", "s(uselevmean, k=3)", "s(trait_value.diameter.at.breast.height..1.3.m., k=3)","s(trait_value.leaf.area , k=3)","s(trait_value.leaf.dry.mass, k=3)" ,"s(trait_value.seed.mass, k=3)", "s(trait_value.whole.plant.height, k=3)", "s(OrigValueStr.Leaf.length, k=3)", "s(OrigValueStr.Leaf.thickness, k=3)" ,"s(OrigValueStr.Leaf.width, k=3)", "s(OrigValueStr.Plant.growth.rate, k=3)","s(OrigValueStr.Plant.height.vegetative, k=3)" , "s(OrigValueStr.Plant.lifespan..longevity.,k=3)", "s(OrigValueStr.Stem.diameter, k=3)", "s(bygenus_pests_each_city[,1], k=2)","s(bygenus_pests_each_city[,3], k=3)","s(bygenus_pests_each_city[,5], k=2)","s(bygenus_pests_each_city[,6], k=2)","s(bygenus_pests_each_city[,7], k=2)","s(bygenus_pests_each_city[,9], k=2)","s(bygenus_pests_each_city[,10], k=3)","s(bygenus_pests_each_city[,11], k=2)","s(bygenus_pests_each_city[,13], k=2)","s(bygenus_pests_each_city[,14], k=2)","s(bygenus_pests_each_city[,15], k=2)", "s(data_year, k=3)")
# bygenus_pests_each_city[,3]<-bygenus_pests_each_city[,3]+bygenus_pests_each_city[,4]
# bygenus_pests_each_city[,7]<-bygenus_pests_each_city[,7]+bygenus_pests_each_city[,8]
# bygenus_pests_each_city[,11]<-bygenus_pests_each_city[,11]+bygenus_pests_each_city[,12]
# bygenus_pests_each_city[,15]<-bygenus_pests_each_city[,15]+bygenus_pests_each_city[,16]

#fit total tree models by size class via fwd selection gam (too many predictors otherwise - be wary of categorical vars inflating coefficients to unrealistic values)
data$inc_1969<-as.numeric(as.character(gsub(",","",data$inc_1969)))
data$inc_1969[which(is.na(data$inc_1969)==T)]<-mean(data$inc_1969, na.rm=T)
data$inc_1979<-as.numeric(as.character(gsub(",","",data$inc_1979)))
data$inc_1979[which(is.na(data$inc_1979)==T)]<-mean(data$inc_1979, na.rm=T)
data$inc_1989<-as.numeric(as.character(gsub(",","",data$inc_1989)))
data$inc_1989[which(is.na(data$inc_1989)==T)]<-mean(data$inc_1989, na.rm=T)
data$small<-round(data$small)
#allUS$phzone<-as.factor(allUS$phzone)
allUS$ecoprovmaj<-as.factor(allUS$ecoprovmaj)
#data[41, colnames(allUS)[which(colnames(allUS)%in%colnames(data))]]<-allUS[match,colnames(allUS)[which(colnames(allUS)%in%colnames(data))]]
#rich_dat[nyc, colnames(data)[c(-2, -67)][which(colnames(data)[c(-2, -67)]%in%colnames(rich_dat))]]<-data[41,colnames(data)[c(-2, -67)][which(colnames(data)[c(-2, -67)]%in%colnames(rich_dat))]]
#rich_dat[nyc,"phzone"]<-72
#rich_dat[nyc,"ecoprovmaj"]<-221
#data[41, "ecoprovmaj"]<-221
data$ecoprovmaj<-factor(data$ecoprovmaj, levels=levels(allUS$ecoprovmaj))
data$log_small<-log1p(data$small)
data$small_cor<-round(data$small_cor)
#data<-subset(data, city2!="NEW YORK NY")
#for (i in c(3:36)){

#tot_small_gam<-gam(as.formula(paste("small_cor~",paste(varz[c(1:23)],collapse="+"), sep="")), select=T, method="GCV.Cp", data=data, family="poisson", drop.unused.levels = F)
#varz[21],varz[35],varz[18],varz[33],varz[27],varz[4],varz[13]
#varz[21],varz[16],varz[35],varz[14],varz[34],varz[33],varz[3],varz[18],varz[13] 
# cat(i,(tot_small_gam)$aic, "\n")
#}
# for (i in c(3:36)){
#   tot_small_gam2<-gam(as.formula(paste("log_small~",varz[28],varz[32],varz[25],varz[21],varz[36],sep="+")), select=T, method="GCV.Cp", data=data, family="gaussian", drop.unused.levels = F)
#   #varz[21],varz[35],varz[18],varz[33],varz[27],varz[4],varz[13]
#   #varz[21],varz[16],varz[35],varz[14],varz[34],varz[33],varz[3],varz[18],varz[13] 
#   cat(i,(tot_small_gam2)$aic, "\n")
# }
pred_small_trees<-predict.gam(tot_small_gam, type="response")
data$med<-round(data$med)
layout(1)
par(mar=c(4,4,2,2))
par(oma=c(0,0,0,0))
plot(pred_small_trees,data$small_cor, xlab="Predicted Small Trees", ylab="Observed Small Trees", pch=19, col=alpha("gray30",0.75))
abline(0,1)

plot(pred_med_trees,data$med_cor, xlab="Predicted Medium Trees", ylab="Observed Medium Trees", pch=19, col=alpha("gray30",0.75))
abline(0,1)

plot(pred_large_trees,data$large_cor, xlab="Predicted Large Trees", ylab="Observed Large Trees", pch=19, col=alpha("gray30",0.75))
abline(0,1)

data$med_cor<-round(data$med_cor)
#for (i in c(3:36)){
 # tot_med_gam<-gam(as.formula(paste("med_cor~",paste(varz[c(1:23)],collapse="+"), sep="")), select=T, method="GCV.Cp", data=data, family="poisson", drop.unused.levels = F)
 # ,varz[15], varz[3],varz[27],varz[10],varz[13],varz[35],varz[17],varz[7]
  #21],varz[12],varz[3],varz[32],varz[25],varz[6
# cat(i,(tot_med_gam)$aic, "\n")
#}
pred_med_trees<-predict.gam(tot_med_gam, type="response")

data$large_cor<-round(data$large_cor)
#for (i in c(3:36)){
#  tot_large_gam<-gam(as.formula(paste("large_cor~", paste(varz[c(1:23)],collapse="+"), sep="")), select=T, method="GCV.Cp", data=data, family="poisson", drop.unused.levels = F)
#cat(i,(tot_large_gam)$aic, "\n")
#}
pred_large_trees<-predict.gam(tot_large_gam, type="response")

##Two-part models, fit brt or gam to binomial small tree presence, then log(n_small+1) gaussian model to subset where present, rescaling to get correct mean expectations (either by site or countrywide - by site seems to work worse for some reason)

r2mse<-function(obs, pred)
{
1-((sum((wpred_sub[,obs]-(pred))^2))/(sum((wpred_sub[,obs]-mean(wpred_sub[,obs]))^2)))
}

plot_pred<-function(obs, pred)
{
  plot(wpred_sub[,obs]~pred, xlab="Predicted # Trees", ylab="Observed # Trees")
  abline(0,1)
}

rescale_constant<-function(prednum, predexist,sizex)
{
  return(prednum*predexist*(sum(wpred_sub[,sizex])/sum(prednum*predexist)))
}

rescale_all<-function(prednum,sizex)
{
  return(prednum*(sum(wpred_data[,sizex]/sum(prednum))))
}

rescale_city<-function(prednum,predexist,size)
{
 nt<-rep(0,nrow(rich_dat))
 rescale<-rep(0,nrow(rich_dat))
 for (i in 1:length(unique(data$city))) # city-specific rescaling - predicted trees in city sum to total predicted in sum in all-tree model
 {
  cityx<-which(wpred_data$city%in%unique(data$city)[i])
  rescale[cityx]<-sum(prednum[cityx]*predexist[cityx])
  nt[cityx]<-(subset(wpred_data, city==unique(data$city)[i])[,size])[1]
 }
 return(prednum*predexist*(nt/rescale))
}

rescale_city2<-function(pred,size)
{
  nt<-rep(0,nrow(wpred_data))
  rescale<-rep(0,nrow(wpred_data))
  for (i in 1:length(unique(data$city))) # city-specific rescaling - predicted trees in city sum to total predicted in sum in all-tree model
  {
    cityx<-which(wpred_data$city%in%unique(data$city)[i])
    rescale[cityx]<-sum(pred[cityx])
    nt[cityx]<-(subset(wpred_data, city==unique(data$city)[i])[,size])[1]
  }
  return(pred*(nt/rescale))
}

rescale_all_city<-function(prednum,predexist,size)
{
  nt<-rep(0,nrow(rich_dat))
  rescale<-rep(0,nrow(rich_dat))
  for (i in 1:56) # city-specific rescaling - predicted trees in city sum to total predicted in sum in all-tree model
  {
    cityx<-which(wpred_data$city%in%data$city[i])
    rescale[cityx]<-sum(prednum[cityx])
    nt[cityx]<-eval(parse(text=paste("pred",size,"trees",sep="_")))[i]
  }
  return(prednum*(nt/rescale))
}

preds_gam<-function(mod, size=NULL)
{
  if (is.null(size)==T)
  {
  preds<-(predict.gam(mod, newdata=wpred_sub, type="response"))
  }
  if (is.null(size)==F)
  {
    preds<-rep(0,nrow(wpred_sub))
    preds[which(wpred_sub[,size]>0)]<-predict.gam(mod, type="response")
  }
  preds[which(is.na(preds)==T)]<-0
  return(preds)
}

preds_ziP<-function(mod)
{
  preds<-rep(0,nrow(wpred_sub))
  preds<-predict.gam(mod, type="response", newdata=wpred_sub)
  preds[which(is.na(preds)==T)]<-0
  return(preds)
}

preds_brt<-function(mod)
{
  return(predict.gbm(mod, newdata=wpred_sub, n.trees=mod$n.trees, type="response"))
}

preds_brtzip<-function(mod, size)
{
  preds<-rep(0,nrow(wpred_sub))
  if(size=="med"){preds[which(wpred_sub$SP%in%wpred_sub_med$SP)]<-predict.gbm(mod, newdata=wpred_sub_med, n.trees=mod$n.trees, type="response")}
  if(size=="large"){preds[which(wpred_sub$SP%in%wpred_aub_large$SP)]<-predict.gbm(mod, newdata=wpred_sub_large, n.trees=mod$n.trees, type="response")}
  if(size=="small"){preds[which(wpred_sub$SP%in%wpred_sub_large$SP)]<-predict.gbm(mod, newdata=wpred_sub_large, n.trees=mod$n.trees, type="response")}
  return(preds)
}

## ziP combined models, concurrently estimates logistic and poisson gam
rich_dat$city<-as.character(rich_dat$city)
pred_each<-(cbind(as.character(data$city), pred_small_trees, pred_med_trees,pred_large_trees))
pred_each<-as.data.frame(pred_each)
pred_each[,2]<-as.numeric(as.character(pred_each[,2]))
pred_each[,3]<-as.numeric(as.character(pred_each[,3]))
pred_each[,4]<-as.numeric(as.character(pred_each[,4]))
colnames(pred_each)[1]<-"city"
rich_dat$small<-round(rich_dat$small,0)
#data$PLACEFIPS[41]<-"NYC-other"
#rich_dat$data_year<-data$data_year[match(rich_dat$PLACEFIPS, data$PLACEFIPS)]
#all_small_mod2<-gam(as.formula(paste("log1p(small)~SP+",paste(varz[c(3:20, 22:59)], collapse="+"),sep="+")),data=wpred_data,family=gaussian, drop.unused.levels = F, select=T, method="GCV.Cp")
#rich_dat$city[which(rich_dat$city=="41")]<-"NYC"
wpred_data<-merge(rich_dat[,which(colnames(rich_dat)%in%colnames(data)==T)], pred_each, by="city")

#pred_num<-(preds_ziP(all_small_mod2))
## final_pred<-rescale_all(pred_num,"small")
 #r2mse("small", final_pred)
 #plot_pred("small", final_pred)
# 
#comment out later!!##
#wpred_data<-cbind(rich_dat, bygenus_pests_each_city)
wpred_data$ecoprovmaj<-factor(wpred_data$ecoprovmaj, levels=levels(data$ecoprovmaj))
wpred_data$is_small<-as.numeric(wpred_data$small>0)
rich_dat_small<-subset(wpred_data, is_small==1)
is_small_brt<-gbm.step(data=wpred_data, gbm.x=c(2,7,10,13:20,23, 25:36, 41:52, 53), gbm.y=56, n.folds=10, n.trees=30, family='bernoulli', tree.complexity =3, learning.rate=0.1, drop.unused.levels=F)
rich_dat_small$ecoprovmaj<-factor(rich_dat_small$ecoprovmaj, levels=levels(data$ecoprovmaj))
rich_dat_small$phzone<-factor(rich_dat_small$phzone, levels=levels(allUS$phzone))
 rich_dat_small$log_n_small<-log(rich_dat_small$small)

 small_brt<-gbm.step(data=rich_dat_small, gbm.x=c(2,7,10,13:20,23, 25:36, 41:52, 53), gbm.y=57,n.folds=10, n.trees=30, family='gaussian', tree.complexity = 3, learning.rate=0.5, drop.unused.levels=F)

varz2<-gsub(")$",",bs=\\'cr\\')",varz)
all_small_mod<-bam(as.formula(paste("round(small)~s(pred_small_trees, k=3, bs='cr')+genus+", paste(varz2[c(1:35)],collapse="+"), sep="")), select=T, method="fREML", data=wpred_data, nthreads=4, chunk.size = 1000, drop.unused.levels = F, discrete=T, family=ziP())


is_small_brt_sep<-list()
 small_brt_sep<-list()
# all_small_mod<-list()
r2_gam<-rep(0,48)
for (spp in 1:48)
{
 wpred_sub<-subset(wpred_data, genus==unique(wpred_data$genus)[spp])
 wpred_sub$is_small<-wpred_sub$small>0
 rich_dat_small<-subset(wpred_sub, is_small==1)
#  if (nrow(rich_dat_small)>10)
#  {
#  is_small_brt_sep[[spp]]<-gbm.step(data=wpred_sub, gbm.x=c(2,7,10,13:20,23, 25:36, 53), gbm.y=56,n.folds=10, n.trees=30, family='bernoulli', tree.complexity = 3, learning.rate=0.005, drop.unused.levels=F)
#  i=0.005
#  while (is_small_brt_sep[[spp]]$n.trees<1000)
#  {
#    i=i/5
#    is_small_brt_sep[[spp]]<-gbm.step(data=wpred_sub, gbm.x=c(2,7,10,13:20,23, 25:36,  53), gbm.y=56,n.folds=10, n.trees=30, family='bernoulli', tree.complexity = 3, learning.rate=i, drop.unused.levels=F)
#  }
#  #wpred_data$is_small<-wpred_data$small>0
# # 
#  rich_dat_small$ecoprovmaj<-factor(rich_dat_small$ecoprovmaj, levels=levels(data$ecoprovmaj))
# # #rich_dat_small$phzone<-factor(rich_dat_small$phzone, levels=levels(allUS$phzone))
#  rich_dat_small$log_n_small<-log(rich_dat_small$small)
# if (nrow(rich_dat_small)>30)
# {
# small_brt_sep[[spp]]<-gbm.step(data=rich_dat_small, gbm.x=c(2,7,10,13:20,23, 25:36, 53), gbm.y=57,n.folds=10, n.trees=30, family='gaussian', tree.complexity = 3, learning.rate=0.005, drop.unused.levels=F)
# i=0.005
# while (small_brt_sep[[spp]]$n.trees<1000)
# {
#   i=i/10
#   small_brt_sep[[spp]]<-gbm.step(data=rich_dat_small, gbm.x=c(2,7,10,13:20,23, 25:36, 53), gbm.y=57,n.folds=10, n.trees=30, family='gaussian', tree.complexity = 3, learning.rate=i, drop.unused.levels=F)
# }
# # 
# # 
# # 
#  pred_num<-exp(preds_brt(small_brt_sep[[spp]]))
# # #pred_num<-exp(preds_gam(small_mod))
# # #pred_exist<-preds_gam(is_small_mod)
#  pred_exist<-preds_brt(is_small_brt_sep[[spp]])
#  final_pred<-rescale_constant(pred_num, pred_exist,"small")
#  }
# }
# if (nrow(rich_dat_small)<=48)
# {
#   all_small_mod[[spp]]<-gam(as.formula(paste("log_n_small~s(pred_small_trees, k=3)", paste(varz[c(3:4)],collapse="+"),sep=""), select=T, method="GCV.Cp", data=rich_dat_small,family=gaussian, drop.unused.levels = F)
# #final_pred<-preds_ziP(all_small_mod[[spp]])
#   pred_num<-exp(preds_gam(all_small_mod[[spp]]))
#   #pred_num<-exp(preds_gam(small_mod))
#   #pred_exist<-preds_gam(is_small_mod)
#   pred_exist<-preds_brt(is_small_brt[[spp]])
#   final_pred<-rescale_constant(pred_num, pred_exist,"small")
#   
# }


#is_small_brt<-gbm.step(data=wpred_data, gbm.x=c(2,7:8,11:29, 32:34,36:58, 63:75), gbm.y=78,n.folds=10, n.trees=30, family='bernoulli', tree.complexity = 5, learning.rate=0.1, drop.unused.levels=F)
#is_small_mod<-gam(as.formula(paste("is_small~s(pred_small_trees, k=3)+", paste(varz[c(3:36)],collapse="+"), sep="")), select=T, method="GCV.Cp", data=wpred_sub, family=binomial,drop.unused.levels = F)
#   cat(i, all_large_mod$aic, "\n")
# #}
#colnames(wpred_data)[90:105]<-c("bl","bm","bh","bvh", "sl", "sm", "sh", "sv", "pl", "pm", "ph", "pvh", "dl", "dm", "dh", "dvh")
#is_small_brt<-gbm.step(data=wpred_data, gbm.x=c(2,11:22,40:78, 81:88, 90:105), gbm.y=106, n.folds=10, n.trees=30, family='bernoulli', tree.complexity = 3, learning.rate=0.05, drop.unused.levels=F)
# rich_dat_small<-subset(wpred_data, is_small==1)
#  rich_dat_small$ecoprovmaj<-factor(rich_dat_small$ecoprovmaj, levels=levels(data$ecoprovmaj))
# # #rich_dat_small$phzone<-factor(rich_dat_small$phzone, levels=levels(allUS$phzone))
 # rich_dat_small$log_n_small<-log(rich_dat_small$small)
# small_brt<-gbm.step(data=rich_dat_small, gbm.x=c(2,7:8,11:29, 32:34,38:60, 65:77), gbm.y=81,n.folds=10, n.trees=30, family='gaussian', tree.complexity = 5, learning.rate=0.1, drop.unused.levels=F)
# 
# small_mod<-gam(as.formula(paste("log_n_small~s(pred_small_trees, k=3)+", paste(varz[c(3:36)],collapse="+"), sep="")), select=T, method="GCV.Cp", data=rich_dat_small, family='gaussian',drop.unused.levels = F)

#colnames(rich_dat_small)[90:105]<-c("bl","bm","bh","bvh", "sl", "sm", "sh", "sv", "pl", "pm", "ph", "pvh", "dl", "dm", "dh", "dvh")
#small_mod<-bam(as.formula(paste("small~s(pred_small_trees, k=3)+genus+", paste(varz[c(3:48)],collapse="+"), sep="")), select=T, method="GCV.Cp", data=rich_dat_small, family=poisson)

#small_brt<-gbm.step(data=rich_dat_small, gbm.x=c(2,20,27:64, 67:74,81:92,93), gbm.y=97, n.folds=10, n.trees=30, family='gaussian', tree.complexity = 4, learning.rate=0.03)
#best small is 2 part gam (poisson and binomial), but qq plot looks bad

#final_pred<-rescale_city(pred_num, pred_exist,"pred_small_trees")
#final_pred<-rescale_city(final_pred, "small_cor")
#ratio_small<-sum(wpred_data[,"small"])/sum(pred_num*pred_exist)
pred_exist<-preds_brt(is_small_brt)
pred_num<-exp(preds_brt(small_brt))
final_pred<-rescale_constant(pred_num, pred_exist, "small")
#final_pred<-preds_ziP(all_small_mod)
 r2[spp]<-r2mse("small", final_pred)
#plot_pred("small", final_pred)
#wpred_data$pred_small[which(wpred_data$genus==unique(wpred_data$genus)[i])]<-final_pred
 }
}
r2_sep_gam<-read.csv('small_r2_sep_gam2.csv')
sep_small_gam<-readRDS("small_sep_gam2.RDS")
sep_small_num_brt<-small_brt_sep
sep_small_exist_brt<-is_small_brt_sep
sep_r2<-r2
#gam_spp<-which(which(lengths(sep_gam)>0)%in%sep_spp)
sep_spp<-which(r2<sep_r2)
r2_comb<-c(r2[which(1:48%in%sep_spp==F)], sep_r2[sep_spp])
r2_comb2<-r2_comb
r2_comb2[which(1:48%in%sep_spp==F)]<-r2_comb[20:48]
r2_comb2[which(1:48%in%sep_spp==T)]<-r2_comb[1:19]
gam_sep_spp<-which(r2_sep_gam[,1]-r2_comb2>0)
r2_comb2[gam_sep_spp]<-r2_sep_gam[gam_sep_spp,1]
max(r2_comb2)
model_sel_small<-rep(0,48)
#model_sel_small[gamspp]<-"gam"
model_sel_small[which(1:48%in%sep_spp==F)]<-"brt"
model_sel_small[gam_sep_spp]<-"gam_sep"
model_sel_small[sep_spp[which(sep_spp%in%gam_sep_spp==F)]]<-"brt_sep"
write.csv(model_sel_small, file="model_sel_small2.csv", row.names=F)

for (spp in 1:48)
{
  wpred_sub<-subset(wpred_data, genus==unique(wpred_data$genus)[spp])
  if (spp%in%sep_spp==F)
  {
    pred_exist<-preds_brt(is_small_brt)
    pred_num<-exp(preds_brt(small_brt))
    final_pred<-rescale_constant(pred_num, pred_exist, "small")
  }
  if (spp%in%sep_spp==T)
  {
    pred_exist<-preds_brt(sep_small_exist_brt[[spp]])
    pred_num<-exp(preds_brt(sep_small_num_brt[[spp]]))
    final_pred<-rescale_constant(pred_num, pred_exist, "small")
  }
  if (spp%in%gam_sep_spp)
  {
   final_pred<-preds_ziP(sep_small_gam[[spp]]) 
  }
  wpred_data$pred_small[which(wpred_data$genus==unique(wpred_data$genus)[spp])]<-final_pred
}
wpred_sub<-wpred_data
r2mse('small', wpred_data$pred_small)
plot_pred('small', wpred_data$pred_small)
# final_pred<-predict.gam(all_small_mod, wpred_data2, type="response")
 plot(wpred_data$pred_small,wpred_data$small, xlab="Predicted Small Trees", ylab="Observed Small Trees", pch=19, col=alpha("gray30",0.75))
 abline(0,1)
# 
# final_pred<-predict.gam(all_med_mod, wpred_data2, type="response")
# plot(final_pred,wpred_data2$med, xlab="Predicted Medium Trees", ylab="Observed Medium Trees", pch=19, col=alpha("gray30",0.75))
# abline(0,1)
# 
# final_pred<-predict.gam(all_large_mod, wpred_data2, type="response")
# plot(final_pred,wpred_data2$large, xlab="Predicted Large Trees", ylab="Observed Large Trees", pch=19, col=alpha("gray30",0.75))
# abline(0,1)
# 
# sum_isb<-summary(is_small_brt)
# length(which(sum_isb$rel.inf>1))
# plotvar<-which(is_small_brt$var.names==sum_isb$var[9])
# plot(is_small_brt, plotvar)
# 
# 
# sum_sb<-summary(small_brt)
# length(which(sum_sb$rel.inf>1))
# plotvar<-which(small_brt$var.names==sum_sb$var[2])
# plot(small_brt, plotvar)
# plotvar<-which(small_brt$var.names==sum_sb$var[4])
# plot(small_brt, plotvar)
# plotvar<-which(small_brt$var.names==sum_sb$var[5])
# plot(small_brt, plotvar)
# plotvar<-which(small_brt$var.names==sum_sb$var[6])
# plot(small_brt, plotvar)
# plotvar<-which(small_brt$var.names==sum_sb$var[8])
# plot(small_brt, plotvar)
# plotvar<-which()
# 
# plot(small_brt,small_brt$var.names[76])
#all_small_brt<-gbm.step(data=wpred_data, gbm.x=c(2,11:22,40:78, 81:88, 90), gbm.y=4, n.folds=10, n.trees=30, family='gaussian', tree.complexity = 7, learning.rate=0.001, tolerance=0.00000000001)

#pred_num<-preds_brt(all_small_brt)
# final_pred<-pred_num
# r2mse("small", final_pred)
# plot_pred("small", final_pred)


### Medium tree models (brt vs. gam)
# 
# #remove spp not found as medium trees anywhere 
# med_SP<-unique(as.character(rich_dat$SP))[which(tapply(rich_dat$med, rich_dat$SP,sum)>0)]
#wpred_data$pred_small<-final_pred
# rich_dat_med<-subset(wpred_data, SP%in%med_SP)
 #rich_dat_med$log_med<-log(rich_dat_med$med+1)
# 
 #all_med_brt<-gbm.step(data=rich_dat_med, gbm.x=c(2,35:75,79:80, 84:89,92, 96), gbm.y=5, n.folds=10, n.trees=30, family='gaussian', tree.complexity = 5, learning.rate=0.0005, tolerance=0.00000001)
# 
# pred_num<-preds_brtzip(all_med_brt, "med.x")
# final_pred<-rescale_all(pred_num,"med.x")
# r2mse("med.x", final_pred)
# plot_pred("med.x", final_pred)
# 
# all_med_mod<-gam(as.formula(paste("round(med.x,0)~s(pred_small,k=3)+s(pred_med_trees, k=3)",paste(varz[c(1:14, 16:20, 22:48)], collapse="+"),sep="+")), select=T, method="GCV.Cp", data=rich_dat_med, family=ziP(b=1))
# 
# 
# pred_num<-(preds_ziP(all_med_mod, "med.y"))
# final_pred<-rescale_all(pred_num,"med.x")
# r2mse("med.x", final_pred)
# plot_pred("med.x", final_pred)
# 
# large_SP<-unique(rich_dat$SP[which(rich_dat$large.x>0)])
# rich_dat_large<-subset(rich_dat, SP%in%large_SP)
# rich_dat_large$log_large<-log(rich_dat_large$large.x+1)
# 
# #for(i in c(1:14, 16:20, 22:48))
# #{
#  all_large_mod<-gam(as.formula(paste("round(large.x,0)~s(pred_small, k=3)+s(pred_med, k=3)+SP+", paste(varz[c(1:14, 16:20, 22:48)],collapse="+"), sep="")), select=T, method="GCV.Cp", data=rich_dat_large, family=ziP(b=1))
#   cat(i, all_large_mod$aic, "\n")
# #}
# 
#   pred_num<-preds_ziP(all_large_mod, "large.y")
#   final_pred<-rescale_all(pred_num,"large.x")
#   #final_pred<-rescale_all_city(pred_num, "large")
#   r2mse("large.x", final_pred)
#   plot_pred("large.x", final_pred)
#   
#   
#   
# all_large_brt<-gbm.step(data=rich_dat_large, gbm.x=c(2,35:75,78:79, 83:88,92,94), gbm.y=6, n.folds=10, n.trees=30, family='gaussian', tree.complexity = 6, learning.rate=0.0005, tolerance=0.00000000001)
# 
# pred_num<-preds_brtzip(all_large_brt, "large.x")
# final_pred<-rescale_all(pred_num,"large.x")
# #final_pred<-rescale_all_city(pred_num, "large")
# r2mse("large.x", final_pred)
# plot_pred("large.x", final_pred)

 #is_small_mod<-gam(as.formula(paste("is_small~SP+s(pred_small_trees, k=3)+",paste(varz[c(1:14, 16:20, 22:36)], collapse='+'), sep="+")), select=T, method="GCV.Cp", data=wpred_data, family=binomial)
# summary(is_small_mod)
# rqgam.check(is_small_mod) #randomized quantile residuals are more appropriate than gam.check residuals for binomial responses
# gam.check(is_small_mod) # key is qq plot
# is_small_brt<-gbm.step(data=rich_dat, gbm.x=c(2,35:75,79:80, 84:89), gbm.y=91, n.folds=10, n.trees=30, family='bernoulli', tree.complexity = 3, learning.rate=0.01) #fits gbm with optimal number of trees - tweak tree complexity to maximize cv correlation, adjust learning rate and tolerance to ensure at least 1000 trees
# rich_dat_small$log_n_small<-log(rich_dat_small$small.x+1)
# small_brt<-gbm.step(data=rich_dat_small, gbm.x=c(2,4:14,34:74,78:79, 83:88), gbm.y=91, n.folds=10, n.trees=30, family='gaussian', tree.complexity = 3, learning.rate=0.0001)
# rich_dat_small<-subset(rich_dat, small.x>0)
#small_mod2<-gam(as.formula(paste("log(small+1)~SP+",paste(varz[c(1:14,16:20,21,22:36)], collapse="+"),sep="")), select=T, method="GCV.Cp", data=rich_dat_small, family=gaussian)
#pred_num<-preds_gam(small_mod2)
#pred_exist<-preds_gam(is_small_mod)
#final_pred<-rescale_constant(pred_num, pred_exist,"small.x")
#final_pred<-rescale_city(pred_num, pred_exist, "small")
#r2mse("small.x", final_pred)
#plot_pred("small.x", final_pred)

## Medium 2-step models

 #is_med_mod<-gam(as.formula(paste("is_med~SP+s(pred_small, k=3)+s(pred_med_trees, k=3)+",paste(varz[c(1:14, 16:20, 22:48)], collapse='+'), sep="+")), select=T, method="GCV.Cp", data=wpred_data, family=binomial)
# summary(is_med_mod)
# gam.check(is_med_mod)
write.csv(wpred_data$pred_small, file="pred_small_bestmodel2.csv", row.names=F)
wpred_data$is_med<-wpred_data$med>0
all_med_mod<-bam(as.formula(paste("round(med)~s(pred_med_trees, k=3, bs='cr')+s(pred_small, k=3)+genus+", paste(varz2[c(1:35)],collapse="+"), sep="")), select=T, method="fREML", data=wpred_data, nthreads=4, chunk.size = 1000, drop.unused.levels = F, discrete=T, family=ziP())
is_med_mod<-bam(as.formula(paste("is_med~s(pred_small_trees, k=3, bs='cr')+s(pred_med_trees, k=3, bs='cr')+s(pred_small, k=3, bs='cr')+genus+", paste(varz2[c(3:48)],collapse="+"), sep="")), select=T, method="fREML", data=wpred_data, nthreads=4, chunk.size = 1000, drop.unused.levels = F, discrete=T, family=binomial)
is_med_brt<-gbm.step(data=wpred_data, gbm.x=c(2,7,10,13:20,23, 25:36, 41:52, 54,57), gbm.y=58, n.folds=10, n.trees=30, family='bernoulli', tree.complexity = 3, learning.rate=0.025) 
rich_dat_med<-subset(wpred_data, med>0)
rich_dat_med$ecoprovmaj<-factor(rich_dat_med$ecoprovmaj, levels=levels(data$ecoprovmaj))
rich_dat_med$log_n_med<-log(rich_dat_med$med)
med_brt<-gbm.step(data=rich_dat_med, gbm.x=c(2,7,10,13:20,23, 25:36, 41:52, 54,57), gbm.y=60,n.folds=10, n.trees=30, family='gaussian', tree.complexity = 3, learning.rate=0.1, drop.unused.levels=F)

#fits gbm with optimal number of trees - tweak tree complexity to maximize cv correlation, adjust learning rate and tolerance to ensure at least 1000 trees

r2_gam<-rep(0,48)
for (spp in 1:48)
{
  wpred_sub<-subset(wpred_data, genus==unique(wpred_data$genus)[spp])
  final_pred<-preds_ziP(all_med_mod)
  r2_gam[spp]<-r2mse("med", final_pred)
}
r2_brt<-rep(0,48)
for (spp in 1:48)
{
  wpred_sub<-subset(wpred_data, genus==unique(wpred_data$genus)[spp])
  pred_exist<-preds_brt(is_med_brt)
  pred_num<-exp(preds_brt(med_brt))
  final_pred<-rescale_constant(pred_num, pred_exist, "med")
  r2_brt[spp]<-r2mse("med", final_pred)
}
sep_med_exist_brt<-sep_med_num_brt<-list()
r2_sep_brt<-rep(0,48)
for (spp in 23:48)
{
  wpred_sub<-subset(wpred_data, genus==unique(wpred_data$genus)[spp])
  if (sum(wpred_sub$med)==0)
  {
    next
  }
  wpred_sub$is_med<-wpred_sub$med>0
  rich_dat_med<-subset(wpred_sub, is_med==1)
  if (nrow(rich_dat_med)>30)
  {
    sep_med_exist_brt[[spp]]<-gbm.step(data=wpred_sub, gbm.x=c(2,7,10,13:20,23, 25:36, 41:52, 54,57), gbm.y=58,n.folds=10, n.trees=30, family='bernoulli', tree.complexity = 3, learning.rate=0.005, drop.unused.levels=F)
    i=0.005
    while (sep_med_exist_brt[[spp]]$n.trees<1000)
    {
      i=i/5
      sep_med_exist_brt[[spp]]<-gbm.step(data=wpred_sub, gbm.x=c(2,7,10,13:20,23, 25:36, 41:52, 54,57), gbm.y=58,n.folds=10, n.trees=30, family='bernoulli', tree.complexity = 3, learning.rate=i, drop.unused.levels=F)
    }
    
    wpred_data$is_med<-wpred_data$med>0
    
    rich_dat_med$ecoprovmaj<-factor(rich_dat_med$ecoprovmaj, levels=levels(data$ecoprovmaj))
    #rich_dat_small$phzone<-factor(rich_dat_small$phzone, levels=levels(allUS$phzone))
    rich_dat_med$log_n_med<-log(rich_dat_med$med)
   sep_med_num_brt[[spp]]<-gbm.step(data=rich_dat_med, gbm.x=c(2,7,10,13:20,23, 25:36, 41:52, 54,57), gbm.y=59,n.folds=10, n.trees=30, family='gaussian', tree.complexity = 3, learning.rate=0.005, drop.unused.levels=F)
    i=0.005
    while (sep_med_num_brt[[spp]]$n.trees<1000)
    {
      i=i/10
      sep_med_num_brt[[spp]]<-gbm.step(data=rich_dat_med, gbm.x=c(2,7,10,13:20,23, 25:36, 41:52, 54,57), gbm.y=59,n.folds=10, n.trees=30, family='gaussian', tree.complexity = 3, learning.rate=i, drop.unused.levels=F)
    }
    
    
    sep_med_num_brt[[9]]<-lm(med~1, data=rich_dat_med)
    pred_num<-predict.lm(sep_med_num_brt[[9]], newdata=wpred_sub)
    
    pred_num<-exp(preds_brt(sep_med_num_brt[[spp]]))
    #pred_num<-exp(preds_gam(small_mod))
    #pred_exist<-preds_gam(is_small_mod)
    pred_exist<-preds_brt(sep_med_exist_brt[[spp]])
    final_pred<-rescale_constant(pred_num, pred_exist,"med")
    r2_sep_brt[spp]<-r2mse('med',final_pred)
    
  }
}
saveRDS(sep_med_num_brt, file="med_sep_brt2.rds")

#r2_sep_gam<-read.csv('r2_sep_med_gam2.csv')[,1]
r2_sep_brt<-read.csv('r2_sep_brt_med.csv')[,1]
#sep_med_gam<-readRDS("med_sep_gam2.RDS")
sep_med_num_brt<-readRDS("med_sep_brt2.RDS")
sep_med_exist_brt<-readRDS("is_med_sep_brt2.RDS")
sep_spp<-which(r2_sep_gam<r2_sep_brt)
r2_comb<-rep(0,48)
r2_comb[sep_spp]<-r2_sep_brt[sep_spp]
gam_sep_spp<-which(1:48%in%sep_spp==F)
r2_comb[which(1:48%in%sep_spp==F)]<-r2_sep_gam[which(1:48%in%sep_spp==F)]
r2_comb[which(r2_gam>r2_comb)]<-r2_gam[which(r2_gam>r2_comb)]
gamspp<-c(31,46)
brtspp<-which(r2_brt>r2_comb)
r2_comb[which(r2_brt>r2_comb)]<-r2_brt[which(r2_brt>r2_comb)]
r2_comb[9]<- 0.186589
mean(r2_comb)
#wpred_data$pred_small<-read.csv('pred_small_bestmodel.csv')[,1]
model_sel_small<-rep(0,48)
model_sel_small[gam_sep_spp]<-"gam_sep"
model_sel_small[sep_spp]<-"brt_sep"
model_sel_small[gamspp]<-"gam"
model_sel_small[brtspp]<-"brt"
write.csv(model_sel_small, file="model_sel_med2.csv", row.names=F)
for (spp in 1:48)
{
  wpred_sub<-subset(wpred_data, genus==unique(wpred_data$genus)[spp])
  if (spp%in%sep_spp==T)
  {
    if (r2_sep_brt[spp]>0)
    {
    pred_exist<-preds_brt(sep_med_exist_brt[[spp]])
    if (spp!=9)
    {pred_num<-exp(preds_brt(sep_med_num_brt[[spp]]))}
    if (spp==9)
    {
      pred_num<-predict.lm(sep_med_num_brt[[spp]], newdata=rich_dat_med)
    }
    final_pred<-rescale_constant(pred_num, pred_exist, "med")
    }
  }
  if (spp%in%sep_spp==F)
  {
    final_pred<-preds_ziP(sep_med_gam[[spp]])

  }
  if (spp%in%brtspp)
  {
    pred_exist<-preds_brt(is_med_brt)
    pred_num<-exp(preds_brt(med_brt))
    final_pred<-rescale_constant(pred_num, pred_exist, "med")
  }
  if (spp%in%gamspp)
  {
    final_pred<-preds_ziP(all_med_mod)
  }
  wpred_data$pred_med[which(wpred_data$genus==unique(wpred_data$genus)[spp])]<-final_pred
}
wpred_sub<-wpred_data
r2mse('med', wpred_data$pred_med)
plot_pred('med', wpred_data$pred_med)
# final_pred<-predict.gam(all_small_mod, wpred_data2, type="response")
plot(wpred_data$pred_med,wpred_data$med, xlab="Predicted Medium Trees", ylab="Observed Medium Trees", pch=19, col=alpha("gray30",0.75))
abline(0,1)
write.csv(wpred_data$pred_med, file="predmed_sepgen.csv", row.names=F)

sum_imb<-summary(is_med_brt)
length(which(sum_imb$rel.inf>1))
plotvar<-which(is_med_brt$var.names==sum_imb$var[2])
plot(is_med_brt, plotvar)
plotvar<-which(is_med_brt$var.names==sum_imb$var[5])
plot(is_med_brt, plotvar)
plotvar<-which(is_med_brt$var.names==sum_imb$var[6])
plot(is_med_brt, plotvar)
plotvar<-which(is_med_brt$var.names==sum_imb$var[7])
plot(is_med_brt, plotvar)
plotvar<-which(is_med_brt$var.names==sum_imb$var[8])
plot(is_med_brt, plotvar)

rich_dat_med<-subset(wpred_data, med>0)
rich_dat_med$ecoprovmaj<-factor(rich_dat_med$ecoprovmaj, levels=levels(data$ecoprovmaj))
rich_dat_med$log_n_med<-log(rich_dat_med$med)

med_brt<-gbm.step(data=rich_dat_med, gbm.x=c(2,20,27:64, 67:74,81:92,93,94), gbm.y=99, n.folds=10, n.trees=30, family='gaussian', tree.complexity = 5, learning.rate=0.004)
med_brt<-gbm.step(data=rich_dat_med, gbm.x=c(2,7:8,11:29, 32:34,36:58, 63:76,78), gbm.y=83,n.folds=10, n.trees=30, family='gaussian', tree.complexity = 5, learning.rate=0.1, drop.unused.levels=F)
med_mod<-bam(as.formula(paste("round(med)~s(pred_small_trees, k=3, bs='cr')+s(pred_med_trees, k=3, bs='cr')+s(pred_small, k=3, bs='cr')+genus+", paste(varz2[c(3:48)],collapse="+"), sep="")), select=T, method="fREML", data=rich_dat_med, nthreads=4, chunk.size = 1000, drop.unused.levels = F, discrete=T, family=poisson)
#med_mod<-gam(as.formula(paste("log(med+1)~SP+s(pred_small,k=3)+s(pred_med_trees,k=3)+",paste(varz[c(1:14,16:20,22:36)], collapse="+"),sep="")), select=T, method="GCV.Cp", data=rich_dat_med, family=gaussian)
 pred_num<-exp(preds_brt(med_brt))
 pred_exist<-preds_brt(is_med_brt)
 final_pred<-rescale_constant(pred_num, pred_exist,"med")
final_pred<-preds_ziP(all_med_mod)
final_pred<-rescale_city2(final_pred, "pred_med_trees")
#ratio_med<-sum(wpred_data[,"med"])/sum(pred_num*pred_exist)
#final_pred<-rescale_city(pred_num, pred_exist, "med")


r2mse("med", final_pred)
plot_pred("med", final_pred)
wpred_data$pred_med<-final_pred

plot(final_pred,wpred_data2$med, xlab="Predicted Medium Trees", ylab="Observed Medium Trees", pch=19, col=alpha("gray30",0.75))
abline(0,1)

sum_mb<-summary(med_brt)
length(which(sum_mb$rel.inf>1))
plotvar<-which(med_brt$var.names==sum_mb$var[1])
plot(med_brt, plotvar)
plotvar<-which(med_brt$var.names==sum_mb$var[3])
plot(med_brt, plotvar)
plotvar<-which(med_brt$var.names==sum_mb$var[6])
plot(med_brt, plotvar)

wpred_data$is_large<-as.numeric(wpred_data$large>1)
is_large_mod<-gam(as.formula(paste("is_large~s(pred_small,k=3)+s(pred_med,k=3)+s(pred_large_trees, k=3)+SP",paste(varz[c(3:20, 22:48)], collapse='+'), sep="+")), select=T, method="GCV.Cp", data=wpred_data, family=binomial, drop.unused.levels = F)
sum_ilm<-summary(is_large_mod)
imp_terms<-which(sum_ilm$edf>=0.95)
plot(is_large_mod, select=imp_terms[1], ylim=c(-10,20), se=F)
plot(is_large_mod, select=imp_terms[2], ylim=c(-50,200), se=F)
plot(is_large_mod, select=imp_terms[3], ylim=c(-100,50), se=F)
plot(is_large_mod, select=imp_terms[4], ylim=c(-100,200), se=F)
plot(is_large_mod, select=imp_terms[5], ylim=c(-500,50), se=F)
plot(is_large_mod, select=imp_terms[6], ylim=c(-50,50), se=F)
plot(is_large_mod, select=imp_terms[7], ylim=c(-100,30), se=F)
plot(is_large_mod, select=imp_terms[8], ylim=c(-100,50), se=F)
plot(is_large_mod, select=imp_terms[9], ylim=c(-20,50), se=F)
plot(is_large_mod, select=imp_terms[10], ylim=c(-75,20), se=F)


#var<-colnames(wpred_data)[c(2,35:75,79:80, 84:89,92,94)]
#is_large_brt<-gbm.step(data=wpred_data, gbm.x=c(2,11:22,40:78, 81:88, 92,94,95), gbm.y=26, n.folds=10, n.trees=30, family='bernoulli', tree.complexity = 1, learning.rate=0.001) #fits gbm with optimal number of trees - tweak tree complexity to maximize cv correlation, adjust learning rate and tolerance to ensure at least 1000 trees
rich_dat_large<-subset(wpred_data, large>0)
rich_dat_large$ecoprovmaj<-factor(rich_dat_large$ecoprovmaj, levels=levels(data$ecoprovmaj))
rich_dat_large$log_n_large<-log(rich_dat_large$large)
# large_brt<-gbm.step(data=rich_dat_large, gbm.x=c(2,11:22,40:78, 81:88, 92,94,95), gbm.y=96, bag.fraction = 0.95,n.folds=10, n.trees=30, family='gaussian', tree.complexity = 2, learning.rate=0.003)
#not enough data to use select function, manually do fwd sel
 for (i in c(3:14, 16:20, 22:48))
 {
 large_mod<-gam(as.formula(paste("log_n_large~s(pred_med, k=3)+s(pred_large_trees, k=3)+s(pred_med, k=3)+s(pred_small, k=3)+SP",varz[18],sep="+")), select=T, method="GCV.Cp", data=rich_dat_large, family=gaussian)
 
#imp_terms<-which(summary(large_mod)$edf>0.95)
#plot(large_mod, select=imp_terms[1], se=F)
#plot(large_mod, select=imp_terms[2], se=F)

  cat(i, large_mod$aic, "\n")
 }

all_large_mod<-bam(as.formula(paste("round(large)~s(pred_large_trees, k=3, bs='cr')+s(pred_small, k=3, bs='cr')+s(pred_med, k=3, bs='cr')+genus+", paste(varz[c(1:35)],collapse="+"), sep="")), select=T, method="fREML", data=wpred_data, nthreads=4, chunk.size = 1000, drop.unused.levels = F, discrete=T, family=ziP())
is_large_mod<-bam(as.formula(paste("is_large~s(pred_small_trees, k=3, bs='cr')+s(pred_med_trees, k=3, bs='cr')+s(pred_large_trees, k=3, bs='cr')+s(pred_small, k=3)+s(pred_med, k=3, bs='cr')+genus+", paste(varz[c(3:48)],collapse="+"), sep="")), select=T, method="fREML", data=wpred_data, nthreads=4, chunk.size = 1000, drop.unused.levels = F, discrete=T, family=binomial())
large_mod<-bam(as.formula(paste("(large)~s(pred_small_trees, k=3, bs='cr')+s(pred_med_trees, k=3, bs='cr')+s(pred_large_trees, k=3, bs='cr')+s(pred_med, k=3, bs='cr')+genus+", paste(varz[c(3:48)],collapse="+"), sep="")), select=T, method="fREML", data=rich_dat_large, nthreads=4, chunk.size = 1000, drop.unused.levels = F, discrete=T, family=poisson)
is_large_brt<-gbm.step(data=wpred_data, gbm.x=c(2,7,10,13:20,23, 25:36, 41:52, 55,57,59), gbm.y=60,n.folds=10, n.trees=30, family='bernoulli', tree.complexity = 3, learning.rate=0.1, drop.unused.levels=F)
large_brt<-gbm.step(data=rich_dat_large, gbm.x=c(2,7,10,13:20,23, 25:36, 41:52, 55,57,59), gbm.y=61,n.folds=10, n.trees=30, family='gaussian', tree.complexity = 3, learning.rate=0.1, drop.unused.levels=F)

pred_num<-(preds_gam(large_mod, "large"))
pred_exist<-preds_gam(is_large_mod)

pred_num<-exp(preds_brt(large_brt))
pred_exist<-preds_brt(is_large_brt)
ratio_large<-sum(wpred_data[,"large"])/sum(pred_num*pred_exist)
final_pred<-rescale_constant(pred_num, pred_exist,"large")
final_pred<-rescale_city(pred_num, pred_exist, "pred_large_trees")
final_pred<-preds_ziP(all_large_mod)
final_pred2<-rescale_city2(final_pred, "large")
r2mse("large", final_pred)
plot_pred("large", final_pred)
wpred_data$pred_large<-final_pred

sep_large_exist_brt<-list()
sep_large_num_brt<-readRDS('sep_large_num_brt.rds')
r2_sep_brt<-rep(0,48)
for (spp in 2:48)
{
  wpred_sub<-subset(wpred_data, genus==unique(wpred_data$genus)[spp])
  if (sum(wpred_sub$large)==0)
  {
    next
  }
  wpred_sub$is_large<-wpred_sub$large>0
  rich_dat_large<-subset(wpred_sub, is_large==1)
  if (nrow(rich_dat_large)>48)
  {
    sep_large_exist_brt[[spp]]<-gbm.step(data=wpred_sub, gbm.x=c(7:8,11:29, 32:34,38:60,77:79,81,83), gbm.y=84,n.folds=10, n.trees=30, family='bernoulli', tree.complexity = 3, learning.rate=0.005, drop.unused.levels=F)
    i=0.005
    while (sep_large_exist_brt[[spp]]$n.trees<1000)
    {
      i=i/5
      sep_large_exist_brt[[spp]]<-gbm.step(data=wpred_sub, gbm.x=c(7:8,11:29, 32:34,38:60,77:79,81,83), gbm.y=84,n.folds=10, n.trees=30, family='bernoulli', tree.complexity = 3, learning.rate=i, drop.unused.levels=F)
    }
    
    # wpred_data$is_large<-wpred_data$large>0
    # 
     rich_dat_large$ecoprovmaj<-factor(rich_dat_large$ecoprovmaj, levels=levels(data$ecoprovmaj))
    # #rich_dat_small$phzone<-factor(rich_dat_small$phzone, levels=levels(allUS$phzone))
     rich_dat_large$log_n_large<-log(rich_dat_large$large)
     sep_large_num_brt[[spp]]<-gbm.step(data=rich_dat_large, gbm.x=c(7:8,11:29, 32:34,38:60,77:79,81,83), gbm.y=86,n.folds=10, n.trees=30, family='gaussian', tree.complexity = 3, learning.rate=0.005, drop.unused.levels=F)
    i=0.005
    while (sep_large_num_brt[[spp]]$n.trees<1000)
    {
      i=i/10
      sep_large_num_brt[[spp]]<-gbm.step(data=rich_dat_large, gbm.x=c(7:8,11:29, 32:34,38:60,77:79,81,83), gbm.y=86,n.folds=10, n.trees=30, family='gaussian', tree.complexity = 3, learning.rate=i, drop.unused.levels=F)
    }

    
    
    pred_num<-exp(preds_brt(sep_large_num_brt[[spp]]))
    #pred_num<-exp(preds_gam(small_mod))
    #pred_exist<-preds_gam(is_small_mod)
    pred_exist<-preds_brt(sep_large_exist_brt[[spp]])
    final_pred<-rescale_constant(pred_num, pred_exist,"large")
    r2_sep_brt[spp]<-r2mse('large',final_pred)
    
  }
}

r2_gam<-rep(0,48)
for (spp in 1:48)
{
  wpred_sub<-subset(wpred_data, genus==unique(wpred_data$genus)[spp])
  final_pred<-preds_ziP(all_large_mod)
  r2_gam[spp]<-r2mse("large", final_pred)
}
r2_brt<-rep(0,48)
for (spp in 1:48)
{
  wpred_sub<-subset(wpred_data, genus==unique(wpred_data$genus)[spp])
  pred_exist<-preds_brt(is_large_brt)
  pred_num<-exp(preds_brt(large_brt))
  final_pred<-rescale_constant(pred_num, pred_exist, "large")
  r2_brt[spp]<-r2mse("large", final_pred)
}
r2_sep_gam<-read.csv('r2_sep_gam_large.csv')[,1]
r2_sep_brt<-read.csv('r2_sep_brt_large.csv')[,2]
r2_sep_brt[12]<-0.6613408
large_sep_gam<-readRDS("large_sep_gam.RDS")


sep_large_num_brt<-readRDS("large_sep_brt.RDS")
sep_large_exist_brt<-readRDS("is_large_sep_brt.RDS")
sep_spp<-which(r2_sep_gam<r2_sep_brt)
r2_comb<-rep(0,48)
r2_comb[sep_spp]<-r2_sep_brt[sep_spp]
gam_sep_spp<-which(1:48%in%sep_spp==F)
r2_comb[which(1:48%in%sep_spp==F)]<-r2_sep_gam[which(1:48%in%sep_spp==F)]
gamspp<-which(r2_gam>r2_comb)
r2_comb[which(r2_gam>r2_comb)]<-r2_gam[which(r2_gam>r2_comb)]
brtspp<-which(r2_brt>r2_comb)
r2_comb[which(r2_brt>r2_comb)]<-r2_brt[which(r2_brt>r2_comb)]

min(r2_comb)
model_sel_small<-rep(0,48)
model_sel_small[gam_sep_spp]<-"gam_sep"
model_sel_small[sep_spp]<-"brt_sep"
model_sel_small[gamspp]<-"gam"
model_sel_small[brtspp]<-"brt"
write.csv(model_sel_small, file="model_sel_large.csv", row.names=F)

for (spp in 1:48)
{
  wpred_sub<-subset(wpred_data, genus==unique(wpred_data$genus)[spp])
  if (spp%in%sep_spp==T)
  {
    if (r2_sep_brt[spp]>0)
    {
      pred_exist<-preds_brt(sep_large_exist_brt[[spp]])
      pred_num<-exp(preds_brt(sep_large_num_brt[[spp]]))
      final_pred<-rescale_constant(pred_num, pred_exist, "large")
    }
  }
  if (spp%in%sep_spp==F)
  {
    if (length(sep_large_gam[[spp]])>0)
    {final_pred<-preds_ziP(sep_large_gam[[spp]])}
    
  }
  if (spp%in%brtspp)
  {
    pred_exist<-preds_brt(is_large_brt)
    pred_num<-exp(preds_brt(large_brt))
    final_pred<-rescale_constant(pred_num, pred_exist, "large")
  }
  if (spp%in%gamspp)
  {
    final_pred<-preds_ziP(all_large_mod)
  }
  wpred_data$pred_large[which(wpred_data$genus==unique(wpred_data$genus)[spp])]<-final_pred
}
wpred_sub<-wpred_data
r2mse('large', wpred_data$pred_large)
plot_pred('large', wpred_data$pred_large)
# final_pred<-predict.gam(all_small_mod, wpred_data2, type="response")
plot(wpred_data$pred_small,wpred_data$small, xlab="Predicted Large Trees", ylab="Observed Large Trees", pch=19, col=alpha("gray30",0.75))
abline(0,1)
write.csv(wpred_data$pred_med, file="predmed_sepgen.csv", row.names=F)
wpred_data$pred_med<-read.csv('predmed_sepgen.csv')[,1]
##add in predicted total # trees as predictor 
plot(final_pred2,wpred_data$large, xlab="Predicted Large Trees", ylab="Observed Large Trees", pch=19, col=alpha("gray30",0.75))
abline(0,1)
write.csv(wpred_data, file="predalltrees_sepgen.csv", row.names=F)
##predict on US
 wpred_data<-read.csv('~/Downloads/predalltrees_sepgen.csv')
m<-lm(wpred_data$small~wpred_data$pred_small-1)
m2<-lm(wpred_data$med~wpred_data$pred_med-1)
m3<-lm(wpred_data$large~wpred_data$pred_large-1)
library(mg)
m4<-lm(data$small_cor~predict.gam(tot_small_gam, type='response', newdata=data)-1)
m5<-lm(data$med_cor~pred_med_trees-1)
m6<-lm(data$large_cor~pred_large_trees-1)
allUS<-read.csv('~/Downloads/entire_predictors_fipsfix.csv')
model_sel_small<-read.csv('model_sel_small.csv')
model_sel_med<-read.csv('model_sel_med.csv')
model_sel_large<-read.csv('model_sel_large.csv')
wpred_sub<-wpred_data<-read.csv('predalltrees_sepgen.csv')
ratio_each_small_nonsep<-ratio_each_med_nonsep<-ratio_each_large_nonsep<-rep(0,48)
ratio_each_small<-ratio_each_med<-ratio_each_large<-rep(0,48)
for(i in 1:48)
{
  wpred_sub<-subset(wpred_data, genus==unique(wpred_data$genus)[i])
ratio_each_small_nonsep[i]<-sum(wpred_sub$small)/sum(exp(preds_brt(small_brt)*preds_brt(is_small_brt)))
ratio_each_med_nonsep[i]<-sum(wpred_sub$med)/sum(exp(preds_brt(med_brt)*preds_brt(is_med_brt)))
ratio_each_large_nonsep[i]<-sum(wpred_sub$large)/sum(exp(preds_brt(large_brt)*preds_brt(is_large_brt)))
if (i<47)
{
 if (length(sep_small_num_brt[[i]])>0)
 {ratio_each_small[i]<-sum(wpred_sub$small)/sum(exp(preds_brt(sep_small_num_brt[[i]])*preds_brt(sep_small_exist_brt[[i]])))}
  if (length(sep_med_exist_brt[[i]])>0)
  {
 ratio_each_med[i]<-sum(wpred_sub$med)/sum(exp(preds_brt(sep_med_num_brt[[i]])*preds_brt(sep_med_exist_brt[[i]])))
  }
  if (length(sep_large_exist_brt[[i]])>0)
  {
 ratio_each_large[i]<-sum(wpred_sub$large)/sum(exp(preds_brt(sep_large_num_brt[[i]])*preds_brt(sep_large_exist_brt[[i]])))
  }
}
}
write.csv(data.frame(ratio_each_small=ratio_each_small, ratio_each_med=ratio_each_med, ratio_each_large=ratio_each_large, ratio_each_small_nonsep=ratio_each_small_nonsep, ratio_each_med_nonsep=ratio_each_med_nonsep, ratio_each_large_nonsep=ratio_each_large_nonsep), row.names=F, file='ratios_forpred.csv')
# getmode <- function(v) {
#   uniqv <- unique(v)
#   uniqv[which.max(tabulate(match(v, uniqv)))]
# }
colnames(allUS)[c(54,56)]<-c("bio18", "coastdat.coast3")
colnames(allUS)[c(35,36)]<-c("Mean.YEAR", "Mean.Value")

fix_cols<-c(4:5,8:21,23,25:26, 29:31, 35:57)
matchcol<-match(colnames(allUS)[fix_cols], colnames(wpred_data))
allUS$inc_1969<-as.numeric(gsub(",","",(as.character(allUS$inc_1969))))
allUS$inc_1979<-as.numeric(gsub(",","",(as.character(allUS$inc_1979))))
allUS$inc_1989<-as.numeric(gsub(",","",(as.character(allUS$inc_1989))))
allUS$Mean.Value<-as.numeric(as.character(allUS$Mean.Value))
allUS$Mean.YEAR<-as.numeric(as.character(allUS$Mean.YEAR))

allUS_fix<-allUS
for (i in 1:length(fix_cols))
{
  allUS_fix[which(allUS_fix[,fix_cols[i]]>max(wpred_data[,matchcol[i]], na.rm=T)),fix_cols[i]]<-max(wpred_data[,matchcol[i]], na.rm=T)
  allUS_fix[which(allUS_fix[,fix_cols[i]]<min(wpred_data[,matchcol[i]], na.rm=T)),fix_cols[i]]<-min(wpred_data[,matchcol[i]], na.rm=T)
}
# allUS_small<-allUS
# for (i in 1:length(fix_cols))
# {
#   allUS_small[which(allUS_small[,fix_cols[i]]>max(rich_dat_small[,matchcol[i]], na.rm=T)),fix_cols[i]]<-max(rich_dat_small[,matchcol[i]], na.rm=T)
#   allUS_small[which(allUS_small[,fix_cols[i]]<min(rich_dat_small[,matchcol[i]], na.rm=T)),fix_cols[i]]<-min(rich_dat_small[,matchcol[i]], na.rm=T)
# }
# 
# allUS_med<-allUS
# for (i in 1:length(fix_cols))
# {
#   allUS_med[which(allUS_med[,fix_cols[i]]>max(rich_dat_med[,matchcol[i]], na.rm=T)),fix_cols[i]]<-max(rich_dat_med[,matchcol[i]], na.rm=T)
#   allUS_med[which(allUS_med[,fix_cols[i]]<min(rich_dat_med[,matchcol[i]], na.rm=T)),fix_cols[i]]<-min(rich_dat_med[,matchcol[i]], na.rm=T)
# }
# 
# allUS_large<-allUS
# for (i in 1:length(fix_cols))
# {
#   allUS_large[which(allUS_large[,fix_cols[i]]>max(rich_dat_large[,matchcol[i]], na.rm=T)),fix_cols[i]]<-max(rich_dat_large[,matchcol[i]],na.rm=T)
#   allUS_large[which(allUS_large[,fix_cols[i]]<min(rich_dat_large[,matchcol[i]], na.rm=T)),fix_cols[i]]<-min(rich_dat_large[,matchcol[i]], na.rm=T)
# }


 allUS_fix$inc_1969[which(is.na(allUS_fix$inc_1969)==T)]<-mean(allUS_fix$inc_1969, na.rm=T)
 allUS_fix$inc_1979[which(is.na(allUS_fix$inc_1979)==T)]<-mean(allUS_fix$inc_1979, na.rm=T)
 allUS_fix$inc_1989[which(is.na(allUS_fix$inc_1989)==T)]<-mean(allUS_fix$inc_1989, na.rm=T)
 allUS_fix$Mean.Value[which(is.na(allUS_fix$Mean.Value)==T)]<-mean(allUS_fix$Mean.Value, na.rm=T)
 allUS_fix$Mean.YEAR[which(is.na(allUS_fix$Mean.YEAR)==T)]<-mean(as.numeric(as.character((allUS_fix$Mean.YEAR))), na.rm=T)
# 
# 
# allUS_med$inc_1969[which(is.na(allUS_med$inc_1969)==T)]<-mean(allUS_med$inc_1969, na.rm=T)
# allUS_med$inc_1979[which(is.na(allUS_med$inc_1979)==T)]<-mean(allUS_med$inc_1979, na.rm=T)
# allUS_med$inc_1989[which(is.na(allUS_med$inc_1989)==T)]<-mean(allUS_med$inc_1989, na.rm=T)
# allUS_med$Mean.Value[which(is.na(allUS_med$Mean.Value)==T)]<-mean(allUS_med$Mean.Value, na.rm=T)
# allUS_med$Mean.YEAR[which(is.na(allUS_med$Mean.YEAR)==T)]<-mean(as.numeric(as.character((allUS_med$Mean.YEAR))), na.rm=T)
# 
# allUS_large$inc_1969[which(is.na(allUS_large$inc_1969)==T)]<-mean(allUS_large$inc_1969, na.rm=T)
# allUS_large$inc_1979[which(is.na(allUS_large$inc_1979)==T)]<-mean(allUS_large$inc_1979, na.rm=T)
# allUS_large$inc_1989[which(is.na(allUS_large$inc_1989)==T)]<-mean(allUS_large$inc_1989, na.rm=T)
# allUS_large$Mean.Value[which(is.na(allUS_large$Mean.Value)==T)]<-mean(allUS_large$Mean.Value, na.rm=T)
# allUS_large$Mean.YEAR[which(is.na(allUS_large$Mean.YEAR)==T)]<-mean(as.numeric(as.character((allUS_large$Mean.YEAR))), na.rm=T)
sub<-wpred_data[1,65:76]
genera<-unique(wpred_data$genus)
rm(canopy)
predicted_trees<-matrix(0, 1,76)
colnames(predicted_trees)<-c(colnames(allUS), "genus", colnames(sub), "pred_small_trees", "pred_med_trees", "pred_large_trees", "pred_small", "pred_med", "pred_large")
colnames(predicted_trees)[33:34]<-colnames(allUS_fix)[33:34]<-c("Mean.YEAR.y", "Mean.Value.y")
for (i in 1:nrow(allUS))
{
to_add_small<-cbind(allUS_fix[rep(i, 48),],genera, sub)
to_add_small$ecoprovmaj<-as.factor(to_add_small$ecoprovmaj)
to_add_small$pred_small_trees<-predict.gam(tot_small_gam, type="response", newdata=to_add_small)
#to_add_med<-cbind(allUS_med[rep(i, 48),],genera, sub)
to_add_small$pred_small_trees[which((to_add_small$pred_small_trees>max(wpred_data$pred_small_trees))==T)]<-max(wpred_data$pred_small_trees)
to_add_small$pred_small_trees[which((to_add_small$pred_small_trees<min(wpred_data$pred_small_trees))==T)]<-min(wpred_data$pred_small_trees)
#to_add_med$ecoprovmaj<- as.factor(to_add_med$ecoprovmaj)
#to_add_med$pred_small_trees<-to_add_small$pred_small_trees
to_add_small$pred_med_trees<-predict.gam(tot_med_gam, type="response", newdata=to_add_small)
to_add_small$pred_med_trees[which((to_add_small$pred_med_trees>max(wpred_data$pred_med_trees))==T)]<-max(wpred_data$pred_med_trees)
to_add_small$pred_med_trees[which((to_add_small$pred_med_trees<min(wpred_data$pred_med_trees))==T)]<-min(wpred_data$pred_med_trees)
#to_add_large<-cbind(allUS_large[rep(i, 48),],genera, sub)
#to_add_large$ecoprovmaj<- as.factor(to_add_large$ecoprovmaj)
#to_add_large$pred_small_trees<-to_add_small$pred_small_trees
#to_add_large$pred_med_trees<-to_add_med$pred_med_trees
to_add_small$pred_large_trees<-predict.gam(tot_large_gam, type="response", newdata=to_add_small)
to_add_small$pred_large_trees[which((to_add_small$pred_large_trees>max(wpred_data$pred_large_trees))==T)]<-max(wpred_data$pred_large_trees)
to_add_small$pred_large_trees[which((to_add_small$pred_large_trees<min(wpred_data$pred_large_trees))==T)]<-min(wpred_data$pred_large_trees)
colnames(to_add_small)[58]<-"genus"
#colnames(to_add_med)[57]<-"genus"
#colnames(to_add_large)[57]<-"genus"
# small_is_add<-predict.gbm(is_small_brt,newdata=to_add_small, n.trees=is_small_brt$n.trees, type="response")
# small_add<-exp(predict.gbm(small_brt,newdata=to_add_small, n.trees=small_brt$n.trees, type="response"))
# to_add_small$pred_small<-small_add*small_is_add*ratio_small
to_add_small$pred_small<-to_add_small$pred_med<-to_add_small$pred_large<-rep(0, nrow(to_add_small))
to_add_small$pred_small[which(model_sel_small=="gam")]<-predict.gam(all_small_mod, type="response", newdata=to_add_small[which(model_sel_small=="gam"),])
for(spp in which(model_sel_small=="gam_sep"))
{
  to_add_small$pred_small[spp]<-predict.gam(sep_small_gam[[spp]], type="response", newdata=to_add_small[spp,])
}
for (spp in which(model_sel_small=="brt_sep"))
{
  to_add_small$pred_small[spp]<-exp(predict.gbm(sep_small_num_brt[[spp]], type="response", newdata=to_add_small[spp,], n.trees=sep_small_num_brt[[spp]]$n.trees))*predict.gbm(sep_small_exist_brt[[spp]], type="response", newdata=to_add_small[spp,],n.trees=sep_small_exist_brt[[spp]]$n.trees)*ratio_each_small[[spp]]
}
to_add_small$pred_small[which(model_sel_small=="brt")]<-exp(predict.gbm(small_brt, type="response", newdata=to_add_small[which(model_sel_small=="brt"),], n.trees=small_brt$n.trees))*predict.gbm(is_small_brt, type="response", newdata=to_add_small[which(model_sel_small=="brt"),], n.trees=is_small_brt$n.trees)*ratio_each_small_nonsep[which(model_sel_small=="brt")]



to_add_small$pred_med[which(model_sel_med=="gam")]<-predict.gam(all_med_mod, type="response", newdata=to_add_small[which(model_sel_med=="gam"),])
for(spp in which(model_sel_med=="gam_sep"))
{
  to_add_small$pred_med[spp]<-predict.gam(sep_med_gam[[spp]], type="response", newdata=to_add_small[spp,])
}
for (spp in which(model_sel_med=="brt_sep"))
{
  to_add_small$pred_med[spp]<-exp(predict.gbm(sep_med_num_brt[[spp]], type="response", newdata=to_add_small[spp,], n.trees=sep_med_num_brt[[spp]]$n.trees))*predict.gbm(sep_med_exist_brt[[spp]], type="response", newdata=to_add_small[spp,],n.trees=sep_med_exist_brt[[spp]]$n.trees)*ratio_each_med[[spp]]
}
to_add_small$pred_med[which(model_sel_med=="brt")]<-exp(predict.gbm(med_brt, type="response", newdata=to_add_small[which(model_sel_med=="brt"),], n.trees=med_brt$n.trees))*predict.gbm(is_med_brt, type="response", newdata=to_add_small[which(model_sel_med=="brt"),], n.trees=is_med_brt$n.trees)*ratio_each_med_nonsep[which(model_sel_med=="brt")]

to_add_small$pred_large[which(model_sel_large=="gam")]<-predict.gam(all_large_mod, type="response", newdata=to_add_small[which(model_sel_large=="gam"),])
for(spp in which(model_sel_large=="gam_sep"))
{
  to_add_small$pred_large[spp]<-predict.gam(sep_large_gam[[spp]], type="response", newdata=to_add_small[spp,])
}
for (spp in which(model_sel_large=="brt_sep"))
{
  to_add_small$pred_large[spp]<-exp(predict.gbm(sep_large_num_brt[[spp]], type="response", newdata=to_add_small[spp,], n.trees=sep_large_num_brt[[spp]]$n.trees))*predict.gbm(sep_large_exist_brt[[spp]], type="response", newdata=to_add_small[spp,],n.trees=sep_large_exist_brt[[spp]]$n.trees)*ratio_each_large[[spp]]
}
to_add_small$pred_large[which(model_sel_large=="brt")]<-exp(predict.gbm(large_brt, type="response", newdata=to_add_small[which(model_sel_large=="brt"),], n.trees=large_brt$n.trees))*predict.gbm(is_large_brt, type="response", newdata=to_add_small[which(model_sel_large=="brt"),], n.trees=is_large_brt$n.trees)*ratio_each_large_nonsep[which(model_sel_large=="brt")]
#to_add_med$pred_small<-to_add_small$pred_small
# med_sub<-subset(to_add_med, SP%in%rich_dat_med$SP)
# med_is_add<-predict.gbm(is_med_brt,newdata=med_sub, n.trees=is_med_brt$n.trees, type="response")
# med_add<-exp(predict.gbm(med_brt,newdata=med_sub, n.trees=med_brt$n.trees, type="response"))
# to_add_med$pred_med<-rep(0,48)
# to_add_med$pred_med[which(genera%in%rich_dat_med$SP)]<-med_add*med_is_add*ratio_med
#to_add_small$pred_med<-predict.gam(all_med_mod, type="response", newdata=to_add_small)
#to_add_large$pred_small<-to_add_small$pred_small
#to_add_large$pred_med<-to_add_med$pred_med
# large_sub<-subset(to_add_large, SP%in%rich_dat_large$SP)
# large_is_add<-predict.gam(is_large_mod,newdata=large_sub,type="response")
# large_add<-exp(predict.gam(large_mod,newdata=large_sub, type="response"))
# to_add_large$pred_large<-rep(0,48)
# to_add_large$pred_large[which(genera%in%rich_dat_large$SP)]<-large_add*large_is_add*ratio_large
#to_add_small$pred_large<-predict.gam(all_large_mod, type="response", newdata=to_add_small)
predicted_trees<-rbind(predicted_trees, to_add_small)
}
predicted_trees<-predicted_trees[2:nrow(predicted_trees),]
write.csv(predicted_trees, file="predicted_streettrees_allUS.csv", row.names=F)
saveRDS(all_large_mod, file="large_gam_nonsep.rds")
saveRDS(large_brt, file="large_brt_nonsep.rds")
saveRDS(is_large_brt, file="is_large_brt_nonsep.rds")
saveRDS(med_brt, file="med_brt_nonsep.rds")
saveRDS(is_med_brt, file="is_med_brt_nonsep.rds")
saveRDS(sep_large_num_brt, file="sep_large_num_brt.rds")
saveRDS(sep_large_exist_brt, file="sep_large_exist_brt.rds")
saveRDS(sep_med_num_brt, file="sep_med_num_brt.rds")
saveRDS(sep_med_exist_brt, file="seo_med_exist_brt.rds")
saveRDS(sep_large_gam, file="sep_large_gam.rds")

saveRDS(all_small_mod, file="small_mod_cor.rds")
saveRDS(all_med_mod, file="med_gam_nonsep.rds")
saveRDS(tot_small_gam, file="tot_small_gam_cor2.rds")
saveRDS(tot_med_gam, file="tot_med_gam_cor2.rds")
saveRDS(tot_large_gam, file="tot_large_gam_cor2.rds")

setwd('~/Downloads/')
all_large_mod<-readRDS('large_gam_nonsep.rds')
all_med_mod<-readRDS('med_gam_nonsep.rds')
all_small_mod<-readRDS('small_gam_nonsep.rds')
tot_small_gam<-readRDS( file="tot_small_gam_cor2.rds")
tot_med_gam<-readRDS( file="tot_med_gam_cor2.rds")
tot_large_gam<-readRDS( file="tot_large_gam_cor2.rds")
is_large_brt<-readRDS('is_large_brt_nonsep.RDS')
large_brt<-readRDS('large_brt_nonsep.RDS')
is_med_brt<-readRDS('is_med_brt_nonsep.RDS')
med_brt<-readRDS('med_brt_nonsep.RDS')
is_small_brt<-readRDS('is_small_brt_nonsep.RDS')
small_brt<-readRDS('small_brt_nonsep.RDS')

sum_tsg<-summary(tot_small_gam)
imp_terms<-which(sum_tsg$edf>=0.95)
plot(tot_small_gam, select=imp_terms[1], ylim=c(-5,5), se=F)
plot(tot_small_gam, select=imp_terms[2], ylim=c(-5,5), se=F)
plot(tot_small_gam, select=imp_terms[3], ylim=c(-5,5), se=F)
plot(tot_small_gam, select=imp_terms[4], ylim=c(-5000,20000), se=F)
plot(tot_small_gam, select=imp_terms[5], ylim=c(-2,2), se=F)
plot(tot_small_gam, select=imp_terms[6], ylim=c(-5,50), se=F)
plot(tot_small_gam, select=imp_terms[7], ylim=c(-20,5), se=F)

sum_tmg<-summary(tot_med_gam)
imp_terms<-which(sum_tmg$edf>=0.95)
plot(tot_med_gam, select=imp_terms[1], ylim=c(-0.5,0.5), se=F)
plot(tot_med_gam, select=imp_terms[2], ylim=c(-4,4), se=F)
plot(tot_med_gam, select=imp_terms[3], ylim=c(-4,4), se=F)
plot(tot_med_gam, select=imp_terms[4], ylim=c(-800,50), se=F)
plot(tot_med_gam, select=imp_terms[5], ylim=c(-3,3), se=F)
plot(tot_med_gam, select=imp_terms[6], ylim=c(-60,20), se=F)
plot(tot_med_gam, select=imp_terms[7], ylim=c(-15,5), se=F)
plot(tot_med_gam, select=imp_terms[8], ylim=c(-200,2500), se=F)

sum_tlg<-summary(tot_large_gam)
imp_terms<-which(sum_tlg$edf>=0.95)
plot(tot_large_gam, select=imp_terms[1], ylim=c(-15,5), se=F)
plot(tot_large_gam, select=imp_terms[2], ylim=c(-2,7), se=F)

wpred_data$pred_large<-final_pred
write.csv(wpred_data, file="model_predictions_cities_apr2019.csv", row.names=F)


sub_data<-wpred_data[sample(1:nrow(wpred_data), 10000, replace=F),]

all_small_mod<-gam(as.formula(paste("round(small)~s(pred_small_trees, k=3)+genus+", paste(varz[c(3:48)],collapse="+"), sep="")), select=T, method="GCV.Cp", data=sub_data, nthreads=4, drop.unused.levels = F, family=ziP())

all_small_mod2<-bam(as.formula(paste("round(small)~s(pred_small_trees, k=3, bs='cr')+genus+", paste(varz2[c(3:48)],collapse="+"), sep="")), select=T, method="fREML", data=sub_data, nthreads=4, chunk.size = 3333, drop.unused.levels = F, discrete=T, family=ziP())

all_small_mod3<-bam(as.formula(paste("round(small)~s(pred_small_trees, k=3, bs='cr')+genus+", paste(varz2[c(3:48)],collapse="+"), sep="")), select=T, method="fREML", data=sub_data, nthreads=4, chunk.size = 5000, drop.unused.levels = F, discrete=T, family=ziP())

write.csv(data, file="street_tree_cor.csv", row.names = F)
write.csv(wpred_data, file="street_tree_each_cor.csv", row.names = F)
