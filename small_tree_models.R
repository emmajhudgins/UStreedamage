rm(list=c(ls()))
write=F
library(gbm)
library(mgcv)
library(dismo)
setwd('~/Downloads/')
source('totaltree_clean.R')
source('treemod_funcs.R')

dd<-sample(1:12,12)

prop_small<-aggregate(rich_dat$small~rich_dat$genus, FUN=function(x)sum(x)/sum(rich_dat$small))
pie(prop_small$`rich_dat$small`)
pie(prop_small$`rich_dat$small`, labels = prop_small$`rich_dat$genus`)
prop_small<-prop_small[order(prop_small$`rich_dat$small`, decreasing=T),]
prop_other<-data.frame(cbind("other", sum(prop_small$`rich_dat$small`[12:48])))
colnames(prop_other)=colnames(prop_small)
prop_small_reduced<-rbind(prop_small[1:11,], prop_other)
par(mar=c(2,2,2,2))
par(oma=c(0,0,0,0))
library(viridis)
pie(as.numeric(prop_small_reduced$`rich_dat$small`), labels = paste0(prop_small_reduced$`rich_dat$genus`, " ", round(as.numeric(prop_small_reduced$`rich_dat$small`) * 100,1), "%"), cex=0.75, col=viridis(12)[dd], init.angle = 90, clockwise=T, main="Inventoried small tree breakdown by genus", font=3)

prop_med<-aggregate(rich_dat$med~rich_dat$genus, FUN=function(x)sum(x)/sum(rich_dat$med))
prop_med<-prop_med[order(prop_med$`rich_dat$med`, decreasing=T),]
prop_other<-data.frame(cbind("other", sum(prop_med$`rich_dat$med`[12:48])))
colnames(prop_other)=colnames(prop_med)
prop_med_reduced<-rbind(prop_med[1:11,], prop_other)
par(mar=c(2,2,2,2))
par(oma=c(0,0,0,0))
library(viridis)
pie(as.numeric(prop_med_reduced$`rich_dat$med`), labels = paste0(prop_med_reduced$`rich_dat$genus`, " ", round(as.numeric(prop_med_reduced$`rich_dat$med`) * 100,1), "%"), cex=0.75, col=viridis(12)[dd], init.angle = 90, clockwise=T, main="Inventoried medium tree breakdown by genus", font=3)


prop_large<-aggregate(rich_dat$large~rich_dat$genus, FUN=function(x)sum(x)/sum(rich_dat$large))
prop_large<-prop_large[order(prop_large$`rich_dat$large`, decreasing=T),]
prop_other<-data.frame(cbind("other", sum(prop_large$`rich_dat$large`[9:48])))
colnames(prop_other)=colnames(prop_large)
prop_large_reduced<-rbind(prop_large[1:8,], prop_other)
par(mar=c(2,2,2,2))
par(oma=c(0,0,0,0))
library(viridis)
pie(as.numeric(prop_large_reduced$`rich_dat$large`), labels = paste0(prop_large_reduced$`rich_dat$genus`, " ", round(as.numeric(prop_large_reduced$`rich_dat$large`) * 100,1), "%"), cex=0.75, col=viridis(12)[dd], init.angle = 90, clockwise=T, main="Inventoried large tree breakdown by genus", font=3)

fit_brt=F
fit_sep_gam=F
fit_sep_brt=F
write=F
varz2<-gsub(")$",",bs=\\'cr\\')",varz)
wpred_data_part<-wpred_data[,1:6]
wpred_data<-wpred_data[!duplicated(wpred_data_part),]
if (fit_gam==F)
{
  all_small_exist_mod<-readRDS('all_small_exist_mod4.rds')
  all_small_num_mod<-readRDS('all_small_num_mod4.rds')
}
if(fit_brt==F)
{
  is_small_brt<-readRDS("is_small_brt4.rds")
  small_brt<-readRDS("small_brt4.rds")
}
if(fit_sep_gam==F)
{
  sep_small_exist_gam<-readRDS("sep_small_exist_gam4.rds")
  sep_small_num_gam<-readRDS("sep_small_num_gam4.rds")
}
if(fit_sep_brt==F)
{
 sep_small_num_brt<-readRDS("sep_small_num_brt4.rds")
sep_small_exist_brt<-readRDS("sep_small_exist_brt4.rds")
}


wpred_data$ecoprovmaj<-factor(wpred_data$ecoprovmaj, levels=levels(data$ecoprovmaj))
wpred_data$is_small<-as.numeric(wpred_data$small>0)
rich_dat_small<-subset(wpred_data, is_small==1)
rich_dat_small$ecoprovmaj<-factor(rich_dat_small$ecoprovmaj, levels=levels(data$ecoprovmaj))
rich_dat_small$log_n_small<-log(rich_dat_small$small)


if (fit_brt==T)
{
is_small_brt<-gbm.step(data=wpred_data, gbm.x=c(2,7,10,13:20,23, 25:36, 41:52, 53), gbm.y=56, n.folds=10, n.trees=30, family='bernoulli', tree.complexity =3, learning.rate=0.1, drop.unused.levels=F)
i=0.1
while(is_small_brt$n.trees<1000)
{
  i=i/10
  is_small_brt<-gbm.step(data=wpred_data, gbm.x=c(2,7,10,13:20,23, 25:36, 41:52, 53), gbm.y=56, n.folds=10, n.trees=30, family='bernoulli', tree.complexity =3, learning.rate=i, drop.unused.levels=F)
}
small_brt<-gbm.step(data=rich_dat_small, gbm.x=c(2,7,10,13:20,23, 25:36, 41:52, 53), gbm.y=57,n.folds=10, n.trees=30, family='gaussian', tree.complexity = 3, learning.rate=0.5, drop.unused.levels=F)
while(small_brt$n.trees<1000)
{
  i=i/10
  small_brt<-gbm.step(data=rich_dat_small, gbm.x=c(2,7,10,13:20,23, 25:36, 41:52, 53), gbm.y=57,n.folds=10, n.trees=30, family='gaussian', tree.complexity = 3, learning.rate=i, drop.unused.levels=F)
}
}

if(fit_gam==T)
{
  all_small_exist_mod<-bam(as.formula(paste("is_small~s(pred_small_trees, k=3, bs='cr')+genus+", paste(varz2[c(1:35)],collapse="+"), sep="")), select=T, method="fREML", data=wpred_data, nthreads=4, chunk.size = 1000, drop.unused.levels = F, discrete=T, family=binomial)
  rich_dat_small<-subset(wpred_data, is_small==1)
  all_small_num_mod<-bam(as.formula(paste("round(small)~s(pred_small_trees, k=3, bs='cr')+genus+", paste(varz2[c(1:35)],collapse="+"), sep="")), select=T, method="fREML", data=rich_dat_small, nthreads=4, chunk.size = 1000, drop.unused.levels = F, discrete=T, family=poisson)
}
if (fit_sep_gam==T)
{
sep_small_exist_gam<-sep_small_num_gam<-list()
for (spp in 22:48)
{
  wpred_sub<-subset(wpred_data, genus==unique(wpred_data$genus)[spp])
  wpred_sub$is_small<-wpred_sub$small>0
  sep_small_exist_gam[[spp]]<-gam(as.formula(paste("is_small~s(pred_small_trees, k=3)+", paste(varz[c(1:23)],collapse="+"),sep="")), select=T, method="GCV.Cp", data=wpred_sub,family=binomial, drop.unused.levels = F)
  rich_dat_small<-subset(wpred_sub, is_small==1)
  rich_dat_small$ecoprovmaj<-factor(rich_dat_small$ecoprovmaj, levels=levels(data$ecoprovmaj))
  rich_dat_small$log_n_small<-log(rich_dat_small$small)
  if (nrow(rich_dat_small)>80)
  {
  sep_small_num_gam[[spp]]<-gam(as.formula(paste("round(small)~s(pred_small_trees, k=3)+", paste(varz[c(1:23)],collapse="+"),sep="")), select=T, method="GCV.Cp", data=rich_dat_small,family=poisson, drop.unused.levels = F)  
  }
  if (nrow(rich_dat_small)<=80)
  {
    sep_small_num_gam[[spp]]<-lm(round(small)~1, data=rich_dat_small)
  }
}
} 
if (fit_sep_brt==T)
{
sep_small_exist_brt<-sep_small_num_brt<-list()
for (spp in 22:48)
{
  wpred_sub<-subset(wpred_data, genus==unique(wpred_data$genus)[spp])
  wpred_sub$is_small<-wpred_sub$small>0
  rich_dat_small<-subset(wpred_sub, is_small==1)

    sep_small_exist_brt[[spp]]<-gbm.step(data=wpred_sub, gbm.x=c(7,10,13:20,23, 25:36, 53), gbm.y=56,n.folds=10, n.trees=30, family='bernoulli', tree.complexity = 3, learning.rate=0.005, drop.unused.levels=F)
    i=0.005
    while (sep_small_exist_brt[[spp]]$n.trees<1000)
    {
      i=i/5
      sep_small_exist_brt[[spp]]<-gbm.step(data=wpred_sub, gbm.x=c(7,10,13:20,23, 25:36, 53), gbm.y=56,n.folds=10, n.trees=30, family='bernoulli', tree.complexity = 3, learning.rate=i, drop.unused.levels=F)
    }
    if (nrow(rich_dat_small)>80)
    {
     rich_dat_small$ecoprovmaj<-factor(rich_dat_small$ecoprovmaj, levels=levels(data$ecoprovmaj))
    rich_dat_small$log_n_small<-log(rich_dat_small$small)
    sep_small_num_brt[[spp]]<-gbm.step(data=rich_dat_small, gbm.x=c(7,10,13:20,23, 25:36, 53), gbm.y=57,n.folds=10, n.trees=30, family='gaussian', tree.complexity = 3, learning.rate=0.005, drop.unused.levels=F)
    i=0.005
    if (length(sep_small_num_brt)>0)
    {
    while (sep_small_num_brt[[spp]]$n.trees<1000)
    {
      i=i/10
      sep_small_num_brt[[spp]]<-gbm.step(data=rich_dat_small, gbm.x=c(7,10,13:20,23, 25:36, 53), gbm.y=57,n.folds=10, n.trees=30, family='gaussian', tree.complexity = 3, learning.rate=i, drop.unused.levels=F)
    }
    }
  }
}
}
r2_small_both_brt<-r2_small_both_gam<-r2_small_both_sepbrt<-r2_small_both_sepgam<-r2_small_nonsepgam_nonsepbrt<-r2_small_nonsepgam_sepgam<-r2_small_nonsepgam_sepbrt<-r2_small_nonsepbrt_nonsepgam<-r2_small_nonsepbrt_sepgam<-r2_small_nonsepbrt_sepbrt<-r2_small_sepgam_nonsepgam<-r2_small_sepgam_nonsepbrt<-r2_small_sepgam_sepbrt<-r2_small_sepbrt_nonsepgam<-r2_small_sepbrt_nonsepbrt<-r2_small_sepbrt_sepgam<-rep(0,48)
for (spp in 31:48)
{
  wpred_sub<-subset(wpred_data, genus==unique(wpred_data$genus)[spp])
##both nonsep gam
pred_exist<-preds_gam(all_small_exist_mod)
pred_num<-preds_gam(all_small_num_mod)
final_pred<-rescale_constant(pred_num, pred_exist,"small")
r2_small_both_gam[spp]<-r2mse("small", final_pred)

##both nonsep brt
pred_exist<-preds_brt(is_small_brt)
pred_num<-exp(preds_brt(small_brt))
final_pred<-rescale_constant(pred_num, pred_exist, "small")
r2_small_both_brt[spp]<-r2mse("small", final_pred)

##both sep gam
if (!spp%in%c(23,31))
{
pred_exist<-preds_gam(sep_small_exist_gam[[spp]])
pred_num<-(preds_gam(sep_small_num_gam[[spp]]))
final_pred<-rescale_constant(pred_num, pred_exist,"small")
r2_small_both_sepgam[spp]<-r2mse("small", final_pred)
}

##both sep brt
if(length(sep_small_num_brt[[spp]])>0)
   {
  pred_exist<-preds_brt(sep_small_exist_brt[[spp]])
  pred_num<-exp(preds_brt(sep_small_num_brt[[spp]]))
  final_pred<-rescale_constant(pred_num, pred_exist, "small")
  r2_small_both_sepbrt[spp]<-r2mse("small", final_pred)
}

##exist nonsep gam, num nonsep brt
pred_exist<-preds_gam(all_small_exist_mod)
pred_num<-exp(preds_brt(small_brt))
final_pred<-rescale_constant(pred_num, pred_exist,"small")
r2_small_nonsepgam_nonsepbrt[spp]<-r2mse("small", final_pred)

##exist nonsep gam, num sep gam
if (!spp%in%c(23,31))
{
pred_exist<-preds_gam(all_small_exist_mod)
pred_num<-(preds_gam(sep_small_num_gam[[spp]]))
final_pred<-rescale_constant(pred_num, pred_exist,"small")
r2_small_nonsepgam_sepgam[spp]<-r2mse("small", final_pred)
}
if(length(sep_small_num_brt[[spp]])>0)
{
##exist nonsep gam, num sep brt
pred_exist<-preds_gam(all_small_exist_mod)
pred_num<-exp(preds_brt(sep_small_num_brt[[spp]]))
final_pred<-rescale_constant(pred_num, pred_exist,"small")
r2_small_nonsepgam_sepbrt[spp]<-r2mse("small", final_pred)
}

##exist nonsep brt, num nonsep gam
pred_exist<-preds_brt(is_small_brt)
pred_num<-(preds_gam(all_small_num_mod))
final_pred<-rescale_constant(pred_num, pred_exist, "small")
r2_small_nonsepbrt_nonsepgam[spp]<-r2mse("small", final_pred)

##exist nonsep brt, num sep gam
if (!spp%in%c(23,31))
{
pred_exist<-preds_brt(is_small_brt)
pred_num<-(preds_gam(sep_small_num_gam[[spp]]))
final_pred<-rescale_constant(pred_num, pred_exist, "small")
r2_small_nonsepbrt_sepgam[spp]<-r2mse("small", final_pred)
}

if(length(sep_small_num_brt[[spp]])>0)
{
##exist nonsep brt, num sep brt
pred_exist<-preds_brt(is_small_brt)
pred_num<-exp(preds_brt(sep_small_num_brt[[spp]]))
final_pred<-rescale_constant(pred_num, pred_exist, "small")
r2_small_nonsepbrt_sepbrt[spp]<-r2mse("small", final_pred)
}
if (!spp%in%c(23,31))
{
##exist sep gam, num nonsep gam
pred_exist<-preds_gam(sep_small_exist_gam[[spp]])
pred_num<-(preds_gam(all_small_num_mod))
final_pred<-rescale_constant(pred_num, pred_exist, "small")
r2_small_sepgam_nonsepgam[spp]<-r2mse("small", final_pred)

##exist sep gam, num nonsep brt
pred_exist<-preds_gam(sep_small_exist_gam[[spp]])
pred_num<-exp(preds_brt(small_brt))
final_pred<-rescale_constant(pred_num, pred_exist, "small")
r2_small_sepgam_nonsepbrt[spp]<-r2mse("small", final_pred)

if(length(sep_small_num_brt[[spp]])>0)
{
##exist sep gam, num sep brt
pred_exist<-preds_gam(sep_small_exist_gam[[spp]])
pred_num<-exp(preds_brt(sep_small_num_brt[[spp]]))
final_pred<-rescale_constant(pred_num, pred_exist, "small")
r2_small_sepgam_sepbrt[spp]<-r2mse("small", final_pred)
}
}
##exist sep brt, num nonsep gam
pred_exist<-preds_brt(sep_small_exist_brt[[spp]])
pred_num<-(preds_gam(all_small_num_mod))
final_pred<-rescale_constant(pred_num, pred_exist, "small")
r2_small_sepbrt_nonsepgam[spp]<-r2mse("small", final_pred)

##exist sep brt, num nonsep brt
pred_exist<-preds_brt(sep_small_exist_brt[[spp]])
pred_num<-exp(preds_brt(small_brt))
final_pred<-rescale_constant(pred_num, pred_exist, "small")
r2_small_sepbrt_nonsepbrt[spp]<-r2mse("small", final_pred)

##exist sep brt, num sep gam
if (!spp%in%c(23,31))
{
pred_exist<-preds_brt(sep_small_exist_brt[[spp]])
pred_num<-(preds_gam(sep_small_num_gam[[spp]]))
final_pred<-rescale_constant(pred_num, pred_exist, "small")
r2_small_sepbrt_sepgam[spp]<-r2mse("small", final_pred)
}
}

r2_tot<-cbind(r2_small_both_brt,r2_small_both_gam,r2_small_both_sepbrt,r2_small_both_sepgam,r2_small_nonsepgam_nonsepbrt,r2_small_nonsepgam_sepgam,r2_small_nonsepgam_sepbrt,r2_small_nonsepbrt_nonsepgam,r2_small_nonsepbrt_sepgam,r2_small_nonsepbrt_sepbrt,r2_small_sepgam_nonsepgam,r2_small_sepgam_nonsepbrt,r2_small_sepgam_sepbrt,r2_small_sepbrt_nonsepgam,r2_small_sepbrt_nonsepbrt,r2_small_sepbrt_sepgam)
r2_max<-model_sel<-rep(0,48)
for (i in 1:48)
{
  r2_max[i]<-max(r2_tot[i,])
  model_sel[i]<-which.max(r2_tot[i,])
}
model_names<-colnames(r2_tot)[model_sel]
table(model_names)
# r2_comb<-rep(0,48)
# sep_spp<-which(r2_sep_small_gam<r2_small_sep_brt)
# r2_comb[sep_spp]<-r2_small_sep_brt[sep_spp]
# gam_sep_spp<-which(1:48%in%sep_spp==F)
# r2_comb[which(1:48%in%sep_spp==F)]<-r2_sep_small_gam[which(1:48%in%sep_spp==F)]
# gamspp<-which(r2_small_gam>r2_comb)
# r2_comb[gamspp]<-r2_small_gam[gamspp]
# brtspp<-which(r2_small_brt>r2_comb)
# r2_comb[brtspp]<-r2_small_brt[brtspp]
# altspp<-which(r2_comb==0)
# while(min(r2_comb)<=0)
# {
# sep_spp<-which(r2_sep_small_gam<r2_small_sep_brt)
# r2_comb[sep_spp]<-r2_small_sep_brt[sep_spp]
# gam_sep_spp<-which(1:48%in%sep_spp==F)
# r2_comb[which(1:48%in%sep_spp==F)]<-r2_sep_small_gam[which(1:48%in%sep_spp==F)]
# gamspp<-which(r2_small_gam>r2_comb)
# r2_comb[gamspp]<-r2_small_gam[gamspp]
# brtspp<-which(r2_small_brt>r2_comb)
# r2_comb[brtspp]<-r2_small_brt[brtspp]
# 
# min(r2_comb)
# 
# 
# if (any(r2_comb==0))
# {
#   for (spp in which(r2_comb==0))
#   {
#     wpred_sub<-subset(wpred_data, genus==unique(wpred_data$genus)[spp])
#     wpred_sub$is_small<-wpred_sub$small>0
#     rich_dat_small<-subset(wpred_sub, is_small==1)
#       sep_small_exist_brt[[spp]]<-gbm.step(data=wpred_sub, gbm.x=c(7,10,13:20,23, 25:36, 53), gbm.y=56,n.folds=10, n.trees=30, family='bernoulli', tree.complexity = 3, learning.rate=0.005, drop.unused.levels=F)
#       i=0.005
#       while (sep_small_exist_brt[[spp]]$n.trees<1000)
#       {
#         i=i/5
#         sep_small_exist_brt[[spp]]<-gbm.step(data=wpred_sub, gbm.x=c(7,10,13:20,23, 25:36, 53), gbm.y=56,n.folds=10, n.trees=30, family='bernoulli', tree.complexity = 3, learning.rate=i, drop.unused.levels=F)
#       }
#       rich_dat_small$ecoprovmaj<-factor(rich_dat_small$ecoprovmaj, levels=levels(data$ecoprovmaj))
#       rich_dat_small$log_n_small<-log(rich_dat_small$small)
#       sep_small_num_brt[[spp]]<-lm(small~1, data=rich_dat_small)
#       pred_num<-predict.lm(sep_small_num_brt[[spp]], newdata=wpred_sub)
#       pred_exist<-preds_brt(sep_small_exist_brt[[spp]])
#       final_pred<-rescale_constant(pred_num, pred_exist,"small")
#       r2_small_sep_brt[spp]<-r2mse("small", final_pred)
#   }
# }
# }
# 
# model_sel_small<-rep(0,48)
# model_sel_small[gam_sep_spp]<-"gam_sep"
# model_sel_small[sep_spp]<-"brt_sep"
# model_sel_small[gamspp]<-"gam"
# model_sel_small[brtspp]<-"brt"
# 
# for (spp in 1:48)
# {
#   wpred_sub<-subset(wpred_data, genus==unique(wpred_data$genus)[spp])
#   if (spp%in%sep_spp==T)
#   {
#     if (r2_small_sep_brt[spp]>0)
#     {
#       pred_exist<-preds_brt(sep_small_exist_brt[[spp]])
#       if(spp%in%altspp)
#       {
#         pred_num<-predict.lm(sep_small_num_brt[[spp]], newdata=wpred_sub)
#       }
#       if (spp%in%altspp==F)
#       {
#       pred_num<-exp(preds_brt(sep_small_num_brt[[spp]]))
#       }
#       final_pred<-rescale_constant(pred_num, pred_exist, "small")
#     }
#   }
#   if (spp%in%sep_spp==F)
#   {
#     final_pred<-preds_ziP(sep_small_gam[[spp]])
#   }
#   if (spp%in%brtspp)
#   {
#     pred_exist<-preds_brt(is_small_brt)
#     pred_num<-exp(preds_brt(small_brt))
#     final_pred<-rescale_constant(pred_num, pred_exist, "small")
#   }
#   if (spp%in%gamspp)
#   {
#     final_pred<-preds_ziP(all_small_mod)
#   }
#   wpred_data$pred_small[which(wpred_data$genus==unique(wpred_data$genus)[spp])]<-final_pred
# }
# 
# wpred_sub<-wpred_data
# r2mse('small', wpred_data$pred_small)
# plot_pred('small', wpred_data$pred_small)
# plot(wpred_data$pred_small,wpred_data$small, xlab="Predicted SmallTrees", ylab="Observed Small Trees", pch=19, col=alpha("gray30",0.75))
# abline(0,1)
# ratio_each_small_nonsep<-rep(0,48)
# ratio_each_small<-rep(0,48)

# for(i in 1:48)
# {
#   wpred_sub<-subset(wpred_data, genus==unique(wpred_data$genus)[i])
#   ratio_each_small_nonsep[i]<-sum(wpred_sub$small)/sum(exp(preds_brt(small_brt)*preds_brt(is_small_brt)))
#   if (i<=length(sep_small_num_brt))
#   {
#     if (i%in%altspp)
#     {
#       ratio_each_small[i]<-sum(wpred_sub$sall)/sum(predict.lm(sep_small_num_brt[[i]], newdata=wpred_sub)*preds_brt(sep_small_exist_brt[[i]]))
#       next
#     }
#     if (length(sep_small_num_brt[[i]])>0)
#     {ratio_each_small[i]<-sum(wpred_sub$small)/sum(exp(preds_brt(sep_small_num_brt[[i]])*preds_brt(sep_small_exist_brt[[i]])))}
#   }
# }
ratio<-rep(0,48)
for (i in 1:48)
{
  wpred_sub<-subset(wpred_data, genus==unique(wpred_data$genus)[i])
  if(model_sel[i]==1) #both nonsep brt
  {
    final_pred<-rescale_constant(exp(preds_brt(small_brt)), preds_brt(is_small_brt), "small")
    ratio[i]<-sum(wpred_sub$small)/sum(exp(preds_brt(small_brt))*preds_brt(is_small_brt))
  }
  if(model_sel[i]==2) #both nonsep gam
  {
    final_pred<-rescale_constant((preds_gam(all_small_num_mod)), preds_gam(all_small_exist_mod), "small")
    ratio[i]<-sum(wpred_sub$small)/sum(preds_gam(all_small_num_mod)*preds_gam(all_small_exist_mod))
  }
  if(model_sel[i]==3) #both sepbrt
  {
    final_pred<-rescale_constant(exp(preds_brt(sep_small_num_brt[[i]])), preds_brt(sep_small_exist_brt[[i]]), "small")
    ratio[i]<-sum(wpred_sub$small)/sum(exp(preds_brt(sep_small_num_brt[[i]]))*preds_brt(sep_small_exist_brt[[i]]))
  }
  if(model_sel[i]==4) #both sepgam
  {
    final_pred<-rescale_constant((preds_gam(sep_small_num_gam[[i]])), preds_gam(sep_small_exist_gam[[i]]), "small")
    ratio[i]<-sum(wpred_sub$small)/sum((preds_gam(sep_small_num_gam[[i]]))*preds_gam(sep_small_exist_gam[[i]]))
  }
  if(model_sel[i]==5) #nonsepgam, nonsepbrt
  {
    final_pred<-rescale_constant(exp(preds_brt(small_brt)), preds_gam(all_small_exist_mod), "small")
    ratio[i]<-sum(wpred_sub$small)/sum(exp(preds_brt(small_brt))*preds_gam(all_small_exist_mod))
  }
  if(model_sel[i]==6) #nonsepgam, sepgam
  {
    final_pred<-rescale_constant((preds_gam(sep_small_num_gam[[i]])), preds_gam(all_small_exist_mod), "small")
    ratio[i]<-sum(wpred_sub$small)/sum((preds_gam(sep_small_num_gam[[i]]))*preds_gam(all_small_exist_mod))
  }
  if(model_sel[i]==7) #nonsepgam, sepbrt
  {
    final_pred<-rescale_constant(exp(preds_brt(sep_small_num_brt[[i]])), preds_gam(all_small_exist_mod), "small")
    ratio[i]<-sum(wpred_sub$small)/sum(exp(preds_brt(sep_small_num_brt[[i]]))*preds_gam(all_small_exist_mod))
  }
  if(model_sel[i]==8) #nonsepbrt, nonsepgam
  {
    final_pred<-rescale_constant((preds_gam(all_small_num_mod)), preds_brt(is_small_brt), "small")
    ratio[i]<-sum(wpred_sub$small)/sum((preds_gam(all_small_num_mod))*preds_brt(is_small_brt))
  }
  if(model_sel[i]==9) #nonsepbrt, sepgam
  {
    final_pred<-rescale_constant((preds_gam(sep_small_num_gam[[i]])), preds_brt(is_small_brt), "small")
    ratio[i]<-sum(wpred_sub$small)/sum((preds_gam(sep_small_num_gam[[i]]))*preds_brt(is_small_brt))
  }
  if(model_sel[i]==10)
  {
    final_pred<-rescale_constant(exp(preds_brt(sep_small_num_brt[[i]])), preds_brt(is_small_brt), "small")
    ratio[i]<-sum(wpred_sub$small)/sum(exp(preds_brt(sep_small_num_brt[[i]]))*preds_brt(is_small_brt))
  }
  if(model_sel[i]==11)
  {
    final_pred<-rescale_constant((preds_gam(all_small_num_mod)), preds_gam(sep_small_exist_gam[[i]]), "small")
    ratio[i]<-sum(wpred_sub$small)/sum((preds_gam(all_small_num_mod))*preds_gam(sep_small_exist_gam[[i]]))
  }
  if(model_sel[i]==12)
  {
    final_pred<-rescale_constant(exp(preds_brt(small_brt)), preds_gam(sep_small_exist_gam[[i]]), "small")
    ratio[i]<-sum(wpred_sub$small)/sum((exp(preds_brt(small_brt)))*preds_gam(sep_small_exist_gam[[i]]))
  }
  if(model_sel[i]==13)
  {
    final_pred<-rescale_constant(exp(preds_brt(sep_small_num_brt[[i]])), preds_gam(sep_small_exist_gam[[i]]), "small")
    ratio[i]<-sum(wpred_sub$small)/sum((exp(preds_brt(sep_small_num_brt[[i]])))*preds_gam(sep_small_exist_gam[[i]]))
  }
  if(model_sel[i]==14)
  {
    final_pred<-rescale_constant((preds_gam(all_small_num_mod)), preds_brt(sep_small_exist_brt[[i]]), "small")
    ratio[i]<-sum(wpred_sub$small)/sum(((preds_gam(all_small_num_mod)))*preds_brt(sep_small_exist_brt[[i]]))
  }
  if(model_sel[i]==15)
  {
    final_pred<-rescale_constant(exp(preds_brt(small_brt)), preds_brt(sep_small_exist_brt[[i]]), "small")
    ratio[i]<-sum(wpred_sub$small)/sum((exp(preds_brt(small_brt)))*preds_brt(sep_small_exist_brt[[i]]))
  }
  if(model_sel[i]==16)
  {
    final_pred<-rescale_constant((preds_gam(sep_small_num_gam[[i]])), preds_brt(sep_small_exist_brt[[i]]), "small")
    ratio[i]<-sum(wpred_sub$small)/sum(((preds_gam(sep_small_num_gam[[i]])))*preds_brt(sep_small_exist_brt[[i]]))
  }
  
wpred_data$pred_small[which(wpred_data$genus==unique(wpred_data$genus[i]))]<-final_pred
}
plot(wpred_data$small~wpred_data$pred_small)

if (write==T)
{
  saveRDS(sep_small_num_brt, file="sep_small_num_brt3.rds")
  saveRDS(sep_small_exist_brt, file="sep_small_exist_brt3.rds")
  saveRDS(sep_small_num_gam, file="sep_small_num_gam3.rds")
  saveRDS(sep_small_exist_gam, file="sep_small_exist_gam3.rds")
  saveRDS(all_small_num_mod, file="all_small_num_mod3.rds")
  saveRDS(all_small_exist_mod, file="all_small_exist_mod3.rds")
  saveRDS(is_small_brt, file="is_small_brt3.rds")
  saveRDS(small_brt, file="small_brt3.rds")
  write.csv(wpred_data$pred_small, file="predsmall4.csv", row.names=F)
  write.csv(r2_tot, file="r2_tot3.csv", row.names=F)
  write.csv(model_sel, file="model_sel_small3.csv", row.names=F)
  write.csv(ratio, file="ratios_small3.csv", row.names=F)
}

