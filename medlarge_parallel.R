library(mgcv)
library(dismo)
library(iterators)
library(gbm)
source('totaltree_clean.R')
source('treemod_funcs.R')

varz2<-gsub(")$",",bs=\\'cr\\')",varz)
wpred_data_part<-wpred_data[,1:6]
wpred_data<-wpred_data[!duplicated(wpred_data_part),]

#all_small_num_mod<-readRDS('all_small_num_mod4.rds')
#all_small_exist_mod<-readRDS('all_small_exist_mod4.rds')
all_med_num_mod<-readRDS('all_med_num_mod4.rds')
all_med_exist_mod<-readRDS('all_med_exist_mod4.rds')
all_large_num_mod<-readRDS('all_large_num_mod4.rds')
all_large_exist_mod<-readRDS('all_large_exist_mod4.rds')



#small_brt<-readRDS('small_brt4.rds')
 large_brt<-readRDS('large_brt4.rds')
 med_brt<-readRDS('med_brt4.rds')
#is_small_brt<-readRDS('is_small_brt4.rds')
 is_large_brt<-readRDS('is_large_brt4.rds')
 is_med_brt<-readRDS('is_med_brt4.rds')

# sep_small_num_gam<-readRDS('sep_small_num_gam4.rds')
# sep_small_exist_gam<-readRDS('sep_small_exist_gam4.rds')
sep_med_num_gam<-readRDS('sep_med_num_gam4.rds')
sep_med_exist_gam<-readRDS('sep_med_exist_gam4.rds')
sep_large_num_gam<-readRDS('sep_large_num_gam4.rds')
sep_large_exist_gam<-readRDS('sep_large_exist_gam4.rds')

# sep_small_num_brt<-readRDS('sep_small_num_brt4.rds')
 sep_large_num_brt<-readRDS('sep_large_num_brt4.rds')
 sep_med_num_brt<-readRDS('sep_med_num_brt4.rds')

 #sep_small_exist_brt<-readRDS('sep_small_exist_brt4.rds')
 sep_large_exist_brt<-readRDS('sep_large_exist_brt4.rds')
 sep_med_exist_brt<-readRDS('sep_med_exist_brt4.rds')

#ratios_small<-read.csv('ratios_small4.csv')[,1]
ratios_med<-read.csv('ratios_med4.csv')[,1]
ratios_large<-read.csv('ratios_large4.csv')[,1]

#model_sel_small<-read.csv('model_sel_small4.csv')
model_sel_med<-read.csv('model_sel_med4.csv')[,1]
model_sel_large<-read.csv('model_sel_large4.csv')[,1]
 
predsmall<-read.csv('est_small.csv')

fix_cols<-c(4:5,8:21,23,25:26, 29:31, 33:55)
matchcol<-match(colnames(allUS)[fix_cols], colnames(wpred_data))
allUS$inc_1969<-as.numeric(gsub(",","",(as.character(allUS$inc_1969))))
allUS$inc_1979<-as.numeric(gsub(",","",(as.character(allUS$inc_1979))))
allUS$inc_1989<-as.numeric(gsub(",","",(as.character(allUS$inc_1989))))
allUS$Mean.Value<-as.numeric(as.character(allUS$Mean.Value))
allUS$Mean.YEAR<-as.numeric(as.character(allUS$Mean.YEAR))

allUS_fix<-allUS
for (i in which(is.na(matchcol)==F))
{
  allUS_fix[which(allUS_fix[,fix_cols[i]]>max(wpred_data[,matchcol[i]], na.rm=T)),fix_cols[i]]<-max(wpred_data[,matchcol[i]], na.rm=T)
  allUS_fix[which(allUS_fix[,fix_cols[i]]<min(wpred_data[,matchcol[i]], na.rm=T)),fix_cols[i]]<-min(wpred_data[,matchcol[i]], na.rm=T)
}
allUS_fix$inc_1969[which(is.na(allUS_fix$inc_1969)==T)]<-mean(allUS_fix$inc_1969, na.rm=T)
allUS_fix$inc_1979[which(is.na(allUS_fix$inc_1979)==T)]<-mean(allUS_fix$inc_1979, na.rm=T)
allUS_fix$inc_1989[which(is.na(allUS_fix$inc_1989)==T)]<-mean(allUS_fix$inc_1989, na.rm=T)
allUS_fix$Mean.Value[which(is.na(allUS_fix$Mean.Value)==T)]<-mean(allUS_fix$Mean.Value, na.rm=T)
allUS_fix$Mean.YEAR[which(is.na(allUS_fix$Mean.YEAR)==T)]<-mean(as.numeric(as.character((allUS_fix$Mean.YEAR))), na.rm=T)


## outlier models for each predictor, if one is outside range (2 se?) then don't trust entry
genera<-unique(wpred_data$genus)
predicted_trees2<-matrix(0, 1,62)
#+22)
colnames(predicted_trees2)<-c(colnames(allUS), "genus", "pred_small_trees","pred_med_trees", "pred_large_trees","pred_small", "pred_med", "pred_large")
                             
                             #"both_brt", "both_gam", "both_sepbrt", "both_sepgam", "nonsepgam_nonsepbrt", "nonsepgam_sepgam", "nonsepgam_sepbrt", "nonsepbrt_nonsepgam", "nonsepbrt_sepgam", "nonsepbrt_sepbrt", "sepgam_nonsepgam", "sepgam_nonsepbrt", "sepgam_sepbrt", "sepbrt_nonsepgam", "sepbrt_nonsepbrt", "sepbrt_sepgam")
                             #, paste("collin_",varz3[c(1:22)], sep=""))
#maxes<-data.frame(max_small=rep(0, nrow(allUS)),spp_small=rep("0", nrow(allUS)), max_med=rep(0, nrow(allUS)),spp_med=rep("0", nrow(allUS)), max_large=rep(0, nrow(allUS)), spp_large=rep("0", nrow(allUS)))
#maxes$spp_small<-maxes$spp_med<-maxes$spp_large<-as.character(maxes$spp_large)
predicted_trees<-list()
allUS_fix<-allUS_fix[complete.cases(allUS_fix[,fix_cols]),]
allUS_fix<-subset(allUS_fix, ecoprovmaj!=0)
for (j in 1:nrow(allUS_fix))
{
  to_add_small<-cbind(allUS_fix[rep(j, 48),],genera, matrix(0, 48,6))
  colnames(to_add_small)<-colnames(predicted_trees2)
  to_add_small$ecoprovmaj<-as.factor(to_add_small$ecoprovmaj)
  to_add_small$pred_small_trees<-exp(predict.gbm(tot_small_brt, n.trees=tot_small_brt$n.trees,newdata=to_add_small))
   to_add_small$pred_small_trees[which((to_add_small$pred_small_trees>270729))]<-270729
  to_add_small$pred_small_trees[which((to_add_small$pred_small_trees<min(wpred_data$pred_small_trees))==T)]<-min(wpred_data$pred_small_trees)
    to_add_small$pred_med_trees<-exp(predict.gbm(tot_med_brt, n.trees=tot_med_brt$n.trees, type="response", newdata=to_add_small))
   to_add_small$pred_med_trees[which((to_add_small$pred_med_trees>153765))]<-153765
   to_add_small$pred_med_trees[which((to_add_small$pred_med_trees<min(wpred_data$pred_med_trees))==T)]<-min(wpred_data$pred_med_trees)
    to_add_small$pred_large_trees<-exp(predict.gbm(tot_large_brt,n.trees=tot_large_brt$n.trees, type="response", newdata=to_add_small))
   to_add_small$pred_large_trees[which((to_add_small$pred_large_trees>61116)==T)]<-61116
   to_add_small$pred_large_trees[which((to_add_small$pred_large_trees<min(wpred_data$pred_large_trees))==T)]<-min(wpred_data$pred_large_trees)
   colnames(to_add_small)[56]<-"genus"
to_add_small$pred_small<-predsmall[((j-1)*48+1):(j*48),1]
to_add_small2<-to_add_small
to_add_small2$pred_small[which(to_add_small2$pred_small>max(wpred_data$pred_small))]<-max(wpred_data$pred_small)
to_add_small2$pred_small[which(to_add_small2$pred_small<min(wpred_data$pred_small))]<-min(wpred_data$pred_small)
for (i in 1:48)
{
     if (model_sel_med[i]==1) 
     {
     to_add_small$pred_med[i]<-exp(preds_brt_new(med_brt, newdata=to_add_small2[i,]))*preds_brt_new(is_med_brt, newdata=to_add_small2[i,])*ratios_med[i]
     }
     # if (model_sel_med[i]==2)
     # {
     #  to_add_small$pred_med[i]<-preds_gam_new(all_med_num_mod, newdata=to_add_small2[i,])*preds_gam_new(all_med_exist_mod, newdata=to_add_small2[i,])*ratios_med[i]
     # }

        rich_dat_med<-subset(wpred_data, genus==unique(wpred_data$genus)[i])
        rich_dat_med<-subset(rich_dat_med, med>0)
        rich_dat_large<-subset(wpred_data, genus==unique(wpred_data$genus)[i])
        rich_dat_large<-subset(rich_dat_large, large>0)
         sepdat<-to_add_small2[i,]
        

         for (k in which(is.na(matchcol)==F))
         {
           sepdat[which(sepdat[,fix_cols[k]]>max(rich_dat_med[,matchcol[k]], na.rm=T)),fix_cols[k]]<-max(rich_dat_med[,matchcol[k]], na.rm=T)
           sepdat[which(sepdat[,fix_cols[k]]<min(rich_dat_med[,matchcol[k]], na.rm=T)),fix_cols[k]]<-min(rich_dat_med[,matchcol[k]], na.rm=T)
         }
        sepdat$pred_small[which(sepdat$pred_small>max(rich_dat_med$pred_small))]<-max(rich_dat_med$pred_small)
         sepdat$pred_small[which(sepdat$pred_small<min(rich_dat_med$pred_small))]<-min(rich_dat_med$pred_small)
      
         # if (model_sel_med[i]==3)
         # {
         #   to_add_small$pred_med[i]<-exp(preds_brt_new(sep_med_num_brt[[i]], newdata=sepdat))*preds_brt_new(sep_med_exist_brt[[i]], newdata=sepdat)*ratios_med[i]
         # }
         # if (model_sel_med[i]==7)
         # {
         #   to_add_small$pred_med[i]<-exp(preds_brt_new(sep_med_num_brt[[i]], newdata=sepdat))*preds_gam_new(all_med_exist_mod, newdata=to_add_small2[i,])*ratios_med[i]
         # }
         if (model_sel_med[i]==10)
         {
           to_add_small$pred_med[i]<-exp(preds_brt_new(sep_med_num_brt[[i]], newdata=sepdat))*preds_brt_new(is_med_brt, newdata=to_add_small2[i,])*ratios_med[i]
         }
         # if (model_sel_med[i]==13)
         # {
         #   to_add_small$pred_med[i]<-exp(preds_brt_new(sep_med_num_brt[[i]], newdata=sepdat))*preds_gam_new(sep_med_exist_gam[[i]], newdata=sepdat)*ratios_med[i]
         # }  
         # if (model_sel_med[i]==14)
         # {
         #   to_add_small$pred_med[i]<-preds_gam_new(all_med_num_mod, newdata=to_add_small2[i,])*preds_brt_new(sep_med_exist_brt[[i]], newdata=sepdat)*ratios_med[i]
         # }  
         if (model_sel_med[i]==15)
         {
           to_add_small$pred_med[i]<-exp(preds_brt_new(med_brt, newdata=to_add_small2[i,]))*preds_brt_new(sep_med_exist_brt[[i]], newdata=sepdat)*ratios_med[i]
         } 
         # if (model_sel_med[i]==11)
         # {
         #   to_add_small$pred_med[i]<-preds_gam_new(all_med_num_mod, newdata=to_add_small2[i,])*preds_gam_new(sep_med_exist_gam[[i]], newdata=sepdat)*ratios_med[i]
         # }
         if (model_sel_med[i]==12)
         {
           to_add_small$pred_med[i]<-exp(preds_brt_new(med_brt, newdata=to_add_small2[i,]))*preds_gam_new(sep_med_exist_gam[[i]], newdata=sepdat)*ratios_med[i]
         }
         if (model_sel_med[i]==4)
         {
           to_add_small$pred_med[i]<-preds_gam_new(sep_med_num_gam[[i]], newdata=sepdat)*preds_gam_new(sep_med_exist_gam[[i]], newdata=sepdat)*ratios_med[i]
         }
         if (model_sel_med[i]==6)
         {
           to_add_small$pred_med[i]<-preds_gam_new(sep_med_num_gam[[i]], newdata=sepdat)*preds_gam_new(all_med_exist_mod, newdata=to_add_small2[i,])*ratios_med[i]
         }
         if (model_sel_med[i]==9)
         {
           to_add_small$pred_med[i]<-preds_gam_new(sep_med_num_gam[[i]], newdata=sepdat)*preds_brt_new(is_med_brt, newdata=to_add_small2[i,])*ratios_med[i]
         }
         if (model_sel_med[i]==5)
         {
           to_add_small$pred_med[i]<-exp(preds_brt_new(med_brt, newdata=to_add_small2[i,]))*preds_gam_new(all_med_exist_mod, newdata=to_add_small2[i,])*ratios_med[i]
         }
         if (model_sel_med[i]==16)
         {
           to_add_small$pred_med[i]<-preds_gam_new(sep_med_num_gam[[i]], newdata=sepdat)*preds_brt_new(sep_med_exist_brt[[i]], newdata=sepdat)*ratios_med[i]
         }
         # if (model_sel_med[i]==8)
         # {
         #   to_add_small$pred_med[i]<-preds_gam_new(all_med_num_mod, newdata=to_add_small2[i,])*preds_brt_new(is_med_brt, newdata=to_add_small2[i,])*ratios_med
         # }
}
to_add_small3<-to_add_small2
to_add_small3$pred_med<-to_add_small$pred_med
to_add_small3$pred_med[which(to_add_small3$pred_med>max(wpred_data$pred_med))]<-max(wpred_data$pred_med)
to_add_small3$pred_med[which(to_add_small3$pred_med<min(wpred_data$pred_med))]<-min(wpred_data$pred_med)

for (i in 1:48)
{
  sepdat_l<-to_add_small3[i,]
  
  for (k in which(is.na(matchcol)==F))
  {
  sepdat_l[which(sepdat[,fix_cols[k]]>max(rich_dat_large[,matchcol[k]], na.rm=T)),fix_cols[k]]<-max(rich_dat_large[,matchcol[k]], na.rm=T)
  sepdat_l[which(sepdat[,fix_cols[k]]<min(rich_dat_large[,matchcol[k]], na.rm=T)),fix_cols[k]]<-min(rich_dat_large[,matchcol[k]], na.rm=T)
  
}

sepdat_l$pred_med[which(sepdat_l$pred_med>max(rich_dat_large$pred_med))]<-max(rich_dat_large$pred_med)
sepdat_l$pred_med[which(sepdat_l$pred_med<min(rich_dat_large$pred_med))]<-min(rich_dat_large$pred_med)
         if(model_sel_large[i]==1)
         {
           to_add_small$pred_large[i]<-exp(preds_brt_new(large_brt, newdata=to_add_small3[i,]))*preds_brt_new(is_large_brt, newdata=to_add_small3[i,])*ratios_large[i]
         }
         # if (model_sel_large[i]==2)
         # {
         #   to_add_small$pred_large[i]<-preds_gam_new(all_large_num_mod, newdata=to_add_small3[i,])*preds_gam_new(all_large_exist_mod, newdata=to_add_small3[i,])*ratios_large[i]
         # }
         if (model_sel_large[i]==3)
         {
           to_add_small$pred_large[i]<-exp(preds_brt_new(sep_large_num_brt[[i]], newdata=sepdat))*preds_brt_new(sep_large_exist_brt[[i]], newdata=sepdat_l)*ratios_large[i]
         }
        
         # if (model_sel_large[i]==7)
         # {
         #   to_add_small$pred_large[i]<-exp(preds_brt_new(sep_large_num_brt[[i]], newdata=sepdat_l))*preds_gam_new(all_large_exist_mod, newdata=to_add_small3[i,])*ratios_large[i]
         # }

         if (model_sel_large[i]==10)
         {
           to_add_small$pred_large[i]<-exp(preds_brt_new(sep_large_num_brt[[i]], newdata=sepdat_l))*preds_brt_new(is_large_brt, newdata=to_add_small3[i,])*ratios_large[i]
         }

         if (model_sel_large[i]==13)
         {
           to_add_small$pred_large[i]<-exp(preds_brt_new(sep_large_num_brt[[i]], newdata=sepdat))*preds_gam_new(sep_large_exist_gam[[i]], newdata=sepdat_l)*ratios_large[i]
         } 

         # if (model_sel_large[i]==14)
         # {
         #   to_add_small$pred_large[i]<-preds_gam_new(all_large_num_mod, newdata=to_add_small3[i,])*preds_brt_new(sep_large_exist_brt[[i]], newdata=sepdat_l)*ratios_large[i]
         # } 

         if (model_sel_large[i]==15)
         {
           to_add_small$pred_large[i]<-exp(preds_brt_new(large_brt, newdata=to_add_small3[i,]))*preds_brt_new(sep_large_exist_brt[[i]], newdata=sepdat_l)*ratios_large[i]
         } 

         if (model_sel_large[i]==11)
         {
           to_add_small$pred_large[i]<-preds_gam_new(all_large_num_mod, newdata=to_add_small3[i,])*preds_gam_new(sep_large_exist_gam[[i]], newdata=sepdat_l)*ratios_large[i]
         }

         if (model_sel_large[i]==12)
         {
           to_add_small$pred_large[i]<-exp(preds_brt_new(large_brt, newdata=to_add_small3[i,]))*preds_gam_new(sep_large_exist_gam[[i]], newdata=sepdat_l)*ratios_large[i]
         }

           if (model_sel_large[i]==4)
           {
             to_add_small$pred_large[i]<-preds_gam_new(sep_large_num_gam[[i]], newdata=sepdat)*preds_gam_new(sep_large_exist_gam[[i]], newdata=sepdat_l)*ratios_large[i]
           }

           # if (model_sel_large[i]==6)
           # {
           #   to_add_small$pred_large[i]<-preds_gam_new(sep_large_num_gam[[i]], newdata=sepdat_l)*preds_gam_new(all_large_exist_mod, newdata=to_add_small3[i,])*ratios_large[i]
           # }

           if (model_sel_large[i]==9)
           {
             to_add_small$pred_large[i]<-preds_gam_new(sep_large_num_gam[[i]], newdata=sepdat_l)*preds_brt_new(is_large_brt, newdata=to_add_small3[i,])*ratios_large[i]
           }

         # if (model_sel_large[i]==16)
         # {
         #   to_add_small$pred_large[i]<-preds_gam_new(sep_large_num_gam[[i]], newdata=sepdat_l)*preds_brt_new(sep_large_exist_brt[[i]], newdata=sepdat_l)*ratios_large[i]
         # }

         if (model_sel_large[i]==5)
         {
           to_add_small$pred_large[i]<-exp(preds_brt_new(large_brt, newdata=to_add_small3[i,]))*preds_gam_new(all_large_exist_mod, newdata=to_add_small3[i,])*ratios_large[i]
         }

         # if (model_sel_large[i]==8)
         # {
         #   to_add_small$pred_large[i]<-preds_gam_new(all_large_num_mod, newdata=to_add_smal3l[i,])*preds_brt_new(is_large_brt, newdata=to_add_small[i,])*ratios_large
         # }
}
  predicted_trees[[j]]<-to_add_small
  if (j%/%1000==0)
  {
    predicted_trees_all<-do.call(rbind, predicted_trees)
    write.csv(predicted_trees_all, file="predicted_alltrees_allUS.csv", row.names=F)
  }
}
predicted_trees_all<-do.call(rbind, predicted_trees)
write.csv(predicted_trees_all, file="predicted_alltrees_allUS.csv", row.names=F)
