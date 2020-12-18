setwd('~/Desktop/OneDrive - McGill University/Grad/scripts/') # update to your working directory
estimate_mort=F # turn on to calculate mortality
calculate_prop=F # turn on to calculate proportional mortality
discounter<-rep(0,46)
for (i in seq(0,45, by=1))
{
  discounter[i+1]<-0.98^i # 2% annual discount rate
}
data2<-read.csv(data2, file='data2_clean.csv')
host_gen_small<-readRDS("host_gen_small_clean.rds")
host_gen_med<-readRDS("host_gen_med_clean.rds")
host_gen_large<-readRDS("host_gen_large_clean.rds")
pred_trees_huge<-readRDS('pred_trees_big.rds')
pred_trees_huge$pred_large[which(is.na(pred_trees_huge$pred_large)==T)]<-0
pred_trees_huge$pred_med[which(is.na(pred_trees_huge$pred_med)==T)]<-0
pred_trees_huge$pred_small[which(is.na(pred_trees_huge$pred_small)==T)]<-0


grid_small<-readRDS('grid_small.rds')
grid_med<-readRDS('grid_med.rds')
grid_large<-readRDS('grid_large.rds')
ave_discount<-rep(0,10)
for (i in 1:10)
{
  ave_discount[i]<-mean(discounter[((i-1)*5):(i*5)])
}
mort<-read.csv('potter_mort_all3.csv')
small_st_treat<-450*(117.244/103.157)
med_st_treat<-600*(117.244/103.157)
large_st_treat<-1200*(117.244/103.157)

if (estimate_mort==T)
{
  future_presences<-readRDS('~/Desktop/OneDrive - McGill University/Grad/scripts/presences_time_noforce3.rds')
  for (i in c(1:2,4:64))
  {
    future_presences[[i]]<-future_presences[[i]][,which(colSums(future_presences[[i]])!=0)]
  }
  future_presences[rr[c(-20,-61)]]<-future_presences
  new<-which(1:75%in%rr==F)
  future_presences2<-readRDS('~/Desktop/OneDrive - McGill University/Grad/scripts/presences_time_rr3.rds')
  for (i in 1:9)
  {
    future_presences[[new[i]]]<-future_presences2[[i]][,which(colSums(future_presences2[[i]])!=0)]
  }
  future_presences<-future_presences[-c(4,25,69)]
  for (i in 1:72)
  {
    for (j in 2:ncol(future_presences[[i]]))
    {
      k<-which(!future_presences[[i]][,j-1]%in%future_presences[[i]][,j])
      if (length(k)>0)
      {
        l<-which(future_presences[[i]][,j]==0)
        future_presences[[i]][l[1:length(k)],j]<-future_presences[[i]][k,j-1]
      }
    }
  }
  for (i in 1:72)
  {
    for (j in 1:ncol(future_presences[[i]]))
    {
      future_presences[[i]][,j]<-future_presences[[i]][order(future_presences[[i]][,j], decreasing=T),j]
    }
  }
lag=10 # initial lag

  cumulative_presences<-list()
  for (i in c(1:72))
  {
    cumulative_presences[[i]]<-matrix(0,3372,max(which(colSums(future_presences[[i]])!=0))-(lag/5))

    time2=1
    for (time in ((lag/5)+1):max(which(colSums(future_presences[[i]])!=0)))
    {
      cumulative_presences[[i]][,time2]<-c(future_presences[[i]][which((future_presences[[i]][1:length(which(future_presences[[i]][,time-(lag/5)]!=0)),time-(lag/5)]%in%future_presences[[i]][1:length(which(future_presences[[i]][,time-(lag/5)-1]!=0)),time-(lag/5)-1])==F),time2], rep(0,3372-length(which((future_presences[[i]][1:length(which(future_presences[[i]][,time-(lag/5)]!=0)),time-(lag/5)]%in%future_presences[[i]][1:length(which(future_presences[[i]][,time-(lag/5)-1]!=0)),time-(lag/5)-1])==F))))
      time2=time2+1
    }
  }
  
 
host_infestation_small=host_infestation_med=host_infestation_large=array(0,dim=c(3372,72,48,10))

for (i in 1:72)
{
  host_gen_expanded_small<-matrix(unlist(rep(host_gen_small[i,],3372)), nrow=3372,ncol=48, byrow=T)
  host_gen_expanded_med<-matrix(unlist(rep(host_gen_med[i,],3372)), nrow=3372,ncol=48, byrow=T)
  host_gen_expanded_large<-matrix(unlist(rep(host_gen_large[i,],3372)), nrow=3372,ncol=48, byrow=T)
  if (i %in% which(data2$Guild=="Defoliators"))
  {
    lag2=50
  }
  if (i %in% which(data2$Guild=="Suckers"))
  {
    lag2=100
  }
  if (i %in% which(data2$Guild=="Borers"))
  {
    lag2=10
  }
  for (t in 1:10)
  {
    q=0
    host_infestation_small[cumulative_presences[[i]][,(ncol(cumulative_presences[[i]])-10+t-q)],i,,t]<-host_infestation_small[cumulative_presences[[i]][,(ncol(cumulative_presences[[i]])-10+t-q)],i,,t]+host_gen_expanded_small[cumulative_presences[[i]][,(ncol(cumulative_presences[[i]])-10+t-q)],]*(1/((lag2+5)/5))
    host_infestation_med[cumulative_presences[[i]][,(ncol(cumulative_presences[[i]])-10+t-q)],i,,t]<- host_infestation_med[cumulative_presences[[i]][,(ncol(cumulative_presences[[i]])-10+t-q)],i,,t]+host_gen_expanded_med[cumulative_presences[[i]][,(ncol(cumulative_presences[[i]])-10+t-q)],]*(1/((lag2+5)/5))
    host_infestation_large[cumulative_presences[[i]][,(ncol(cumulative_presences[[i]])-10+t-q)],i,,t]<-host_infestation_large[cumulative_presences[[i]][,(ncol(cumulative_presences[[i]])-10+t-q)],i,,t]+host_gen_expanded_large[cumulative_presences[[i]][,(ncol(cumulative_presences[[i]])-10+t-q)],]*(1/((lag2+5)/5))
    if (lag2>=5)
    {
      for (q in 1:(lag2/5))
      {
        host_infestation_small[cumulative_presences[[i]][,(ncol(cumulative_presences[[i]])-10+t-q)],i,,t]<-host_infestation_small[cumulative_presences[[i]][,(ncol(cumulative_presences[[i]])-10+t-q)],i,,t]+host_gen_expanded_small[cumulative_presences[[i]][,(ncol(cumulative_presences[[i]])-10+t-q)],]*(1/((lag2+5)/5))
        host_infestation_med[cumulative_presences[[i]][,(ncol(cumulative_presences[[i]])-10+t-q)],i,,t]<- host_infestation_med[cumulative_presences[[i]][,(ncol(cumulative_presences[[i]])-10+t-q)],i,,t]+host_gen_expanded_med[cumulative_presences[[i]][,(ncol(cumulative_presences[[i]])-10+t-q)],]*(1/((lag2+5)/5))
        host_infestation_large[cumulative_presences[[i]][,(ncol(cumulative_presences[[i]])-10+t-q)],i,,t]<-host_infestation_large[cumulative_presences[[i]][,(ncol(cumulative_presences[[i]])-10+t-q)],i,,t]+host_gen_expanded_large[cumulative_presences[[i]][,(ncol(cumulative_presences[[i]])-10+t-q)],]*(1/((lag2+5)/5))
      }
    }
  }
}
his_t<-apply(host_infestation_small[,,,4:10],1:3,FUN=sum)
him_t<-apply(host_infestation_med[,,,4:10],1:3,FUN=sum)
hil_t<-apply(host_infestation_large[,,,4:10],1:3,FUN=sum)
mort2<-colMeans(mort)[2:8]
host_gen_small2<-matrix(as.vector(approx(x=cbind(c(0,seq(2,8))),y=cbind(as.vector(c(0,mort2))), xout=as.vector(unlist(c(host_gen_small))))$y), nrow=72, ncol=48)
host_gen_med2<-matrix(as.vector(approx(x=cbind(c(0,seq(2,8))),y=cbind(as.vector(c(0,mort2))), xout=as.vector(unlist(c(host_gen_med))))$y), nrow=72, ncol=48)
host_gen_large2<-matrix(as.vector(approx(x=cbind(c(0,seq(2,8))),y=cbind(as.vector(c(0,mort2))), xout=as.vector(unlist(c(host_gen_large))))$y), nrow=72, ncol=48)
pests=str_to_sentence(data2$LATIN_NAME[c(def,suck,bore)])
trees=str_to_sentence(genus)
tree_nm="Fraxinus"
pest_nm="Agrilus planipennis"
tree=which(trees==tree_nm)
pest=which(pests==pest_nm)
mort_small<-his_t[,pest2,tree]
mort_med<-him_t[,pest2,tree]
mort_large<-hil_t[,pest2,tree]
saveRDS(mort_small, "small_frac_mort.RDS")
saveRDS(mort_med, "med_frac_mort.RDS")
saveRDS(mort_large, "large_frac_mort.RDS")
}

if (estimate_mort==F)
{
  mort_small<-readRDS('small_frac_mort.RDS')
  mort_med<-readRDS('med_frac_mort.RDS')
  mort_large<-readRDS('large_frac_mort.RDS')
}
bestguess<-readRDS('bestguess_genus.RDS')
sum(bestguess[which(mort_med==max(mort_med)),pest,tree])
sum(bestguess[,45,20])
all_i<-pred_trees_huge%>%group_by(genus,i,STFIPS.x, PLACEFIPS, FIPS)%>%summarise_at(c('pred_small', 'pred_med', 'pred_large'), sum)
all_i<-subset(all_i, genus==tolower(tree_nm))
all_i$ID<-match(all_i$i,grid$ID)
all_i$mort_city<-mort_small[all_i$ID]*all_i$pred_small+mort_med[all_i$ID]*all_i$pred_med+mort_large[all_i$ID]*all_i$pred_large

sum(all_i$mort_city, na.rm=T)

all_i$prop_ash<-signif(all_i$mort_city/(all_i$pred_small+all_i$pred_med+all_i$pred_large),2)

length(which(all_i$ID%in%which(mort_med==max(mort_med)&(bestguess[,45,20]!=0))))
hist(all_i$prop_ash, xlim=c(0.25,0.8), ylim=c(0,5000), breaks=seq(0,1,length.out=100))
all_small<-aggregate(all_i$pred_small[which(all_i$ID%in%which(mort_med==max(mort_med)&(bestguess[,45,20]!=0)))],by=list(all_i$PLACEFIPS[which(all_i$ID%in%which(mort_med==max(mort_med)&(bestguess[,45,20]!=0)))]), sum)
all_med<-aggregate(all_i$pred_med[which(all_i$ID%in%which(mort_med==max(mort_med)&(bestguess[,45,20]!=0)))],by=list(all_i$PLACEFIPS[which(all_i$ID%in%which(mort_med==max(mort_med)&(bestguess[,45,20]!=0)))]), sum)
all_large<-aggregate(all_i$pred_large[which(all_i$ID%in%which(mort_med==max(mort_med)&(bestguess[,45,20]!=0)))],by=list(all_i$PLACEFIPS[which(all_i$ID%in%which(mort_med==max(mort_med)&(bestguess[,45,20]!=0)))]), sum)
all_trees<-pred_trees_huge%>%group_by(i,STFIPS.x, PLACEFIPS, FIPS)%>%summarise_at(c('pred_small', 'pred_med', 'pred_large'), sum)
all_trees$ID<-match(all_trees$i,grid$ID)

prop_ash<-sum(all_i$mort_city[which(all_i$ID%in%which(mort_med==max(mort_med)))])/sum((all_trees$pred_small[match(all_trees$PLACEFIPS[which(all_trees$ID%in%which(mort_med==max(mort_med)))],all_small$Group.1)]+all_trees$pred_med[match(all_trees$PLACEFIPS[which(all_trees$ID%in%which(mort_med==max(mort_med)))],all_small$Group.1)]+all_trees$pred_large[match(all_trees$PLACEFIPS[which(all_trees$ID%in%which(mort_med==max(mort_med)))],all_small$Group.1)]))

3768058/sum((all_trees$pred_small[match(all_trees$PLACEFIPS[which(all_trees$ID%in%which(mort_med==max(mort_med)))],all_small$Group.1)]+all_trees$pred_med[match(all_trees$PLACEFIPS[which(all_trees$ID%in%which(mort_med==max(mort_med)))],all_small$Group.1)]+all_trees$pred_large[match(all_trees$PLACEFIPS[which(all_trees$ID%in%which(mort_med==max(mort_med)))],all_small$Group.1)]))
3768058/sum(all_i$pred_small+all_i$pred_med+all_i$pred_large)


m<-SpatialPointsDataFrame(coords=cbind(grid$X_coord[which((bestguess[,45,20]!=0))], grid$Y_coord[which((bestguess[,45,20]!=0))]), data=setNames(as.data.frame((bestguess[which((bestguess[,45,20]!=0)),45,20])),"cost"))
# # Create two panels side by side
par(mar=c(0.5,0.5,0.5,0.5))
par(oma=c(0,0,0,4))
layout(t(1:2), widths=c(6,1))
bins<-seq(0,6,length.out=50)
bins<-10^bins
m$Col<-pal(50)[findInterval(m$cost, bins)+1]
plot(transform_usa, lwd=0.2, main=NULL)
pal<-viridis

points(cbind(grid$X_coord[which(mort_med==max(mort_med)&(bestguess[,45,20]!=0))], grid$Y_coord[which(mort_med==max(mort_med)&(bestguess[,45,20]!=0))]), pch=15, cex=0.5, col=m$Col)
plot(transform_usa,add=TRUE, lwd=2)
image(1,1:50,t(1:50), col=pal(50), axes=FALSE)
par(las=1)
axis(4,labels=c("0",expression(10^{2}), expression(10^{4}),expression(10^{6})),at=c(seq(0,50,length.out=4)), cex.axis=1)
par(las=0)
mtext(side=4, "Street Tree Mortality (2020-2050)", line=3)

