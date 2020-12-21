library(stringr)
library(dplyr)
estimate_mort=F # turn on to calculate mortality

data2<-read.csv('datanorm.csv', stringsAsFactors = FALSE)# pest data
rr<-which(data2$YEAR>=10)
data2<-data2[-c(25,69),]
data2<-data2[-4,]

host_gen_small<-readRDS("host_gen_small_clean.rds") # pest severity data for small, medium, large trees
host_gen_med<-readRDS("host_gen_med_clean.rds")
host_gen_large<-readRDS("host_gen_large_clean.rds")

def<-which(data2$Guild=="Defoliators")
bore<-which(data2$Guild=="Borers")
suck<-which(data2$Guild=="Suckers")

grid_small<-readRDS('grid_small.rds') # predicted small, medium, large trees  by grid cell
grid_med<-readRDS('grid_med.rds')
grid_large<-readRDS('grid_large.rds')

genus<-readRDS('genera.RDS') # host genera

#can change focal pest and tree
pests=str_to_sentence(data2$LATIN_NAME[c(def,suck,bore)])
trees=str_to_sentence(genus)
tree_nm="Fraxinus"
pest_nm="Agrilus planipennis"
tree=which(trees==tree_nm)
pest=which(pests==pest_nm)
pest2=which(data2$LATIN_NAME==pest_nm)
if (estimate_mort==T)
{
  future_presences<-readRDS('presences_time_noforce3.rds')
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
  if (i %in% def)
  {
    lag2=50
  }
  if (i %in% suck)
  {
    lag2=100
  }
  if (i %in% bore)
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
bestguess<-apply(sweep(his_t,2:3, host_gen_small2, "*"),1:3,sum)+apply(sweep(him_t,2:3, host_gen_med2, "*"),1:3,sum)+apply(sweep(hil_t,2:3, host_gen_large2, "*"),1:3,sum)
}

if (estimate_mort==F)
{
  bestguess<-readRDS('mostlikely_genus.RDS')
}

sum(bestguess[which(mort_med==max(mort_med)),pest,tree])/sum(bestguess)
