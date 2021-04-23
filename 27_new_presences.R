### New pest detections by grid cell (10-yr lag) ###

## This code is licensed under the GPL v3.0 ##
## written by Emma J. Hudgins (2020) ##
## PhD Candidate, McGill University ##
## Montreal, QC, Canada ##

## For questions, e-mail emma.hudgins@mail.mcgill.ca ##

library(stringr)
library(dplyr)
library(here)
setwd(paste0(here(), '/output'))
data2<-read.csv('../data/datanorm.csv', stringsAsFactors = FALSE)# pest data from Hudgins et al. 2017
rr<-which(data2$YEAR>=10) # subset to pests present for at least 10 yrs as of 2008
data2<-data2[-c(25,69),] # remove pests with no range in US forests
data2<-data2[-4,]# remove pests with no range in US forests


cumulative_presences<-readRDS('presences_time_noforce3.rds') # pest spread predictions following Hudgins et al. (2020)
cumulative_presences[[1]]<-readRDS('presences_time_eab.rds')[[1]]# updated to include observed eab distributions to 2020
cumulative_presences[[49]]<-readRDS('presences_time_hwa.rds')[[49]]#updated to include HWA historical spread
cumulative_presences[[51]]<-readRDS('presences_time_gm.rds')[[51]]#updated to include GM historical spread
cumulative_presences[[54]]<-readRDS('presences_time_bbd.rds')[[54]]#updated to include BBD historical spread

for (i in c(1:2,4:64))# remove ALB
{
  cumulative_presences[[i]]<-cumulative_presences[[i]][,which(colSums(cumulative_presences[[i]])!=0)]
}
cumulative_presences[rr[c(-20,-61)]]<-cumulative_presences #  remove 2 spp without forested range

# now add species present <10yrs from separate file
new<-which(1:75%in%rr==F)
cumulative_presences2<-readRDS('presences_time_rr2.rds')
for (i in 1:9)
{
  cumulative_presences[[new[i]]]<-cumulative_presences2[[i]][,which(colSums(cumulative_presences2[[i]])!=0)]
}
cumulative_presences<-cumulative_presences[-c(4,25,69)] # remove ALB and 2 spp without forested range
for (i in 1:72)
{
  for (j in 2:ncol(cumulative_presences[[i]]))
  {
    k<-which(!cumulative_presences[[i]][,j-1]%in%cumulative_presences[[i]][,j])
    if (length(k)>0)
    {
      l<-which(cumulative_presences[[i]][,j]==0)
      cumulative_presences[[i]][l[1:length(k)],j]<-cumulative_presences[[i]][k,j-1]
    }
  }
}
for (i in 1:72)
{
  for (j in 1:ncol(cumulative_presences[[i]]))
  {
    cumulative_presences[[i]][,j]<-cumulative_presences[[i]][order(cumulative_presences[[i]][,j], decreasing=T),j]
  }
}


lag=10 # initial lag (fixed at 10 years)

#Calculate all invaded sites at each timestep for each species
new_presences<-list()
for (i in c(1:72))
{
  new_presences[[i]]<-matrix(0,3372,max(which(colSums(cumulative_presences[[i]])!=0))-(lag/5))
  
  time2=1
  for (time in ((lag/5)+1):max(which(colSums(cumulative_presences[[i]])!=0)))
  {
    new_presences[[i]][,time2]<-c(cumulative_presences[[i]][which((cumulative_presences[[i]][1:length(which(cumulative_presences[[i]][,time-(lag/5)]!=0)),time-(lag/5)]%in%cumulative_presences[[i]][1:length(which(cumulative_presences[[i]][,time-(lag/5)-1]!=0)),time-(lag/5)-1])==F),time2], rep(0,3372-length(which((cumulative_presences[[i]][1:length(which(cumulative_presences[[i]][,time-(lag/5)]!=0)),time-(lag/5)]%in%cumulative_presences[[i]][1:length(which(cumulative_presences[[i]][,time-(lag/5)-1]!=0)),time-(lag/5)-1])==F))))
    time2=time2+1
  }
  while (ncol(new_presences[[i]])<10)
  {
    new_presences[[i]]<-cbind(rep(0, 3372), new_presences[[i]])
  }
}
saveRDS(new_presences, file="../output/new_presences.RDS")
