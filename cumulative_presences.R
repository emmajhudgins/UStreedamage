### New pest detections by grid cell (10-yr lag) ###

## This code is licensed under the GPL v3.0 ##
## written by Emma J. Hudgins (2020) ##
## PhD Candidate, McGill University ##
## Montreal, QC, Canada ##

## For questions, e-mail emma.hudgins@mail.mcgill.ca ##


library(stringr)
library(dplyr)

data2<-read.csv('datanorm.csv', stringsAsFactors = FALSE)# pest data from Hudgins et al. 2017
rr<-which(data2$YEAR>=10) # subset to pests present for at least 10 yrs as of 2008
data2<-data2[-c(25,69),] # remove pests with no range in US forests
data2<-data2[-4,]# remove pests with no range in US forests


future_presences<-readRDS('presences_time_noforce3.rds') # pest spread predictions following Hudgins et al. (2020)
future_presences[[1]]<-readRDS('presences_time_eab.rds')[[1]]# pest spread predictions following Hudgins et al. (2020)

for (i in c(1:2,4:64))# remove ALB
{
  future_presences[[i]]<-future_presences[[i]][,which(colSums(future_presences[[i]])!=0)]
}
future_presences[rr[c(-20,-61)]]<-future_presences #  remove 2 spp without forested range

# now add species present <10yrs from separate file
new<-which(1:75%in%rr==F)
future_presences2<-readRDS('~/Desktop/OneDrive - McGill University/Grad/scripts/presences_time_rr3.rds')
for (i in 1:9)
{
  future_presences[[new[i]]]<-future_presences2[[i]][,which(colSums(future_presences2[[i]])!=0)]
}
future_presences<-future_presences[-c(4,25,69)] # remove ALB and 2 spp without forested range
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


lag=10 # initial lag (fixed at 10 years)

#Calculate all invaded sites at each timestep for each species
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
saveRDS(cumulative_presences, file="cumulative_presences.RDS")
