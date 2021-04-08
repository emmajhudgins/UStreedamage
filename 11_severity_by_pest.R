### Pest severity by tree size class and genus ###

## This code is licensed under the GPL v3.0 ##
## written by Emma J. Hudgins (2020) ##
## PhD Candidate, McGill University ##
## Montreal, QC, Canada ##

## For questions, e-mail emma.hudgins@mail.mcgill.ca ##

rm(list=ls()) 
library('data.table')
library(plyr)
library(here)
setwd(here())
write_out=F
###read in data
data2<-read.csv('./data/datanorm.csv', stringsAsFactors = FALSE) # pest data
FIA<-read.csv('./data/FIAcodes_notypos_2019.csv', stringsAsFactors = FALSE) # host tree distributional data
genus<-readRDS('./data/genera.RDS') # host genera (column names)
inv_dat<-read.csv('./data/inv_dat_complete.csv') # pest severity data from 
allprop<-read.csv('./data/all_prop_mostdat.csv') # proportions of tree species by genus
codematch<-read.csv('./data/fia_newcodes.csv') # PLANTS codes conversion to FIA codes
severity<-read.csv('./data/each_spp_pesthost.csv') # severity data from Potter et al. 2019 supplement
hostspp<-read.csv("./data/FIA_spp_sepcols_2019.csv", stringsAsFactors = F) # host tree codes by pest


###determine whether each tree species is a host for each pest (data from Laura Blackburn use in Hudgins et al. 2017;2020)
fia<-list()
FIA$FIA<-as.character(FIA$FIA)
fia<-strsplit(FIA$FIA, split=", ")
for (i in 1:75)
{fia[[i]]<-gsub("^", "FIA_", fia[[i]])
fia[[i]]<-gsub("_(\\d{3}$)", "_0\\1", fia[[i]])
fia[[i]]<-gsub("_(\\d{2}$)", "_00\\1", fia[[i]])
}

if (write_out==T)
{ 
  host_gen<-as.data.frame(matrix(0, 75, 48))
 for (i in 1:75)
 {
   host_gen[i,which(genus%in%hostspp$Species[which(hostspp$Code%in%fia[[i]])])]<-1
 }
 host_gen<-host_gen[-c(which(rowSums(host_gen)==0)),]
host_gen<-host_gen[-4,]
write.csv(host_gen, file="./output/host_sharing_total.csv", row.names=F)# matrix of host susceptibility by pest
}

if (write_out==F)
{
host_gen<-read.csv('./output/host_sharing_total.csv')
}

stan_est<-t(seq(1,8)) # severity codes A-H (1-8). Pests assumed to be at least B given they were reported as intermediate impact by Aukema et al. (see MS)
data2<-data2[-c(25,69),]# remove species with no host data
data2<-data2[-4,]
for (i in 1:nrow(host_gen))
{
host_gen[i,which(host_gen[i,]==1)]<-stan_est[2]# set all pests to at least level 2 severity since they are considered 'intermediate impact' (see MS)
}
inv_dat$Genus<-tolower(as.character(inv_dat$genus2))
colnames(allprop)[1]<-"code.y"
allprop2<-merge(allprop,codematch[,c("sci", "code.y", "FIA..Code")], by="code.y", all.x=F, all.y=F) # join species proportions to plants codes

allprop2<-allprop2[!(duplicated(allprop2)),]

#get proportion by code
prop_small_code<-aggregate(allprop2$prop_small~allprop2$FIA..Code, FUN=sum)
prop_med_code<-aggregate(allprop2$prop_med~allprop2$FIA..Code, FUN=sum)
prop_large_code<-aggregate(allprop2$prop_large~allprop2$FIA..Code, FUN=sum)

#merge with inv_dat using scientific name
codematch$sci<-toupper(codematch$sci)
colnames(inv_dat)[2]<-'sci'
inv_dat_fia<-merge(inv_dat, codematch[,c(1,2,4)], by="sci")
inv_dat_fia<-inv_dat_fia[!duplicated(inv_dat_fia),]
inv_dat_fia$severity2<-rep(0,nrow(inv_dat_fia))
allprop2$genus<-gsub("\\s.*", "", allprop2$sci)


#match pests to genera by severity in Potter or Aukema
inv_dat_fia$severity2[which(inv_dat_fia$severity==0)]<-stan_est[2]
inv_dat_fia$severity2[which(inv_dat_fia$severity==1)]<-stan_est[3]
inv_dat_fia$severity2[which(inv_dat_fia$severity==3)]<-stan_est[4]
inv_dat_fia$severity2[which(inv_dat_fia$severity==5)]<-stan_est[5]
inv_dat_fia$severity2[which(inv_dat_fia$severity==8)]<-stan_est[6]
inv_dat_fia$severity2[which(inv_dat_fia$severity==9)]<-stan_est[7]
inv_dat_fia$severity2[which(inv_dat_fia$severity==10)]<-stan_est[8]

inv_dat_fia<-subset(inv_dat_fia, FIA..Code.y%in%allprop2$FIA..Code)
inv_dat_fia<-merge(inv_dat_fia, allprop2, 'sci')
inv_dat_fia<-data.table(inv_dat_fia)

## create summary tables used in MS supplement

tab1<-apply(table(inv_dat_fia$Genus, inv_dat_fia$clean_pest,inv_dat_fia$severity2)/c(table(inv_dat_fia$clean_pest)), c(1,3), sum)
tab2<-table(inv_dat_fia$clean_pest, inv_dat_fia$severity2)/c(table(inv_dat_fia$clean_pest))
tab3<-apply(table(inv_dat_fia$genus.1, inv_dat_fia$clean_pest,inv_dat_fia$severity2)/c(table(inv_dat_fia$clean_pest)), c(1,3), sum)

write.csv(tab1, "./output/tab1.csv")
write.csv(tab2, "./output/tab2.csv")
write.csv(tab3, "./output/tab3.csv")

#rescale severity by proportion
agg_sev_small<-ddply(inv_dat_fia, .(clean_pest,genus2),function(x){data.frame(severity_small=weighted.mean(x$severity2, x$prop_small))})
agg_sev_med<-ddply(inv_dat_fia, .(clean_pest,genus2),function(x){data.frame(severity_med=weighted.mean(x$severity2, x$prop_med))})
agg_sev_large<-ddply(inv_dat_fia, .(clean_pest,genus2),function(x){data.frame(severity_large=weighted.mean(x$severity2, x$prop_large))})


#convert to matrix format (pest rows, genera columns)
colnames(host_gen)<-genus
host_gen_small<-host_gen
host_gen_med<-host_gen
host_gen_large<-host_gen
for (i in  1:72)
{
    host_gen_small[i,which(colnames(host_gen_small)%in%agg_sev_small$genus2[which(agg_sev_small$clean_pest==data2$LATIN_NAME[i])])]<-agg_sev_small$severity_small[which(agg_sev_small$clean_pest==data2$LATIN_NAME[i])]
    host_gen_med[i,which(colnames(host_gen_med)%in%agg_sev_med$genus2[which(agg_sev_med$clean_pest==data2$LATIN_NAME[i])])]<-agg_sev_med$severity_med[which(agg_sev_med$clean_pest==data2$LATIN_NAME[i])]
    host_gen_large[i,which(colnames(host_gen_large)%in%agg_sev_large$genus2[which(agg_sev_large$clean_pest==data2$LATIN_NAME[i])])]<-agg_sev_large$severity_large[which(agg_sev_large$clean_pest==data2$LATIN_NAME[i])]
}
#set missing species to minimum based on Aukema (see MS)
host_gen_small[host_gen_small==1]<-stan_est[2]
host_gen_med[host_gen_med==1]<-stan_est[2]
host_gen_large[host_gen_large==1]<-stan_est[2]

##write output
saveRDS(host_gen_small, file='./output/host_gen_small.RDS')
saveRDS(host_gen_med, file='./output/host_gen_med.RDS')
saveRDS(host_gen_large, file='./output/host_gen_large.RDS')
