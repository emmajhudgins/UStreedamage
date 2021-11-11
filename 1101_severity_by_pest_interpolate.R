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
library(tidygeocoder)
library(pdist)
library(sf)
#setwd(here())
write_out=F
###read in data
data2<-read.csv('./data/datanorm.csv', stringsAsFactors = FALSE) # pest data
FIA<-read.csv('./data/FIAcodes_notypos_2019.csv', stringsAsFactors = FALSE) # host tree distributional data
genus<-readRDS('./data/genera.RDS') # host genera (column names)
inv_dat<-read.csv('./data/inv_dat_complete.csv') # pest severity data from 
eachprop<-read.csv('~/Downloads/eachspp_eachcityprop.csv') # proportions of tree species by genus by city (private )
codematch<-read.csv('./data/fia_newcodes.csv') # PLANTS codes conversion to FIA codes
severity<-read.csv('./data/each_spp_pesthost.csv') # severity data from Potter et al. 2019 supplement
hostspp<-read.csv("./data/FIA_spp_sepcols_2019.csv", stringsAsFactors = F) # host tree codes by pest

eachprop$city2<-gsub("_.*", "", eachprop$city)
# coords<-geo(city=eachprop$city,state=eachprop$state, limit=1)
# coords2<-geo(city=eachprop$city2,state=eachprop$state, limit=1)
# coords3<-geo(state=eachprop$state, limit=1)
# saveRDS(coords, file='./output/georeferenced_cities.RDS')
# saveRDS(coords2, file='./output/georeferenced_cities2.RDS')
# saveRDS(coords3, file='./output/georeferenced_cities3.RDS')
# 
# 
# coords[which(is.na(coords$lat)),2:4]<-coords2[which(is.na(coords$lat)),2:4]
# coords[which(is.na(coords$lat)),2:4]<-coords3[which(is.na(coords$lat)),1:3]
#saveRDS(coords, file="./output.collated_cities.RDS")
coords<-readRDS('./output/collated_cities.RDS')
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
colnames(eachprop)[4]<-c("code.y")
allprop2<-merge(eachprop,codematch[,c("sci", "code.y", "FIA..Code")], by="code.y", all.x=F, all.y=F) # join species proportions to plants codes

allprop2<-allprop2[!(duplicated(allprop2)),]

#get proportion by code
prop_small_code<-aggregate(allprop2$prop_small, by=list(allprop2$city,allprop2$FIA..Code), FUN=sum)
prop_med_code<-aggregate(allprop2$prop_med, by=list(allprop2$city,allprop2$FIA..Code), FUN=sum)
prop_large_code<-aggregate(allprop2$prop_large, by=list(allprop2$city,allprop2$FIA..Code), FUN=sum)

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


#rescale severity by proportion
agg_sev_small<-ddply(inv_dat_fia, .(clean_pest,genus2, city),function(x){data.frame(severity_small=weighted.mean(x$severity2, x$prop_small, na.rm=T))})
agg_sev_med<-ddply(inv_dat_fia, .(clean_pest,genus2, city),function(x){data.frame(severity_med=weighted.mean(x$severity2, x$prop_med, na.rm=T))})
agg_sev_large<-ddply(inv_dat_fia, .(clean_pest,genus2, city),function(x){data.frame(severity_large=weighted.mean(x$severity2, x$prop_large, na.rm=T))})

#add city data
city_data<-read.csv('./data/countydatanorm_march.csv')
#find closest tree-level proportion data for each city

data_sf <- st_as_sf(coords, coords = c("long", "lat"),
                    # Change to your CRS
                    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
data_sf<-st_transform(data_sf, '+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs')
nn_dist<-0
for (i in 1:nrow(city_data)){
nn_dist[i]<-which.min(pdist(cbind(city_data$X_coord[i], city_data$Y_coord[i]),st_coordinates(data_sf))@dist)
}
colnames(host_gen)<-genus
host_gen_small_each<-replicate(nrow(city_data), host_gen, simplify=F)
host_gen_med_each<-replicate(nrow(city_data), host_gen, simplify=F)
host_gen_large_each<-replicate(nrow(city_data), host_gen, simplify=F)
agg_sev_small$severity_small[which(is.nan(agg_sev_small$severity_small))]<-agg_sev_med$severity_med[which(is.nan(agg_sev_small$severity_small))]
agg_sev_small$severity_small[which(is.nan(agg_sev_small$severity_small))]<-agg_sev_large$severity_large[which(is.nan(agg_sev_small$severity_small))]
for(i in which(is.na(agg_sev_small$severity_small)==T)){
  agg_sev_small$severity_small[i]<-mean(agg_sev_small$severity_small[which(agg_sev_small$genus2%in%agg_sev_small$genus2[i])], na.rm=T)
}

agg_sev_med$severity_med[which(is.nan(agg_sev_med$severity_med))]<-agg_sev_small$severity_small[which(is.nan(agg_sev_med$severity_med))]
agg_sev_med$severity_med[which(is.nan(agg_sev_med$severity_med))]<-agg_sev_large$severity_large[which(is.nan(agg_sev_med$severity_med))]
for(i in which(is.na(agg_sev_med$severity_med)==T)){
  agg_sev_med$severity_med[i]<-mean(agg_sev_med$severity_med[which(agg_sev_med$genus2%in%agg_sev_med$genus2[i])], na.rm=T)
}

agg_sev_large$severity_large[which(is.nan(agg_sev_large$severity_large))]<-agg_sev_med$severity_med[which(is.nan(agg_sev_large$severity_large))]
agg_sev_large$severity_large[which(is.nan(agg_sev_large$severity_large))]<-agg_sev_small$severity_small[which(is.nan(agg_sev_large$severity_large))]
for(i in which(is.na(agg_sev_large$severity_large)==T)){
  agg_sev_large$severity_large[i]<-mean(agg_sev_large$severity_large[which(agg_sev_large$genus2%in%agg_sev_large$genus2[i])], na.rm=T)
}

for (j in 1:nrow(city_data))
{
#convert to matrix format (pest rows, genera columns)
agg_sev_small_city<-subset(agg_sev_small, city==data_sf$city[nn_dist[j]])
agg_sev_med_city<-subset(agg_sev_med, city==data_sf$city[nn_dist[j]])
agg_sev_large_city<-subset(agg_sev_large, city==data_sf$city[nn_dist[j]])
for (i in  1:72)
{
  if (any(agg_sev_small_city$clean_pest==data2$LATIN_NAME[i]))
  {
    host_gen_small_each[[j]][i,which(colnames(host_gen)%in%agg_sev_small_city$genus2[which(agg_sev_small_city$clean_pest==data2$LATIN_NAME[i])])]<-agg_sev_small_city$severity_small[which(agg_sev_small_city$clean_pest==data2$LATIN_NAME[i])]
    host_gen_med_each[[j]][i,which(colnames(host_gen)%in%agg_sev_med_city$genus2[which(agg_sev_med_city$clean_pest==data2$LATIN_NAME[i])])]<-agg_sev_med_city$severity_med[which(agg_sev_med_city$clean_pest==data2$LATIN_NAME[i])]
    host_gen_large_each[[j]][i,which(colnames(host_gen)%in%agg_sev_large_city$genus2[which(agg_sev_large_city$clean_pest==data2$LATIN_NAME[i])])]<-agg_sev_large_city$severity_large[which(agg_sev_large_city$clean_pest==data2$LATIN_NAME[i])]
  }
}
}
#set missing species to minimum based on Aukema (see MS)
for (i in 1:(3372)){
host_gen_small_each[[i]][which(host_gen_small_each[[i]]==1)]<-stan_est[2]
host_gen_med_each[[i]][which(host_gen_med_each[[i]]==1)]<-stan_est[2]
host_gen_large_each[[i]][which(host_gen_large_each[[i]]==1)]<-stan_est[2]
}

##write output
saveRDS(host_gen_small_each, file='./output/host_gen_small_each.RDS')
saveRDS(host_gen_med_each, file='./output/host_gen_med_each.RDS')
saveRDS(host_gen_large_each, file='./output/host_gen_large_each.RDS')

## intersect with city-level exposure prior to mortality calculation
####2 load data ####
data2<-read.csv('./data/datanorm.csv', stringsAsFactors = FALSE)# pest data from Hudgins et al. 2017
rr<-which(data2$YEAR>=10) # subset to pests present for at least 10 yrs as of 2008
data2<-data2[-c(25,69),] # remove pests with no range in US forests
data2<-data2[-4,]# remove pests with no range in US forests
genus<-readRDS('./data/genera.RDS') # host genera (column names)
mort<-read.csv('./output/potter_mort_all3.csv')[10001:20000,] # mortality risk from Potter et al. and STAN model (only take 10k samples after warmup)
prop_areas_places<-read.csv('./data/prop_place_area_2019.csv')#community information with regards to 50x50km grid
pests=str_to_sentence(data2$LATIN_NAME) #(rows)
trees=str_to_sentence(genus)

host_sharing<-read.csv('./output/host_sharing_total.csv')  # 0/1 matrix of whether a pest impacts a tree genus

#row indices for pest guilds (pathogens removed)
def<-which(data2$Guild=="Defoliators") 
bore<-which(data2$Guild=="Borers")
suck<-which(data2$Guild=="Suckers")

# 50x50km grid spatial information
grid<-read.csv('./data/countydatanorm_march.csv')

#outputs of treegrid script, trees in each cell of each genus across land uses
grid_small<-readRDS('./output/grid_small.rds') 
grid_med<-readRDS('./output/grid_med.rds')
grid_large<-readRDS('./output/grid_large.rds')

#new cumulative sites invaded over time for each pest as a result of Hudgins et al. 2020 spread model (result of new_presences script)
new_presences<-readRDS('./output/new_presences.RDS')
cumulative_newpest<-readRDS('./output/presences_time_newpest.rds')
####3.0 CODE SWITCHES######

lag2=10 # fixed mortality debt (only relevant when bg==F)

####3.1 lags and discounting ####
# corresponding lag length name
lengths=data.frame(short=10, mid=50, long=100)
length<-colnames(lengths)[which(lengths==lag2)]
#discount rate
discounter<-rep(0,46) #2% discount rate
for (i in seq(0,45, by=1))
{
  discounter[i+1]<-0.98^i
}
ave_discount<-rep(0,10) 
for (i in 1:10)
{
  ave_discount[i]<-mean(discounter[((i-1)*5):(i*5)])
}

#tree removal and replacement costs after accounting for inflation since Aukema et al. 2011
small_st_treat<-450*(117.244/103.157)
med_st_treat<-600*(117.244/103.157)
large_st_treat<-1200*(117.244/103.157)

small_res_treat<-600*(117.244/103.157)
med_res_treat<-800*(117.244/103.157)
large_res_treat<-1500*(117.244/103.157)



#Array of tree exposure with [sites, pests, trees, time]
host_infestation_small=host_infestation_med=host_infestation_large=array(0,dim=c(3372,72,48,10))

for (i in 1:72)
{
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
  
  susc<-which(host_sharing[i,]>0)
  for (t in 1:10)
  {
    for (q in 0:((lag2/5)-1))
    {
      presences<-new_presences[[i]][,(ncol(new_presences[[i]])-10+t-q)]
      if (sum(presences)>0 & (ncol(new_presences[[i]])-10+t-q)>0)
      {
        host_infestation_small[presences,i,susc,t]<-host_infestation_small[presences,i,susc,t]+grid_small[presences,susc]*(1/(lag2/5))
        host_infestation_med[presences,i,susc,t]<- host_infestation_med[presences,i,susc,t]+grid_med[presences,susc]*(1/(lag2/5))
        host_infestation_large[presences,i,susc,t]<-host_infestation_large[presences,i,susc,t]+grid_large[presences,susc]*(1/(lag2/5))
        
    }
  }  
}
}

#total tree exposure over time (2020-2050)
#total costs (different dims than above)
his<-aperm(apply(host_infestation_small[,,,4:10]*ave_discount[1:7],1:3,sum), c(2,3,1))
him<-aperm(apply(host_infestation_med[,,,4:10]*ave_discount[1:7],1:3,sum), c(2,3,1))
hil<-aperm(apply(host_infestation_large[,,,4:10]*ave_discount[1:7],1:3,sum), c(2,3,1))
#total tree mortality (different dims than above)
his_t<-aperm(apply(host_infestation_small[,,,4:10],1:3,sum), c(2,3,1))
him_t<-aperm(apply(host_infestation_med[,,,4:10],1:3,sum), c(2,3,1))
hil_t<-aperm(apply(host_infestation_large[,,,4:10],1:3,sum), c(2,3,1))

  mort2<-colMeans(mort)[2:8]

  host_gen_small2<-lapply(host_gen_small_each,function(z){matrix(approx(x=cbind(c(0,2:8)),y=cbind(as.vector(c(0,mort2[1:7]))), xout=unlist(z))$y,72,48)})
  
  host_gen_med2<-lapply(host_gen_med_each,function(z){matrix(approx(x=cbind(c(0,2:8)),y=cbind(as.vector(c(0,mort2[1:7]))), xout=unlist(z))$y,72,48)})

  host_gen_large2<-lapply(host_gen_large_each,function(z){matrix(approx(x=cbind(c(0,2:8)),y=cbind(as.vector(c(0,mort2[1:7]))), xout=unlist(z))$y,72,48)})
  
  by_pest_st_costs<-apply(simplify2array(host_gen_small2)*his,1,sum)*small_st_treat+apply(simplify2array(host_gen_med2)*him,1,sum)*med_st_treat+apply(simplify2array(host_gen_large2)*hil,1,sum)*large_st_treat
 
  by_pest_treemort<-apply(simplify2array(host_gen_small2)*his_t,1,sum)+apply(simplify2array(host_gen_med2)*him_t,1,sum)+apply(simplify2array(host_gen_large2)*hil_t,1,sum)

#### 6.1 Annualized cost and tree mortality quantiles #####
sc<-0.02*(sum(by_pest_st_costs[c(def,suck,bore)]))/(1-(1+0.02)^-30)

sc
sum(by_pest_treemort)

