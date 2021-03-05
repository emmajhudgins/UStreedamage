### Pest Damage forecasts ###

## This code is licensed under the GPL v3.0 ##
## written by Emma J. Hudgins (2020) ##
## PhD Candidate, McGill University ##
## Montreal, QC, Canada ##

## For questions, e-mail emma.hudgins@mail.mcgill.ca ##

####1 load packages ####
rm(list=ls()) 
library(stringr)
library(dplyr)
library(here)
setwd(here)
####2 load data ####
data2<-read.csv('datanorm.csv', stringsAsFactors = FALSE)# pest data from Hudgins et al. 2017
rr<-which(data2$YEAR>=10) # subset to pests present for at least 10 yrs as of 2008
data2<-data2[-c(25,69),] # remove pests with no range in US forests
data2<-data2[-4,]# remove pests with no range in US forests
genus<-readRDS('genera.RDS') # host genera (column names)
mort<-read.csv('~/Desktop/OneDrive - McGill University/Turtles/potter_mort_all3.csv')[10001:20000,] # mortality risk from Potter et al. and STAN model (only take 10k samples after warmup)
prop_areas_places<-read.csv('~/Downloads/prop_place_area_2019.csv')#community information with regards to 50x50km grid
pests=str_to_sentence(data2$LATIN_NAME) #(rows)
trees=str_to_sentence(genus)

host_gen_small<-readRDS("host_gen_small.rds") # pest severity data (rows) for each tree genus  (columns) ofsmall, trees , using potter scale but standardized for species frequency in street tree data 
host_gen_med<-readRDS("host_gen_med.rds") # same but medium
host_gen_large<-readRDS("host_gen_large.rds") # large
host_sharing<-read.csv('~/Desktop/OneDrive - McGill University/Grad/scripts/host_sharing_total.csv')[,genus] # 0/1 matrix of whether a pest impacts a tree genus

#row indices for pest guilds (pathogens removed)
def<-which(data2$Guild=="Defoliators") 
bore<-which(data2$Guild=="Borers")
suck<-which(data2$Guild=="Suckers")

# predicted small, medium, large trees  by grid cell across 3 land uses
grid<-read.csv('~/Desktop/OneDrive - McGill University/Grad/scripts/countydatanorm_march.csv')
grid_small<-readRDS('grid_small.rds') 
grid_med<-readRDS('grid_med.rds')
grid_large<-readRDS('grid_large.rds')
grid_res_small<-readRDS('grid_res_small.rds') 
grid_res_med<-readRDS('grid_res_med.rds')
grid_res_large<-readRDS('grid_res_large.rds')
grid_com_small<-readRDS('grid_com_small.rds') 
grid_com_med<-readRDS('grid_com_med.rds')
grid_com_large<-readRDS('grid_com_large.rds')

#new cumulative sites invaded over time for each pest as a result of Hudgins et al. 2020 spread model
new_presences<-readRDS('new_presences.RDS')


####3.0 CODE SWITCHES######

estimate_mort=T #Should tree mortality be estimated or loaded?
bg=T # should most likely scenario or specific mortality debt (lag2) scenario be used?
lag2=10 # fixed mortality debt (only relevant when bg==F)
newpest=F # forecasting exposure for a hypothetically newly established invader

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

#if (newpest==F)
#{
  ####4 Mortality estimation ####
 # if (estimate_mort==T)
 # {
    
    #### 4.1 calculate exposure ####
    #Array of tree exposure with [sites, pests, trees, time]
    host_infestation_small=host_infestation_med=host_infestation_large=array(0,dim=c(3372,72,48,10))
host_res_infestation_small=host_res_infestation_med=host_res_infestation_large=array(0,dim=c(3372,72,48,10))
host_com_infestation_small=host_com_infestation_med=host_com_infestation_large=array(0,dim=c(3372,72,48,10))

    for (i in 1:72)
    {
      if (bg==T) # if using most likely mortality debt, use species-specific mortality debt scenarios
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
      }
      susc<-which(host_sharing[i,]>0)
      for (t in 1:10)
      {
        for (q in 0:((lag2/5)-1))
        {
          presences<-new_presences[[i]][,(ncol(new_presences[[i]])-10+t-q)]
          if (sum(presences)>0)
          {
            host_infestation_small[presences,i,susc,t]<-host_infestation_small[presences,i,susc,t]+grid_small[presences,susc]*(1/(lag2/5))
            host_infestation_med[presences,i,susc,t]<- host_infestation_med[presences,i,susc,t]+grid_med[presences,susc]*(1/(lag2/5))
            host_infestation_large[presences,i,susc,t]<-host_infestation_large[presences,i,susc,t]+grid_large[presences,susc]*(1/(lag2/5))

            host_res_infestation_small[presences,i,susc,t]<-host_res_infestation_small[presences,i,susc,t]+grid_res_small[presences,susc]*(1/(lag2/5))
            host_res_infestation_med[presences,i,susc,t]<- host_res_infestation_med[presences,i,susc,t]+grid_res_med[presences,susc]*(1/(lag2/5))
            host_res_infestation_large[presences,i,susc,t]<-host_res_infestation_large[presences,i,susc,t]+grid_res_large[presences,susc]*(1/(lag2/5))
  
            host_com_infestation_small[presences,i,susc,t]<-host_com_infestation_small[presences,i,susc,t]+grid_com_small[presences,susc]*(1/(lag2/5))
            host_com_infestation_med[presences,i,susc,t]<- host_com_infestation_med[presences,i,susc,t]+grid_com_med[presences,susc]*(1/(lag2/5))
            host_com_infestation_large[presences,i,susc,t]<-host_com_infestation_large[presences,i,susc,t]+grid_com_large[presences,susc]*(1/(lag2/5))
          }
        }
      }  
      }
    
    
    if (bg==T)
    {
      #total tree exposure over time (2020-2050)
      his_t<-apply(host_infestation_small[,,,4:10],1:3,FUN=sum)
      him_t<-apply(host_infestation_med[,,,4:10],1:3,FUN=sum)
      hil_t<-apply(host_infestation_large[,,,4:10],1:3,FUN=sum)
      #total costs (
      his<-apply(host_infestation_small[,,,4:10]*ave_discount[1:7],1:3,FUN=sum)
      him<-apply(host_infestation_med[,,,4:10]*ave_discount[1:7],1:3,FUN=sum)
      hil<-apply(host_infestation_large[,,,4:10]*ave_discount[1:7],1:3,FUN=sum)
      
      ### mean mortality behaviour
      mort2<-colMeans(mort)[2:8]
      
      #interpolate proportional mortality based on STAN model output (posterior mean in this case), and Potter et al. severity categories

      host_gen_small2<-matrix(approx(x=cbind(c(0,1,3,5,8,9,10)),y=cbind(as.vector(c(0,mort2[2:7]))), xout=as.vector(unlist(c(host_gen_small))))$y, nrow=72, ncol=48)
      host_gen_small2[which(host_sharing==1&host_gen_small2==0)]<-mort2[2]
      host_gen_small2<-matrix(unlist(host_gen_small2), nrow=72, ncol=48)
      
      host_gen_med2<-matrix(approx(x=cbind(c(0,1,3,5,8,9,10)),y=cbind(as.vector(c(0,mort2[2:7]))), xout=as.vector(unlist(c(host_gen_med))))$y, nrow=72, ncol=48)
      host_gen_med2[which(host_sharing==1&host_gen_med2==0)]<-mort2[2]
      host_gen_med2<-matrix(unlist(host_gen_med2), nrow=72, ncol=48)
      
      
      host_gen_large2<-matrix(approx(x=cbind(c(0,1,3,5,8,9,10)),y=cbind(as.vector(c(0,mort2[2:7]))), xout=as.vector(unlist(c(host_gen_large))))$y, nrow=72, ncol=48)
      host_gen_large2[which(host_sharing==1&host_gen_large2==0)]<-mort2[2]
      host_gen_large2<-matrix(unlist(host_gen_large2), nrow=72, ncol=48)
      
      mean_mort<-(apply(sweep(his_t,2:3,host_gen_small2, "*"),1:3,sum)+apply(sweep(him_t,2:3, host_gen_med2, "*"),1:3,sum)+apply(sweep(hil_t,2:3, host_gen_large2, "*"),1:3,sum))
      by_site_st_mort<-apply(sweep(his_t,2:3, host_gen_small2, "*"),1:3,sum)+apply(sweep(him,2:3, host_gen_med2, "*"),1:3,sum)+apply(sweep(hil,2:3, host_gen_large2, "*"),1:3,sum)
      
      saveRDS(mean_mort, 'mean_mort.RDS')
      saveRDS(by_site_st_mort, 'by_site_st_mort.RDS')
      
      ###5 Cost conversion ####
      
      #tree removal and replacement costs after accounting for inflation since Aukema et al. 2011
      small_st_treat<-450*(117.244/103.157)
      med_st_treat<-600*(117.244/103.157)
      large_st_treat<-1200*(117.244/103.157)
      
      small_res_treat<-600*(117.244/103.157)
      med_res_treat<-800*(117.244/103.157)
      large_res_treat<-1500*(117.244/103.157)
      
      by_site_st_costs<-apply(sweep(sweep(his,2:3, host_gen_small2, "*"),2,small_st_treat,"*"),1:3,sum)+apply(sweep(sweep(him,2:3, host_gen_med2, "*"),2,med_st_treat,"*"),1:3,sum)+apply(sweep(sweep(hil,2:3, host_gen_large2, "*"),2,large_st_treat,"*"),1:3,sum)
      
      temporal_costs<-apply(sweep(sweep(host_infestation_small[,,,4:10],2:3, host_gen_small2, "*"),2,small_st_treat,"*"),4,sum)+apply(sweep(sweep(host_infestation_med[,,,4:10],2:3, host_gen_med2, "*"),2,med_st_treat,"*"),4,sum)+apply(sweep(sweep(host_infestation_large[,,,4:10],2:3, host_gen_large2, "*"),2,large_st_treat,"*"),4,sum)
      saveRDS(by_site_st_costs, 'by_site_st_costs.RDS')
      saveRDS(temporal_costs, paste('temporal_costs',length,'.RDS'))
    }
    
    
    ### 6 full posterior predictive distribution ####
    
    #total costs (different dims than above)
    his<-apply(host_infestation_small[,,,4:10]*ave_discount[1:7],2:3,FUN=sum)
    him<-apply(host_infestation_med[,,,4:10]*ave_discount[1:7],2:3,FUN=sum)
    hil<-apply(host_infestation_large[,,,4:10]*ave_discount[1:7],2:3,FUN=sum)
    #non-street trees (community and residential)
    hcis<-apply(host_com_infestation_small[,,,4:10]*ave_discount[1:7],2:3,FUN=sum)
    hcim<-apply(host_com_infestation_med[,,,4:10]*ave_discount[1:7],2:3,FUN=sum)
    hcil<-apply(host_com_infestation_large[,,,4:10]*ave_discount[1:7],2:3,FUN=sum)
    hris<-apply(host_res_infestation_small[,,,4:10]*ave_discount[1:7],2:3,FUN=sum)
    hrim<-apply(host_res_infestation_med[,,,4:10]*ave_discount[1:7],2:3,FUN=sum)
    hril<-apply(host_res_infestation_large[,,,4:10]*ave_discount[1:7],2:3,FUN=sum)
    
    #total tree mortality (different dims than above)
    his_t<-apply(host_infestation_small[,,,4:10],2:3,FUN=sum)
    him_t<-apply(host_infestation_med[,,,4:10],2:3,FUN=sum)
    hil_t<-apply(host_infestation_large[,,,4:10],2:3,FUN=sum)
    hcis_t<-apply(host_com_infestation_small[,,,4:10],2:3,FUN=sum)
    hcim_t<-apply(host_com_infestation_med[,,,4:10],2:3,FUN=sum)
    hcil_t<-apply(host_com_infestation_large[,,,4:10],2:3,FUN=sum)
    hris_t<-apply(host_res_infestation_small[,,,4:10],2:3,FUN=sum)
    hrim_t<-apply(host_res_infestation_med[,,,4:10],2:3,FUN=sum)
    hril_t<-apply(host_res_infestation_large[,,,4:10],2:3,FUN=sum)
    
    
    
    by_pest_st_costs<-by_pest_com_costs<-by_pest_res_costs<-matrix(0, 10000,72)
    by_pest_treemort<-by_pest_com_treemort<-by_pest_res_treemort<-matrix(0, 10000,72)
    
    for (i in 1:10000)
    {
      mort2<-mort[i,2:8]
      
      host_gen_small2<-matrix(approx(x=cbind(c(0,1,3,5,8,9,10)),y=cbind(as.vector(c(0,mort2[2:7]))), xout=as.vector(unlist(c(host_gen_small))))$y, nrow=72, ncol=48)
      host_gen_small2[which(host_sharing==1&host_gen_small2==0)]<-mort2[2]
      host_gen_small2<-matrix(unlist(host_gen_small2), nrow=72, ncol=48)
     
       host_gen_med2<-matrix(approx(x=cbind(c(0,1,3,5,8,9,10)),y=cbind(as.vector(c(0,mort2[2:7]))), xout=as.vector(unlist(c(host_gen_med))))$y, nrow=72, ncol=48)
      host_gen_med2[which(host_sharing==1&host_gen_med2==0)]<-mort2[2]
      host_gen_med2<-matrix(unlist(host_gen_med2), nrow=72, ncol=48)
      
      
      host_gen_large2<-matrix(approx(x=cbind(c(0,1,3,5,8,9,10)),y=cbind(as.vector(c(0,mort2[2:7]))), xout=as.vector(unlist(c(host_gen_large))))$y, nrow=72, ncol=48)
      host_gen_large2[which(host_sharing==1&host_gen_large2==0)]<-mort2[2]
      host_gen_large2<-matrix(unlist(host_gen_large2), nrow=72, ncol=48)
      
      by_pest_st_costs[i,]<-apply(his*host_gen_small2,1, sum)*small_st_treat+apply(him*host_gen_med2,1, sum)*med_st_treat+apply(hil*host_gen_large2,1, sum)*large_st_treat
      # #
      # by_pest_com_costs[i,]<-apply(hcis*host_gen_small2,1, sum)*small_st_treat+apply(hcim*host_gen_med2,1, sum)*med_st_treat+apply(hcil*host_gen_large2,1, sum)*large_st_treat
      # by_pest_res_costs[i,]<-apply(hris*host_gen_small2,1, sum)*small_res_treat+apply(hrim*host_gen_med2,1, sum)*med_res_treat+apply(hril*host_gen_large2,1, sum)*large_res_treat
      
      by_pest_treemort[i,]<-apply(his_t*host_gen_small2,1, sum)+apply(him_t*host_gen_med2,1, sum)+apply(hil_t*host_gen_large2,1, sum)
      # by_pest_com_treemort[i,]<-apply(hcis_t*host_gen_small2,1, sum)+apply(hcim_t*host_gen_med2,1, sum)+apply(hcil_t*host_gen_large2,1, sum)
      # by_pest_res_treemort[i,]<-apply(hris_t*host_gen_small2,1, sum)+apply(hrim_t*host_gen_med2,1, sum)+apply(hril_t*host_gen_large2,1, sum)
    }
    #### 6.1 Annualized cost and tree mortality quantiles #####
    sc<-0.02*(apply(by_pest_st_costs[,c(def,suck,bore)],1,sum))/(1-(1+0.02)^-30)
    rc<-0.02*(apply(by_pest_res_costs[,c(def,suck,bore)],1,sum))/(1-(1+0.02)^-30)
    cc<-0.02*(apply(by_pest_com_costs[,c(def,suck,bore)],1,sum))/(1-(1+0.02)^-30)
    
    quantile(sc,0.025)
    quantile(sc,0.975)
    mean(sc)
    
    quantile(rc,0.025)
    quantile(rc,0.975)
    mean(rc)
    
    quantile(cc,0.025)
    quantile(cc,0.975)
    mean(cc)
    
    #street tree losses 
    quantile(rowSums(by_pest_treemort),0.025)
    quantile(rowSums(by_pest_treemort),0.975)
    mean(rowSums(by_pest_treemort))
    
    quantile(rowSums(by_pest_com_treemort),0.025)
    quantile(rowSums(by_pest_com_treemort),0.975)
    mean(rowSums(by_pest_com_treemort))
    
    quantile(rowSums(by_pest_res_treemort),0.025)
    quantile(rowSums(by_pest_res_treemort),0.975) 
    mean(rowSums(by_pest_res_treemort))
    
    if (bg==F)
    {
      saveRDS(list(by_pest_treemort,by_pest_com_treemort,by_pest_res_treemort),paste0("morality_",length,".RDS"))
      saveRDS(list(sc,cc,rc),paste0("costquantiles_",length,".RDS"))
    }
    if (bg==T)
    {
      saveRDS(list(by_pest_treemort,by_pest_com_treemort,by_pest_res_treemort),"morality_bestguess.RDS" )
      saveRDS(list(sc,cc,rc),paste0("costquantiles_bestguess.RDS"))
    }
   pest_percent<- data.frame(sp=data2$COMMON_NAM,mort=apply(mean_mort,2,sum)/sum(mean_mort))
   pest_costs<- data.frame(sp=data2$COMMON_NAM,mort=apply(by_pest_st_costs,2,mean))
   
     if (estimate_mort==F)
      {
        if (bg==T)
        {
          mean_mort<-readRDS('mean_mort.RDS')
          by_site_st_mort<-readRDS('by_site_st_mort.RDS')
          by_site_st_costs<-readRDS('by_site_st_costs.RDS')
      
      #can change focal pest and tree
      tree_nm="Fraxinus"
      pest_nm="Agrilus planipennis"
      tree=which(trees==tree_nm)
      pest=which(pests==pest_nm)
      lag2=10 # must correspond to bestguess lag for that species
      plot(cumsum(order(apply(mean_mort,1,sum))))
      order(apply(mean_mort,1,sum))
      mort_max<-grid$ID[unique(c(new_presences[[pest]][,(ncol(new_presences[[pest]])-10+4):(ncol(new_presences[[pest]])-((lag2-5)/5))]))]
      
      notyet_max<-grid$ID[unique(c(new_presences[[pest]][,(ncol(new_presences[[pest]]))]))]
      sum(by_site_st_costs[which(grid$ID%in%mort_max),,])
      #saturated communities
      comms<-(unique(prop_areas_places$PLACEFIPS[which(prop_areas_places$ID_2%in%mort_max)]))
      #partially saturated communities
      notyet_comms<-(unique(prop_areas_places$PLACEFIPS[prop_areas_places$ID_2%in%notyet_max]))
      
    #fractional importance
      sum(mean_mort[which(grid$ID%in%mort_max),pest,tree])/sum(mean_mort[,,]) #high impact zone vs. all mortality
      length(comms)/length(unique(prop_areas_places$PLACEFIPS))
      sum(mean_mort[which(grid$ID%in%mort_max),pest,tree])/sum((grid_small[,20]+grid_med[,20]+grid_large[,20])[which(grid$ID%in%mort_max)]) # high impact zone vs. all ash trees
      sum(mean_mort[which(grid$ID%in%mort_max),pest,tree])/sum(rowSums(grid_small+grid_med+grid_large)[which(grid$ID%in%mort_max)])   # high impact zone vs all street trees in this zone
    }
  }
  
  #### 6 forecast new pest ####
  if (newpest==T)
  {
    
    lag=10
    lag2=10
    cumulative_newpest<-list()
    for (i in c(1:3))
    {
      cumulative_newpest[[i]]<-matrix(0,3372,max(which(colSums(future_newpest[[i]])!=0))-(lag/5))
      
      time2=1
      for (time in ((lag/5)+1):max(which(colSums(future_newpest[[i]])!=0)))
      {
        cumulative_newpest[[i]][,time2]<-c(future_newpest[[i]][which((future_newpest[[i]][1:length(which(future_newpest[[i]][,time-(lag/5)]!=0)),time-(lag/5)]%in%future_newpest[[i]][1:length(which(future_newpest[[i]][,time-(lag/5)-1]!=0)),time-(lag/5)-1])==F),time2], rep(0,3372-length(which((future_newpest[[i]][1:length(which(future_newpest[[i]][,time-(lag/5)]!=0)),time-(lag/5)]%in%future_newpest[[i]][1:length(which(future_newpest[[i]][,time-(lag/5)-1]!=0)),time-(lag/5)-1])==F))))
        time2=time2+1
      }
    }
    
    
    host_newpest_small=host_newpest_med=host_newpest_large=array(0,dim=c(3372,72,48,4))
    for (i in 1:3)
    {
      
      for (t in 1:4)
      {
        for (q in 0:((lag2-5)/5))
        {
          presences<-unique(c(new_presences[[i]][,t-q]))
          host_newpest_small[presences,i,,t]<-host_newpest_small[presences,i,,t]+grid_small[presences,]*(1/(lag2/5))
          host_newpest_med[presences,i,,t]<- host_newpest_med[presences,i,,t]+grid_med[presences,]*(1/(lag2/5))
          host_newpest_large[presences,i,,t]<-host_newpest_large[presences,i,,t]+grid_large[presences,]*(1/(lag2/5))
        }
      }  
    }
  }
  total_maple_damage<- apply(host_newpest_small[,c(1:3),c(3),]+host_newpest_med[,c(1:3),c(3),]+host_newpest_large[,c(1:3),c(3),],c(1:2),sum) 
  colSums(total_maple_damage)
  total_maple_death<-apply(host_newpest_small[,c(1:3),c(3),]*stan_est[1,8]+host_newpest_med[,c(1:3),c(3),]*stan_est[1,8]+host_newpest_large[,c(1:3),c(3),]*stan_est[1,8],c(1:2),sum)
  colSums(total_maple_death)
  total_maple_cost<-apply(host_newpest_small[,c(1:3),c(3),]*450*(117.244/103.157)*stan_est[1,8]+host_newpest_med[,c(1:3),c(3),]*(117.244/103.157)*800*stan_est[1,8]+host_newpest_large[,c(1:3),c(3),]*(117.244/103.157)*1200*stan_est[1,8],c(1:2),sum)
  colSums(total_maple_cost)
  total_oak_damage<- apply(host_newpest_small[,c(1:3),c(40),]+host_newpest_med[,c(1:3),c(40),]+host_newpest_large[,c(1:3),c(40),],c(1:2),sum) 
  colSums(total_oak_damage)
  total_oak_cost<-apply(host_newpest_small[,c(1:3),c(40),]*(117.244/103.157)*800*stan_est[1,8]+host_newpest_med[,c(1:3),c(40),]*(117.244/103.157)*800*stan_est[1,8]+host_newpest_large[,c(1:3),c(40),]*(117.244/103.157)*800*stan_est[1,8],c(1:2),sum)
  colSums(total_oak_cost)
}
