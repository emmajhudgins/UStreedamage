### Theoretic analysis of STAN models for estimating pest severity and cost ## 
#1=gamma, 2=weibull, 3=lnorm, 4=pareto
rm(list=ls()) 
require(here)
require(rstan)
setwd(here())
i=1
lhc<-read.csv('./data/lhc_beta_data.csv') # latin hypercube simulated cost data based on lhc-sampled parameters
pars<-read.csv('./data/lhc_draws_beta.csv') #latin hypercube sampled beta parameters

for(i in 1:1000)
{
  rstan_options(auto_write = TRUE)
  y<-as.numeric(lhc[i,])
  m_beta<-stan(file="beta_mort_lhc.stan",data = list(y=y),pars=c("shape", "scale", "log_lik"),iter=10000, control=list(adapt_delta=0.999999),cores=1) #fit beta mortality model
  pp_par1<-length(which(extract(m_beta)$shape<=pars[i,1]))/20000 #extract percentiles in which parameters fall for pp-plots
  pp_par2<-length(which(extract(m_beta)$scale<=pars[i,2]))/20000
  
  write.table(cbind(i,pp_par1, pp_par2), file=paste("lhc_sim.csv", sep='.'), row.names=F, append=T, sep=",", col.names=F) # append output to file (delete between runs to avoid merging with old data)
}


###empirical models##
#thresholds from Potter et al. 
oneT=0.01
threeT=0.10
fiveT=0.25
eightT=0.95

#calculate number of pests in each severity category from Potter et al.
inv_dat_reduced<-read.csv("./data/each_spp_pesthost_patho.csv") 
lengths<-aggregate(inv_dat_reduced$genus~inv_dat_reduced$pest,FUN=length) 
colnames(lengths)[1]<-"pest"
 lengths_each<-merge(inv_dat_reduced,lengths, by="pest")
 inv_dat_reduced$severity<-as.numeric(inv_dat_reduced$severity)
 #divide each pest's mean impact by the number of genera it impacts and sum the numebr in each category
 one_spp<-sum((1*inv_dat_reduced$severity==1)/lengths_each$`inv_dat_reduced$genus`)
 three_spp<-sum((1*inv_dat_reduced$severity==3)/lengths_each$`inv_dat_reduced$genus`)
 five_spp<-sum((1*inv_dat_reduced$severity==5)/lengths_each$`inv_dat_reduced$genus`)
 eight_spp<-sum((1*inv_dat_reduced$severity==8)/lengths_each$`inv_dat_reduced$genus`)
 nine_spp<-sum((1*inv_dat_reduced$severity==9)/lengths_each$`inv_dat_reduced$genus`)
 ten_spp<-sum((1*inv_dat_reduced$severity==10)/lengths_each$`inv_dat_reduced$genus`)
 #add on lower category numbers from Aukema et al. 2011
 subone_spp<-33+sum((1*inv_dat_reduced$severity==0)/lengths_each$`inv_dat_reduced$genus`)
 subsubone_spp<-418-60
 #run STAN code
  m_beta<-stan(file="beta_mort.stan",data = list(oneT=oneT, threeT=threeT, fiveT=fiveT, eightT=eightT, subsubone_spp=subsubone_spp, subone_spp=subone_spp, one_spp=one_spp, three_spp=three_spp, five_spp=five_spp, eight_spp=eight_spp, nine_spp=nine_spp, ten_spp=ten_spp),pars=c("shape", "scale", "log_lik", "nineT", "suboneT", "subsuboneT"),iter=10000, control=list(adapt_delta=0.999999),cores=4)

##sample the posterior
 beta_samps<-extract(m_beta)
 subsubone<-subone<-one<-three<-five<-eight<-nine<-ten<-rep(0,20000)
 for (i in 1:20000)
 {
   rand_beta<-rbeta(100000,shape1=beta_samps$shape[i], shape2=beta_samps$scale[i])
   subsubone[i]<-sample(rand_beta[which(rand_beta<beta_samps$subsuboneT[i])],1)
   subone[i]<-sample(rand_beta[which(rand_beta>=beta_samps$subsuboneT[i] & rand_beta<beta_samps$suboneT[i]+beta_samps$subsuboneT[i])],1)
   one[i]<-sample(rand_beta[which(rand_beta>=beta_samps$suboneT[i]+beta_samps$subsuboneT[i] & rand_beta<oneT)],1)
   three[i]<-sample(rand_beta[which(rand_beta>=oneT& rand_beta<threeT)],1)
   five[i]<-sample(rand_beta[which(rand_beta>=threeT& rand_beta<fiveT)],1)
  eight[i]<-sample(rand_beta[which(rand_beta>=fiveT& rand_beta<eightT)],1)
  if (length(which(rand_beta>=eightT& rand_beta<beta_samps$nineT[i])>0))
  {
  nine[i]<-sample(rand_beta[which(rand_beta>=eightT& rand_beta<beta_samps$nineT[i])],1)
  }
  if (length(which(rand_beta>=beta_samps$nineT[i])))
  {
  ten[i]<-sample(rand_beta[which(rand_beta>=beta_samps$nineT[i])],1)
  }
 }
 for( j in which(nine==0))
 {
   while(nine[j]==0)
   {
   rand_beta<-rbeta(100000,shape1=beta_samps$shape[j], shape2=beta_samps$scale[j])
   if (length(which(rand_beta>=eightT& rand_beta<beta_samps$nineT[i])>0))
   {
   nine[j]<-sample(rand_beta[which(rand_beta>=eightT& rand_beta<beta_samps$nineT[i])],1)}
   }
 }
 for( j in which(ten==0))
 {
   while(ten[j]==0)
   {
     rand_beta<-rbeta(100000,shape1=beta_samps$shape[j], shape2=beta_samps$scale[j])
     if (length(which(rand_beta>=beta_samps$nineT[i])))
     {
     ten[j]<-sample(rand_beta[which(rand_beta>=beta_samps$nineT[i])],1)}
   }
 }


#output sample mortality rates in each category
  write.csv(cbind((subsubone),(subone),(one),(three),(five), (eight), (nine), (ten)), file="./output/potter_mort_all3.csv", row.names=F)

