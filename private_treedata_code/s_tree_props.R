library(sas7bdat)

files_CA<-list.files('~/Downloads/CA_RI_VA_2/CA/')
files_RI<-list.files('~/Downloads/CA_RI_VA_2/RI/')
files_VA<-list.files('~/Downloads/CA_RI_VA_2/VA/')
files_IA<-list.files('~/Downloads/IA_and_WI_2/IA/')
files_WI<-list.files('~/Downloads/IA_and_WI_2/WI/')
files_CO<-list.files('~/Downloads/CO_MI_NJ/CO/')
files_MI<-list.files('~/Downloads/CO_MI_NJ/MI/')
files_NJ<-list.files('~/Downloads/CO_MI_NJ/NJ/')
files_MO<-list.files('~/Downloads/MO_PA_RI/MO/')
files_PA<-list.files('~/Downloads/MO_PA_RI/PA/')
files_RI2<-list.files('~/Downloads/MO_PA_RI/RI/')
files_IL<-list.files('~/Downloads/IL_and_WA/IL/')
files_WA<-list.files('~/Downloads/IL_and_WA/WA/')
files_KS<-list.files('~/Downloads/KS_MT_ND_NE_SD_UT/KS/')
files_MT<-list.files('~/Downloads/KS_MT_ND_NE_SD_UT/MT/')
files_ND<-list.files('~/Downloads/KS_MT_ND_NE_SD_UT/ND/')
files_NE<-list.files('~/Downloads/KS_MT_ND_NE_SD_UT/NE/')
files_SD<-list.files('~/Downloads/KS_MT_ND_NE_SD_UT/SD/')
files_UT<-list.files('~/Downloads/KS_MT_ND_NE_SD_UT/UT/')
files_FL<-list.files('~/Downloads/FL_and_OR/FL/')
files_OR<-list.files('~/Downloads/FL_and_OR/OR/')


files_CA<-subset(files_CA, grepl("street", files_CA))
files_RI<-subset(files_RI, grepl("street", files_RI))
files_VA<-subset(files_VA, grepl("street", files_VA))
files_IA<-subset(files_IA, grepl("street", files_IA))
files_WI<-subset(files_WI, grepl("street", files_WI))
files_CO<-subset(files_CO, grepl("street", files_CO))
files_MI<-subset(files_MI, grepl("street", files_MI))
files_NJ<-subset(files_NJ, grepl("street", files_NJ))
files_MO<-subset(files_MO, grepl("street", files_MO))
files_PA<-subset(files_PA, grepl("street", files_PA))
files_RI2<-subset(files_RI2, grepl("street", files_RI2))
files_IL<-subset(files_IL, grepl("street", files_IL))
files_WA<-subset(files_WA, grepl("street", files_WA))
files_KS<-subset(files_KS, grepl("street", files_KS))
files_MT<-subset(files_MT, grepl("street", files_MT))
files_ND<-subset(files_ND, grepl("street", files_ND))
files_NE<-subset(files_NE, grepl("street", files_NE))
files_SD<-subset(files_SD, grepl("street", files_SD))
files_UT<-subset(files_UT, grepl("street", files_UT))
files_FL<-subset(files_FL, grepl("street", files_FL))
files_OR<-subset(files_OR, grepl("street", files_OR))


big_data<-data.frame(state="0",city="0", SppCode="0", SP="0", small=0, med=0, large=0, trees=0)
for (i in 1:length(files_CA))
{
  city<-gsub("_street.*","", files_CA[i])
  setwd('~/Downloads/CA_RI_VA_2/CA')
  data<-read.sas7bdat(files_CA[i])
  colnames(data)<-gsub("Sp", "SP", colnames(data))
  colnames(data)<-gsub("sp", "SP", colnames(data))
  big_data<-rbind(big_data, setNames(data.frame(cbind(rep("CA", nrow(data)),rep(city, nrow(data)),as.character(data$SPpCode), as.character(data$SPeciesName),data$SP_count_0_30, data$SP_count_30_61,data$SP_count_GT61, data$tot_SP_count)), colnames(big_data)))
}
big_data<-big_data[2:nrow(big_data),]
for (i in 1:length(files_RI))
{
  city<-gsub("_street.*","", files_RI[i])
  setwd('~/Downloads/CA_RI_VA_2/RI')
  data<-read.sas7bdat(files_RI[i])
  colnames(data)<-gsub("Sp", "SP", colnames(data))
  colnames(data)<-gsub("sp", "SP", colnames(data))
  big_data<-rbind(big_data, setNames(data.frame(cbind(rep("RI", nrow(data)),rep(city, nrow(data)),as.character(data$SPpCode), as.character(data$SPeciesName),data$SP_count_0_30, data$SP_count_30_61,data$SP_count_GT61, data$tot_SP_count)), colnames(big_data)))
}
for (i in 1:length(files_VA))
{
  city<-gsub("_street.*","", files_VA[i])
  setwd('~/Downloads/CA_RI_VA_2/VA')
  data<-read.sas7bdat(files_VA[i])
  colnames(data)<-gsub("Sp", "SP", colnames(data))
  colnames(data)<-gsub("sp", "SP", colnames(data))
  big_data<-rbind(big_data, setNames(data.frame(cbind(rep("VA", nrow(data)),rep(city, nrow(data)),as.character(data$SPpCode), as.character(data$SPeciesName),data$SP_count_0_30, data$SP_count_30_61,data$SP_count_GT61, data$tot_SP_count)), colnames(big_data)))
}
for (i in 1:length(files_IA))
{
  city<-gsub("_street.*","", files_IA[i])
  setwd('~/Downloads/IA_and_WI_2/IA')
  data<-read.sas7bdat(files_IA[i])
  colnames(data)<-gsub("Sp", "SP", colnames(data))
  colnames(data)<-gsub("sp", "SP", colnames(data))
  colnames(data)<-gsub("SPCODE", "SPpCode", colnames(data))
  colnames(data)<-gsub("ScientificName", "SPeciesName", colnames(data))
  colnames(data)<-gsub("gt", "GT", colnames(data))
  colnames(data)<-gsub("ScientificName", "SPeciesName", colnames(data))
  colnames(data)<-gsub("Count", "count", colnames(data))
  colnames(data)<-gsub("SPCode", "SPpCode", colnames(data))
  colnames(data)<-gsub("gt", "GT", colnames(data))
  colnames(data)<-gsub("tot_count", "tot_SP_count", colnames(data))
  colnames(data)<-gsub("SPpcode", "SPpCode", colnames(data))
  big_data<-rbind(big_data, setNames(data.frame(cbind(rep("IA", nrow(data)),rep(city, nrow(data)),as.character(data$SPpCode), as.character(data$SPeciesName),data$SP_count_0_30, data$SP_count_30_61,data$SP_count_GT61, data$tot_SP_count)), colnames(big_data)))
}
for (i in 1:length(files_WI))
{
  city<-gsub("_street.*","", files_WI[i])
  setwd('~/Downloads/IA_and_WI_2/WI')
  data<-read.sas7bdat(files_WI[i])
  colnames(data)<-gsub("Sp", "SP", colnames(data))
  colnames(data)<-gsub("sp", "SP", colnames(data))
  big_data<-rbind(big_data, setNames(data.frame(cbind(rep("WI", nrow(data)),rep(city, nrow(data)),as.character(data$SPpCode), as.character(data$SPeciesName),data$SP_count_0_30, data$SP_count_30_61,data$SP_count_GT61, data$tot_SP_count)), colnames(big_data)))
}
for (i in 1:length(files_CO))
{
  city<-gsub("_street.*","", files_CO[i])
  setwd('~/Downloads/CO_MI_NJ/CO')
  data<-read.sas7bdat(files_CO[i])
  colnames(data)<-gsub("Sp", "SP", colnames(data))
  colnames(data)<-gsub("sp", "SP", colnames(data))
  big_data<-rbind(big_data, setNames(data.frame(cbind(rep("CO", nrow(data)),rep(city, nrow(data)),as.character(data$SPpCode), as.character(data$SPeciesName),data$SP_count_0_30, data$SP_count_30_61,data$SP_count_GT61, data$tot_SP_count)), colnames(big_data)))
}
for (i in 1:length(files_MI))
{
  city<-gsub("_street.*","", files_MI[i])
  setwd('~/Downloads/CO_MI_NJ/MI')
  data<-read.sas7bdat(files_MI[i])
  colnames(data)<-gsub("Sp", "SP", colnames(data))
  colnames(data)<-gsub("sp", "SP", colnames(data))
  big_data<-rbind(big_data, setNames(data.frame(cbind(rep("MI", nrow(data)),rep(city, nrow(data)),as.character(data$SPpCode), as.character(data$SPeciesName),data$SP_count_0_30, data$SP_count_30_61,data$SP_count_GT61, data$tot_SP_count)), colnames(big_data)))
}
for (i in 1:length(files_NJ))
{
  city<-gsub("_street.*","", files_NJ[i])
  setwd('~/Downloads/CO_MI_NJ/NJ')
  data<-read.sas7bdat(files_NJ[i])
  colnames(data)<-gsub("Sp", "SP", colnames(data))
  colnames(data)<-gsub("sp", "SP", colnames(data))
  big_data<-rbind(big_data, setNames(data.frame(cbind(rep("NJ", nrow(data)),rep(city, nrow(data)),as.character(data$SPpCode), as.character(data$SPeciesName),data$SP_count_0_30, data$SP_count_30_61,data$SP_count_GT61, data$tot_SP_count)), colnames(big_data)))
}
for (i in 1:length(files_MO))
{
  city<-gsub("_street.*","", files_MO[i])
  setwd('~/Downloads/MO_PA_RI/MO')
  data<-read.sas7bdat(files_MO[i])
  colnames(data)<-gsub("Sp", "SP", colnames(data))
  colnames(data)<-gsub("sp", "SP", colnames(data))
  big_data<-rbind(big_data, setNames(data.frame(cbind(rep("MO", nrow(data)),rep(city, nrow(data)),as.character(data$SPpCode), as.character(data$SPeciesName),data$SP_count_0_30, data$SP_count_30_61,data$SP_count_GT61, data$tot_SP_count)), colnames(big_data)))
}
for (i in 1:length(files_PA))
{
  city<-gsub("_street.*","", files_PA[i])
  setwd('~/Downloads/MO_PA_RI/PA')
  data<-read.sas7bdat(files_PA[i])
  colnames(data)<-gsub("Sp", "SP", colnames(data))
  colnames(data)<-gsub("sp", "SP", colnames(data))
  big_data<-rbind(big_data, setNames(data.frame(cbind(rep("PA", nrow(data)),rep(city, nrow(data)),as.character(data$SPpCode), as.character(data$SPeciesName),data$SP_count_0_30, data$SP_count_30_61,data$SP_count_GT61, data$tot_SP_count)), colnames(big_data)))
}
for (i in 1:length(files_RI2))
{
  city<-gsub("_street.*","", files_RI2[i])
  setwd('~/Downloads/MO_PA_RI/RI')
  data<-read.sas7bdat(files_RI2[i])
  colnames(data)<-gsub("Sp", "SP", colnames(data))
  colnames(data)<-gsub("sp", "SP", colnames(data))
  big_data<-rbind(big_data, setNames(data.frame(cbind(rep("RI", nrow(data)),rep(city, nrow(data)),as.character(data$SPpCode), as.character(data$SPeciesName),data$SP_count_0_30, data$SP_count_30_61,data$SP_count_GT61, data$tot_SP_count)), colnames(big_data)))
}
for (i in 1:length(files_IL))
{
  city<-gsub("_street.*","", files_IL[i])
  setwd('~/Downloads/IL_and_WA/IL')
  data<-read.sas7bdat(files_IL[i])
  colnames(data)<-gsub("Sp", "SP", colnames(data))
  colnames(data)<-gsub("sp", "SP", colnames(data))
  big_data<-rbind(big_data, setNames(data.frame(cbind(rep("IL", nrow(data)),rep(city, nrow(data)),as.character(data$SPpCode), as.character(data$SPeciesName),data$SP_count_0_30, data$SP_count_30_61,data$SP_count_GT61, data$tot_SP_count)), colnames(big_data)))
}
for (i in 1:length(files_WA))
{
  city<-gsub("_street.*","", files_WA[i])
  setwd('~/Downloads/IL_and_WA/WA')
  data<-read.sas7bdat(files_WA[i])
  colnames(data)<-gsub("Sp", "SP", colnames(data))
  colnames(data)<-gsub("sp", "SP", colnames(data))
  big_data<-rbind(big_data, setNames(data.frame(cbind(rep("WA", nrow(data)),rep(city, nrow(data)),as.character(data$SPpCode), as.character(data$SPeciesName),data$SP_count_0_30, data$SP_count_30_61,data$SP_count_GT61, data$tot_SP_count)), colnames(big_data)))
}
for (i in 1:length(files_MT))
{
  city<-gsub("_street.*","", files_MT[i])
  setwd('~/Downloads/KS_MT_ND_NE_SD_UT/MT')
  data<-read.sas7bdat(files_MT[i])
  colnames(data)<-gsub("Sp", "SP", colnames(data))
  colnames(data)<-gsub("sp", "SP", colnames(data))
  big_data<-rbind(big_data, setNames(data.frame(cbind(rep("MT", nrow(data)),rep(city, nrow(data)),as.character(data$SPpCode), as.character(data$SPeciesName),data$SP_count_0_30, data$SP_count_30_61,data$SP_count_GT61, data$tot_SP_count)), colnames(big_data)))
}
for (i in 1:length(files_ND))
{
  city<-gsub("_street.*","", files_ND[i])
  setwd('~/Downloads/KS_MT_ND_NE_SD_UT/ND')
  data<-read.sas7bdat(files_ND[i])
  colnames(data)<-gsub("Sp", "SP", colnames(data))
  colnames(data)<-gsub("sp", "SP", colnames(data))
  big_data<-rbind(big_data, setNames(data.frame(cbind(rep("ND", nrow(data)),rep(city, nrow(data)),as.character(data$SPpCode), as.character(data$SPeciesName),data$SP_count_0_30, data$SP_count_30_61,data$SP_count_GT61, data$tot_SP_count)), colnames(big_data)))
}
for (i in 1:length(files_NE))
{
  city<-gsub("_street.*","", files_NE[i])
  setwd('~/Downloads/KS_MT_ND_NE_SD_UT/NE')
  data<-read.sas7bdat(files_NE[i])
  colnames(data)<-gsub("Sp", "SP", colnames(data))
  colnames(data)<-gsub("sp", "SP", colnames(data))
  big_data<-rbind(big_data, setNames(data.frame(cbind(rep("NE", nrow(data)),rep(city, nrow(data)),as.character(data$SPpCode), as.character(data$SPeciesName),data$SP_count_0_30, data$SP_count_30_61,data$SP_count_GT61, data$tot_SP_count)), colnames(big_data)))
}
for (i in 1:length(files_SD))
{
  city<-gsub("_street.*","", files_SD[i])
  setwd('~/Downloads/KS_MT_ND_NE_SD_UT/SD')
  data<-read.sas7bdat(files_SD[i])
  colnames(data)<-gsub("Sp", "SP", colnames(data))
  colnames(data)<-gsub("sp", "SP", colnames(data))
  big_data<-rbind(big_data, setNames(data.frame(cbind(rep("SD", nrow(data)),rep(city, nrow(data)),as.character(data$SPpCode), as.character(data$SPeciesName),data$SP_count_0_30, data$SP_count_30_61,data$SP_count_GT61, data$tot_SP_count)), colnames(big_data)))
}
for (i in 1:length(files_FL))
{
  city<-gsub("_street.*","", files_FL[i])
  setwd('~/Downloads/FL_and_OR/FL')
  data<-read.sas7bdat(files_FL[i])
  colnames(data)<-gsub("Sp", "SP", colnames(data))
  colnames(data)<-gsub("sp", "SP", colnames(data))
  big_data<-rbind(big_data, setNames(data.frame(cbind(rep("FL", nrow(data)),rep(city, nrow(data)),as.character(data$SPpCode), as.character(data$SPeciesName),data$SP_count_0_30, data$SP_count_30_61,data$SP_count_GT61, data$tot_SP_count)), colnames(big_data)))
}
for (i in 1:length(files_OR))
{
  city<-gsub("_street.*","", files_OR[i])
  setwd('~/Downloads/FL_and_OR/OR')
  data<-read.sas7bdat(files_OR[i])
  colnames(data)<-gsub("Sp", "SP", colnames(data))
  colnames(data)<-gsub("sp", "SP", colnames(data))
  big_data<-rbind(big_data, setNames(data.frame(cbind(rep("OR", nrow(data)),rep(city, nrow(data)),as.character(data$SPpCode), as.character(data$SPeciesName),data$SP_count_0_30, data$SP_count_30_61,data$SP_count_GT61, data$tot_SP_count)), colnames(big_data)))
}
big_data$small<-as.numeric(big_data$small)
big_data$med<-as.numeric(big_data$med)
big_data$large<-as.numeric(big_data$large)

big_data$genus<-gsub("\\s.*","", big_data$SP)
big_data$genus<-tolower(big_data$genus)
big_data$small[which(is.nan(big_data$small))]<-0
big_data$med[which(is.nan(big_data$med))]<-0
big_data$large[which(is.nan(big_data$large))]<-0

genera<-read.csv('~/Desktop/OneDrive - McGill University/Grad/scripts/updated_hosts_frank.csv')[,1]
big_data<-subset(big_data, genus%in%tolower(genera))
write.csv(big_data, file="street_trees_species.csv", row.names=F)
total_small<-aggregate(big_data$small~big_data$genus+big_data$city, FUN=sum, drop=F)
total_med<-aggregate(big_data$med~big_data$genus+big_data$city, FUN=sum, drop=F)
total_large<-aggregate(big_data$large~big_data$genus+big_data$city, FUN=sum, drop=F)


colnames(total_small)[1:2]<-c("genus","city")
big_data<-merge(big_data, cbind(total_small, total_med[,3],total_large[,3]),by=c("genus","city"))
big_data<-subset(big_data, is.na(SP)==F)
big_data$`big_data$small`[which(is.na(big_data$`big_data$small`)==T)]<-0
big_data$`total_med[, 3]`[which(is.na(big_data$`total_med[, 3]`)==T)]<-0
big_data$`total_large[, 3]`[which(is.na(big_data$`total_large[, 3]`)==T)]<-0

big_data$prop_small<-big_data$small/big_data$`big_data$small`
big_data$prop_med<-big_data$med/big_data$`total_med[, 3]`
big_data$prop_large<-big_data$large/big_data$`total_large[, 3]`
big_data$prop_small[which(is.nan(big_data$prop_small)==T)]<-NA
big_data$prop_med[which(is.nan(big_data$prop_med)==T)]<-NA
big_data$prop_large[which(is.nan(big_data$prop_large)==T)]<-NA
big_data$prop_small[which((big_data$prop_small==Inf)==T)]<-NA
big_data$prop_med[which((big_data$prop_med==Inf)==T)]<-NA
big_data$prop_large[which((big_data$prop_large==Inf)==T)]<-NA
big_data$genus<-tolower(big_data$genus)
big_data$SP<-tolower(big_data$SP)
big_data$SP<-gsub("spp.", "", big_data$SP)
big_data$SP<-gsub("species", "", big_data$SP)
big_data$SP<-gsub("spp", "", big_data$SP)
big_data$SP<-gsub("\\s$", "", big_data$SP)
big_data$SP<-gsub("sp.", "", big_data$SP)
big_data$SP<-gsub("\\s\\.", "", big_data$SP)
big_data$SP<-gsub("\\s$", "", big_data$SP)
big_data$SP<-gsub("\\sx", "", big_data$SP)

city_total_small<-aggregate(big_data$small~big_data$city+big_data$state, FUN=sum)
city_total_med<-aggregate(big_data$med~big_data$city+big_data$state, FUN=sum)
city_total_large<-aggregate(big_data$large~big_data$city+big_data$state, FUN=sum)
city_totals<-merge(city_total_small, city_total_med, by="big_data$city")
city_totals<-merge(city_totals, city_total_large, by="big_data$city")

city_totals<-city_totals[,c(1,2,3,5,7)]
colnames(city_totals)<-c("city", "state","small", "med", "large")

write.csv(city_totals, file="~/Downloads/street_trees_city.csv", row.names=F)
big_data2<-subset(big_data, genus%in%tolower(genera))
for (j in 1:length(unique(big_data$city)))
{
  bdsub<-subset(big_data2, city%in%unique(big_data$city)[j])
  gensub<-unique(bdsub$genus)
  for (i in 1:length(gensub))
  {
    allspp<-unique(big_data2$SppCode[which(big_data2$genus%in%gensub[i])])
    missing<-allspp[which(allspp%in%bdsub$SppCode==F)]
    if (gensub[i]%in%bdsub$genus)
    {
      genssub<-subset(bdsub, genus%in%gensub[i])[1,]
      big_data2<-rbind(big_data2, setNames(data.frame(cbind(rep(gensub[i], length(missing)),rep(as.character(bdsub[1,2]), length(missing)),rep(as.character(bdsub[1,3]), length(missing)), as.character(missing), rep(0, length(missing)), rep(0, length(missing)),rep(0, length(missing)),rep(0, length(missing)),rep(0, length(missing)),rep(genssub[,10], length(missing) ),rep(genssub[,11], length(missing) ),rep(genssub[,12], length(missing) ),rep(ifelse(is.na(genssub[1,13]),NA,0), length(missing)),rep(ifelse(is.na(genssub[1,14]),NA,0), length(missing)),rep(ifelse(is.na(genssub[1,15]),NA,0), length(missing)) )), colnames(big_data2)))
    }
  }
}
big_data2$prop_small<-as.numeric(big_data2$prop_small)
big_data2$prop_med<-as.numeric(big_data2$prop_med)
big_data2$prop_large<-as.numeric(big_data2$prop_large)
big_data2$prop_small[which(is.na(big_data2$prop_small))]<-0
big_data2$prop_med[which(is.na(big_data2$prop_med))]<-0
big_data2$prop_large[which(is.na(big_data2$prop_large))]<-0
write.csv(big_data2, file="eachspp_eachcityprop.csv", row.names=F)
summary<-aggregate(big_data2$prop_small~big_data2$SppCode, FUN=mean, na.rm=T)
summary2<-aggregate(big_data2$prop_med~big_data2$SppCode, FUN=mean, na.rm=T)
summary3<-aggregate(big_data2$prop_large~big_data2$SppCode, FUN=mean, na.rm=T)

colnames(summary)[1]<-colnames(summary2)[1]<-colnames(summary3)[1]<-c("SppCode")
all_prop<-merge(summary, summary2, 'SppCode', all.x=T,all.y=T)
all_prop<-merge(all_prop, summary3, 'SppCode', all.x=T,all.y=T)
colnames(all_prop)[2:4]<-c('prop_small', 'prop_med', 'prop_large')
#all_prop<-merge(all_prop, unique(big_data[,c("SppCode", "SP")]), by="SppCode", all.y=F)
write.csv(all_prop, file="all_prop_mostdat.csv", row.names=F)


big_data2$small<-as.numeric(big_data2$small)
big_data2$small[which(is.nan(big_data2$small))]<-0
big_data2$med<-as.numeric(big_data2$med)
big_data2$med[which(is.nan(big_data2$med))]<-0
big_data2$large<-as.numeric(big_data2$large)
big_data2$large[which(is.nan(big_data2$large))]<-0
gen_small<-aggregate(big_data2$small~big_data2$genus+big_data2$city, FUN=sum, drop=F)
gen_small<-subset(gen_small, gen_small[,2]!=0)
gen_small[is.na(gen_small[,3]),3]<-0

gen_med<-aggregate(big_data2$med~big_data2$genus+big_data2$city, FUN=sum, drop=F)
gen_med<-subset(gen_med, gen_med[,2]!=0)
gen_med[is.na(gen_med[,3]),3]<-0


gen_large<-aggregate(big_data2$large~big_data2$genus+big_data2$city, FUN=sum, drop=F)
gen_large<-subset(gen_large, gen_large[,2]!=0)
gen_large[is.na(gen_large[,3]),3]<-0

genus_lev<-cbind(gen_small, gen_med[,3], gen_large[,3])
colnames(genus_lev)<-c("genus", "city","small", "med", "large" )

citystate<-unique(cbind(as.character(big_data$city), as.character(big_data$state)))
genus_lev<-cbind(genus_lev, citystate[match(genus_lev$city, citystate[,1]),2])
colnames(genus_lev)[6]<-"state"
write.csv(genus_lev, file="~/Downloads/genus_level_streettrees.csv", row.names=F)
for (i in 1:48)
{
  png(paste0(unique(rich_dat$genus)[i], "pred.png"))
  sub<-subset(rich_dat, genus==unique(rich_dat$genus)[i])
  plot(predict.gam(all_small_mod, type="response", newdata=sub)~sub$small, main=unique(rich_dat$genus)[i])
  points(y=predict.gam(all_small_mod, type="response", newdata=sub)[which(sub$STFIPS.x==25)],x=sub$small[which(sub$STFIPS.x==25)], col="red")
  points(y=predict.gam(all_small_mod, type="response", newdata=sub)[which(sub$STFIPS.x==48)],x=sub$small[which(sub$STFIPS.x==48)], col="blue")
  abline(0,1)
  dev.off()
}