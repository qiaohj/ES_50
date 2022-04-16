library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
sp<-readRDS("../Tables/eBird_Checklist/ebird_state_checklist.rda")
iucn_status<-data.table(read.csv("../Tables/redlist_species_data_3752fd69-a2ab-4550-9648-6dd9736eaa78/simple_summary.csv", stringsAsFactors = F))
cols<-c("scientificName", "orderName", "familyName", "redlistCategory")
iucn_status<-iucn_status[, ..cols]
sp_iucn_stauts<-merge(sp, iucn_status, by.x="SCIENTIFIC_NAME", by.y="scientificName")
unique(sp_iucn_stauts$redlistCategory)

#remove , "Extinct", "Extinct in the Wild" , "Data Deficient"
sp_iucn_stauts<-sp_iucn_stauts[redlistCategory %in% 
                                 c("Least Concern", "Near Threatened", "Vulnerable", "Endangered",
                                   "Critically Endangered")]
sp_iucn_stauts_country<-sp_iucn_stauts[, .(N_Species=length(unique(SCIENTIFIC_NAME)), N=sum(N), 
                                           mean_N=mean(N),
                                           N_Observer=sum(N_Observer)), 
                                       by=list(COUNTRY, COUNTRY_CODE, GID_0,
                                               redlistCategory)]
sp_N_country<-sp_iucn_stauts[, .(N_Species_all=length(unique(SCIENTIFIC_NAME)), N_all=sum(N), N_Observer_all=sum(N_Observer)), 
                                       by=list(COUNTRY)]
sp_iucn_stauts_country<-merge(sp_iucn_stauts_country,sp_N_country, by="COUNTRY")
sp_iucn_stauts_country$N_P<-sp_iucn_stauts_country$N/sp_iucn_stauts_country$N_all
sp_iucn_stauts_country$N_species_P<-sp_iucn_stauts_country$N_Species/sp_iucn_stauts_country$N_Species_all


ggplot(sp_iucn_stauts_country)+geom_point(aes(x=redlistCategory, y=N_P))
p<-ggplot(sp_iucn_stauts_country)+geom_boxplot(aes(x=redlistCategory, y=N_P*100))+
  scale_y_log10()+theme_bw()+labs(x="IUCN Red List Categories and Criteria", y="Proportion (%) of number of observation")
ggsave(p, filename="../Figures/IUCN_Based_VW/redlist_category_p_observations.png", width=6, height=4)
p<-ggplot(sp_iucn_stauts_country)+geom_boxplot(aes(x=redlistCategory, y=N_species_P*100))+
  scale_y_log10()+theme_bw()+labs(x="IUCN Red List Categories and Criteria", y="Proportion (%) of number of species")
p
ggsave(p, filename="../Figures/IUCN_Based_VW/redlist_category_p_species.png", width=6, height=4)

p<-ggplot(sp_iucn_stauts_country)+geom_boxplot(aes(x=redlistCategory, y=mean_N))+
  scale_y_log10()+theme_bw()+labs(x="IUCN Red List Categories and Criteria", y="mean number of observations per speciees")
p
ggsave(p, filename="../Figures/IUCN_Based_VW/redlist_category_number_of_observation_per_species.png", width=6, height=4)

sum(sp_iucn_stauts_country[,.(N_P=mean(N_P)), by=list(redlistCategory)]$N_P)

quantiles<-quantile(sp_iucn_stauts_country$mean_N, c(0.25, 0.75))
IQR<-quantiles[2]-quantiles[1]
upperlimit<-quantiles[2]+1.5*IQR

p<-ggplot(sp_iucn_stauts_country)+geom_point(aes(x=redlistCategory, y=mean_N))+
  theme_bw()+labs(x="IUCN Red List Categories and Criteria", y="mean number of observations per speciees")
p
sp_iucn_stauts_country[mean_N<=upperlimit,.(mean_N=mean(mean_N)), by=list(redlistCategory)]
sp_iucn_stauts_country[,.(mean_N=mean(mean_N)), by=list(redlistCategory)]


sp_country<-sp_iucn_stauts[, .(N_Species=length(unique(SCIENTIFIC_NAME)), N=sum(N), 
                                           mean_N=mean(N),
                                           N_Observer=sum(N_Observer)), 
                                       by=list(SCIENTIFIC_NAME, COUNTRY, COUNTRY_CODE, GID_0,
                                               redlistCategory)]
target_species_list<-sp_country[N<1e6]
ggplot(sp_country)+geom_histogram(aes(x=N), bins=100)+
  geom_vline(aes(xintercept=5e3))+
  scale_x_log10()
ggplot(sp_country[N<=5e3])+geom_histogram(aes(x=N), bins=100)+
  geom_vline(aes(xintercept=5e3))

quantiles<-quantile(sp_country$N, c(0.25, 0.75))
IQR<-quantiles[2] - quantiles[1]
upperlimit<-quantiles[2]+1.5*IQR


quantiles<-quantile(sp_country$N, c(0.25, 0.75))
IQR<-2*quantiles[2]
upperlimit<-quantiles[2]+1.5*IQR

sd<-sd(sp_country$N)

upperlimit_sd<-mean(sp_country$N)+3*sd

upperlimit<-1e6
target_list<-sp_country[N<=upperlimit]
cols<-c("SCIENTIFIC_NAME", "COUNTRY")
target_list<-target_list[, ..cols]

sp_iucn_stauts_filtered<-merge(sp_iucn_stauts, target_list, by=c("SCIENTIFIC_NAME", "COUNTRY"))

sp_iucn_stauts_country_filtered<-sp_iucn_stauts_filtered[, .(N_Species=length(unique(SCIENTIFIC_NAME)), N=sum(N), 
                                           mean_N=mean(N),
                                           N_Observer=sum(N_Observer)), 
                                       by=list(COUNTRY, COUNTRY_CODE, GID_0,
                                               redlistCategory)]
sp_N_country_filtered<-sp_iucn_stauts_filtered[, .(N_Species_all=length(unique(SCIENTIFIC_NAME)), N_all=sum(N), N_Observer_all=sum(N_Observer)), 
                             by=list(COUNTRY)]
sp_iucn_stauts_country_filtered<-merge(sp_iucn_stauts_country_filtered,sp_N_country_filtered, by="COUNTRY")
sp_iucn_stauts_country_filtered$N_P<-sp_iucn_stauts_country_filtered$N/sp_iucn_stauts_country_filtered$N_all
sp_iucn_stauts_country_filtered$N_species_P<-sp_iucn_stauts_country_filtered$N_Species/sp_iucn_stauts_country_filtered$N_Species_all


ggplot(sp_iucn_stauts_country_filtered)+geom_point(aes(x=redlistCategory, y=N_P))
p<-ggplot(sp_iucn_stauts_country_filtered)+geom_boxplot(aes(x=redlistCategory, y=N_P*100))+
  scale_y_log10()+theme_bw()+labs(x="IUCN Red List Categories and Criteria", y="Proportion (%) of number of observation")
p
ggsave(p, filename="../Figures/IUCN_Based_VW/redlist_category_number_of_observation_p_iqr_thresholded.png", width=6, height=4)

sum(sp_iucn_stauts_country_filtered[,.(N_P=mean(N_P)), by=list(redlistCategory)]$N_P)

sp_iucn_stauts_country_filtered[,.(mean_N=mean(mean_N), sd_N=sd(mean_N)), by=list(redlistCategory)]

p<-ggplot(sp_iucn_stauts_country_filtered)+geom_boxplot(aes(x=redlistCategory, y=mean_N))+
  theme_bw()+labs(x="IUCN Red List Categories and Criteria", y="mean number of observations per speciees")
p
ggsave(p, filename="../Figures/IUCN_Based_VW/redlist_category_number_of_observation_per_species.png", width=6, height=4)
