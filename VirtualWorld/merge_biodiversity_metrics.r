library(raster)
library(data.table)
library(ggplot2)
library(sf)
library(dplyr)
library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(vegan)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
seeds<-readRDS("../Objects/virtual_lands/virtual_lands_property.rda")
i=1
full_mask<-readRDS("../Objects/full_mask.rda")
biod_se_list<-list()
es_se_list<-list()
for (i in c(1:nrow(seeds))){
  print(i)
  seed<-seeds[i]
  folder<-sprintf("../Objects/virtual_lands/items/%d", i)
  target<-sprintf("%s/biodiversity.rda", folder)
  if (T){
    biod<-readRDS(target)
    biod_se_without_NA<-biod[, .(simpson=mean(v, na.rm=T), sd_simpson=sd(v, na.rm = T),
                                 shannon=mean(shannon, na.rm=T), sd_shannon=sd(shannon, na.rm = T),
                                 invsimp=mean(invsimp, na.rm=T), sd_invsimp=sd(invsimp, na.rm = T),
                                 unbias.simp=mean(unbias.simp, na.rm=T), sd_unbias.simp=sd(unbias.simp, na.rm = T),
                                 alpha=mean(alpha, na.rm=T), sd_alpha=sd(alpha, na.rm = T),
                                 species.richness=mean(species.richness, na.rm=T), sd_species.richness=sd(species.richness, na.rm = T),
                                 Pielou_evenness=mean(J, na.rm=T), sd_Pielou_evenness=sd(J, na.rm = T),
                                 Hill_0=mean(Hill_0, na.rm=T), sd_Hill_0=sd(Hill_0, na.rm = T),
                                 Hill_0.25=mean(Hill_0.25, na.rm=T), sd_Hill_0.25=sd(Hill_0.25, na.rm = T),
                                 Hill_0.5=mean(Hill_0.5, na.rm=T), sd_Hill_0.5=sd(Hill_0.5, na.rm = T),
                                 Hill_1=mean(Hill_1, na.rm=T), sd_Hill_1=sd(Hill_1, na.rm = T),
                                 Hill_2=mean(Hill_2, na.rm=T), sd_Hill_2=sd(Hill_2, na.rm = T),
                                 Hill_4=mean(Hill_4, na.rm=T), sd_Hill_4=sd(Hill_4, na.rm = T),
                                 Hill_8=mean(Hill_8, na.rm=T), sd_Hill_8=sd(Hill_8, na.rm = T),
                                 Hill_16=mean(Hill_16, na.rm=T), sd_Hill_16=sd(Hill_16, na.rm = T),
                                 Hill_32=mean(Hill_32, na.rm=T), sd_Hill_32=sd(Hill_32, na.rm = T),
                                 Hill_64=mean(Hill_64, na.rm=T), sd_Hill_64=sd(Hill_64, na.rm = T),
                                 Hill_Inf=mean(Hill_Inf, na.rm=T), sd_Hill_Inf=sd(Hill_Inf, na.rm = T)), 
                             by=list(res)]
    biod_se_without_NA$NA_Type<-"Without NA"
    #simpson 1.0000000 
    #shannon 0.000000
    #invsimp Inf          
    #unbias.simp -1          
    #alpha 1                
    #species.richness 0 
    #Pielou's evenness 0
    biod<-merge(biod, full_mask, by=c("mask", "res"), all=T)
    NoData<-biod[is.na(v), .(N_NoData=.N), by=list(res)]
    biod[is.na(v)]$v<-1
    biod[is.na(shannon)]$shannon<-0
    biod[is.na(invsimp)]$invsimp<-Inf
    biod[is.na(unbias.simp)]$unbias.simp<- -1
    biod[is.na(alpha)]$alpha<-1
    biod[is.na(species.richness)]$species.richness<-0
    biod[is.na(J)]$J<-0
    biod[is.na(Hill_0)]$Hill_0<-0
    biod[is.na(Hill_0.25)]$Hill_0.25<-0
    biod[is.na(Hill_0.5)]$Hill_0.5<-0
    biod[is.na(Hill_1)]$Hill_1<-0
    biod[is.na(Hill_2)]$Hill_2<-0
    biod[is.na(Hill_4)]$Hill_4<-0
    biod[is.na(Hill_8)]$Hill_8<-0
    biod[is.na(Hill_16)]$Hill_16<-0
    biod[is.na(Hill_32)]$Hill_32<-0
    biod[is.na(Hill_64)]$Hill_64<-0
    biod[is.na(Hill_Inf)]$Hill_Inf<-0
    
    biod_se_with_NA<-biod[, .(simpson=mean(v, na.rm=T), sd_simpson=sd(v, na.rm = T),
                              shannon=mean(shannon, na.rm=T), sd_shannon=sd(shannon, na.rm = T),
                              invsimp=mean(invsimp, na.rm=T), sd_invsimp=sd(invsimp, na.rm = T),
                              unbias.simp=mean(unbias.simp, na.rm=T), sd_unbias.simp=sd(unbias.simp, na.rm = T),
                              alpha=mean(alpha, na.rm=T), sd_alpha=sd(alpha, na.rm = T),
                              species.richness=mean(species.richness, na.rm=T), sd_species.richness=sd(species.richness, na.rm = T),
                              Pielou_evenness=mean(J, na.rm=T), sd_Pielou_evenness=sd(J, na.rm = T),
                              Hill_0=mean(Hill_0, na.rm=T), sd_Hill_0=sd(Hill_0, na.rm = T),
                              Hill_0.25=mean(Hill_0.25, na.rm=T), sd_Hill_0.25=sd(Hill_0.25, na.rm = T),
                              Hill_0.5=mean(Hill_0.5, na.rm=T), sd_Hill_0.5=sd(Hill_0.5, na.rm = T),
                              Hill_1=mean(Hill_1, na.rm=T), sd_Hill_1=sd(Hill_1, na.rm = T),
                              Hill_2=mean(Hill_2, na.rm=T), sd_Hill_2=sd(Hill_2, na.rm = T),
                              Hill_4=mean(Hill_4, na.rm=T), sd_Hill_4=sd(Hill_4, na.rm = T),
                              Hill_8=mean(Hill_8, na.rm=T), sd_Hill_8=sd(Hill_8, na.rm = T),
                              Hill_16=mean(Hill_16, na.rm=T), sd_Hill_16=sd(Hill_16, na.rm = T),
                              Hill_32=mean(Hill_32, na.rm=T), sd_Hill_32=sd(Hill_32, na.rm = T),
                              Hill_64=mean(Hill_64, na.rm=T), sd_Hill_64=sd(Hill_64, na.rm = T),
                              Hill_Inf=mean(Hill_Inf, na.rm=T), sd_Hill_Inf=sd(Hill_Inf, na.rm = T)), 
                          by=list(res)]
    
    biod_se_with_NA$NA_Type<-"With NA"
    biod_se<-rbindlist(list(biod_se_with_NA, biod_se_without_NA))
    biod_se<-merge(biod_se, NoData, by="res", all=T)
    biod_se[is.na(N_NoData)]$N_NoData<-0
    biod_se$index<-i
    biod_se_list[[length(biod_se_list)+1]]<-biod_se
  }
  es<-readRDS(sprintf("%s/es.rda", folder))
  es$available<-!is.na(es$es)
  es_se<-es[, .(N_Cell=.N, es=mean(es, na.rm=T), sd_es=sd(es, na.rm=T)), 
            by=list(res, esNum, available)]
  es_se$index<-i
  es_se_list[[length(es_se_list)+1]]<-es_se
}
biod_se_list<-rbindlist(biod_se_list)
saveRDS(biod_se_list, "../Objects/biod_metrics.rda")
es_se_list<-rbindlist(es_se_list)
saveRDS(es_se_list, "../Objects/es_metrics.rda")

virtual_lands_property<-readRDS("../Objects/virtual_lands/virtual_lands_property.rda")
biod_se_list<-merge(biod_se_list, virtual_lands_property, by="index")
saveRDS(biod_se_list, "../Objects/biod_metrics.rda")
es_se_list<-merge(es_se_list, virtual_lands_property, by="index")
saveRDS(es_se_list, "../Objects/es_metrics.rda")
