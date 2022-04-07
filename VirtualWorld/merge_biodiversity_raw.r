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
biod_with_na_list<-list()
biod_without_na_list<-list()
es_list<-list()
for (i in c(1:nrow(seeds))){
  print(i)
  seed<-seeds[i]
  folder<-sprintf("../Objects/virtual_lands/items/%d", i)
  target<-sprintf("%s/biodiversity.rda", folder)
  if (T){
    biod<-readRDS(target)
    biod_full<-biod
    biod_full$index<-i
    biod_full<-merge(biod_full, seed, by="index")
    biod_without_na_list[[length(biod_without_na_list)+1]]<-biod_full
    #simpson 1.0000000 
    #shannon 0.000000
    #invsimp Inf          
    #unbias.simp -1          
    #alpha 1                
    #species.richness 0 
    #Pielou's evenness 0
    biod<-merge(biod, full_mask, by=c("mask", "res"), all=T)
    NoData<-biod[is.na(v), .(N_NoData=.N), by=list(res)]
    biod<-merge(biod, NoData, by="res", all=T)
    biod[is.na(N_NoData)]$N_NoData<-0
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
    
    biod_full<-biod
    biod_full$index<-i
    biod_full<-merge(biod_full, seed, by="index")
    biod_with_na_list[[length(biod_with_na_list)+1]]<-biod_full
  }
  es<-readRDS(sprintf("%s/es.rda", folder))
  es$available<-!is.na(es$es)
  es$index<-i
  es<-merge(es, seed, by="index")
  es_list[[length(es_list)+1]]<-es
}
biod_with_na_list<-rbindlist(biod_with_na_list)
biod_without_na_list<-rbindlist(biod_without_na_list)
es_list<-rbindlist(es_list)

saveRDS(biod_with_na_list, "../Objects/biod_with_na_raw.rda")
saveRDS(biod_without_na_list, "../Objects/biod_without_na_raw.rda")
saveRDS(es_list, "../Objects/es_raw.rda")


