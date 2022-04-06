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
library(entropart)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")

full_mask<-readRDS("../Objects/full_mask.rda")
virtual_species<-readRDS("../Objects/virtual_species/virtual_species.rda")
species_points<-st_as_sf(virtual_species, coords = c("x", "y"))
random_sampling<-readRDS("../Objects/virtual_observing_events/random_observing_events.rda")
collected_sp_index<-st_contains(random_sampling, species_points)
f<-"../Objects/virtual_observing_events/random_observations.rda"
saveRDS(collected_sp_index, f)
occ_index<-readRDS(f)
occ_index<-unlist(occ_index)
occ<-virtual_species[ID %in% occ_index]
occ$probability<-runif(nrow(occ), 0, 1)
occ<-occ[probability>=0.5]
if (F){
  ggplot(occ[sample(nrow(occ), 2000)])+geom_point(aes(x=x, y=y))+
    geom_sf(data=random_sampling, fill=NA)
}

j=1
biodiversity_list<-list()
es_list<-list()

for (res in c("100km", "50km", "20km", "10km", "5km", "2km", "1km")){
  occ_se<-occ[, .(N=.N), by=c(sprintf("mask_%s", res), "sp", "sp_id")]
  colnames(occ_se)[1]<-"mask"
  for (es_threshold in c(200, 100, 50, 20, 10)){
    print(paste(res, es_threshold))
    esNum<-es_threshold
    es = occ_se %>%
      group_split(mask) %>%
      map(~ .x %>%
            group_by(sp_id) %>%
            summarize(occCount = sum(N))
      ) %>%
      map(~ deframe(.x)) %>%
      modify_if(~ length(.x) <= esNum, ~ NA) %>% # run only if more than 50 species
      modify_if(~ !anyNA(.x), ~ Hurlbert(.x, esNum)) %>%
      map(~ unname(.x)) %>%
      flatten_dbl()
    
    cell = occ_se %>%
      group_split(mask) %>%
      map(~ .x %>% mutate(mask = as.character(mask))) %>%
      map_chr(~ unique(.x$mask))
    
    es50Table = tibble(cell,es)
    es50Table$res<-res
    es50Table$esNum<-es_threshold
    es_list[[length(es_list)+1]]<-es50Table
  }
  
  t_m<-pivot_wider(occ_se, id_cols=mask, names_from = sp, values_from=N, values_fill=0)
  
  d2<-t_m[,-1]
  location<-t_m$mask
  biodiversity_metrics<-data.frame(mask=location, res=res, metrics="simpson",
                                   v = diversity(d2, "simpson"))
  
  biodiversity_metrics$shannon<-diversity(d2, "shannon")
  biodiversity_metrics$invsimp<-diversity(d2, "invsimpson")
  biodiversity_metrics$unbias.simp<-rarefy(d2, 100) - 1
  biodiversity_metrics$alpha <- fisher.alpha(d2)
  biodiversity_metrics$species.richness <- specnumber(d2) ## rowSums(BCI > 0) does the same...
  biodiversity_metrics$J <- biodiversity_metrics$shannon/log(biodiversity_metrics$species.richness)
  hill<-renyi(d2, hill=T)
  for (k in 1:length(hill)){
    biodiversity_metrics[,sprintf("Hill_%s", names(hill)[k])]<-hill[[k]]
  }
  biodiversity_list[[length(biodiversity_list)+1]]<-biodiversity_metrics
}
biodiversity_lista<-rbindlist(biodiversity_list)
es_lista<-rbindlist(es_list)

saveRDS(biodiversity_lista, "../Objects/virtual_observing_events/random_biodiversity_metrics.rda")
saveRDS(es_lista, "../Objects/virtual_observing_events/random_es_metrics.rda")

full_mask<-readRDS("../Objects/full_mask.rda")

if (T){
  biod<-readRDS("../Objects/virtual_observing_events/random_biodiversity_metrics.rda")
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
  saveRDS(biod_se, "../Objects/random_sampling_biod_metrics.rda")
}
es<-readRDS("../Objects/virtual_observing_events/random_es_metrics.rda")
es$available<-!is.na(es$es)
es_se<-es[, .(N_Cell=.N, es=mean(es, na.rm=T), sd_es=sd(es, na.rm=T)), 
          by=list(res, esNum, available)]
saveRDS(es_se, "../Objects/random_sampling_es_metrics.rda")
