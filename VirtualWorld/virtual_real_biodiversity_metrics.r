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
occ<-virtual_species

j=1
biodiversity_list<-list()
es_list<-list()
res<-"10km"
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

saveRDS(biodiversity_lista, "../Objects/virtual_observing_events/real_biodiversity_metrics.rda")
saveRDS(es_lista, "../Objects/virtual_observing_events/real_es_metrics.rda")
