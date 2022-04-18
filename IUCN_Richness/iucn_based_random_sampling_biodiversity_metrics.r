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
library(ggpubr)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")

add_location<-function(indices, location, type){
  location$metric<-indices
  location$type<-type
  location
}
full_mask<-readRDS("../Objects/full_mask.rda")

picked_grids<-readRDS("../Objects/virtual_lands/picked_grids.rda")
grid_index=1
effort_distance<-1000
observing_events<-readRDS("../Objects/virtual_observing_events/random_observing_events.rda")

for (grid_index in c(1:nrow(picked_grids))){
  base<-sprintf("../Objects/GRIDS_S/%s", picked_grids[grid_index,]$index)
  virtual_species<-readRDS(sprintf("../Objects/GRIDS_S/species_points_%d.rda", picked_grids[grid_index,]$index))
                                                       
  species_points<-st_as_sf(virtual_species, coords = c("x", "y"))
  print(grid_index)
  folder<-sprintf("%s/random_sampling", base)
  if (!dir.exists(folder)){
    dir.create(folder)
  }
  f<-sprintf("%s/observations.rda", folder)
  
  opoints<-st_as_sf(observing_events, coords = c("x", "y"))
  obuffers<-st_buffer(opoints, effort_distance)
  collected_sp_index<-st_contains(obuffers, species_points)
  saveRDS(collected_sp_index, f)
  
  #100 LC, 40 NT, 20 Vu, 10 En, CR
  sample_probability_Least_Concern<-1
  sample_probability_Near_Threatened<-0.4
  sample_probability_Vulnerable<-0.2
  sample_probability_Endangered<-0.1
  sample_probability_Critically_Endangered<-0.1
  probabilities<-list("Least Concern"=sample_probability_Least_Concern,
                      "Vulnerable"=sample_probability_Vulnerable,
                      "Near Threatened"=sample_probability_Near_Threatened,
                      "Endangered"=sample_probability_Endangered,
                      "Critically Endangered"=sample_probability_Critically_Endangered)
  folder<-sprintf("%s/random_sampling", base)
  target<-sprintf("%s/biodiversity.rda", folder)
  f<-sprintf("%s/observations.rda", folder)
  f_es<-sprintf("%s/es.rda", folder)
  occ_index<-readRDS(f)
  occ_index<-unlist(occ_index)
  virtual_species<-virtual_species[iucn_category %in% c("Critically Endangered", "Endangered",
                                                        "Least Concern", "Near Threatened", "Vulnerable")]
   
  occ<-virtual_species[ID %in% occ_index]
  occ_list<-list()
  for (cate in unique(occ$iucn_category)){
    occ_item<-occ[iucn_category==cate]
    occ_item<-occ_item[sample(nrow(occ_item), nrow(occ_item) * probabilities[[cate]])]
    occ_list[[length(occ_list)+1]]<-occ_item
  }
  occ<-rbindlist(occ_list)
  if (F){
    table(occ$iucn_category)
    table(occ_list$iucn_category)
  }
  biodiversity_list<-list()
  es_list<-list()
  for (res in c("100km", "50km", "20km", "10km", "5km", "2km", "1km")){
    print(paste("Grid:", grid_index, "seeds:", "random", "@", res))
    occ_se<-occ[, .(N=.N), by=c(sprintf("mask_%s", res), "sp", "sp_id")]
    colnames(occ_se)[1]<-"mask"
    if (!file.exists(f_es)){
      for (es_threshold in c(200, 100, 50, 20, 10)){
        
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
    }
    if (F){
      occ_se<-rbind(occ_se, data.frame(mask=999999, sp="SP_1_3", sp_id=1, N=0))
    }
    t_m<-pivot_wider(occ_se, id_cols=mask, names_from = sp, values_from=N, values_fill=0)
    
    d2<-t_m[,-1]
    location<-t_m$mask
    biodiversity_metrics<-data.frame(mask=location, res=res, metrics="simpson",
                                     v = diversity(d2, "simpson"))
    
    biodiversity_metrics$shannon<-diversity(d2, "shannon")
    biodiversity_metrics$invsimp<-diversity(d2, "invsimpson")
    #biodiversity_metrics$unbias.simp<-rarefy(d2, 100) - 1
    #biodiversity_metrics$alpha <- fisher.alpha(d2)
    biodiversity_metrics$species.richness <- specnumber(d2) ## rowSums(BCI > 0) does the same...
    biodiversity_metrics$J <- biodiversity_metrics$shannon/log(biodiversity_metrics$species.richness)
    hill<-renyi(d2, hill=T)
    for (k in 1:length(hill)){
      biodiversity_metrics[,sprintf("Hill_%s", names(hill)[k])]<-hill[[k]]
    }
    biodiversity_list[[length(biodiversity_list)+1]]<-biodiversity_metrics
    
    
  }
  if (!file.exists(f_es)){
    es_list<-rbindlist(es_list)
    saveRDS(es_list, f_es)
  }
  biodiversity_list<-rbindlist(biodiversity_list)
  
  saveRDS(biodiversity_list, target)
  
  
  
  
}