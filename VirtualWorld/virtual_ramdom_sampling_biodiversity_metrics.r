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
if (F){
  full_mask<-list()
  res<-"100km"
  for (res in c("100km", "50km", "20km", "10km", "5km", "2km", "1km")){
    mask_p<-data.table(rasterToPoints(raster(sprintf("../Objects/masks/mask_%s.tif", res))))
    mask_p$res<-res
    colnames(mask_p)[3]<-"mask"
    full_mask[[length(full_mask)+1]]<-mask_p
  }
  full_mask<-rbindlist(full_mask)
  saveRDS(full_mask, "../Objects/full_mask.rda")
  mask<-raster("../Objects/mask.tif")
  res<-100000
  for (res in c(100000, 50000, 20000, 10000, 5000, 2000, 1000)){
    print(res)
    mask_10km<-projectRaster(mask, crs=crs(mask), res=c(res, res))
    values(mask_10km)<-c(1:length(values(mask_10km)))
    writeRaster(mask_10km, sprintf("../Objects/masks/mask_%dkm.tif", res/1000), overwrite=T)
  }
  virtual_species<-readRDS("../Objects/virtual_species/virtual_species.rda")
  virtual_species$ID<-c(1:nrow(virtual_species))
  virtual_species$sp<-paste("SP", virtual_species$rep, virtual_species$rarity, sep="_")
  sp_index<-data.table(sp=unique(virtual_species$sp), sp_id=c(1:length(unique(virtual_species$sp))))
  virtual_species<-merge(virtual_species, sp_index, by="sp")
  setorderv(virtual_species, c("ID"))
  for (res in c(100000, 50000, 20000, 10000, 5000, 2000, 1000)){
    mask_item<-raster(sprintf("../Objects/masks/mask_%dkm.tif", res/1000))
    virtual_species[, (sprintf("mask_%dkm", res/1000)) := 
                      raster::extract(mask_item, data.frame(x=virtual_species$x, virtual_species$y))]
  }
  saveRDS(virtual_species, "../Objects/virtual_species/virtual_species.rda")
  
}
add_location<-function(indices, location, type){
  location$metric<-indices
  location$type<-type
  location
}
full_mask<-readRDS("../Objects/full_mask.rda")
virtual_species<-readRDS("../Objects/virtual_species/virtual_species.rda")
#species_points<-st_as_sf(virtual_species, coords = c("x", "y"))
seeds<-readRDS("../Objects/virtual_lands/centers.rda")
#coms<-expand.grid(mask=c("100km", "50km", "20km", "10km", "5km", "2km", "1km"),
#                  es=c(200, 100, 50, 20, 10), stringsAsFactors = F)

#seeds<-seeds[sample(nrow(seeds), nrow(seeds))]
sample_probability<-0.5
i<-369
for (i in c(1:nrow(seeds))){
  #for (i in c(1:400)){
  folder<-sprintf("../Objects/virtual_lands/items/%d", i)
  target<-sprintf("%s/biodiversity.rda", folder)
  if (file.exists(target)){
    if (file.size(target)>100){
      next()
    }
  }
  saveRDS(NULL, target)
  f<-sprintf("%s/observations.rda", folder)
  f_es<-sprintf("%s/es.rda", folder)
  occ_index<-readRDS(f)
  occ_index<-unlist(occ_index)
  occ<-virtual_species[ID %in% occ_index]
  occ$probability<-runif(nrow(occ), 0, 1)
  occ<-occ[probability>=0.5]
  j=1
  biodiversity_list<-list()
  es_list<-list()
  for (res in c("100km", "50km", "20km", "10km", "5km", "2km", "1km")){
    print(paste(i, res))
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
  if (!file.exists(f_es)){
    es_list<-rbindlist(es_list)
    saveRDS(es_list, f_es)
  }
  biodiversity_list<-rbindlist(biodiversity_list)
  
  saveRDS(biodiversity_list, target)
  
  
  if (F){
    effort_distance<-1000
    road_points<-readRDS(sprintf("%s/road_points.rda", folder))
    observing_events<-readRDS(sprintf("%s/observing_events.rda", folder))
    road_colors<-c("offroad"="white", "on road"="black", "2km buffer"="grey", "5km buffer"="lightgrey")
    
    colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                           "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    observing_colors<-c("offroad"=colorBlindBlack8[2], "on road"=colorBlindBlack8[3], 
                        "2km buffer"=colorBlindBlack8[4], "5km buffer"=colorBlindBlack8[5])
    points<-st_as_sf(observing_events, coords = c("x", "y"))
    buffers<-st_buffer(points, effort_distance)
    #ggplot()+geom_sf(data=buffers, aes(color=type))
    
    ggplot(road_points)+geom_tile(aes(x=x, y=y, fill=type))+
      geom_sf(data=buffers, aes(color=type))+
      geom_tile(data=road_points[type=="on road"], aes(x=x, y=y, fill=type))+
      geom_tile(data=occ[sample(nrow(occ), 10000)], aes(x=x, y=y), color="red")+
      scale_fill_manual(values=road_colors)+
      scale_color_manual(values=observing_colors)+
      theme_bw()
    
  }
}


