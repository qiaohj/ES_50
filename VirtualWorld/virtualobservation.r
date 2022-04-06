library(raster)
library(data.table)
library(ggplot2)
library(sf)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
effort_distance<-1000
if (F){
  random_observing_events<-readRDS("../Objects/virtual_observing_events/random_observing_events.rda")
  
  collected_sp_index<-st_contains(random_observing_events, species_points)
  saveRDS(collected_sp_index, "../Objects/virtual_species/virtual_random_collection.rda")
  #collected_sp_index<-unlist(collected_sp_index)
  #collected_sp<-species_points[collected_sp_index,]
  if (F){
    plot(st_geometry(random_observing_events))
    random_points<-collected_sp[sample(nrow(collected_sp), 1000),]
    plot(st_geometry(random_points), add=T, pch=".")
  }
}
virtual_species<-readRDS("../Objects/virtual_species/virtual_species.rda")
species_points<-st_as_sf(virtual_species, coords = c("x", "y"))
seeds<-readRDS("../Objects/virtual_lands/centers.rda")
#seeds<-seeds[sample(nrow(seeds), nrow(seeds))]
for (i in c(1:nrow(seeds))){
  
  print(i)
  folder<-sprintf("../Objects/virtual_lands/items/%d", i)
  f<-sprintf("%s/observations.rda", folder)
  if (file.exists(f)){
    if (file.size(f)>=100){
      next()
    }
    
  }
  saveRDS(NULL, f)
  observing_events<-readRDS(sprintf("%s/observing_events.rda", folder))
  opoints<-st_as_sf(observing_events, coords = c("x", "y"))
  obuffers<-st_buffer(opoints, effort_distance)
  collected_sp_index<-st_contains(obuffers, species_points)
  saveRDS(collected_sp_index, f)
  
}
