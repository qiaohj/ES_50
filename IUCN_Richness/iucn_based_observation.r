library(raster)
library(data.table)
library(ggplot2)
library(sf)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
effort_distance<-1000
picked_grids<-readRDS("../Objects/virtual_lands/picked_grids.rda")
args = commandArgs(trailingOnly=TRUE)
j<-as.numeric(args[1])
if (is.na(j)){
  j<-5
}
base<-sprintf("../Objects/GRIDS_S/%s", picked_grids[j,]$index)
if (!dir.exists(base)){
  dir.create(base)
}
virtual_species<-readRDS(sprintf("../Objects/GRIDS_S/species_points_%d.rda", picked_grids[j,]$index))
species_points<-st_as_sf(virtual_species, coords = c("x", "y"))

if (F){
  random_observing_events<-readRDS("../Objects/virtual_observing_events/random_observing_events.rda")
  collected_sp_index<-st_contains(random_observing_events, species_points)
  saveRDS(collected_sp_index, sprintf("%s/virtual_random_collection.rda", base))
  #collected_sp_index<-unlist(collected_sp_index)
  #collected_sp<-species_points[collected_sp_index,]
  if (F){
    plot(st_geometry(random_observing_events))
    random_points<-collected_sp[sample(nrow(collected_sp), 1000),]
    plot(st_geometry(random_points), add=T, pch=".")
  }
}

seeds<-readRDS("../Objects/virtual_lands/centers.rda")
#seeds<-seeds[sample(nrow(seeds), nrow(seeds))]
i=1
for (i in c(1:nrow(seeds))){
  
  print(paste(j, "/", i))
  folder<-sprintf("%s/%d", base, i)
  if (!dir.exists(folder)){
    dir.create(folder)
  }
  f<-sprintf("%s/observations.rda", folder)
  if (file.exists(f)){
    print("skip")
    #if (file.size(f)>=100){
      next()
    #}
    
  }
  saveRDS(NULL, f)
  folder_vl<-sprintf("../Objects/virtual_lands/items/%d", i)
  observing_events<-readRDS(sprintf("%s/observing_events.rda", folder_vl))
  opoints<-st_as_sf(observing_events, coords = c("x", "y"))
  obuffers<-st_buffer(opoints, effort_distance)
  collected_sp_index<-st_contains(obuffers, species_points)
  saveRDS(collected_sp_index, f)
}
