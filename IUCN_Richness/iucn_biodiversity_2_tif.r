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
rm(list=ls())
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
picked_grids<-readRDS("../Objects/virtual_lands/picked_grids.rda")
seeds<-readRDS("../Objects/virtual_lands/virtual_lands_property.rda")
i=1
full_mask<-readRDS("../Objects/full_mask.rda")
masks<-list()
masks_p<-list()
for (res_i in c("100km", "50km", "20km", "10km", "5km", "2km", "1km" )){
  masks[[res_i]]<-raster(sprintf("../Objects/masks/mask_%s.tif", res_i))
  p<-data.table(rasterToPoints(masks[[res_i]]))
  colnames(p)[3]<-"mask"
  masks_p[[res_i]]<-p
}
if (F){
  sampling_proportion<-0.5
  folxxx<-"random_sampling"
  for (grid_index in c(1:nrow(picked_grids))){
    print(paste(grid_index, folxxx, sampling_proportion))
    if (sampling_proportion==1){
      biod<-readRDS(sprintf("../Objects/GRIDS_S/%d/%s/biodiversity.rda", picked_grids[grid_index,]$index, folxxx))
      target_folder<-sprintf("../Objects/GRIDS_TIF/%d/%s", picked_grids[grid_index,]$index, folxxx)
      es<-readRDS(sprintf("../Objects/GRIDS_S/%d/%s/es.rda", picked_grids[grid_index,]$index, folxxx))
      
      
    }else{
      biod<-readRDS(sprintf("../Objects/GRIDS_S/%d/%s/biodiversity_%.1f.rda", 
                            picked_grids[grid_index,]$index, folxxx, sampling_proportion))
      target_folder<-sprintf("../Objects/GRIDS_TIF/%d_sampling_%.1f/%s", 
                             picked_grids[grid_index,]$index, sampling_proportion, folxxx)
      es<-readRDS(sprintf("../Objects/GRIDS_S/%d/%s/es_%.1f.rda", 
                          picked_grids[grid_index,]$index, folxxx, sampling_proportion))
      
    }
    dir.create(target_folder, showWarnings = F, recursive = F)
    biod<-merge(biod, full_mask, by=c("mask", "res"), all=T)
    
    NoData<-biod[is.na(v), .(N_NoData=.N), by=list(res)]
    biod<-merge(biod, NoData, by="res", all=T)
    biod[is.na(N_NoData)]$N_NoData<-0
    biod[is.na(v)]$v<-1
    biod[is.na(shannon)]$shannon<-0
    biod[is.na(invsimp)]$invsimp<-Inf
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
    res_i<-"10km"
    for (res_i in c("100km", "50km", "20km", "10km", "5km", "2km", "1km" )){
      for (col_item in c("v", "shannon", "invsimp", "species.richness", "J", "Hill_0", "Hill_0.25", "Hill_0.5", "Hill_1", 
                         "Hill_2", "Hill_4", "Hill_8", "Hill_16", "Hill_32", "Hill_64", "Hill_Inf")){
        mask<-masks[[res_i]]
        biod_item<-biod[res==res_i]
        values(mask)<-biod_item[, get(col_item)]
        writeRaster(mask, sprintf("%s/%s_%s.tif", target_folder, res_i, col_item), overwrite=T)
      }
    }
    
    es$mask<-as.numeric(es$cell)
    for (res_i in c("100km", "50km", "20km", "10km", "5km", "2km", "1km" )){
      for (col_item in c(unique(es$esNum))){
        mask<-masks[[res_i]]
        p<-masks_p[[res_i]]
        es_item<-es[res==res_i&esNum==col_item]
        es_item<-merge(es_item, p, by="mask", all=T)
        values(mask)<-es_item$es
        writeRaster(mask, sprintf("%s/%s_es%d.tif", target_folder, res_i, col_item), overwrite=T)
      }
    }
    
  }
}
grid_index=1
args = commandArgs(trailingOnly=TRUE)
grid_index=1
grid_index<-as.numeric(args[1])
if (is.na(grid_index)){
  grid_index<-5
}
sampling_proportion<-0.1
i=3761
#for (grid_index in c(1:nrow(picked_grids))){
base<-sprintf("../Objects/GRIDS_S/%s", picked_grids[grid_index,]$index)
TIF_base<-sprintf("../Objects/GRIDS_TIF/%s", picked_grids[grid_index,]$index)
for (i in c(1:nrow(seeds))){
  print(paste(grid_index, i, sampling_proportion))
  seed<-seeds[i]
  folder<-sprintf("%s/%d", base, i)
  if (sampling_proportion==1){
    target<-sprintf("%s/biodiversity.rda", folder)
  }else{
    target<-sprintf("%s/biodiversity_sampling_%.1f.rda", folder, sampling_proportion)
  }
  if (sampling_proportion==1){
    target_folder<-sprintf("%s/%d", TIF_base, i)
  }else{
    target_folder<-sprintf("%s_sampling_%.1f/%d", TIF_base, sampling_proportion, i)
  }
  if (!dir.exists(target_folder)){
    dir.create(target_folder, recursive = T)
  }else{
    next()
  }
  biod<-readRDS(target)
  biod<-merge(biod, full_mask, by=c("mask", "res"), all=T)
  
  NoData<-biod[is.na(v), .(N_NoData=.N), by=list(res)]
  biod<-merge(biod, NoData, by="res", all=T)
  biod[is.na(N_NoData)]$N_NoData<-0
  biod[is.na(v)]$v<-1
  biod[is.na(shannon)]$shannon<-0
  biod[is.na(invsimp)]$invsimp<-Inf
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
  res_i<-"10km"
  for (res_i in c("100km", "50km", "20km", "10km", "5km", "2km", "1km" )){
    for (col_item in c("v", "shannon", "invsimp", "species.richness", "J", "Hill_0", "Hill_0.25", "Hill_0.5", "Hill_1", 
                       "Hill_2", "Hill_4", "Hill_8", "Hill_16", "Hill_32", "Hill_64", "Hill_Inf")){
      mask<-masks[[res_i]]
      biod_item<-biod[res==res_i]
      values(mask)<-biod_item[, get(col_item)]
      writeRaster(mask, sprintf("%s/%s_%s.tif", target_folder, res_i, col_item), overwrite=T)
    }
  }
  if (sampling_proportion==1){
    es<-readRDS(sprintf("%s/es.rda", folder))
  }else{
    es<-readRDS(sprintf("%s/es_%.1f.rda", folder, sampling_proportion))
  }
  es$mask<-as.numeric(es$cell)
  for (res_i in c("100km", "50km", "20km", "10km", "5km", "2km", "1km" )){
    for (col_item in c(unique(es$esNum))){
      mask<-masks[[res_i]]
      p<-masks_p[[res_i]]
      es_item<-es[res==res_i&esNum==col_item]
      es_item<-merge(es_item, p, by="mask", all=T)
      values(mask)<-es_item$es
      writeRaster(mask, sprintf("%s/%s_es%d.tif", target_folder, res_i, col_item), overwrite=T)
    }
  }
}
#}



