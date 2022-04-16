library(data.table)
library(raster)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
picked_grids<-readRDS("../Objects/virtual_lands/picked_grids.rda")
j<-1
mask<-raster("../Objects/mask.tif")
mask_p<-data.frame(rasterToPoints(mask))
#for (j in rev(c(1:nrow(picked_grids)))){
for (j in rev(c(3))){
  sp<-list.files(sprintf("../Objects/GRIDS/%d", picked_grids[j,]$index), pattern="\\.tif$", full.names = T)
  sp<-sp[!(grepl("grid\\.tif", sp))]
  sp<-sp[!(grepl("_100km\\.tif", sp))]
  item<-sp[1]
  for (item in rev(sp)){
    print(paste(j, item))
    if (file.exists(gsub("\\.tif", "_100km\\.tif", item))){
      next()
    }
    sp_dis<-raster(item)
    v<-values(sp_dis)
    v[v==255]<-0
    v[is.na(v)]<-0
    v[v==1]<-1
    values(sp_dis)<-v
    extent(sp_dis)<-extent(mask)
    sp_dis_100km_p<-mask_p
    sp_dis_100km_p$v<-extract(sp_dis, sp_dis_100km_p[, c("x", "y")])
    sp_dis_100km<-mask
    values(sp_dis_100km)<-sp_dis_100km_p$v
    writeRaster(sp_dis_100km, gsub("\\.tif", "_100km\\.tif", item), overwrite=T)
  }
  
}

library(data.table)
library(raster)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")

picked_grids<-readRDS("../Objects/virtual_lands/picked_grids.rda")
j<-1
mask<-raster("../Objects/mask.tif")
mask_p<-data.frame(rasterToPoints(mask))
mask_f<-"mask_100km"
for (mask_f in c("mask_100km", "mask_50km", "mask_20km", "mask_10km", "mask_5km", "mask_2km", "mask_1km")){
  print(mask_f)
  mask_x<-raster(sprintf("../Objects/masks/%s.tif", mask_f))
  mask_p[, mask_f]<-extract(mask_x, mask_p[, c("x", "y")])
}
iucn_status<-read.csv("../Tables/redlist_species_data_3752fd69-a2ab-4550-9648-6dd9736eaa78/simple_summary.csv", stringsAsFactors = F)
j=1
#for (j in rev(c(1:nrow(picked_grids)))){
for (j in rev(c(5))){
  sp<-list.files(sprintf("../Objects/GRIDS/%d", picked_grids[j,]$index), pattern="_100km\\.tif$", full.names = T)
  
  item<-sp[1]
  sp_id<-1
  sp_p_list<-list()
  for (item in sp){
    print(paste(j, item))
    sp_name<-gsub(sprintf("../Objects/GRIDS/%d/", picked_grids[j,]$index), "", item)
    sp_name<-gsub("_100km\\.tif", "", sp_name)
    sp_name<-gsub("_", " ", sp_name)
    iucn_status_sp<-iucn_status[which(iucn_status$scientificName==sp_name),]
    if (nrow(iucn_status_sp)==0){
      iucn_category<-"unknown"
    }else{
      iucn_category<-iucn_status_sp$redlistCategory
    }
    sp_dis<-raster(item)
    sp_p<-mask_p
    sp_p$v<-extract(sp_dis, sp_p[, c("x", "y")])
    sp_p<-sp_p[which(sp_p$v==1),]
    if (nrow(sp_p)==0){
      next()
    }
    sp_p$sp_id<-sp_id
    sp_p$sp<-sp_name
    sp_p$iucn_category<-iucn_category
    sp_p_list[[length(sp_p_list)+1]]<-sp_p
    sp_id<-sp_id+1
  }
  sp_p_list<-rbindlist(sp_p_list)
  sp_p_list$ID<-c(1:nrow(sp_p_list))
  saveRDS(sp_p_list, sprintf("../Objects/GRIDS_S/species_points_%d.rda", picked_grids[j,]$index))
}

