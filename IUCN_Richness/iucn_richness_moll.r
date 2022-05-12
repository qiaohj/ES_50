library(raster)
library(data.table)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
mask_1km<-raster("../Raster/mask_1km.tif")
print("Reading mask points")
mask_points_full_resolutions<-readRDS("../Objects/mask_points_full_resolutions.rda")
setkeyv(mask_points_full_resolutions, "mask_1km")
species_list<-list.files("/media/huijieqiao/QNAS/ES50/IUCN_1km/", pattern="\\.tif")
species_list<-species_list[sample(length(species_list), length(species_list))]
cols<-c("x", "y")
i=1
for (i in c(1:length(species_list))){
  sp<-species_list[i]
  print(paste(i, length(species_list), sp))
  target<-sprintf("/media/huijieqiao/QNAS/ES50/IUCN_1km_Tables/%s", gsub("\\.tif", "\\.rda", sp))
  if (file.exists(target)){
    if (file.size(target)>100){
      print("skip")
      next()
    }
  }
  saveRDS(NULL, target)
  print("Raster to points")
  r<-data.table(rasterToPoints(raster(sprintf("/media/huijieqiao/QNAS/ES50/IUCN_1km/%s", sp))))
  if (nrow(r)==0){
    asdf
    next()
  }
  print("extracting mask values")
  r$mask_1km<-extract(mask_1km, r[, ..cols])
  r$x<-NULL
  r$y<-NULL
  r$sp<-gsub("\\.tif", "", sp)
  colnames(r)[1]<-"elevation"
  print("merging")
  r_with_mask<-merge(r, mask_points_full_resolutions, by="mask_1km")
  saveRDS(r_with_mask, target)
}

if (F){
  library(sf)
  sp<-"Rhyticeros_narcondami"
  shp<-readRDS(sprintf("/media/huijieqiao/SSD_Fast/ES50_eBird/Objects/IUCN_Distributions/Birds_RAW/RAW/%s.rda", 
                       gsub("_", " ",sp)))
  shp<-st_read(sprintf("/media/huijieqiao/SSD_Fast/ES50_eBird/Objects/IUCN_Distributions/Birds_RAW/Shape/%s/%s.shp", 
                       sp, sp))
  plot(shp$geom)
  st_area(shp)
  r<-raster(sprintf("/media/huijieqiao/QNAS/ES50/IUCN_30s/%s.tif", sp))
  plot(r)
}