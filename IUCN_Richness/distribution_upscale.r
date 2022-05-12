
library(gdalUtilities)
library(raster)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
species_list<-list.files("/media/huijieqiao/QNAS/ES50/IUCN_1km/", pattern="\\.tif")
species_list<-species_list[sample(length(species_list), length(species_list))]
i=1
mask<-raster("../Raster/mask_1km.tif")
for (i in c(1:length(species_list))){
  print(paste(i, length(species_list), item))
  item<-species_list[i]
  t<-sprintf("/media/huijieqiao/QNAS/ES50/IUCN_10km/%s", item)
  if (file.exists(t)){
    next()
  }
  saveRDS(NULL, t)
  gdalwarp(srcfile=sprintf("/media/huijieqiao/QNAS/ES50/IUCN_1km/%s", item), 
           dstfile=t, tr=c(10000, 10000), t_srs=proj4string(mask), dryrun=F, overwrite=T)
  gdalwarp(srcfile=t, 
           dstfile=sprintf("/media/huijieqiao/QNAS/ES50/IUCN_100km/%s", item), 
           tr=c(100000, 100000), t_srs=proj4string(mask), dryrun=F, overwrite=T)
  if (F){
    plot(raster("/media/huijieqiao/QNAS/ES50/IUCN_1km/Trochalopteron_ngoclinhense.tif"))
    plot(raster("/media/huijieqiao/QNAS/ES50/IUCN_10km/Pterocles_namaqua.tif"))
    plot(raster("/media/huijieqiao/QNAS/ES50/IUCN_100km/Pterocles_namaqua.tif"))
  }
}