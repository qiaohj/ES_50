library(raster)
library(rgdal)
library(rgeos)
library(MASS)
library(cluster)
library(data.table)
library(sf)
library(fasterize)
library(rmapshaper)
library(stars)
library(gdalUtilities)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
species_list<-list.files("../Objects/IUCN_Distributions/Birds_MOLL/RAW", pattern="\\.rda")

if (F){
  for (i in c(length(species_list):1)){
    print(paste(i, length(species_list)))
    item<-species_list[i]
    if (!file.exists(sprintf("../Objects/IUCN_Distributions/Birds_MOLL/Fixed/%s", item))){
      next()
    }
    dis<-readRDS(sprintf("../Objects/IUCN_Distributions/Birds_MOLL/Fixed/%s", item))
    if (dir.exists(sprintf("../Objects/IUCN_Distributions/Birds_MOLL/Shape/%s", 
                            gsub("\\.rda", "", item)))){
      next()
    }
    dir.create(sprintf("../Objects/IUCN_Distributions/Birds_MOLL/Shape/%s", 
                       gsub("\\.rda", "", item)))
    if ((class(dis$Shape)[1]=="sfc_GEOMETRY")|(class(dis$Shape)[1]=="sfc_MULTISURFACE")){
      dis_new<-st_cast(st_sfc(dis$Shape), "MULTIPOLYGON")
      st_geometry(dis)<-dis_new
    }
    
    st_write(dis, sprintf("../Objects/IUCN_Distributions/Birds_MOLL/Shape/%s/%s", 
                          gsub("\\.rda", "", item), gsub("\\.rda", "\\.shp", item)))
    
  }
  
  areas<-list()
  for (i in c(length(species_list):1)){
    print(paste(i, length(species_list)))
    item<-species_list[i]
    if (!file.exists(sprintf("../Objects/IUCN_Distributions/Birds_MOLL/Fixed/%s", item))){
      next()
    }
    dis<-readRDS(sprintf("../Objects/IUCN_Distributions/Birds_MOLL/Fixed/%s", item))
    area<-as.numeric(sum(st_area(dis))/1e6)
    area_item<-data.frame(sp=gsub("\\.rda", "", item), area=area)
    areas[[length(areas)+1]]<-area_item
  }
  areas<-rbindlist(areas)
  saveRDS(areas, "../Objects/IUCN_Distributions/areas.rda")
  
}

library(raster)
library(rgdal)
library(rgeos)
library(MASS)
library(cluster)
library(data.table)
library(sf)
library(fasterize)
library(rmapshaper)
library(stars)
library(gdalUtilities)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
species_list<-readRDS("../Objects/IUCN_Distributions/areas.rda")
species_list<-species_list[between(area, 0, 1e6)]
species_list<-species_list[sample(nrow(species_list), nrow(species_list))]
for (i in c(1:nrow(species_list))){
  
  item<-species_list[i]$sp
  print(paste(i, nrow(species_list), item))
  shp<-sprintf("../Objects/IUCN_Distributions/Birds_MOLL/Shape/%s/%s.shp", item, item)
  if (!file.exists(shp)){
    next()
  }
  f<-sprintf("/media/huijieqiao/QNAS/ES50/GEOTIFF/%s.tif", item)
  if (file.exists(f)){
    next()
  }
  template<-"gdalwarp -crop_to_cutline -dstnodata -9999 -cutline .%s ../Raster/ndvi/ndvi_mask_250m.tif %s"
  gdalwarp(crop_to_cutline=T, dstnodata=-9999, cutline=shp, overwrite=F, 
           srcfile="../Raster/ndvi/ndvi_mask_250m.tif", dstfile=f)
}



library(raster)
library(rgdal)
library(rgeos)
library(MASS)
library(cluster)
library(data.table)
library(sf)
library(fasterize)
library(rmapshaper)
library(stars)
library(gdalUtilities)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
species_list<-readRDS("../Objects/IUCN_Distributions/areas.rda")
species_list<-species_list[between(area, 0, 1e6)]
cmd<-c()
template<-"gdal_calc.py -A /media/huijieqiao/QNAS/ES50/GEOTIFF/%s.tif --calc='ceil(abs(A))' --outfile=%s --NoDataValue=-9999 --type=Int32"
i<-1
for (i in c(1:nrow(species_list))){
  item<-species_list[i]
  f<-sprintf("/media/huijieqiao/SSD_Fast/ES50_eBird/Objects/IUCN_Distributions/Birds_MOLL/GEOTIFF/%s.tif", item$sp)
  if (file.exists(f)){
    next()
  }
  ccc<-sprintf(template, item$sp, f)
  cmd<-c(cmd, ccc)
}
write.table(cmd, "~/Downloads/convert.sh", row.names = F, quote=F, col.names = F)
