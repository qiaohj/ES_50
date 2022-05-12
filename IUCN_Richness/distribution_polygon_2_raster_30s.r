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
if (F){
  r<-raster("../Raster/bioclim/wc2.1_30s_elev.tif")
  mask<-raster("../Raster/mask_1km.tif")
}
mask<-raster("../Raster/mask_1km.tif")
vessel<-NULL
if (F){
  species_list<-list.files("../Objects/IUCN_Distributions/Birds_RAW/RAW/", pattern="\\.rda")
  for (i in c(1:length(species_list))){
    print(paste(i, length(species_list)))
    item<-species_list[i]
    
    item<-gsub("\\.rda", "", item)
    rda<-sprintf("../Objects/IUCN_Distributions/Birds_RAW/RAW/%s.rda", item)
    if (!file.exists(rda)){
      next()
    }
    item<-gsub(" ", "_", item)
    
    folder<-sprintf("../Objects/IUCN_Distributions/Birds_RAW/Shape/%s", 
                    item)
    if (dir.exists(folder)){
      next()
    }
    dir.create(folder)
    dis<-readRDS(rda)
    if ((class(dis$Shape)[1]=="sfc_GEOMETRY")|(class(dis$Shape)[1]=="sfc_MULTISURFACE")){
      dis_new<-st_cast(st_sfc(dis$Shape), "MULTIPOLYGON")
      st_geometry(dis)<-dis_new
    }
    
    st_write(dis, sprintf("%s/%s.shp", 
                          folder, item))
    
  }
}

#species_list<-readRDS("../Objects/IUCN_Distributions/areas.rda")

species_list<-list.files("../Objects/IUCN_Distributions/Birds_RAW/RAW/", pattern="\\.rda")
species_list<-species_list[sample(length(species_list), length(species_list))]

for (i in c(1:length(species_list))){
  print(paste(i, length(species_list)))
  
  
  item<-species_list[i]
  item<-gsub(" ", "_", item)
  item<-gsub("\\.rda", "", item)
  print(paste(i, nrow(species_list), item))
  shp<-sprintf("../Objects/IUCN_Distributions/Birds_RAW/Shape/%s/%s.shp", item, item)
  if (!file.exists(shp)){
    next()
  }
  f<-sprintf("/media/huijieqiao/QNAS/ES50/IUCN_30s/%s.tif", item)
  if (file.exists(f)){
    if (file.size(f)>100){
      f_1km<-sprintf("/media/huijieqiao/QNAS/ES50/IUCN_1km/%s.tif", item)
      if (!file.exists(f_1km)){
        print(paste("converting", f_1km))
        gdalwarp(srcfile=f, dstfile=f_1km, tr=c(1000, 1000), t_srs=proj4string(mask), dryrun=F)
      }
      next()
    }
    
  }
  
  
  
  if (F){
    gdalwarp(crop_to_cutline=T, dstnodata=-9999, cutline=shp, overwrite=T, 
           srcfile="../Raster/bioclim/wc2.1_30s_elev.tif", dstfile=f, dryrun=F)
    shp_obj<-st_read(shp)
    is_valid<-st_is_valid(shp_obj)
    for (j in c(1:length(is_valid))){
      #if (is_valid[j]==F){
      print("fixing polygons")
      shp_obj[j,]$geometry<-st_make_valid(shp_obj[j,])$geometry
      
      #break()
      #}
    }
    st_write(shp_obj, shp, append = F)
    
    saveRDS(NULL, f)
  }
  if (is.null(vessel)){
    vessel<-sf::st_read(dsn = "../Shape/BOTW/BOTW.gdb", 
                        layer = "All_Species")
  }
  shp_obj<-vessel[which(vessel$binomial==gsub("_", " ", item)),]
  st_write(shp_obj, shp, append = F)
  rr<-raster("../Raster/bioclim/wc2.1_30s_elev.tif")
  xx<-fasterize(shp_obj, rr)
  print("writing tif in 30s")
  writeRaster(xx, f, overwrite=T)
  print(paste("converting", f_1km))
  f_1km<-sprintf("/media/huijieqiao/QNAS/ES50/IUCN_1km/%s.tif", item)
  if (!file.exists(f_1km)){
    gdalwarp(srcfile=f, dstfile=f_1km, tr=c(1000, 1000), t_srs=proj4string(mask), dryrun=F)
  }
  out <- tryCatch(
    {
      
    },
    error=function(cond) {
      
      return(NA)
    },
    warning=function(cond) {
      
      return(NULL)
    },
    finally={
      
    }
  )
  
  
  if (F){
    vessel<-sf::st_read(dsn = "../Shape/BOTW/BOTW.gdb", 
                        layer = "All_Species")
    shp_obj2<-vessel[which(vessel$binomial=="Pterodroma axillaris"),]
    st_write(shp_obj2, shp, append = F)
    plot(shp_obj2$Shape)
    crs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
        +datum=WGS84 +units=m +no_defs"
    laes_shp<-st_transform(shp_obj, crs=crs)
    plot(laes_shp$geometry, col="red")
    library(fasterize)
    
    plot(xx)
    x1<-st_make_valid(shp_obj[2,])
    ss<-st_buffer(shp_obj, dist=0)
    plot(x1$geometry, col="red")
    plot(shp_obj[2,]$geometry, col="red")
    plot(shp_obj$geometry, col="red")
    lonlat_new<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +lat_0=-90 +lon_0=180 +no_defs"
    ss<-st_transform(shp_obj, crs=lonlat_new)
    plot(ss$geometry, col="red")
    rrr<-raster("/media/huijieqiao/QNAS/ES50/IUCN_30s/Tarsiger_chrysaeus.tif")
    plot(rrr)
    
    rrr<-raster(f_1km)
    plot(rrr)
  }
}


