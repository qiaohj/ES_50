library(sf)
library(raster)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
sf_use_s2(FALSE)
roads<-sf::st_read("../Shape/GRIP4_GlobalRoads/GRIP4_GlobalRoads.gdb")
#build<-raster("../Raster/GHS_BUILT_LDS2014_GLOBE_R2018A_54009_250_V2_0/GHS_BUILT_LDS2014_GLOBE_R2018A_54009_250_V2_0.tif")
moll_crs<-"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
roads_moll<-st_transform(roads, st_crs(moll_crs))
st_write(roads_moll, "../Shape/GRIP4_GlobalRoads/GRIP4_GlobalRoads_moll.gpkg")
#gdal_rasterize -a GP_RTP -te -18041000 -9000000 18041000 9000000 -tr 250 250 -ot Int16 GRIP4_GlobalRoads_moll.gpkg road.tif
#gdal_proximity.py road.tif road_2km.tif -distunits PIXEL -maxdist 8
#gdal_proximity.py road.tif road_5km.tif -distunits PIXEL -maxdist 20
road<-raster("../Shape/GRIP4_GlobalRoads/road.tif")


if (F){
  union_moll<-st_union(roads_moll[c(1:2),])
  road_buffer_2km<-st_buffer(union_moll, dist=2000)
  plot(st_geometry(road_buffer_2km))
  plot(st_geometry(union_moll), add=T, col="red")
  
  roads_moll_simplied<-st_simplify(roads_moll[c(1:100),], dTolerance=200)
  plot(st_geometry(roads_moll_simplied[1,]))
  plot(st_geometry(roads_moll[c(1),]), add=T, col="red")
  
  #roads_moll_union<-st_union(roads_moll)
  
  #road_buffer_2km<-st_buffer(roads_moll_union, dist=2000)
  
  #saveRDS(road_buffer_2km, "../Shape/GRIP4_GlobalRoads/Buffer/road_buffer_2km.rda")
  
  #road_buffer_5km<-st_buffer(roads_moll_union, dist=5000)
  
  #saveRDS(road_buffer_5km, "../Shape/GRIP4_GlobalRoads/Buffer/road_buffer_5km.rda")
  
  road_buffer_2km<-st_buffer(roads_moll, dist=2000)
  
  saveRDS(road_buffer_2km, "../Shape/GRIP4_GlobalRoads/Buffer/road_buffer_2km_raw.rda")
  
  road_buffer_2km_union<-st_union(road_buffer_2km)
  
  saveRDS(road_buffer_2km_union, "../Shape/GRIP4_GlobalRoads/Buffer/road_buffer_2km_union.rda")
  
  road_buffer_5km<-st_buffer(roads_moll_union, dist=5000)
  
  saveRDS(road_buffer_5km, "../Shape/GRIP4_GlobalRoads/Buffer/road_buffer_5km.rda")
  
  road_buffer_5km_union<-st_union(road_buffer_5km)
  
  saveRDS(road_buffer_5km_union, "../Shape/GRIP4_GlobalRoads/Buffer/road_buffer_5km_union.rda")
  
  road_buffer_2km<-readRDS("../Shape/GRIP4_GlobalRoads/Buffer/road_buffer_2km_raw.rda")
  
}
