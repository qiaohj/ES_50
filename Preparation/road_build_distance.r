library(raster)
library(data.table)
library(sf)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
road_2km<-raster("../Raster/road/road_2km.tif")
road_5km<-raster("../Raster/road/road_5km.tif")
build_2km<-raster("../Raster/build/build_2km.tif")
build_5km<-raster("../Raster/build/build_5km.tif")

state_list<-readRDS("../Tables/state_list.rda")
country_codes<-unique(state_list$GID_0)
i=1
i=212
j=4
ll_crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
country_codes<-country_codes[sample(length(country_codes), length(country_codes))]
for (i in c(1:length(country_codes))){
  country<-country_codes[i]
  state_codes<-unique(state_list[GID_0==country]$GID_1)
  state_codes<-state_codes[sample(length(state_codes), length(state_codes))]
  
  for (j in c(1:length(state_codes))){
    
    state<-state_codes[j]
    print(paste(i, "/", length(country_codes), ":", country, j, "/", length(state_codes), ":", state))
    rda<-sprintf("../Tables/eBird_mapbased_checklist_with_realm/%s/ebird_raw_%s.rda", country, state)
    
    if (!file.exists(rda)){
      next()
    }
    target<-sprintf("../Tables/eBird_mapbased_checklist_with_realm/%s/ebird_build_road_%s.rda", 
                    country, state)
    if (file.exists(target)){
      next()
    }
    saveRDS(NULL, target)
    df<-readRDS(rda)
    df_se<-df[, .(N=.N), by=list(SCIENTIFIC_NAME, LOCALITY_ID, LATITUDE, LONGITUDE)]
    
    points <- st_as_sf(x = df_se,                         
                   coords = c("LONGITUDE", "LATITUDE"),
                   crs = ll_crs)
    points_moll<-st_transform(points, st_crs(road_2km))
    if (F){
      
      plot(st_geometry(points_moll), add=T)
      system.time({xxx<-extract(build_2km, points_moll)})
      system.time({yyy<-fast_extract(build_2km, points_moll)})
    }
    print("Extracting build 2km buffer")
    df_se$build_2km<-raster::extract(build_2km, points_moll)
    df_se[build_2km==65535]$build_2km<-NA
    print("Extracting build 5km buffer")
    df_se$build_5km<-df_se$build_2km
    if (nrow(df_se[is.na(build_5km)])>0){
      df_se[is.na(build_5km)]$build_5km<-raster::extract(build_5km, points_moll[is.na(df_se$build_5km),])
    }
    print("Extracting road 2km buffer")
    df_se$road_2km<-raster::extract(road_2km, points_moll)
    df_se[road_2km==65535]$road_2km<-NA
    print("Extracting road 5km buffer")
    df_se$road_5km<-df_se$road_2km
    if (nrow(df_se[is.na(road_5km)])>0){
      df_se[is.na(road_5km)]$road_5km<-raster::extract(road_5km, points_moll[is.na(df_se$road_5km),])
    }
    
    saveRDS(df_se, target)
  }
}