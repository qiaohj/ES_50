library(sf)
library(data.table)
library(gdalUtilities)

setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
sf_use_s2(FALSE)

if (F){
  vessel<-sf::st_read(dsn = "../Shape/BOTW/BOTW.gdb", 
                      layer = "All_Species")
  
  ensure_multipolygons <- function(X) {
    tmp1 <- tempfile(fileext = ".gpkg")
    tmp2 <- tempfile(fileext = ".gpkg")
    st_write(X, tmp1)
    ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
    Y <- st_read(tmp2)
    st_sf(st_drop_geometry(X), geom = st_geometry(Y))
  }
  vessel_fixed<-ensure_multipolygons(vessel)
  saveRDS(vessel_fixed, "../Shape/Birdlife_202101_fixed.rda")
  
  birdlife_checklist<-data.table(binomial=vessel_fixed$binomial,
                                 presence=vessel_fixed$presence,
                                 origin=vessel_fixed$origin,
                                 seasonal=vessel_fixed$seasonal,
                                 Shape_Area=vessel_fixed$Shape_Area)
  birdlife_checklist<-birdlife_checklist[,.(area=sum(Shape_Area)),
                                            by=list(binomial, presence, origin, seasonal)]
  saveRDS(birdlife_checklist, "../Shape/Birdlife_202101_checklist.rda")
}
vessel_fixed<-readRDS("../Shape/Birdlife_202101_fixed.rda")
states<-sf::st_read(dsn = "../Shape/gadm36_levels_shp", 
                    layer = "gadm36_1")
state_list<-data.frame(NAME_0=states$NAME_0, NAME_1=states$NAME_1)
state_list<-unique(state_list)
species_list<-unique(vessel_fixed$binomial)

st_bbox_by_feature = function(x) {
  x = st_geometry(x)
  f <- function(y) st_as_sfc(st_bbox(y), crs=st_crs(y))
  do.call("c", lapply(x, f))
}

species_bbox<-st_bbox_by_feature(vessel_fixed)
st_crs(species_bbox)<-st_crs(vessel_fixed)
state_item<-data.table(state_list)[NAME_0=="Brazil"&NAME_1=="Amazonas"]

for (i in c(1:nrow(state_list))){
  print(paste(i, nrow(state_list)))
  
  state_item<-state_list[i,]
  target<-sprintf("../Tables/State_Checklist/%s.%s.rda", state_item$NAME_0,
                  gsub("/", ".", state_item$NAME_1))
  if (file.exists(target)){
    print("skip")
    next()
  }
  saveRDS(NULL, target)
  state_shape<-states[which((states$NAME_0==state_item$NAME_0)&(states$NAME_1==state_item$NAME_1)),]
  if (F){
    plot(st_geometry(state_shape))
    plot(st_geometry(extent_state), add=T)
    
    plot(st_geometry(birds), col="grey")
    plot(st_geometry(state_shape), col="red", add=T)
  }

  extent_state<-st_as_sfc(st_bbox(state_shape))
  int_index<-st_intersects(extent_state, species_bbox)
  int_index<-unlist(int_index)
  birds<-vessel_fixed[int_index,]
  birds_index<-st_intersects(state_shape, birds)
  birds_index<-unlist(birds_index)
  birds_checklist<-birds[birds_index,]
  birds_checklist<-data.table(binomial=birds_checklist$binomial,
                              presence=birds_checklist$presence,
                              origin=birds_checklist$origin,
                              seasonal=birds_checklist$seasonal)
  birds_checklist<-birds_checklist[, .(presence=min(presence), origin=min(origin), seasonal=min(seasonal)),
                                   by=list(binomial)]
  birds_checklist$state<-state_item$NAME_1
  birds_checklist$country<-state_item$NAME_0
  saveRDS(birds_checklist, target)
}

#check result
if (F){
  amazon<-data.table(state_list)
  amazon<-amazon[NAME_0=="Brazil"&NAME_1=="Amazonas"]
  target<-sprintf("../Tables/State_Checklist/%s.%s.rda", amazon$NAME_0,
                  gsub("/", ".", amazon$NAME_1))
  checklists<-readRDS(target)
  filtered_checklists<-checklists[(presence %in% c(1,2))&(origin %in% c(1))&(seasonal %in% c(1:4))]
  
  birdlife_checklist<-readRDS("../Shape/Birdlife_202101_checklist.rda")
  filtered_checklists<-merge(filtered_checklists, birdlife_checklist, 
                             by=c("binomial", "presence", "origin", "seasonal"))
  hist(filtered_checklists$area)
  min_species<-filtered_checklists[area<=quantile(filtered_checklists$area, 0.01)]
  filtered_checklists[binomial %in% min_species$binomial]
  poly<-vessel_fixed[which(vessel_fixed$binomial %in% min_species$binomial),]
  amazon_ploy<-states[which((states$NAME_0=="Brazil")&(states$NAME_1=="Amazonas")),]
  
  plot(st_geometry(amazon_ploy), col="grey")
  plot(st_geometry(poly), add=T, col="red")
  
  #st_intersects(amazon_ploy, poly)
}
