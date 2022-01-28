library(sf)
library(data.table)
library(gdalUtilities)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
sf_use_s2(FALSE)

states<-sf::st_read(dsn = "../Shape/gadm36_levels_shp", 
                    layer = "gadm36_1")
state_list<-data.table(GID_0=states$GID_0, GID_1=states$GID_1, 
                       NAME_0=states$NAME_0, NAME_1=states$NAME_1,
                       HASC_1=states$HASC_1)
ecoregion<-sf::st_read(dsn = "../Shape/Ecoregions2017", 
                       layer = "Ecoregions2017")

sp_region_all<-readRDS("../Tables/birdlife_ecoregion.rda")
cols<-c("scientific_name", "REALM")
simpligied_sp_realm<-unique(sp_region_all[, ..cols])
simpligied_sp_realm$REALM_LABEL<-1

cols<-c("scientific_name", "BIOME_NUM")
simpligied_sp_biome<-unique(sp_region_all[, ..cols])
simpligied_sp_biome$BIOME_LABEL<-1

if (F){
  files<-list.dirs("../Tables/eBird_mapbased_checklist", full.names = F)
  blank_list<-c()
  f<-"ABW"
  for (f in files){
    if (length(list.files(sprintf("../Tables/eBird_mapbased_checklist/%s", f)))==0){
      blank_list<-c(blank_list, f)
    }
  }
  state_list[GID_0 %in% blank_list]
  state_list[!(GID_0 %in% files)]
  
  ebird_country_state[COUNTRY_CODE=="BQ"]
}
country_code<-data.table(read.csv("../Tables/country_code.csv", stringsAsFactors = F))
country_code[COUNTRY=="Brazil"]
projcrs <- st_crs(states)
country<-"United States of America"
country<-"Indonesia"
country<-"Aland Islands"
country<-"Anguilla"
country<-"Namibia"
if (F){
  
}
for (country in unique(country_code$COUNTRY)){
  print(country)
  iso_code<-country_code[COUNTRY %in% country]
  state_item<-state_list[GID_0==iso_code$GID_0]
  target<-sprintf("../Tables/eBird_mapbased_checklist_with_realm/%s", iso_code$GID_0)
  if (dir.exists(target)){
    next()
  }
  dir.create(target)
  source<-sprintf("../Tables/eBird_mapbased_checklist/%s", iso_code$GID_0)
  if (nrow(state_item)==0){
    next()
  }
  raw_f<-sprintf("%s/raw.rda", source)
  if (file.exists(raw_f)){
    raw_record<-readRDS(raw_f)
    cols<-c("LOCALITY_ID", "LATITUDE", "LONGITUDE")
    raw_record<-unique(raw_record[, ..cols])
  }else{
    raw_record<-NULL
  }
  for (i in c(1:nrow(state_item))){
    rda<-sprintf("%s/ebird_raw_%s.rda", source, state_item[i]$GID_1)
    if (!file.exists(rda)){
      next()
    }
    record<-readRDS(rda)
    if (nrow(record)==0){
      next()
    }
    if ("LATITUDE" %in% colnames(record)){
      
    }else{
      record<-merge(record, raw_record, by="LOCALITY_ID")
    }
    points<-st_as_sf(x = record,                         
                     coords = c("LONGITUDE", "LATITUDE"),
                     crs = projcrs)
    index<-st_contains(ecoregion, points)
    j=1
    record$BIOME_NUM<--1
    record$ECO_BIOME_<-""
    record$NNH<--1
    record$ECO_ID<--1
    record$REALM<-""
    
    for (j in c(1:length(index))){
      ind<-index[[j]]
      if (length(ind)==0){
        next()
      }
      layer<-ecoregion[j,]
      record[ind]$BIOME_NUM<-layer$BIOME_NUM
      record[ind]$ECO_BIOME_<-layer$ECO_BIOME_
      record[ind]$NNH<-layer$NNH
      record[ind]$ECO_ID<-layer$ECO_ID
      record[ind]$REALM<-layer$REALM
      
    }
    
    record_labeled<-merge(record, simpligied_sp_realm, 
                          by.x=c("SCIENTIFIC_NAME", "REALM"),
                          by.y=c("scientific_name", "REALM"),
                          all.x=T, all.y=F)
    record_labeled<-merge(record_labeled, simpligied_sp_biome, 
                          by.x=c("SCIENTIFIC_NAME", "BIOME_NUM"),
                          by.y=c("scientific_name", "BIOME_NUM"),
                          all.x=T, all.y=F)
    record_labeled[is.na(REALM_LABEL)]$REALM_LABEL<-0
    record_labeled[is.na(BIOME_LABEL)]$BIOME_LABEL<-0
    checklist<-readRDS(sprintf("%s/ebird_checklist_%s.rda", source, state_item[i]$GID_1))
    checklist$REALM_LABEL<-0
    checklist$BIOME_LABEL<-0
    checklist[SCIENTIFIC_NAME %in% unique(record_labeled[REALM_LABEL==1]$SCIENTIFIC_NAME)]$REALM_LABEL<-1
    checklist[SCIENTIFIC_NAME %in% unique(record_labeled[BIOME_LABEL==1]$SCIENTIFIC_NAME)]$BIOME_LABEL<-1
    saveRDS(checklist, sprintf("%s/ebird_checklist_%s.rda", target, state_item[i]$GID_1))
    saveRDS(record_labeled, sprintf("%s/ebird_raw_%s.rda", target, state_item[i]$GID_1))
    if (F){
      table(record_labeled$REALM_LABEL)
      table(record_labeled$BIOME_LABEL)
      table(checklist$REALM_LABEL)
      table(checklist$BIOME_LABEL)
      plot(st_geometry(layer))
      plot(st_geometry(points[ind,]), add=T, col="red")
      points(record[ind]$LONGITUDE, record[ind]$LATITUDE, col="blue")
    }
  }
}