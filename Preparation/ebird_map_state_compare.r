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
ebird_country_state<-readRDS("../Tables/COUNTRY_STATE_list_202112.rda")
country_code<-data.table(read.csv("../Tables/country_code.csv", stringsAsFactors = F))
country_code[COUNTRY=="Brazil"]
target_countries<-c("United States of America", "Russian Federation", "Canada", "Brazil",
                    "Australia", "India", "China", "Argentina", "Algeria", "Kazakhstan",
                    "Congo, (Kinshasa)", "Saudi Arabia", "Greenland", "Mexico")
country<-target_countries[2]
projcrs <- st_crs(states)


state_list$ebird_state_code<-""
state_list_new<-list()
country<-"Indonesia"
for (country in unique(country_code$COUNTRY)){
  target<-sprintf("../Tables/State_codes/%s.rda", country)
  if (file.exists(target)){
    next()
  }
  saveRDS(NULL, target)
  iso_code<-country_code[COUNTRY %in% country]
  state_item<-state_list[GID_0==iso_code$GID_0]
  ebird_item<-ebird_country_state[COUNTRY_CODE %in% iso_code$COUNTRY_CODE]
  state_item<-merge(state_item, ebird_item, by.x="NAME_1", by.y="STATE", all.x=T, all.y=F)
  state_item$ebird_state_code<-state_item$STATE_CODE
  state_item$N<-NULL
  state_item$COUNTRY_CODE<-NULL
  state_item$STATE_CODE<-NULL
  state_item$COUNTRY<-NULL
  if (nrow(state_item[is.na(ebird_state_code)])==0){
    saveRDS(state_item, target)
    next()
  }
  missing_ebird_item<-ebird_item[!(STATE_CODE %in% state_item$ebird_state_code)]
  if (nrow(missing_ebird_item)==0){
    saveRDS(state_item, target)
    next()
  }
  for (j in c(1:nrow(missing_ebird_item))){
    print(paste(j, nrow(missing_ebird_item), country))
    raw_f<-sprintf("../Tables/eBird_Checklist/%s/ebird_raw_%s.rda",
                   missing_ebird_item[j]$COUNTRY_CODE, missing_ebird_item[j]$STATE_CODE)
    if (!file.exists(raw_f)){
      next()
    }
    ebird_records<-readRDS(raw_f)
    cols<-c("LONGITUDE", "LATITUDE")
    ebird_records<-unique(ebird_records[, ..cols])
    ebird_records_p <- st_as_sf(x = ebird_records,                         
                                coords = c("LONGITUDE", "LATITUDE"),
                                crs = projcrs)
    for (i in c(1:nrow(state_item))){
      if (!is.na(state_item[i]$ebird_state_code)){
        next()
      }
      state_layer<-states[which((states$GID_0==state_item[i]$GID_0)&
                                  (states$GID_1==state_item[i]$GID_1)),]
      in_p<-st_contains(state_layer, ebird_records_p)
      proportion<-length(unlist(in_p))/nrow(ebird_records)
      #print(paste(proportion, state_item[i]$NAME_0, state_item[i]$NAME_1))
      if (proportion>0.9){
        state_item[i]$ebird_state_code<-ebird_item[j]$STATE_CODE
      }
      if (F){
        plot(st_geometry(ebird_records_p), col="blue")
        plot(st_geometry(state_layer), add=T)
        plot(st_geometry(ebird_records_p[unlist(in_p),]), add=T, col="red")
      }
    }
  }
  
  saveRDS(state_item, target)
  state_list_new[[country]]<-state_item
  
}
state_list_new<-list()
country<-"Australia"
for (country in unique(country_code$COUNTRY)){
  target<-sprintf("../Tables/State_codes/%s.rda", country)
  state_item<-readRDS(target)
  state_list_new[[country]]<-state_item
}
state_list_new<-rbindlist(state_list_new)
state_list_new[is.na(ebird_state_code)]
cols<-c("COUNTRY_CODE", "GID_0")
state_list_new<-merge(state_list_new, country_code[,..cols], by="GID_0")
state_list_new[NAME_0=="China"]
saveRDS(state_list_new, "../Tables/Full_State_codes.rda")
