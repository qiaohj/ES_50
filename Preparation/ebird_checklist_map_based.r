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
country<-"United States of America"
country<-"Indonesia"
country<-"Aland Islands"
country<-"Anguilla"
country<-"China"
country<-"Bonaire Sint Eustatius and Saba"
country_code[COUNTRY_CODE=="BQ"]

for (country in rev(unique(country_code$COUNTRY))){
  iso_code<-country_code[COUNTRY %in% country]
  target<-sprintf("../Tables/eBird_mapbased_checklist/%s", iso_code$GID_0)
  if (dir.exists(target)){
    next()
  }
  dir.create(target)
  print(country)
  state_item<-state_list[GID_0==iso_code$GID_0]
  if (nrow(state_item)==0){
    next()
  }
  ebird_item<-ebird_country_state[COUNTRY_CODE=="NA"]
  if (nrow(ebird_item)==0){
    next()
  }
  missing_states<-state_item[!(NAME_1 %in% ebird_item$STATE)]
  if (nrow(missing_states)==0){
    for (j in c(1:nrow(state_item))){
      ebird_sub<-ebird_item[STATE==state_item[j]$NAME_1]
      source<-sprintf("../Tables/eBird_Checklist/%s/ebird_checklist_%s.rda", 
                      ebird_sub$COUNTRY_CODE, ebird_sub$STATE_CODE)
      #dfff<-readRDS(source)
      target_f<-sprintf("%s/ebird_checklist_%s.rda", target, state_item[j]$GID_1)
      file.copy(source, target_f)
      
      source<-sprintf("../Tables/eBird_Checklist/%s/ebird_raw_%s.rda", 
                      ebird_sub$COUNTRY_CODE, ebird_sub$STATE_CODE)
      #dfff<-readRDS(source)
      target_f<-sprintf("%s/ebird_raw_%s.rda", target, state_item[j]$GID_1)
      file.copy(source, target_f)
      
    }
  }else{
    ebird_raw<-list()
    for (j in c(1:nrow(ebird_item))){
      print(paste(country, j, nrow(ebird_item), country))
      raw_f<-sprintf("../Tables/eBird_Checklist/%s/ebird_raw_%s.rda",
                     ebird_item[j]$COUNTRY_CODE, ebird_item[j]$STATE_CODE)
      if (!file.exists(raw_f)){
        next()
      }
      ebird_records<-readRDS(raw_f)
      cols<-c("SCIENTIFIC_NAME", "LONGITUDE", "LATITUDE")
      ebird_raw[[as.character(j)]]<-ebird_records
    }
    ebird_raw<-rbindlist(ebird_raw)
    saveRDS(ebird_raw, sprintf("%s/raw.rda", target))
    ebird_records_p <- st_as_sf(x = ebird_raw,                         
                                coords = c("LONGITUDE", "LATITUDE"),
                                crs = projcrs)
    i=1
    for (i in c(1:nrow(state_item))){
      print(paste(country, i, nrow(state_item)))
      state_layer<-states[which((states$GID_0==state_item[i]$GID_0)&
                                  (states$GID_1==state_item[i]$GID_1)),]
      in_p<-st_contains(state_layer, ebird_records_p)
      
      all_records<-data.table(ebird_records_p[unlist(in_p),])
      all_records$geometry<-NULL
      saveRDS(all_records, sprintf("%s/ebird_raw_%s.rda", target, state_item[i]$GID_1))
      ebird_records_states_se<-all_records[, .(N=.N, N_Locality=length(unique(LOCALITY_ID)),
                                               N_Observer=length(unique(OBSERVER_ID))),
                                           by=list(SCIENTIFIC_NAME, COUNTRY, COUNTRY_CODE,
                                                   STATE, STATE_CODE, GID_0, GID_1)]
      
      saveRDS(ebird_records_states_se, 
              sprintf("%s/ebird_checklist_%s.rda", target, state_item[i]$GID_1))
      if (F){
        plot(st_geometry(ebird_records_p), col="blue")
        plot(st_geometry(state_layer), add=T)
        plot(st_geometry(ebird_records_p[unlist(in_p),]), add=T, col="red")
      }
    }
  }
}
