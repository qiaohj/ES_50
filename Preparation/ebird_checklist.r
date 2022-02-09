library(sf)
library(data.table)
library(gdalUtilities)

setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
sf_use_s2(FALSE)
if (F){
  states<-sf::st_read(dsn = "../Shape/gadm36_levels_shp", 
                      layer = "gadm36_1")
  state_list<-data.table(GID_0=states$GID_0, GID_1=states$GID_1, 
                         NAME_0=states$NAME_0, NAME_1=states$NAME_1)
  saveRDS(state_list, "../Tables/state_list.rda")
}
state_list<-readRDS("../Tables/state_list.rda")
ebird_country_state<-readRDS("../Tables/COUNTRY_STATE_list_202112.rda")
country_code<-data.table(read.csv("../Tables/country_code.csv", stringsAsFactors = F))
i=1
for (i in rev(c(1:nrow(country_code)))){
  country<-country_code[i]
  map_item<-state_list[GID_0==country$GID_0]
  if (nrow(map_item)==0){
    next()
  }
  ebird_country_item<-ebird_country_state[COUNTRY_CODE==country$COUNTRY_CODE]
  if (nrow(ebird_country_item)==0){
    next()
  }
  print(paste(i, nrow(country_code), country$COUNTRY, country$COUNTRY_CODE))
  target<-sprintf("../Tables/eBird_Checklist/%s", country$COUNTRY_CODE)
  if (dir.exists(target)){
    next()
  }
  dir.create(target)
  j=1
  country_checklist<-list()
  for (j in c(1:nrow(ebird_country_item))){
    
    ebird_state_item<-ebird_country_item[j]
    print(paste(i, nrow(country_code), country$COUNTRY, country$COUNTRY_CODE, 
                j, nrow(ebird_country_item), ebird_state_item$STATE, ebird_state_item$STATE_CODE))
    
    all_records<-list()
    
    for (y in c(2010:2019)){
      occ_f<-sprintf("../Tables/COUNTRY_STATE_Split_202112/%s/%s/%d.rda",
                     ebird_state_item$COUNTRY_CODE, ebird_state_item$STATE_CODE, y)
      if (file.exists(occ_f)){
        records<-readRDS(occ_f)
        all_records[[as.character(y)]]<-records
      }
      
    }
    all_records<-rbindlist(all_records)
    if (nrow(all_records)==0){
      next()
    }
    all_records<-all_records[APPROVED==1|REVIEWED==1]
    if (nrow(all_records)==0){
      next()
    }
    all_records$STATE_CODE<-ebird_state_item$STATE_CODE
    all_records$COUNTRY_CODE<-ebird_state_item$COUNTRY_CODE
    all_records$GID_0<-map_item[1]$GID_0
    map_state<-map_item[NAME_1==ebird_state_item$STATE]
    if (nrow(map_state)>0){
      all_records$GID_1<-map_state$GID_1
    }else{
      all_records$GID_1<-""
    }
    saveRDS(all_records, sprintf("%s/ebird_raw_%s.rda", target, ebird_state_item$STATE_CODE))
    species_checklist<-all_records[, .(N=.N, N_Locality=length(unique(LOCALITY_ID)),
                                       N_Observer=length(unique(OBSERVER_ID))),
                                   by=list(SCIENTIFIC_NAME, COUNTRY, COUNTRY_CODE,
                                           STATE, STATE_CODE, GID_0, GID_1)]
    saveRDS(species_checklist, sprintf("%s/ebird_checklist_%s.rda", target, ebird_state_item$STATE_CODE))
    country_checklist[[j]]<-species_checklist
  }
  country_checklist<-rbindlist(country_checklist)
  saveRDS(species_checklist, sprintf("%s/ebird_checklist_%s.rda", target, ebird_state_item$COUNTRY_CODE))
  
}

if (F){
  
  amazon<-data.table(state_list)
  amazon<-amazon[NAME_0=="Brazil"&NAME_1=="Amazonas"]
  
  amazon_ebird<-ebird_country_state[COUNTRY==amazon$NAME_0&STATE==amazon$NAME_1]
  all_records<-list()
  for (y in c(2010:2019)){
    records<-readRDS(sprintf("../Tables/COUNTRY_STATE_Split_202112/%s/%s/%d.rda",
                             amazon_ebird$COUNTRY_CODE, amazon_ebird$STATE_CODE, y))
    all_records[[as.character(y)]]<-records
  }
  all_records<-rbindlist(all_records)
  all_records<-all_records[APPROVED==1|REVIEWED==1]
  species_checklist<-all_records[, .(N=.N, N_Locality=length(unique(LOCALITY_ID)),
                                     N_Observer=length(unique(OBSERVER_ID))),
                                 by=list(SCIENTIFIC_NAME)]
  
  quantile(species_checklist$N)
  colnames(state_list)
  state_list<-data.table(state_list)
  
  cols<-c("NAME_0", "GID_0", "GID_1")
  countries_map_only<-unique(state_list[!(GID_0 %in% country_code$GID_0), ..cols])
  
  cols<-c("COUNTRY", "COUNTRY_CODE")
  countries_ebird_only<-unique(ebird_country_state[!(COUNTRY_CODE %in% country_code$COUNTRY_CODE), ..cols])
}
