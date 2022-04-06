library(data.table)

item<-readRDS("/media/huijieqiao/SSD_Fast/ES50_eBird/Tables/eBird_mapbased_checklist_with_realm/ALB/ebird_raw_ALB.1_1.rda")        

library(sf)
library(data.table)
library(gdalUtilities)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
sf_use_s2(FALSE)

if (F){
  states<-sf::st_read(dsn = "../Shape/gadm36_levels_shp", 
                      layer = "gadm36_1")
  state_list<-data.table(GID_0=states$GID_0, GID_1=states$GID_1, 
                         NAME_0=states$NAME_0, NAME_1=states$NAME_1,
                         HASC_1=states$HASC_1)
  saveRDS(state_list, "../Tables/state_code.rda")
}
if (F){
  state_list<-readRDS("../Tables/state_code.rda")
  
  country_code<-data.table(read.csv("../Tables/country_code.csv", stringsAsFactors = F))
  country_code[COUNTRY=="Brazil"]
  projcrs <- st_crs(states)
  country<-"United States of America"
  country<-"Indonesia"
  country<-"Aland Islands"
  country<-"Anguilla"
  country<-"Namibia"
  biome_list<-list()
  ecoregion_list<-list()
  realm_list<-list()
  for (country in unique(country_code$COUNTRY)){
    
    iso_code<-country_code[COUNTRY %in% country]
    state_item<-state_list[GID_0==iso_code$GID_0]
    if (nrow(state_item)==0){
      next()
    }
    source<-sprintf("../Tables/eBird_mapbased_checklist_with_realm/%s", iso_code$GID_0)
    
    for (i in c(1:nrow(state_item))){
      state_code<-state_item[i]
      print(paste(country, state_code$NAME_1))
      raw_f<-sprintf("%s/ebird_raw_%s.rda", source, state_code$GID_1)
      if (file.exists(raw_f)){
        df_item<-readRDS(raw_f)
        biome<-df_item[, .(N=.N), by=list(SCIENTIFIC_NAME, BIOME_NUM)]
        biome_list[[length(biome_list)+1]]<-biome
        realm<-df_item[, .(N=.N), by=list(SCIENTIFIC_NAME, REALM)]
        realm_list[[length(realm_list)+1]]<-realm
        ecoregion<-df_item[, .(N=.N), by=list(SCIENTIFIC_NAME, ECO_ID)]
        ecoregion_list[[length(ecoregion_list)+1]]<-ecoregion
      }
      
    }
  }
  biome_df<-rbindlist(biome_list)
  ecoregion_df<-rbindlist(ecoregion_list)
  realm_df<-rbindlist(realm_list)
  saveRDS(biome_df, "/media/huijieqiao/SSD_Fast/biome_df.rda")
  saveRDS(ecoregion_df, "/media/huijieqiao/SSD_Fast/ecoregion_df.rda")
  saveRDS(realm_df, "/media/huijieqiao/SSD_Fast/realm_df.rda")
}
if (F){
  biome_df<-readRDS("/media/huijieqiao/SSD_Fast/biome_df.rda")
  biome_df_se<-biome_df[, .(N=sum(N)), by=list(SCIENTIFIC_NAME, BIOME_NUM)]
  saveRDS(biome_df_se, "/media/huijieqiao/SSD_Fast/biome_df2.rda")
  
  ecoregion_df<-readRDS("/media/huijieqiao/SSD_Fast/ecoregion_df.rda")
  ecoregion_df_se<-ecoregion_df[, .(N=sum(N)), by=list(SCIENTIFIC_NAME, ECO_ID)]
  saveRDS(ecoregion_df_se, "/media/huijieqiao/SSD_Fast/ecoregion_df2.rda")
  
  realm_df<-readRDS("/media/huijieqiao/SSD_Fast/realm_df.rda")
  realm_df_se<-realm_df[, .(N=sum(N)), by=list(SCIENTIFIC_NAME, REALM)]
  saveRDS(biome_df_se, "/media/huijieqiao/SSD_Fast/biome_df2.rda")
}
