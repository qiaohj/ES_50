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
i=100
all_checklist<-list()
all_realm_checklist<-list()
all_biome_checklist<-list()
all_ecoregion_checklist<-list()
for (i in c(1:nrow(state_list))){
  print(paste(i, nrow(state_list)))
  state_item<-state_list[i]
  f_checklist<-sprintf("../Tables/eBird_mapbased_checklist_with_realm/%s/ebird_checklist_%s.rda",
                       state_item$GID_0, gsub("/", ".", state_item$GID_1))
  f_raw<-sprintf("../Tables/eBird_mapbased_checklist_with_realm/%s/ebird_raw_%s.rda",
                       state_item$GID_0, gsub("/", ".", state_item$GID_1))
  if (!file.exists(f_checklist)){
    next()
  }
  checklist<-readRDS(f_checklist)
  checklist$GID_1<-state_item$GID_1
  checklist$STATE<-state_item$NAME_1
  all_checklist[[paste(state_item$GID_0, state_item$GID_1)]]<-checklist
  
  raw<-readRDS(f_raw)
  realm_checklist<-raw[, .(N=.N, N_LOCALITY=length(unique(LOCALITY_ID)),
                           N_OBSERVATION=sum(OBSERVATION_COUNT)),
                       by=list(SCIENTIFIC_NAME, REALM, REALM_LABEL)]
  all_realm_checklist[[paste(state_item$GID_0, state_item$GID_1)]]<-realm_checklist
  biome_checklist<-raw[, .(N=.N, N_LOCALITY=length(unique(LOCALITY_ID)),
                           N_OBSERVATION=sum(OBSERVATION_COUNT)),
                       by=list(SCIENTIFIC_NAME, BIOME_NUM, BIOME_LABEL)]
  all_biome_checklist[[paste(state_item$GID_0, state_item$GID_1)]]<-biome_checklist
  ecoregion_checklist<-raw[, .(N=.N, N_LOCALITY=length(unique(LOCALITY_ID)),
                           N_OBSERVATION=sum(OBSERVATION_COUNT)),
                       by=list(SCIENTIFIC_NAME, ECO_ID)]
  all_ecoregion_checklist[[paste(state_item$GID_0, state_item$GID_1)]]<-ecoregion_checklist
}

all_checklist_b<-rbindlist(all_checklist)
all_realm_checklist_b<-rbindlist(all_realm_checklist)
all_biome_checklist_b<-rbindlist(all_biome_checklist)
all_ecoregion_checklist_b<-rbindlist(all_ecoregion_checklist)

saveRDS(all_checklist_b, "../Tables/eBird_Checklist/ebird_state_checklist.rda")
saveRDS(all_realm_checklist_b, "../Tables/eBird_Checklist/ebird_realm_checklist.rda")
saveRDS(all_biome_checklist_b, "../Tables/eBird_Checklist/ebird_biome_checklist.rda")
saveRDS(all_ecoregion_checklist_b, "../Tables/eBird_Checklist/ebird_ecoregion_checklist.rda")
all_checklist_country<-all_checklist_b[, .(N=sum(N), N_LOCALITY=sum(N_Locality)),
                                       by=list(SCIENTIFIC_NAME, COUNTRY, COUNTRY_CODE, GID_0,
                                               REALM_LABEL, BIOME_LABEL)]
saveRDS(all_checklist_country, "../Tables/eBird_Checklist/ebird_country_checklist.rda")
write.csv(all_checklist_b, "../Tables/eBird_Checklist/ebird_state_checklist.csv", row.names = F)
write.csv(all_checklist_country, "../Tables/eBird_Checklist/ebird_country_checklist.csv", row.names = F)
realm_checklist<-all_realm_checklist_b[, .(N=sum(N), N_LOCALITY=sum(N_LOCALITY)),
                                       by=list(SCIENTIFIC_NAME, REALM, REALM_LABEL)]
biome_checklist<-all_biome_checklist_b[, .(N=sum(N), N_LOCALITY=sum(N_LOCALITY)),
                                       by=list(SCIENTIFIC_NAME, BIOME_NUM, BIOME_LABEL)]

ecoregion_checklist<-all_ecoregion_checklist_b[, .(N=sum(N), N_LOCALITY=sum(N_LOCALITY)),
                                       by=list(SCIENTIFIC_NAME, ECO_ID)]
write.csv(realm_checklist, "../Tables/eBird_Checklist/ebird_realm_checklist.csv", row.names = F)
write.csv(biome_checklist, "../Tables/eBird_Checklist/ebird_biome_checklist.csv", row.names = F)
write.csv(ecoregion_checklist, "../Tables/eBird_Checklist/ebird_ecoregion_checklist.csv", row.names = F)

