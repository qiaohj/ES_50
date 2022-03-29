library(data.table)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")

realm_ebird<-data.table(read.csv("../Tables/eBird_Checklist/ebird_realm_checklist.csv", stringsAsFactors = F))
biome_ebird<-data.table(read.csv("../Tables/eBird_Checklist/ebird_biome_checklist.csv", stringsAsFactors = F))
ecoregion_ebird<-data.table(read.csv("../Tables/eBird_Checklist/ebird_ecoregion_checklist.csv", stringsAsFactors = F))

realm_birdlife<-data.table(read.csv("../Tables/Birdlife_Checklist/realm_checklist.csv", stringsAsFactors=F))
ecoregion_birdlife<-data.table(read.csv("../Tables/Birdlife_Checklist/ecoregion_checklist.csv", stringsAsFactors=F))
biome_birdlife<-data.table(read.csv("../Tables/Birdlife_Checklist/biome_checklist.csv", stringsAsFactors=F))


realm_birdlife<-realm_birdlife[!is.na(REALM)]
realm_ebird<-realm_ebird[REALM!=""]
unique(realm_birdlife$REALM)
unique(realm_ebird$REALM)
realm_ebird[REALM==""]
realm<-unique(c(realm_ebird$REALM, realm_birdlife$REALM))[1]
df_full<-list()
for (realm in unique(c(realm_ebird$REALM, realm_birdlife$REALM))){
  checklist_ebird<-unique(realm_ebird[REALM==realm]$SCIENTIFIC_NAME)
  checklist_birdlife<-unique(realm_birdlife[REALM==realm]$scientific_name)
  N_ebird<-length(checklist_ebird)
  N_birdlife<-length(checklist_birdlife)
  overlapped<-checklist_ebird[checklist_ebird %in% checklist_birdlife]
  N_overlapped<-length(overlapped)
  N_completeness_birdlife<-N_overlapped/N_birdlife
  item<-data.table(REALM=realm, N_ebird=N_ebird, N_birdlife=N_birdlife, N_overlapped=N_overlapped, N_completeness_birdlife=N_completeness_birdlife)
  df_full[[realm]]<-item
}
df_full<-rbindlist(df_full)
write.csv(df_full, "../Tables/realm_completeness/realm_completeness.csv", row.names = F)

biome_birdlife<-biome_birdlife[!is.na(BIOME_NUM)]
biome_birdlife<-biome_birdlife[BIOME_NAME!="N/A"]

biome_ebird<-biome_ebird[BIOME_NUM!=-1]
biome_birdlife[, .(N=.N), by=list(BIOME_NUM, BIOME_NAME)]
#biome_ebird[, .(N=.N), by=list(BIOME_NUM, BIOME_NAME)]
biome<-unique(c(biome_birdlife$BIOME_NUM, biome_ebird$BIOME_NUM))[1]
df_full<-list()
for (biome in unique(c(biome_birdlife$BIOME_NUM, biome_ebird$BIOME_NUM))){
  biome_name<-biome_birdlife[BIOME_NUM==biome]$BIOME_NAME[1]
  checklist_ebird<-unique(biome_ebird[BIOME_NUM==biome]$SCIENTIFIC_NAME)
  checklist_birdlife<-unique(biome_birdlife[BIOME_NUM==biome]$scientific_name)
  N_ebird<-length(checklist_ebird)
  N_birdlife<-length(checklist_birdlife)
  overlapped<-checklist_ebird[checklist_ebird %in% checklist_birdlife]
  N_overlapped<-length(overlapped)
  N_completeness_birdlife<-N_overlapped/N_birdlife
  item<-data.table(BIOME_NUM=biome, BIOME_NAME=biome_name,
                   N_ebird=N_ebird, N_birdlife=N_birdlife, 
                   N_overlapped=N_overlapped, 
                   N_completeness_birdlife=N_completeness_birdlife)
  df_full[[biome_name]]<-item
}
df_full<-rbindlist(df_full)
write.csv(df_full, "../Tables/realm_completeness/biome_completeness.csv", row.names = F)

ecoregion_birdlife<-ecoregion_birdlife[!is.na(ECO_ID)]
ecoregion_ebird<-ecoregion_ebird[!is.na(ECO_ID)]
ecoregion_ebird<-ecoregion_ebird[ECO_ID!=-1]
xx<-ecoregion_birdlife[, .(N=.N), by=list(ECO_NAME, ECO_ID)]
xx<-xx[, .(N=.N), by=list(ECO_ID)]
#biome_ebird[, .(N=.N), by=list(BIOME_NUM, BIOME_NAME)]
eco_id<-unique(c(ecoregion_birdlife$ECO_ID, ecoregion_ebird$ECO_ID))[1]
df_full<-list()
for (eco_id in unique(c(ecoregion_birdlife$ECO_ID, ecoregion_ebird$ECO_ID))){
  eco_name<-ecoregion_birdlife[ECO_ID==eco_id]$ECO_NAME[1]
  checklist_ebird<-unique(ecoregion_ebird[ECO_ID==eco_id]$SCIENTIFIC_NAME)
  checklist_birdlife<-unique(ecoregion_birdlife[ECO_ID==eco_id]$scientific_name)
  N_ebird<-length(checklist_ebird)
  N_birdlife<-length(checklist_birdlife)
  overlapped<-checklist_ebird[checklist_ebird %in% checklist_birdlife]
  N_overlapped<-length(overlapped)
  N_completeness_birdlife<-N_overlapped/N_birdlife
  item<-data.table(ECO_ID=eco_id, ECO_NAME=eco_name,
                   N_ebird=N_ebird, N_birdlife=N_birdlife, 
                   N_overlapped=N_overlapped, 
                   N_completeness_birdlife=N_completeness_birdlife)
  df_full[[eco_name]]<-item
}
df_full<-rbindlist(df_full)
write.csv(df_full, "../Tables/realm_completeness/ecoregion_completeness.csv", row.names = F)
