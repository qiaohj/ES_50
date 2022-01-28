library(data.table)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
lists<-list.files("../Tables/Species_ecoregion", pattern="\\.rda")
f<-lists[1]
realm_list<-list()
ecoregion_list<-list()
biome_list<-list()
eco_biome_list<-list()
for (f in lists){
  df<-readRDS(sprintf("../Tables/Species_ecoregion/%s", f))
  cols<-c("scientific_name", "REALM")
  realm_list[[f]]<-unique(df[, ..cols])
  cols<-c("scientific_name", "ECO_NAME", "ECO_ID")
  ecoregion_list[[f]]<-unique(df[, ..cols])
  cols<-c("scientific_name", "BIOME_NAME", "BIOME_NUM")
  biome_list[[f]]<-unique(df[, ..cols])
  cols<-c("scientific_name", "BIOME_NAME", "BIOME_NUM", "ECO_NAME", "ECO_ID", "ECO_BIOME_", "REALM")
  eco_biome_list[[f]]<-unique(df[, ..cols])
  
}
realm_csv<-rbindlist(realm_list)
ecoregion_csv<-rbindlist(ecoregion_list)
biome_csv<-rbindlist(biome_list)
eco_biome_csv<-rbindlist(eco_biome_list)

write.csv(realm_csv, "../Tables/Birdlife_Checklist/realm_checklist.csv", row.names=F)
write.csv(ecoregion_csv, "../Tables/Birdlife_Checklist/ecoregion_checklist.csv", row.names=F)
write.csv(biome_csv, "../Tables/Birdlife_Checklist/biome_checklist.csv", row.names=F)
write.csv(eco_biome_csv, "../Tables/Birdlife_Checklist/eco_biome_checklist.csv", row.names=F)

