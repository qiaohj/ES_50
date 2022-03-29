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
for (i in c(1:length(country_codes))){
  country<-country_codes[i]
  state_codes<-unique(state_list[GID_0==country]$GID_1)
  for (j in c(1:length(state_codes))){
    
    state<-state_codes[j]
    print(paste(i, "/", length(country_codes), ":", country, j, "/", length(state_codes), ":", state))
    rda<-sprintf("../Tables/eBird_mapbased_checklist_with_realm/%s/ebird_build_road_%s.rda", 
                 country, state)
    
    if (!file.exists(rda)){
      next()
    }
    checklist<-readRDS(sprintf("../Tables/eBird_mapbased_checklist_with_realm/%s/ebird_checklist_%s.rda", 
                               country, state))
    records<-readRDS(rda)
    records[build_5km==65535]$build_5km<-NA
    records[road_5km==65535]$road_5km<-NA
    records$build_2km_inout<-ifelse(is.na(records$build_2km), "out", "in")
    records$build_5km_inout<-ifelse(is.na(records$build_5km), "out", "in")
    records$road_2km_inout<-ifelse(is.na(records$road_2km), "out", "in")
    records$road_5km_inout<-ifelse(is.na(records$road_5km), "out", "in")
    records_build_2km<-records[, .(N=sum(N)), by=list(SCIENTIFIC_NAME, build_2km_inout)]
    records_build_2km_in<-records_build_2km[build_2km_inout=="in"]
    colnames(records_build_2km_in)<-c("SCIENTIFIC_NAME", "build_2km_in", "N_build_2km_in")
    records_build_2km_out<-records_build_2km[build_2km_inout=="out"]
    colnames(records_build_2km_out)<-c("SCIENTIFIC_NAME", "build_2km_out", "N_build_2km_out")
    records_build_2km_merged<-merge(records_build_2km_in, records_build_2km_out, by="SCIENTIFIC_NAME", all=T)
    cols<-c("SCIENTIFIC_NAME", "N_build_2km_in", "N_build_2km_out")
    records_build_2km_merged<-records_build_2km_merged[, ..cols]
    records_build_2km_merged[is.na(N_build_2km_in)]$N_build_2km_in<-0
    records_build_2km_merged[is.na(N_build_2km_out)]$N_build_2km_out<-0
    records_build_2km_merged$P_build_2km_in<-records_build_2km_merged$N_build_2km_in/
      (records_build_2km_merged$N_build_2km_in+records_build_2km_merged$N_build_2km_out)
    checklist_merged<-merge(checklist, records_build_2km_merged, by="SCIENTIFIC_NAME")
    
    records_build_5km<-records[, .(N=sum(N)), by=list(SCIENTIFIC_NAME, build_5km_inout)]
    records_build_5km_in<-records_build_5km[build_5km_inout=="in"]
    colnames(records_build_5km_in)<-c("SCIENTIFIC_NAME", "build_5km_in", "N_build_5km_in")
    records_build_5km_out<-records_build_5km[build_5km_inout=="out"]
    colnames(records_build_5km_out)<-c("SCIENTIFIC_NAME", "build_5km_out", "N_build_5km_out")
    records_build_5km_merged<-merge(records_build_5km_in, records_build_5km_out, by="SCIENTIFIC_NAME", all=T)
    cols<-c("SCIENTIFIC_NAME", "N_build_5km_in", "N_build_5km_out")
    records_build_5km_merged<-records_build_5km_merged[, ..cols]
    records_build_5km_merged[is.na(N_build_5km_in)]$N_build_5km_in<-0
    records_build_5km_merged[is.na(N_build_5km_out)]$N_build_5km_out<-0
    records_build_5km_merged$P_build_5km_in<-records_build_5km_merged$N_build_5km_in/
      (records_build_5km_merged$N_build_5km_in+records_build_5km_merged$N_build_5km_out)
    checklist_merged<-merge(checklist_merged, records_build_5km_merged, by="SCIENTIFIC_NAME")
    
    
    records_road_2km<-records[, .(N=sum(N)), by=list(SCIENTIFIC_NAME, road_2km_inout)]
    records_road_2km_in<-records_road_2km[road_2km_inout=="in"]
    colnames(records_road_2km_in)<-c("SCIENTIFIC_NAME", "road_2km_in", "N_road_2km_in")
    records_road_2km_out<-records_road_2km[road_2km_inout=="out"]
    colnames(records_road_2km_out)<-c("SCIENTIFIC_NAME", "road_2km_out", "N_road_2km_out")
    records_road_2km_merged<-merge(records_road_2km_in, records_road_2km_out, by="SCIENTIFIC_NAME", all=T)
    cols<-c("SCIENTIFIC_NAME", "N_road_2km_in", "N_road_2km_out")
    records_road_2km_merged<-records_road_2km_merged[, ..cols]
    records_road_2km_merged[is.na(N_road_2km_in)]$N_road_2km_in<-0
    records_road_2km_merged[is.na(N_road_2km_out)]$N_road_2km_out<-0
    records_road_2km_merged$P_road_2km_in<-records_road_2km_merged$N_road_2km_in/
      (records_road_2km_merged$N_road_2km_in+records_road_2km_merged$N_road_2km_out)
    checklist_merged<-merge(checklist_merged, records_road_2km_merged, by="SCIENTIFIC_NAME")
    
    records_road_5km<-records[, .(N=sum(N)), by=list(SCIENTIFIC_NAME, road_5km_inout)]
    records_road_5km_in<-records_road_5km[road_5km_inout=="in"]
    colnames(records_road_5km_in)<-c("SCIENTIFIC_NAME", "road_5km_in", "N_road_5km_in")
    records_road_5km_out<-records_road_5km[road_5km_inout=="out"]
    colnames(records_road_5km_out)<-c("SCIENTIFIC_NAME", "road_5km_out", "N_road_5km_out")
    records_road_5km_merged<-merge(records_road_5km_in, records_road_5km_out, by="SCIENTIFIC_NAME", all=T)
    cols<-c("SCIENTIFIC_NAME", "N_road_5km_in", "N_road_5km_out")
    records_road_5km_merged<-records_road_5km_merged[, ..cols]
    records_road_5km_merged[is.na(N_road_5km_in)]$N_road_5km_in<-0
    records_road_5km_merged[is.na(N_road_5km_out)]$N_road_5km_out<-0
    records_road_5km_merged$P_road_5km_in<-records_road_5km_merged$N_road_5km_in/
      (records_road_5km_merged$N_road_5km_in+records_road_5km_merged$N_road_5km_out)
    checklist_merged<-merge(checklist_merged, records_road_5km_merged, by="SCIENTIFIC_NAME")
    
    saveRDS(checklist_merged, sprintf("../Tables/eBird_mapbased_checklist_with_realm/%s/ebird_checklist_with_build_road_%s.rda", 
                               country, state))
  }
}



state_checklist<-list()
state_all<-list()
for (i in c(1:length(country_codes))){
  country<-country_codes[i]
  state_codes<-unique(state_list[GID_0==country]$GID_1)
  state_item_checklist<-list()
  state_item_all<-list()
  for (j in c(1:length(state_codes))){
    
    state<-state_codes[j]
    print(paste(i, "/", length(country_codes), ":", country, j, "/", length(state_codes), ":", state))
    rda<-sprintf("../Tables/eBird_mapbased_checklist_with_realm/%s/ebird_build_road_%s.rda", 
                 country, state)
    
    if (!file.exists(rda)){
      next()
    }
    label<-paste(country, state)
    checklist<-readRDS(sprintf("../Tables/eBird_mapbased_checklist_with_realm/%s/ebird_checklist_with_build_road_%s.rda", 
                               country, state))
    state_item_checklist[[label]]<-checklist
    all<-checklist[, .(N=sum(N), 
                      N_build_2km_in=sum(N_build_2km_in), N_build_2km_out=sum(N_build_2km_out),
                      N_build_5km_in=sum(N_build_5km_in), N_build_5km_out=sum(N_build_5km_out),
                      N_road_2km_in=sum(N_road_2km_in), N_road_2km_out=sum(N_road_2km_out),
                      N_road_5km_in=sum(N_road_5km_in), N_road_5km_out=sum(N_road_5km_out)),
                   by=list(COUNTRY, STATE, GID_0, GID_1)]
    all$P_build_2km_in<-all$N_build_2km_in/(all$N_build_2km_in+all$N_build_2km_out)
    all$P_build_5km_in<-all$N_build_5km_in/(all$N_build_5km_in+all$N_build_5km_out)
    all$P_road_2km_in<-all$N_road_2km_in/(all$N_road_2km_in+all$N_road_2km_out)
    all$P_road_5km_in<-all$N_road_5km_in/(all$N_road_5km_in+all$N_road_5km_out)
    state_item_all[[label]]<-all
  }
  state_item_checklist<-rbindlist(state_item_checklist)
  state_item_all<-rbindlist(state_item_all)
  state_checklist[[country]]<-state_item_checklist
  state_all[[country]]<-state_item_all
}
state_checklist<-rbindlist(state_checklist)
state_all<-rbindlist(state_all)
country_checklist<-state_checklist[, .(N=sum(N), 
                                       N_build_2km_in=sum(N_build_2km_in), N_build_2km_out=sum(N_build_2km_out),
                                       N_build_5km_in=sum(N_build_5km_in), N_build_5km_out=sum(N_build_5km_out),
                                       N_road_2km_in=sum(N_road_2km_in), N_road_2km_out=sum(N_road_2km_out),
                                       N_road_5km_in=sum(N_road_5km_in), N_road_5km_out=sum(N_road_5km_out)),
                                   by=list(COUNTRY, GID_0, SCIENTIFIC_NAME)]

country_checklist$P_build_2km_in<-country_checklist$N_build_2km_in/
  (country_checklist$N_build_2km_in+country_checklist$N_build_2km_out)
country_checklist$P_build_5km_in<-country_checklist$N_build_5km_in/
  (country_checklist$N_build_5km_in+country_checklist$N_build_5km_out)
country_checklist$P_road_2km_in<-country_checklist$N_road_2km_in/
  (country_checklist$N_road_2km_in+country_checklist$N_road_2km_out)
country_checklist$P_road_5km_in<-country_checklist$N_road_5km_in/
  (country_checklist$N_road_5km_in+country_checklist$N_road_5km_out)


country_all<-state_all[, .(N=sum(N), 
                                       N_build_2km_in=sum(N_build_2km_in), N_build_2km_out=sum(N_build_2km_out),
                                       N_build_5km_in=sum(N_build_5km_in), N_build_5km_out=sum(N_build_5km_out),
                                       N_road_2km_in=sum(N_road_2km_in), N_road_2km_out=sum(N_road_2km_out),
                                       N_road_5km_in=sum(N_road_5km_in), N_road_5km_out=sum(N_road_5km_out)),
                                   by=list(COUNTRY, GID_0)]

country_all$P_build_2km_in<-country_all$N_build_2km_in/
  (country_all$N_build_2km_in+country_all$N_build_2km_out)
country_all$P_build_5km_in<-country_all$N_build_5km_in/
  (country_all$N_build_5km_in+country_all$N_build_5km_out)
country_all$P_road_2km_in<-country_all$N_road_2km_in/
  (country_all$N_road_2km_in+country_all$N_road_2km_out)
country_all$P_road_5km_in<-country_all$N_road_5km_in/
  (country_all$N_road_5km_in+country_all$N_road_5km_out)

saveRDS(state_checklist, "../Tables/state_checklist_build_road.rda")
saveRDS(state_all, "../Tables/state_all_build_road.rda")
saveRDS(country_checklist, "../Tables/country_checklist_build_road.rda")
saveRDS(country_all, "../Tables/country_all_build_road.rda")

write.csv(state_checklist, "../Tables/state_checklist_build_road.csv", row.names = F)
write.csv(state_all, "../Tables/state_all_build_road.csv", row.names = F)
write.csv(country_checklist, "../Tables/country_checklist_build_road.csv", row.names = F)
write.csv(country_all, "../Tables/country_all_build_road.csv", row.names = F)
