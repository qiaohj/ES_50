library(data.table)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
state_list<-readRDS("../Tables/Full_State_codes.rda")

country_id<-unique(state_list$GID_0)[1]
all_checklist<-list()
for (country_id in unique(state_list$GID_0)){
  country_item<-state_list[GID_0==country_id]
  state_id<-unique(country_item$GID_1)[1]
  for (state_id in unique(country_item$GID_1)){
    state_item<-country_item[GID_1==state_id]
    checklist<-readRDS(sprintf("../Tables/State_Checklist/%s.%s.rda",
                               state_item$NAME_0, gsub("/", ".", state_item$NAME_1)))
    checklist$GID_0<-state_item$GID_0
    checklist$GID_1<-state_item$GID_1
    all_checklist[[paste(state_item$GID_0, state_item$GID_1)]]<-checklist
  }
}
all_checklist_csv<-rbindlist(all_checklist)
all_checklist_csv<-all_checklist_csv[(presence %in% c(1,2))&
                                       (origin %in% c(1))&
                                       (seasonal %in% c(1:4))]
write.csv(all_checklist_csv, "../Tables/Birdlife_Checklist/country_state_checklist.csv", row.names=F)
table(all_checklist_csv$presence)
table(all_checklist_csv$seasonal)
table(all_checklist_csv$origin)
