library(sf)
library(data.table)
library(raster)
library(taxize)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
birdlife_ecoregion<-readRDS("../Tables/birdlife_ecoregion_native_only.rda")

ebirdchecklist<-readRDS("../Tables/eBird_Checklist/ebird_realm_checklist.rda")
ebird_sp<-unique(ebirdchecklist$SCIENTIFIC_NAME)
birdlife_sp<-unique(birdlife_ecoregion$scientific_name)
#Harvest all synonym from IUCN
sp<-birdlife_sp[1]
for (sp in birdlife_sp){
  print(sp)
  target<-sprintf("../Tables/IUCN_Synonym/%s.rda", sp)
  if (file.exists(target)){
    print("SKIP")
    next()
  }
  syn<-synonyms(sp, db="iucn")
  saveRDS(syn, target)
}
synonyms("Garrulax cinereifrons", db="iucn")

#Merge IUCN Name
all_syns<-list()
for (sp in birdlife_sp){
  syn<-readRDS(sprintf("../Tables/IUCN_Synonym/%s.rda", sp))
  if (length(syn[[1]])==0){
    next()
  }
  if (is.na(syn)){
    next()
  }
  
  all_syns[[sp]]<-syn[[1]]
}
all_syns<-rbindlist(all_syns)

ebird_sp_df<-data.table(scientific_name=ebird_sp, ebird="Accepted")
birdlife_sp_df<-data.table(scientific_name=birdlife_sp, birdlife="Accepted")
full_df<-merge(ebird_sp_df, birdlife_sp_df, by="scientific_name", all=T)
full_df[is.na(ebird)]$ebird<-""
full_df[is.na(birdlife)]$birdlife<-""
full_df$accepted_name<-""
for (i in c(1:nrow(full_df))){
  print(paste(i, nrow(full_df)))
  if (full_df[i]$accepted_name==""){
    if (full_df[i]$birdlife=="Accepted"){
      full_df[i]$accepted_name<-full_df[i]$scientific_name
    }else{
      acc<-all_syns[synonym==full_df[i]$scientific_name]
      if (nrow(acc)>0){
        full_df[i]$accepted_name<-acc[1]$accepted_name
      }
    }
  }
}

full_df[accepted_name==""]
write.csv(full_df, "~/Downloads/missing_names.csv", row.names = F)


birdlife_realm<-birdlife_ecoregion
birdlife_realm$NNH<-NULL
birdlife_realm$ECO_NAME<-NULL
birdlife_realm$BIOME_NUM<-NULL
birdlife_realm$BIOME_NAME<-NULL
birdlife_realm$ECO_BIOME_<-NULL
birdlife_realm$ECO_ID<-NULL
birdlife_realm$NNH_NAME<-NULL
birdlife_realm<-unique(birdlife_realm)
birdlife_realm<-birdlife_realm[!is.na(REALM)]
missing_names<-data.table(read.csv("../Tables/missed_ranges.csv", stringsAsFactors = F))
missing_names$ebird<-NULL
missing_names$Common_name<-NULL
missing_names$species<-NULL
missing_names$redlist<-NULL
unique(missing_names$region)
unique(birdlife_realm$REALM)
missing_names[region=="North America"]$region<-"Nearctic"
missing_names[region=="Europe"]$region<-"Palearctic"
#missing_names[region=="Asia"]$region<-"Palearctic"
missing_names[region=="Oceania"]$region<-"Australasia"
missing_names[region=="South America"]$region<-"Neotropic"
missing_names[region=="Africa"]$region<-"Afrotropic"
missing_names[region=="Middle East"]$region<-"Palearctic"
missing_names$source<-"avibased"
birdlife_realm$source<-"birdlife"
colnames(missing_names)[2]<-"REALM"

asia_names<-missing_names[REALM=="Asia"]
asia_names_Palearctic<-asia_names
asia_names_Palearctic$REALM<-"Palearctic"
asia_names_Indomalayan<-asia_names
asia_names_Indomalayan$REALM<-"Indomalayan"
missing_names<-missing_names[REALM!="Asia"]
sp_realm<-rbindlist(list(birdlife_realm, missing_names, asia_names_Palearctic, asia_names_Indomalayan))
saveRDS(sp_realm, "../Tables/species_realm.rda")
full_df<-data.table(read.csv("../Tables/missing_names.csv", stringsAsFactors = F))
accepted_names<-full_df[scientific_name==accepted_name]
syns<-full_df[scientific_name!=accepted_name]
missed_names<-syns[accepted_name==""]
syns<-syns[accepted_name!=""]
full_names<-c(accepted_names$scientific_name, syns$accepted_name, missed_names$scientific_name)

ebird_sp[!(ebird_sp %in% full_names)]
full_names[!(full_names %in% sp_realm$scientific_name)]

full_df[scientific_name=="Acrocephalus rehsei"]
missing_names[scientific_name=="Stilpnia nigrocincta"]
birdlife_ecoregion[scientific_name=="Acrocephalus rehsei"]
accepted_names[scientific_name=="Ptilinopus layardi"]
syns




sp_realm<-readRDS("../Tables/species_realm.rda")
new_names<-data.table(read.csv("../Tables/newbirds11.csv", stringsAsFactors = F))
new_names[species=="Acanthis hornemanni"]
new_names[region=="Neotropical"]$region<-"Neotropic"
new_names[region=="Afrotropical"]$region<-"Afrotropic"
new_names[region=="IndoMalayan"]$region<-"Indomalayan"

unique(new_names$region)[!(unique(new_names$region) %in% unique(sp_realm$REALM))]

cols<-c("species", "region")
new_names<-new_names[, ..cols]
colnames(new_names)<-c("scientific_name", "REALM")
new_names$fixed<-T
sp_realm_2<-merge(sp_realm, new_names, by=c("scientific_name", "REALM"), all=T)
unique(new_names$scientific_name)[!(unique(new_names$scientific_name) %in% unique(sp_realm$scientific_name))]

sp_realm_2[!is.na(fixed)]
saveRDS(sp_realm_2, "../Tables/species_realm.rda")
sp_realm_2[is.na(source)]

