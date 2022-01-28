library(sf)
library(data.table)
library(gdalUtilities)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
sf_use_s2(FALSE)
state_list<-readRDS("../Tables/Full_State_codes.rda")
sensitive_species<-data.table(read.csv("../Tables/ebird_sensitive_species.csv", head=T, stringsAsFactors = F))
country_code<-unique(state_list$GID_0)[1]
state_list$birdlife<-0
state_list$ebird<-0
state_list$ebird_realm<-0
state_list$ebird_biome<-0
state_list$overlap<-0
state_list$overlap_realm<-0
state_list$overlap_biome<-0
state_list$in_sensitive<-0
state_list$in_sensitive_not_ebird<-0
state_list_new<-list()
country_list_new<-list()
country_code<-"TWN"
for (country_code in unique(state_list$GID_0)){
  print(country_code)
  country_item<-state_list[GID_0==country_code]
  birdlife_checklist<-list()
  ebird_checklist<-list()
  i=7
  for (i in c(1:nrow(country_item))){
    state_item<-country_item[i]
    birdlist_state_checklist<-readRDS(sprintf("../Tables/State_Checklist/%s.%s.rda", 
                                               state_item$NAME_0, 
                                              gsub("/", ".", state_item$NAME_1)))
    birdlist_state_checklist<-birdlist_state_checklist[(presence %in% c(1,2))&
                                                         (origin %in% c(1))&
                                                         (seasonal %in% c(1:4))]
    birdlife_checklist[[i]]<-birdlist_state_checklist
    ff<-sprintf("../Tables/eBird_mapbased_checklist_with_realm/%s/ebird_checklist_%s.rda",
                state_item$GID_0, state_item$GID_1)
    if (file.exists(ff)){
      ebird_state_checklist<-readRDS(ff)
      ebird_checklist[[as.character(i)]]<-ebird_state_checklist
    }else{
      ebird_state_checklist<-readRDS("../Tables/eBird_mapbased_checklist_with_realm/blank_table.rda")
    }
    country_item[i]$birdlife<-length(unique(birdlist_state_checklist$binomial))
    country_item[i]$ebird<-length(unique(ebird_state_checklist$SCIENTIFIC_NAME))
    country_item[i]$ebird_realm<-length(unique(ebird_state_checklist[REALM_LABEL==1]$SCIENTIFIC_NAME))
    country_item[i]$ebird_biome<-length(unique(ebird_state_checklist[BIOME_LABEL==1]$SCIENTIFIC_NAME))
    if (nrow(ebird_state_checklist)==0){
      country_item[i]$overlap<-0
      country_item[i]$overlap_realm<-0
      country_item[i]$overlap_biome<-0
    }else{
      country_item[i]$overlap<-length(unique(
        ebird_state_checklist[SCIENTIFIC_NAME %in% birdlist_state_checklist$binomial]$SCIENTIFIC_NAME))
      country_item[i]$overlap_realm<-length(unique(
        ebird_state_checklist[((REALM_LABEL==1)&
                                 (SCIENTIFIC_NAME %in% birdlist_state_checklist$binomial))]$SCIENTIFIC_NAME))
      country_item[i]$overlap_biome<-length(unique(
        ebird_state_checklist[((BIOME_LABEL==1)&
                                 (SCIENTIFIC_NAME %in% birdlist_state_checklist$binomial))]$SCIENTIFIC_NAME))
    }
    country_item[i]$in_sensitive<-length(unique(
      birdlist_state_checklist[binomial %in% sensitive_species$scientific_name]$binomial))
    country_item[i]$in_sensitive_not_ebird<-length(unique(
      birdlist_state_checklist[((binomial %in% sensitive_species$scientific_name)&
                                  (!(binomial %in% ebird_state_checklist$SCIENTIFIC_NAME)))]$binomial))
    
    
  }
  state_list_new[[country_code]]<-country_item
  birdlife_checklist<-rbindlist(birdlife_checklist)
  birdlife_checklist<-unique(birdlife_checklist$binomial)
  ebird_checklist<-rbindlist(ebird_checklist)
  if (nrow(ebird_checklist)==0){
    print(country_item[1]$NAME_0)
    next()
  }
  ebird_checklist_full<-unique(ebird_checklist$SCIENTIFIC_NAME)
  ebird_checklist_realm<-unique(ebird_checklist[REALM_LABEL==1]$SCIENTIFIC_NAME)
  ebird_checklist_biome<-unique(ebird_checklist[BIOME_LABEL==1]$SCIENTIFIC_NAME)
  
  country_df<-country_item[1]
  country_df$birdlife<-length(birdlife_checklist)
  country_df$ebird<-length(ebird_checklist_full)
  country_df$overlap<-length(birdlife_checklist[birdlife_checklist %in% ebird_checklist_full])
  country_df$overlap_realm<-length(birdlife_checklist[birdlife_checklist %in% ebird_checklist_realm])
  country_df$overlap_biome<-length(birdlife_checklist[birdlife_checklist %in% ebird_checklist_biome])
  country_df$in_sensitive<-length(birdlife_checklist[birdlife_checklist %in% sensitive_species$scientific_name])
  country_df$in_sensitive_not_ebird<-length(birdlife_checklist[(birdlife_checklist %in% sensitive_species$scientific_name)&
                                                                 (!(birdlife_checklist %in% ebird_checklist))])
  country_list_new[[country_code]]<-country_df
  
}

country_list_new<-rbindlist(country_list_new)
country_list_new$complessness<-country_list_new$overlap/country_list_new$birdlife
country_list_new$complessness_without_sensitive_species<-country_list_new$overlap/
  (country_list_new$birdlife-country_list_new$in_sensitive_not_ebird)
library(ggplot2)
ggplot(country_list_new)+geom_point(aes(x=complessness, y=complessness_without_sensitive_species))+
  geom_abline()
write.csv(country_list_new, "../Tables/country_compleseness.csv", row.names = F)
state_list_new<-rbindlist(state_list_new)
state_list_new$complessness<-state_list_new$overlap/state_list_new$birdlife
state_list_new$complessness_realm<-state_list_new$overlap_realm/state_list_new$birdlife
state_list_new$complessness_without_sensitive_species<-state_list_new$overlap/
  (state_list_new$birdlife-state_list_new$in_sensitive_not_ebird)
ggplot(state_list_new)+geom_point(aes(x=complessness, y=complessness_realm))+
  geom_abline()
write.csv(state_list_new, "../Tables/state_compleseness.csv", row.names = F)


states<-sf::st_read(dsn = "../Shape/gadm36_levels_shp", 
                    layer = "gadm36_1")

states<-merge(states, state_list_new, by=c( "GID_0", "NAME_0", "GID_1", "NAME_1", "HASC_1"))
sf::st_write(states, "../Shape/Checklist/states.shp", append=FALSE)

countries<-sf::st_read(dsn = "../Shape/gadm36_levels_shp", 
                    layer = "gadm36_0")

countries<-merge(countries, country_list_new, by=c( "GID_0", "NAME_0"))
sf::st_write(countries, "../Shape/Checklist/countries.shp", append=FALSE)

plot(st_geometry(states[which(states$GID_1=="TWN.7_1"),]))
