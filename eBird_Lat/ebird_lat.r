library(sf)
library(data.table)
library(gdalUtilities)
library(raster)
library(ggplot2)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
clim<-stack(list.files("../Raster/bioclim", pattern="\\.tif", full.names = T))
sf_use_s2(FALSE)

states<-sf::st_read(dsn = "../Shape/gadm36_levels_shp", 
                    layer = "gadm36_1")
state_list<-data.table(GID_0=states$GID_0, GID_1=states$GID_1, 
                       NAME_0=states$NAME_0, NAME_1=states$NAME_1,
                       HASC_1=states$HASC_1)
i=100
cols<-c("LONGITUDE_rough", "LATITUDE_rough")
all<-list()
for (i in c(1:nrow(state_list))){
  print(paste(i, nrow(state_list)))
  state_item<-state_list[i]
  f_raw<-sprintf("../Tables/eBird_mapbased_checklist_with_realm/%s/ebird_raw_%s.rda",
                 state_item$GID_0, gsub("/", ".", state_item$GID_1))
  if (!file.exists(f_raw)){
    next()
  }
  df<-readRDS(f_raw)
  df$LATITUDE_rough<-round(df$LATITUDE, digits=1)
  df$LONGITUDE_rough<-round(df$LONGITUDE, digits=1)
  
  df_se<-df[, .(N=.N), by=list(SCIENTIFIC_NAME, REALM_LABEL, LATITUDE_rough, LONGITUDE_rough)]
  #v<-extract(clim, df_se[, ..cols])
  #df_se<-cbind(df_se, v)
  all[[length(all)+1]]<-df_se
}
all<-readRDS("~/Downloads/xxx.rda")
all<-rbindlist(all)
ll<-unique(all_se[, ..cols])
v<-data.table(extract(clim, ll))
ll_with_v<-cbind(ll, v)
all_with_v<-merge(all, ll_with_v, by=c("LATITUDE_rough", "LONGITUDE_rough"))
saveRDS(all_with_v, "../Objects/eBird_Bioclim.rda")


all_with_v$bio1_int<-round(all_with_v$wc2.1_10m_bio_1)
all_with_v$bio12_int<-round(all_with_v$wc2.1_10m_bio_12/100)*100
all_with_v$LATITUDE_int<-round(all_with_v$LATITUDE_rough)
saveRDS(all_with_v, "../Objects/eBird_Bioclim.rda")

library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
all_with_v<-readRDS("../Objects/eBird_Bioclim.rda")
#For bio1
all_with_v<-all_with_v[!is.na(bio1_int)]
species_richness_bio1_full<-all_with_v[, .(species.richness=length(unique(SCIENTIFIC_NAME))),
                                       by=list(bio1_int)]
species_richness_bio1_full$type="ALL Data"
species_richness_bio1_realm<-all_with_v[REALM_LABEL==1, .(species.richness=length(unique(SCIENTIFIC_NAME))),
                                       by=list(bio1_int)]
species_richness_bio1_realm$type<-"Correct Realm only"


df<-rbindlist(list(species_richness_bio1_realm, species_richness_bio1_full))
ggplot(df)+geom_line(aes(y=species.richness, x=bio1_int, color=type))

#For lat
all_with_v<-all_with_v[!is.na(LATITUDE_int)]
species_richness_lat_full<-all_with_v[, .(species.richness=length(unique(SCIENTIFIC_NAME))),
                                       by=list(LATITUDE_int)]
species_richness_lat_full$type="ALL Data"
species_richness_lat_realm<-all_with_v[REALM_LABEL==1, .(species.richness=length(unique(SCIENTIFIC_NAME))),
                                        by=list(LATITUDE_int)]
species_richness_lat_realm$type<-"Correct Realm only"


df<-rbindlist(list(species_richness_lat_realm, species_richness_lat_full))
ggplot(df)+geom_line(aes(y=species.richness, x=LATITUDE_int, color=type))

#For bio12
all_with_v<-all_with_v[!is.na(bio12_int)]
species_richness_bio12_full<-all_with_v[, .(species.richness=length(unique(SCIENTIFIC_NAME))),
                                       by=list(bio12_int)]
species_richness_bio12_full$type="ALL Data"
species_richness_bio12_realm<-all_with_v[REALM_LABEL==1, .(species.richness=length(unique(SCIENTIFIC_NAME))),
                                        by=list(bio12_int)]
species_richness_bio12_realm$type<-"Correct Realm only"


df<-rbindlist(list(species_richness_bio12_realm, species_richness_bio12_full))
ggplot(df)+geom_line(aes(y=species.richness, x=bio12_int, color=type))


#For lat/area
all_with_v<-all_with_v[!is.na(LATITUDE_int)]
species_richness_lat_full<-all_with_v[, .(species.richness=length(unique(SCIENTIFIC_NAME))),
                                      by=list(LATITUDE_int)]
species.richness<-all_with_v[, .(species.richness=length(unique(SCIENTIFIC_NAME))),
                  by=list(LATITUDE_rough, LONGITUDE_rough, LATITUDE_int)]
LAT_N<-species.richness[,.(N_Area=.N), by=list(LATITUDE_int)]
plot(LAT_N$LATITUDE_int, LAT_N$N_Area, type="l")
species_richness_lat_full$type="ALL Data"
species_richness_lat_realm<-all_with_v[REALM_LABEL==1, .(species.richness=length(unique(SCIENTIFIC_NAME))),
                                       by=list(LATITUDE_int)]
species_richness_lat_realm$type<-"Correct Realm only"


df<-rbindlist(list(species_richness_lat_realm, species_richness_lat_full))
df_with_N<-merge(df, LAT_N, by=c("LATITUDE_int"))
df_with_N$species.richness_by_area<-df_with_N$species.richness/df_with_N$N_Area
ggplot(df_with_N)+geom_line(aes(y=species.richness_by_area, x=LATITUDE_int, color=type))
