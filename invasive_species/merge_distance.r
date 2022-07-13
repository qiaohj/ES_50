library(sf)
library(data.table)
library(raster)
library(taxize)
library(units)
sf_use_s2(FALSE)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
syns<-data.table(read.csv("../Tables/missing_names.csv", stringsAsFactors = F))
syns<-syns[ebird=="Accepted"]
syns<-syns[sample(nrow(syns), nrow(syns))]
syns[scientific_name=="Acanthis cabaret",]
all_df<-list()
all_df_out<-list()
for (i in c(1:nrow(syns))){
  item<-syns[i]
  print(paste(i, nrow(syns), item$scientific_name))
  
  ebird_name<-item$scientific_name
  if ((item$scientific_name!=item$accepted_name)&(item$accepted_name!="")){
    accepted_name<-item$accepted_name
  }else{
    accepted_name<-ebird_name
  }
  target<-sprintf("../Tables/Speccies_Split_202112_with_realm_distance/%s.rda", accepted_name)
  if (!file.exists(target)){
    next()
  }
  df<-readRDS(target)
  if (is.null(df)){
    next()
  }
  df$IN_REALM<-ifelse(df$dist==0, T, F)
  df_out<-df[IN_REALM==F]
  if (nrow(df_out)>0){
    all_df_out[[accepted_name]]<-df_out
  }
  df_se<-df[, .(N=.N), by=list(SCIENTIFIC_NAME, YEAR, IN_REALM)]
  df_se$ACCEPTED_NAME<-accepted_name
  all_df[[accepted_name]]<-df_se
}

full_all_df<-rbindlist(all_df)
all_df_out_df<-rbindlist(all_df_out)

full_all_df_in<-full_all_df[IN_REALM==T]
full_all_df_out<-full_all_df[IN_REALM==F]
hist(full_all_df_in$N)
full_all_df_merge<-merge(full_all_df_in, full_all_df_out, by=c("SCIENTIFIC_NAME", "YEAR", "ACCEPTED_NAME"), all=T)
full_all_df_merge$IN_REALM.x<-NULL
full_all_df_merge$IN_REALM.y<-NULL
colnames(full_all_df_merge)[c(4,5)]<-c("N_CORRECT_REALM", "N_WRONG_REALM")

full_all_df_merge[is.na(N_CORRECT_REALM)]$N_CORRECT_REALM<-0
full_all_df_merge[is.na(N_WRONG_REALM)]$N_WRONG_REALM<-0
saveRDS(full_all_df_merge, "../Tables/realm_stat.rda")
write.csv(full_all_df_merge, "../Tables/realm_stat.csv", row.names = F)
saveRDS(all_df_out_df, "../Tables/realm_wrong.rda")
write.csv(all_df_out_df, "../Tables/realm_wrong.csv", row.names = F)
df_out_yearly<-all_df_out_df[, .(N=.N), by=list(SCIENTIFIC_NAME, COUNTRY, STATE, COUNTY, COUNTY_CODE,LOCALITY_ID,
                                                LATITUDE, LONGITUDE,
                                                YEAR, IN_REALM)]
saveRDS(df_out_yearly, "../Tables/realm_wrong_yearly.rda")
write.csv(df_out_yearly, "../Tables/realm_wrong_yearly.csv", row.names = F)

df_out_yearly<-readRDS("../Tables/realm_wrong_yearly.rda")
df_xxx<-df_out_yearly[, .(N=.N), by=list(SCIENTIFIC_NAME, IN_REALM)]
df_xxx[N==max(df_xxx$N)]
full_all_df_merge[SCIENTIFIC_NAME=="Acridotheres cristatellus"]


if (F){
  sp_realm_2[scientific_name=="Sturnus vulgaris"]
  all_realm<-list()
  full_realm<-NULL
  #names(realm_color)<-realms
  
  for (r in realms){
    print(sprintf("Reading %s", r))
    r_shp<-readRDS(sprintf("../Shape/REALM_Ecoregions2017/%s_0.5d_buffer.rda", r))
    #plot(r_shp, border=realm_color[r], main=r)
    all_realm[[gsub("_", "/", r)]]<-r_shp
    if (is.null(full_realm)){
      full_realm<-r_shp
    }else{
      full_realm<-st_union(full_realm, r_shp)
    }
  }
  realms<-unique(sp_realm_2[scientific_name=="Sturnus vulgaris"]$REALM)
  plot(full_realm)
  for (rrr in realms){
    plot(all_realm[[rrr]], border="red", add=T)
  }
  occs<-df_out_yearly[SCIENTIFIC_NAME=="Sturnus vulgaris"]
  occs_sample<-occs[sample(nrow(occs), 10000)]
  points(occs_sample$LONGITUDE, occs_sample$LATITUDE, pch=".", col="blue")
  vessel_fixed<-readRDS("../Shape/Birdlife_202101_fixed.rda")
  items<-vessel_fixed[which(vessel_fixed$binomial=="Acridotheres cristatellus"),]
  library(ggplot2)
  ggplot(items)+geom_sf(aes(color=factor(origin)))
  plot(items$geom, col=items$origin, border=NA)
}