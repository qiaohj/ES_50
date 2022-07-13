library(sf)
library(data.table)
library(raster)
library(taxize)
library(units)
sf_use_s2(FALSE)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
if (F){
  ecoregion<-sf::st_read(dsn = "../Shape/Ecoregions2017", 
                         layer = "Ecoregions2017")
  realm<-unique(ecoregion$REALM)[4]
  for (realm in unique(ecoregion$REALM)){
    print(realm)
    item<-ecoregion[which(ecoregion$REALM==realm),]
    
    item_union<-st_union(item)
    item_fixed<-st_buffer(item_union, dist=0.5)
    #item_union_tran<-st_transform(item_union, crs=3488)
    #item_fixed_tran<-st_buffer(item_union_tran, dist=50000)
    #item_fixed<-st_transform(item_fixed_tran, crs=st_crs(item_union))
    if (F){
      plot(item_union, border="red")
      plot(item_fixed, border="black", add=T)
      
      df <- data.frame(place = "London", 
                       lat = c(51.5074,0), lon = c(0.1278,0),
                       population = 8500000) # just to add some value that is plotable
      projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      df <- st_as_sf(x = df,                         
                     coords = c("lon", "lat"),
                     crs = projcrs)
      item_dist<-st_distance(item_union, df, which="Great Circle")
      plot(item$geometry)
      plot(df$geometry, add=T, col="red")
    }
    
    saveRDS(item_union, sprintf("../Shape/REALM_Ecoregions2017/%s.rda", gsub("/", "_", realm)))
    saveRDS(item_fixed, sprintf("../Shape/REALM_Ecoregions2017/%s_0.5d_buffer.rda", gsub("/", "_", realm)))
    
  }
}
realms<-list.files("../Shape/REALM_Ecoregions2017", pattern="_0.5d_buffer\\.rda")
realms<-gsub("_0.5d_buffer\\.rda", "", realms)
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
#plot(full_realm, border=NA)
i=100
mask_1km<-raster("../Raster/mask_1km.tif")
species_realm<-readRDS("../Tables/species_realm.rda")
full_species_realm<-readRDS("../Tables/species_realm_full_origins.rda")
syns<-data.table(read.csv("../Tables/missing_names.csv", stringsAsFactors = F))
if (F){
  ebirdchecklist<-readRDS("../Tables/eBird_Checklist/ebird_realm_checklist.rda")
  ebird_sp<-unique(ebirdchecklist$SCIENTIFIC_NAME)
  birdlife_sp<-unique(species_realm$scientific_name)
  ebird_sp[!(ebird_sp %in% birdlife_sp)]
}

i=2
syns<-syns[ebird=="Accepted"]
syns<-syns[sample(nrow(syns), nrow(syns))]
syns[scientific_name=="Gymnasio nudipes",]
species_realm[scientific_name=="Gymnasio nudipes"]
#syns<-syns[scientific_name %in% species_realm[!is.na(fixed)]$scientific_name]
accepted_name<-"Gymnasio nudipes"
ebird_name<-accepted_name
for (i in c(1:nrow(syns))){
  item<-syns[i]
  print(paste(i, nrow(syns), item$scientific_name))
  
  ebird_name<-item$scientific_name
  if ((item$scientific_name!=item$accepted_name)&(item$accepted_name!="")){
    accepted_name<-item$accepted_name
  }else{
    accepted_name<-ebird_name
  }
  region_item<-species_realm[scientific_name==accepted_name]
  full_region_item<-full_species_realm[scientific_name==accepted_name]
  if (nrow(region_item[!(REALM %in% full_region_item$REALM)])==0&
      nrow(full_region_item[!(REALM %in% region_item$REALM)])==0){
    #next()
  }
  if ("birdlife" %in% region_item$source){
    #next()
  }
  
  target<-sprintf("../Tables/Speccies_Split_202112_with_realm_distance/%s.rda", accepted_name)
  if (file.exists(target)){
    
    
    if (file.info(target)$mtime>"2022-07-11 07:37:49 CST"){
      next()
    }
    next()
  }
  saveRDS(NULL, target)
  target_species_realm<-unique(species_realm[scientific_name==accepted_name]$REALM)
  
  
  
  if (length(target_species_realm)==0){
    print("no realms")
    next()
  }
  if (length(target_species_realm)>1){
    realms<-NULL
    for (r in target_species_realm){
      if (is.null(realms)){
        realms<-all_realm[[r]]
      }else{
        realms<-st_union(realms, all_realm[[r]])
      }
    }
  }else{
    realms<-all_realm[[target_species_realm]]
  }
  records<-list()
  year<-2015
  for (year in c(2015:2021)){
    tttt<-sprintf("../Tables/Speccies_Split_202112/%s/%d.rda", ebird_name, year)
    if (!file.exists(tttt)){
      print(sprintf("Not found: %s", tttt))
      next()
    }
    records[[length(records)+1]]<-readRDS(tttt)
  }
  if (length(records)==0){
    next()
  }
  records<-rbindlist(records)
  cols<-c("LOCALITY_ID", "LATITUDE", "LONGITUDE")
  unique_records<-data.frame(unique(records[, ..cols]))
  
  points<-st_as_sf(x = unique_records,                         
                 coords = c("LONGITUDE", "LATITUDE"),
                 crs = st_crs(realms))
  #remove all the points in the ocean
  in_p<-st_contains(full_realm, points)
  points<-points[unlist(in_p),]
  
  #find the points in the correct realms
  in_p<-st_contains(realms, points)
  points$dist<--1
  in_p_index<-unlist(in_p)
  if (length(in_p_index)==0){
    in_points<-points[in_p_index,]
    out_points<-points
  }else{
    points[in_p_index, "dist"]<-0
    in_points<-points[in_p_index,]
    out_points<-points[-in_p_index,]
  }
  if (nrow(out_points)>0){
    dists<-st_distance(realms, out_points, which="Great Circle")
    out_points$dist<-as.numeric(dists)
    if (nrow(in_points)>0){
      final_points<-rbindlist(list(in_points, out_points))
    }else{
      final_points<-out_points
    }
  }else{
    final_points<-in_points
  }
  final_points$geometry<-NULL
  final_records<-merge(records, final_points, by="LOCALITY_ID")
  if (F){
    plot(full_realm)
    plot(points$geometry, col="red", add=T)
    
    
    plot(points$geometry, col="black", pch=".")
    plot(realms, add=T, border="red")
    plot(in_points$geometry, add=T, col="red")
    plot(out_points$geometry, add=T, col="blue")
    
    plot(in_points$geometry, add=T, col="red")
    plot(out_points$geometry, add=T, col="blue")
  }
  saveRDS(final_records, target)
}

xx<-readRDS(target)
xx
