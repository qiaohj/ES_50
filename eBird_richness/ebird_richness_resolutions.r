
library(sf)
library(data.table)
library(gdalUtilities)
library(raster)
library(ggplot2)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
if (F){
  cols<-c("x", "y")
  r<-data.table(rasterToPoints(raster("../Raster/mask_1km.tif")))
  r$mask_2km<-raster::extract(raster("../Raster/mask_2km.tif"), r[, ..cols])
  r$mask_5km<-raster::extract(raster("../Raster/mask_5km.tif"), r[, ..cols])
  r$mask_10km<-raster::extract(raster("../Raster/mask_10km.tif"), r[, ..cols])
  r$mask_20km<-raster::extract(raster("../Raster/mask_20km.tif"), r[, ..cols])
  r$mask_50km<-raster::extract(raster("../Raster/mask_50km.tif"), r[, ..cols])
  r$mask_100km<-raster::extract(raster("../Raster/mask_100km.tif"), r[, ..cols])
  saveRDS(r, "../Objects/mask_points_full_resolutions.rda")
}

if (F){
  r<-readRDS("../Objects/mask_points_full_resolutions.rda")
  setkeyv(r, "mask_1km")
  sf_use_s2(FALSE)
  
  states<-sf::st_read(dsn = "../Shape/gadm36_levels_shp", 
                      layer = "gadm36_1")
  state_list<-data.table(GID_0=states$GID_0, GID_1=states$GID_1, 
                         NAME_0=states$NAME_0, NAME_1=states$NAME_1,
                         HASC_1=states$HASC_1)
  i=100
  mask_1km<-raster("../Raster/mask_1km.tif")
  all<-list()
  ll_cols<-c("LONGITUDE", "LATITUDE")
  for (i in c(1:nrow(state_list))){
    print(paste(i, nrow(state_list)))
    state_item<-state_list[i]
    f_raw<-sprintf("../Tables/eBird_mapbased_checklist_with_realm/%s/ebird_raw_%s.rda",
                   state_item$GID_0, gsub("/", ".", state_item$GID_1))
    if (!file.exists(f_raw)){
      next()
    }
    df<-readRDS(f_raw)
    locals<-unique(data.table(LONGITUDE=df$LONGITUDE, LATITUDE=df$LATITUDE, LOCALITY_ID=df$LOCALITY_ID))
    ll_sf<-st_as_sf(locals, coords=ll_cols, crs=st_crs(4326))
    ll_sf_moll<-st_transform(ll_sf, crs=st_crs(mask_1km))
    ll_sf_moll$mask_1km<-raster::extract(mask_1km, st_coordinates(ll_sf_moll))
    ll_sf_moll$geometry<-NULL
    ll_sf_moll<-merge(ll_sf_moll, r, by="mask_1km")
    #ll_sf_moll<-unique(ll_sf_moll)
    df_with_mask<-merge(df, ll_sf_moll, by="LOCALITY_ID")
    df_se<-df_with_mask[, .(N=.N), by=list(SCIENTIFIC_NAME, REALM_LABEL, x, y, 
                                           mask_1km, mask_2km, mask_5km, mask_10km,
                                           mask_20km, mask_50km, mask_100km)]
    #v<-extract(clim, df_se[, ..cols])
    #df_se<-cbind(df_se, v)
    all[[length(all)+1]]<-df_se
  }
  all_df<-rbindlist(all)
  saveRDS(all_df, "../Objects/ebird_richness_resolutions.rda")
}
source("colors.r")
all_df<-readRDS("../Objects/ebird_richness_resolutions.rda")
#100km

res<-"10km"

for (res in c("100km", "50km", "20km", "10km", "5km", "2km", "1km")){
  colname<-sprintf("mask_%s", res)
  all_df$group<-all_df[, ..colname]
  all_df_100km<-all_df[, .(N=sum(N, na.rm = T), 
                           species.richness=length(unique(SCIENTIFIC_NAME))), 
                       by=list(group)]
  mask_100km<-data.table(rasterToPoints(raster(sprintf("../Raster/%s.tif", colname))))
  colnames(mask_100km)[3]<-"group"
  all_df_100km_with_xy<-merge(all_df_100km, mask_100km, by="group", all=T)
  all_df_100km_with_xy<-all_df_100km_with_xy[!is.na(group)]
  hist(all_df_100km_with_xy$N)
  if (res %in% c("100km", "50km")){
    max_fix<-1e4
    if_scientific<-T
  }
  if (res %in% c("20km", "10km")){
    max_fix<-1e3
    if_scientific<-F
  }
  if (res %in% c("20km", "10km")){
    max_fix<-1e3
    if_scientific<-F
  }
  max_richness<-max(all_df_100km_with_xy$N, na.rm = T)
  #hist(grids_full$species.richness)
  breaks<-seq(0, max_fix, by=max_fix/5)
  labs<-format(breaks, scientific=if_scientific)
  labs[6]<-sprintf(">%s up to %s", labs[6], format(max_richness, scientific=if_scientific))
  all_df_100km_with_xy$fixed_N<-all_df_100km_with_xy$N
  all_df_100km_with_xy[N>max_fix]$fixed_N<-max_fix
  p<-ggplot(all_df_100km_with_xy)+geom_tile(aes(x=x, y=y, fill=fixed_N), color=NA)+
    scale_fill_gradient(low=colors_blue[4], high=colors_red[7],
                        breaks=breaks, labels=labs)+
    coord_equal()+
    #scale_fill_gradientn(colors=c(colors_blue[4], colors_blue[5], colors_red[6]),
    #                  values=c(0, 100, 200))
    ggtitle(sprintf("Number of records in eBird (2010-2019) in %s resolution", res))+
    theme_bw()+labs(fill="Number of records")+
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.background = element_rect(fill = "white", color = NA), 
          panel.background = element_blank(), 
          legend.background = element_rect(fill = "white", color = NA))
  p
  ggsave(p, filename=sprintf("../Figures/IUCN_Based_VW/ebird_N_records_%s.png", res), width=10, height=8)
  
  
  if (res %in% c("100km", "50km")){
    max_fix<-1e3
  }
  if (res %in% c("20km", "10km")){
    max_fix<-500
  }
  if (res %in% c("5km", "2km", "1km")){
    max_fix<-500
  }
  max_richness<-max(all_df_100km_with_xy$species.richness, na.rm = T)
  #hist(all_df_100km_with_xy$species.richness)
  breaks<-seq(0, max_fix, by=max_fix/5)
  labs<-format(breaks, scientific=F)
  labs[6]<-sprintf(">%s up to %s", labs[6], format(max_richness, scientific=F))
  all_df_100km_with_xy$fixed_species.richness<-all_df_100km_with_xy$species.richness
  all_df_100km_with_xy[species.richness>max_fix]$fixed_species.richness<-max_fix
  p<-ggplot(all_df_100km_with_xy)+geom_tile(aes(x=x, y=y, fill=fixed_species.richness), color=NA)+
    scale_fill_gradient(low=colors_blue[4], high=colors_red[7],
                        breaks=breaks, labels=labs)+
    coord_equal()+
    #scale_fill_gradientn(colors=c(colors_blue[4], colors_blue[5], colors_red[6]),
    #                  values=c(0, 100, 200))
    ggtitle(sprintf("Species richness in eBird (2010-2019) in %s resolution", res))+
    theme_bw()+labs(fill="Species richness")+
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.background = element_rect(fill = "white", color = NA), 
          panel.background = element_blank(), 
          legend.background = element_rect(fill = "white", color = NA))
  p
  ggsave(p, filename=sprintf("../Figures/IUCN_Based_VW/ebird_species.richness_%s.png", res), width=10, height=8)
}

