library(data.table)
library(raster)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
if (F){
  splist<-list.files("/media/huijieqiao/QNAS/ES50/IUCN_1km_Tables", pattern="\\.rda")
  sp<-splist[1]
  startstr<-expand.grid(a=LETTERS, b=letters)
  startstr<-sprintf("%s%s", startstr$a, startstr$b)
  for (started in startstr){
    target<-sprintf("../Objects/IUCN_Richness/A_to_Z/%s.rda", started)
    if (file.exists(target)){
      next()
    }
    saveRDS(NULL, target)
    richness_list<-list()
    for (i in c(1:length(splist))){
      sp<-splist[i]
      if (!startsWith(sp, started)){
        next()
      }
      print(paste(sp, i, "/", length(splist)))
      df<-readRDS(sprintf("/media/huijieqiao/QNAS/ES50/IUCN_1km_Tables/%s", sp))
      if (is.null(df)){
        next()
      }
      if (nrow(df)==0){
        next()
      }
      richness_list[[sp]]<-df
    }
    if (length(richness_list)==0){
      next()
    }
    print("binding list")
    richness_list<-rbindlist(richness_list)
    print("calculating richness")
    richness_se_1km<-richness_list[,.(species.richness=length(unique(sp))),
                               by=list(mask_1km)]
    richness_se_2km<-richness_list[,.(species.richness=length(unique(sp))),
                                   by=list(mask_2km)]
    richness_se_5km<-richness_list[,.(species.richness=length(unique(sp))),
                                   by=list(mask_5km)]
    richness_se_10km<-richness_list[,.(species.richness=length(unique(sp))),
                                   by=list(mask_10km)]
    richness_se_20km<-richness_list[,.(species.richness=length(unique(sp))),
                                   by=list(mask_20km)]
    richness_se_50km<-richness_list[,.(species.richness=length(unique(sp))),
                                   by=list(mask_50km)]
    richness_se_100km<-richness_list[,.(species.richness=length(unique(sp))),
                                   by=list(mask_100km)]
    richness_se<-list("1km"=richness_se_1km,
                      "2km"=richness_se_2km,
                      "5km"=richness_se_5km,
                      "10km"=richness_se_10km,
                      "20km"=richness_se_20km,
                      "50km"=richness_se_50km,
                      "100km"=richness_se_100km)
    print("saving results")
    saveRDS(richness_se, target)
    #saveRDS(richness_se, sprintf("../Objects/IUCN_Richness/A_to_Z/%s.rda", started))
    
  }
}

if (F){
  library(data.table)
  library(raster)
  library(ggplot2)
  setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
  splist<-list.files("/media/huijieqiao/QNAS/ES50/IUCN_1km_Tables", pattern="\\.rda")
  sp<-splist[1]
  startstr<-expand.grid(a=LETTERS, b=letters)
  startstr<-sprintf("%s%s", startstr$a, startstr$b)
  df_list_1km<-NULL
  df_list_2km<-NULL
  df_list_5km<-NULL
  df_list_10km<-NULL
  df_list_20km<-NULL
  df_list_50km<-NULL
  df_list_100km<-NULL
  started<-"Za"
  for (started in startstr){
    print(started)
    target<-sprintf("../Objects/IUCN_Richness/A_to_Z/%s.rda", started)
    df<-readRDS(target)
    if (is.null(df)){
      next()
    }
    if (is.null(df_list_100km)){
      df_list_2km<-df[["2km"]]
      df_list_5km<-df[["5km"]]
      df_list_10km<-df[["10km"]]
      df_list_20km<-df[["20km"]]
      df_list_50km<-df[["50km"]]
      df_list_100km<-df[["100km"]]
      
    }else{
      df_list_2km<-rbindlist(list(df_list_2km, df[["2km"]]))
      df_list_2km<-df_list_2km[, .(species.richness=sum(species.richness)),
                       by=list(mask_2km)]
      df_list_5km<-rbindlist(list(df_list_5km, df[["5km"]]))
      df_list_5km<-df_list_5km[, .(species.richness=sum(species.richness)),
                               by=list(mask_5km)]
      df_list_10km<-rbindlist(list(df_list_10km, df[["10km"]]))
      df_list_10km<-df_list_10km[, .(species.richness=sum(species.richness)),
                               by=list(mask_10km)]
      df_list_20km<-rbindlist(list(df_list_20km, df[["20km"]]))
      df_list_20km<-df_list_20km[, .(species.richness=sum(species.richness)),
                               by=list(mask_20km)]
      df_list_50km<-rbindlist(list(df_list_50km, df[["50km"]]))
      df_list_50km<-df_list_50km[, .(species.richness=sum(species.richness)),
                               by=list(mask_50km)]
      df_list_100km<-rbindlist(list(df_list_100km, df[["100km"]]))
      df_list_100km<-df_list_100km[, .(species.richness=sum(species.richness)),
                               by=list(mask_100km)]
    }
  }
  saveRDS(df_list_2km, "../Objects/IUCN_Richness/iucn_richness_2km.rda")
  saveRDS(df_list_5km, "../Objects/IUCN_Richness/iucn_richness_5km.rda")
  saveRDS(df_list_10km, "../Objects/IUCN_Richness/iucn_richness_10km.rda")
  saveRDS(df_list_20km, "../Objects/IUCN_Richness/iucn_richness_20km.rda")
  saveRDS(df_list_50km, "../Objects/IUCN_Richness/iucn_richness_50km.rda")
  saveRDS(df_list_100km, "../Objects/IUCN_Richness/iucn_richness_100km.rda")
  shape<-readRDS("../Objects/IUCN_Distributions/Birds_RAW/RAW/Oceanites oceanicus.rda")
  ggplot()+
    geom_sf(data=shape)+
    geom_tile(data=missing_sp, aes(x=x, y=y), fill="red")+
    theme_bw()
  df_list<-df_list_missing_sp
}


source("colors.r")
#df_list<-readRDS("../Objects/IUCN_Richness/iucn_richness_1km.rda")

res<-"1km"

for (res in c("100km", "50km", "20km", "10km", "5km", "2km", "1km")){
  print(res)
  colname<-sprintf("mask_%s", res)
  df_list<-readRDS(sprintf("../Objects/IUCN_Richness/iucn_richness_%s.rda", res))
  df_list$group<-df_list[, ..colname]
  df_list_km<-df_list[, .(species.richness=sum(species.richness, na.rm = T)), 
                       by=list(group)]
  mask_100km<-data.table(rasterToPoints(raster(sprintf("../Raster/%s.tif", colname))))
  colnames(mask_100km)[3]<-"group"
  df_list_km_with_xy<-merge(df_list_km, mask_100km, by="group", all=T)
  df_list_km_with_xy<-df_list_km_with_xy[!is.na(group)]
  mask<-raster(sprintf("../Raster/mask_%s.tif", res))
  v<-values(mask)
  no_na<-!is.na(v)
  v[no_na]<-df_list_km_with_xy$species.richness
  values(mask)<-v
  writeRaster(mask, sprintf("../Figures/IUCN_Based_VW/TIFF/IUCN_richness/IUCN_richness_%s.tif", res), overwrite=T)
  
  
  if (res %in% c("100km", "50km")){
    max_fix<-800
  }
  if (res %in% c("20km", "10km")){
    max_fix<-500
  }
  if (res %in% c("5km", "2km", "1km")){
    max_fix<-500
  }
  max_richness<-max(df_list_km_with_xy$species.richness, na.rm = T)
  #hist(df_list_km_with_xy$species.richness)
  breaks<-seq(0, max_fix, by=max_fix/5)
  labs<-format(breaks, scientific=F)
  labs[6]<-sprintf(">%s up to %s", labs[6], format(max_richness, scientific=F))
  df_list_km_with_xy$fixed_species.richness<-df_list_km_with_xy$species.richness
  df_list_km_with_xy[species.richness>max_fix]$fixed_species.richness<-max_fix
  p<-ggplot(df_list_km_with_xy)+geom_tile(aes(x=x, y=y, fill=fixed_species.richness), color=NA)+
    scale_fill_gradient(low=colors_blue[4], high=colors_red[7],
                        breaks=breaks, labels=labs)+
    coord_equal()+
    #scale_fill_gradientn(colors=c(colors_blue[4], colors_blue[5], colors_red[6]),
    #                  values=c(0, 100, 200))
    ggtitle(sprintf("Species richness of Birdlife in %s resolution", res))+
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
  ggsave(p, filename=sprintf("../Figures/IUCN_Based_VW/iucn_species.richness_%s.png", res), width=10, height=8)
}



