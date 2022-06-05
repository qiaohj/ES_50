library(data.table)
library(ggplot2)
library(raster)
library(sf)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")

source("colors.r")
#df_list<-readRDS("../Objects/IUCN_Richness/iucn_richness_1km.rda")
if (F){
  library(data.table)
  library(raster)
  library(ggplot2)
  setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
  splist<-list.files("/media/huijieqiao/QNAS/ES50/IUCN_1km_Tables", pattern="\\.rda")
  iucn_status<-data.table(read.csv("../Tables/redlist_species_data_3752fd69-a2ab-4550-9648-6dd9736eaa78/simple_summary.csv", stringsAsFactors = F))
  cols<-c("scientificName", "orderName", "familyName", "redlistCategory")
  iucn_status<-iucn_status[, ..cols]
  order<-toupper("PASSERIFORMES")
  order_list<-iucn_status[orderName==order]
  target<-sprintf("../Objects/IUCN_Richness/ORDER/%s.rda", order)
  #endanger
  #order_list<-iucn_status[iucn_status$redlistCategory %in% c("Endangered", "Critically Endangered")]
  #target<-sprintf("../Objects/IUCN_Richness/ORDER/%s.rda", "Endangered")
  
  i<-1
  richness_list<-list()
  for (i in c(1:nrow(order_list))){
    item<-order_list[i]
    print(paste(i, nrow(order_list), item$scientificName))
    ff<-sprintf("/media/huijieqiao/QNAS/ES50/IUCN_1km_Tables/%s.rda", gsub(" ", "_", item$scientificName))
    if (!file.exists(ff)){
      next()
    }
    df<-readRDS(ff)
    if (is.null(df)){
      next()
    }
    if (nrow(df)==0){
      next()
    }
    richness_list[[item$scientificName]]<-df
  }
  
  if (order=="PASSERIFORMES"){
    df_list_1km<-NULL
    df_list_2km<-NULL
    df_list_5km<-NULL
    df_list_10km<-NULL
    df_list_20km<-NULL
    df_list_50km<-NULL
    df_list_100km<-NULL
    for (j in seq(5501, length(richness_list), by=300)){
      start_index<-j
      end_index<-j+300-1
      
      if (end_index>length(richness_list)){
        end_index<-length(richness_list)
      }
      print(paste(start_index, end_index))
      item_df<-rbindlist(richness_list[start_index:end_index])
      richness_se_1km<-item_df[,.(species.richness=length(unique(sp))),
                                     by=list(mask_1km)]
      richness_se_2km<-item_df[,.(species.richness=length(unique(sp))),
                                     by=list(mask_2km)]
      richness_se_5km<-item_df[,.(species.richness=length(unique(sp))),
                                     by=list(mask_5km)]
      richness_se_10km<-item_df[,.(species.richness=length(unique(sp))),
                                      by=list(mask_10km)]
      richness_se_20km<-item_df[,.(species.richness=length(unique(sp))),
                                      by=list(mask_20km)]
      richness_se_50km<-item_df[,.(species.richness=length(unique(sp))),
                                      by=list(mask_50km)]
      richness_se_100km<-item_df[,.(species.richness=length(unique(sp))),
                                       by=list(mask_100km)]
      
      if (is.null(df_list_100km)){
        df_list_1km<-richness_se_1km
        df_list_2km<-richness_se_2km
        df_list_5km<-richness_se_5km
        df_list_10km<-richness_se_10km
        df_list_20km<-richness_se_20km
        df_list_50km<-richness_se_50km
        df_list_100km<-richness_se_100km
        
      }else{
        df_list_1km<-rbindlist(list(df_list_1km, richness_se_1km))
        df_list_1km<-df_list_1km[, .(species.richness=sum(species.richness)),
                                 by=list(mask_1km)]
        df_list_2km<-rbindlist(list(df_list_2km, richness_se_2km))
        df_list_2km<-df_list_2km[, .(species.richness=sum(species.richness)),
                                 by=list(mask_2km)]
        df_list_5km<-rbindlist(list(df_list_5km, richness_se_5km))
        df_list_5km<-df_list_5km[, .(species.richness=sum(species.richness)),
                                 by=list(mask_5km)]
        df_list_10km<-rbindlist(list(df_list_10km, richness_se_10km))
        df_list_10km<-df_list_10km[, .(species.richness=sum(species.richness)),
                                   by=list(mask_10km)]
        df_list_20km<-rbindlist(list(df_list_20km, richness_se_20km))
        df_list_20km<-df_list_20km[, .(species.richness=sum(species.richness)),
                                   by=list(mask_20km)]
        df_list_50km<-rbindlist(list(df_list_50km, richness_se_50km))
        df_list_50km<-df_list_50km[, .(species.richness=sum(species.richness)),
                                   by=list(mask_50km)]
        df_list_100km<-rbindlist(list(df_list_100km, richness_se_100km))
        df_list_100km<-df_list_100km[, .(species.richness=sum(species.richness)),
                                     by=list(mask_100km)]
      }
    }
    richness_se_1km<-df_list_1km
    richness_se_2km<-df_list_2km
    richness_se_5km<-df_list_5km
    richness_se_10km<-df_list_10km
    richness_se_20km<-df_list_20km
    richness_se_50km<-df_list_50km
    richness_se_100km<-df_list_100km
    
  }else{
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
  }
  richness_se<-list("1km"=richness_se_1km,
                    "2km"=richness_se_2km,
                    "5km"=richness_se_5km,
                    "10km"=richness_se_10km,
                    "20km"=richness_se_20km,
                    "50km"=richness_se_50km,
                    "100km"=richness_se_100km)
  print("saving results")
  saveRDS(richness_se, target)
  #order<-"GALLIFORMES"
  #target<-sprintf("../Objects/IUCN_Richness/ORDER/%s.rda", order)
  #richness_se<-readRDS(target)
  df_list_1km<-richness_se[["1km"]]
  mask_100km<-data.table(rasterToPoints(raster(sprintf("../Raster/%s.tif", "mask_1km"))))
  df_list_km_with_xy<-merge(df_list_1km, mask_100km, by="mask_1km", all=T)
  df_list_km_with_xy<-df_list_km_with_xy[!is.na(mask_1km)]
  mask<-raster(sprintf("../Raster/mask_%s.tif", "1km"))
  v<-values(mask)
  no_na<-!is.na(v)
  v[no_na]<-df_list_km_with_xy$species.richness
  values(mask)<-v
  writeRaster(mask, sprintf("../Figures/IUCN_Based_VW/TIFF/IUCN_richness/IUCN_richness_%s_%s.tif", "1km", order),
              overwrite=T)
  
}

#order<-"PASSERIFORMES"
library(sf)
source("colors.r")
map<-raster(sprintf("../Figures/IUCN_Based_VW/TIFF/IUCN_richness/IUCN_richness_%s_%s.tif", "1km", order))
china<-st_read(dsn = "../Shape/china_province", 
                           layer = "bou2_4p")
china_eck4<-st_transform(china, crs=st_crs(map))
Tibet<-china_eck4[which(china_eck4$ADCODE93=="540000"),]
richness<-crop(map, st_bbox(Tibet))
richness<-mask(richness, Tibet)
writeRaster(richness, sprintf("../Figures/IUCN_Based_VW/Order/iucn_species.richness_%s_%s.tif", "1km", order), overwrite=T)
res<-"1km"
df_list_km_with_xy<-data.table(rasterToPoints(richness))
colnames(df_list_km_with_xy)[3]<-"richness"
p<-ggplot(df_list_km_with_xy)+geom_tile(aes(x=x, y=y, fill=richness), color=NA)+
  scale_fill_gradient(low=colors_blue[4], high=colors_red[7])+
  coord_equal()+
  #scale_fill_gradientn(colors=c(colors_blue[4], colors_blue[5], colors_red[6]),
  #                  values=c(0, 100, 200))
  ggtitle(sprintf("Species richness of %s in %s", order, "Tibet"))+
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
#p
ggsave(p, filename=sprintf("../Figures/IUCN_Based_VW/Order/iucn_species.richness_%s_%s.pdf", "1km", order), 
       width=10, height=8)

