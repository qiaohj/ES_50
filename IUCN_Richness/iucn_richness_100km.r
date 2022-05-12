library(raster)
library(rgdal)
library(rgeos)
library(MASS)
library(cluster)
library(data.table)
library(sf)
library(fasterize)
library(rmapshaper)
library(stars)
library(gdalUtilities)
library(ggplot2)

setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")

if (F){
  mask<-raster("../Raster/ndvi/mean_ndvi_moll_10km.tif")
  bio<-raster("../Raster/bioclim/wc2.1_10m_bio_1.tif")
  mask_100km<-projectRaster(bio, res=c(1e5, 1e5), crs=crs(mask), method = "ngb")
  v<-values(mask_100km)
  no<-!is.na(v)
  v[no]<-c(1:length(no[no==T]))
  values(mask_100km)<-v
  plot(mask_100km)
  writeRaster(mask_100km, "../Raster/mask_100km.tif")
  
  mask_100km<-projectRaster(bio, res=c(5e4, 5e4), crs=crs(mask), method = "ngb")
  v<-values(mask_100km)
  no<-!is.na(v)
  v[no]<-c(1:length(no[no==T]))
  values(mask_100km)<-v
  plot(mask_100km)
  writeRaster(mask_100km, "../Raster/mask_50km.tif")
  
  mask_100km<-projectRaster(bio, res=c(2e4, 2e4), crs=crs(mask), method = "ngb")
  v<-values(mask_100km)
  no<-!is.na(v)
  v[no]<-c(1:length(no[no==T]))
  values(mask_100km)<-v
  plot(mask_100km)
  writeRaster(mask_100km, "../Raster/mask_20km.tif")
  
  mask_100km<-projectRaster(bio, res=c(1e4, 1e4), crs=crs(mask), method = "ngb")
  v<-values(mask_100km)
  no<-!is.na(v)
  v[no]<-c(1:length(no[no==T]))
  values(mask_100km)<-v
  plot(mask_100km)
  writeRaster(mask_100km, "../Raster/mask_10km.tif")
  
  mask_100km<-projectRaster(bio, res=c(5e3, 5e3), crs=crs(mask), method = "ngb")
  v<-values(mask_100km)
  no<-!is.na(v)
  v[no]<-c(1:length(no[no==T]))
  values(mask_100km)<-v
  plot(mask_100km)
  writeRaster(mask_100km, "../Raster/mask_5km.tif")
  
  mask_100km<-projectRaster(bio, res=c(2e3, 2e3), crs=crs(mask), method = "ngb")
  v<-values(mask_100km)
  no<-!is.na(v)
  v[no]<-c(1:length(no[no==T]))
  values(mask_100km)<-v
  plot(mask_100km)
  writeRaster(mask_100km, "../Raster/mask_2km.tif", datatype="INT4S", overwrite=T)
  
  mask_100km<-projectRaster(bio, res=c(1e3, 1e3), crs=crs(mask), method = "ngb")
  v<-values(mask_100km)
  no<-!is.na(v)
  v[no]<-c(1:length(no[no==T]))
  values(mask_100km)<-v
  plot(mask_100km)
  writeRaster(mask_100km, "../Raster/mask_1km.tif", datatype="INT4S", overwrite=T)
}

mask<-raster("../Raster/mask_100km.tif")
x = read_stars("../Raster/mask_100km.tif")
grids<-st_as_sf(x[1], as_points = FALSE, merge = FALSE, crs=st_crs(mask))
st_crs(grids)<-st_crs(mask)
grids$index<-c(1:nrow(grids))

species_list<-list.files("../Objects/IUCN_Distributions/Birds_MOLL/RAW", pattern="\\.rda")
overlap_list<-list()
i=8
species_list<-species_list[sample(length(species_list), length(species_list))]
for (i in c(1:length(species_list))){
  print(paste(i, length(species_list)))
  item<-species_list[i]
  if (!file.exists(sprintf("../Objects/IUCN_Distributions/Birds_MOLL/Fixed/%s", item))){
    next()
  }
  
  f<-sprintf("../Objects/IUCN_Distributions/Birds_MOLL/100km_Overlap/%s", item)
  if (file.exists(f)){
    print("skip")
    next()
  }
  saveRDS(NULL, f)
  dis<-readRDS(sprintf("../Objects/IUCN_Distributions/Birds_MOLL/Fixed/%s", item))
  
  overlap<-st_overlaps(grids, dis)
  saveRDS(overlap, f)
}


species_list<-readRDS("../Objects/IUCN_Distributions/areas.rda")
#area_threshold<-1e6
i=1
grids$species.richness<-0
grids_full<-grids
done_sp_list<-c()
for (i in c(1:nrow(species_list))){
  print(paste(i, nrow(species_list)))
  item<-species_list[i]
  if (item$sp %in% done_sp_list){
    print("skip")
    next()
  }
  f<-sprintf("../Objects/IUCN_Distributions/Birds_MOLL/100km_Overlap/%s.rda", item$sp)
  if (file.exists(f)){
    overlap<-readRDS(f)
    if (is.null(overlap)){
      print("skip null")
      next()
    }
    names(overlap)<-paste(c(1:nrow(grids)), "_", sep="")
    x<-unlist(overlap, use.names = T)
    
    if (length(x)>0){
      grid_index<-names(x)
      grid_index<-sapply(strsplit(grid_index,"_"), `[`, 1)
      grid_index<-unique(as.numeric(grid_index))
      grids_full[grid_index,]$species.richness<-grids_full[grid_index,]$species.richness+1  
    }
    
    done_sp_list<-c(done_sp_list, item$sp)
  }
  
}
saveRDS(grids_full, "../Objects/IUCN_Distributions/iucn_richness_full_100km.rda")

picked_grids<-readRDS("../Objects/virtual_lands/picked_grids.rda")
source("colors.r")
max_fix<-500
st_write(grids_full, "../Shape/IUCN_Richness/IUCN_Richness.shp")
grids_full$fixed_species.richness<-grids_full$species.richness
grids_full[grids_full$fixed_species.richness>max_fix,]$fixed_species.richness<-max_fix
max_richness<-max(grids_full$species.richness)
#hist(grids_full$species.richness)
labs<-as.character(seq(100, max_fix, by=100))
labs[5]<-sprintf(">500 up to %d", max_richness)
p<-ggplot(grids_full)+geom_sf(aes(fill=fixed_species.richness), color=NA)+
  scale_fill_gradient(low=colors_blue[4], high=colors_red[7],
                       breaks=seq(100, max_fix, by=100), labels=labs)+
  #scale_fill_gradientn(colors=c(colors_blue[4], colors_blue[5], colors_red[6]),
  #                  values=c(0, 100, 200))
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
ggsave(p, filename="../Figures/IUCN_Based_VW/species.richness_100km.png", width=10, height=8)



p<-ggplot(grids_full)+geom_sf(aes(fill=species.richness), color=NA)+
  geom_sf(data=picked_grids, fill=NA, color="black")+
  geom_sf_text(data=picked_grids, aes(label = label), colour = "black", size=3)+
  scale_fill_gradient(low=colors_blue[4], high=colors_red[7])+
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
ggsave(p, filename="../Figures/IUCN_Based_VW/picked_grids.png", width=10, height=8)


