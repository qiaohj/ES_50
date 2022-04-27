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
  iucn<-st_read("../Shape/BOTW/BOTW.gdb")
}

mask<-raster("../Raster/mask_500km.tif")
x = read_stars("../Raster/mask_500km.tif")
grids<-st_as_sf(x[1], as_points = FALSE, merge = FALSE, crs=st_crs(mask))
st_crs(grids)<-st_crs(mask)
grids$index<-c(1:nrow(grids))

species_list<-list.files("../Objects/IUCN_Distributions/Birds_MOLL/RAW", pattern="\\.rda")
i<-262
ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}

surface_count<-0
for (i in c(1:length(species_list))){
  print(paste(i, length(species_list)))
  item<-species_list[i]
  
  if (file.exists(sprintf("../Objects/IUCN_Distributions/Birds_MOLL/RAW/%s", item))){
    next()
  }
  dis<-readRDS(sprintf("../Objects/IUCN_Distributions/Birds_ECK4/RAW/%s", item))
  
  if ((class(dis$Shape)[1]=="sfc_GEOMETRY")|(class(dis$Shape)[1]=="sfc_MULTISURFACE")){
    if (class(dis$Shape)[1]=="sfc_MULTISURFACE"){
      dis_new<-ensure_multipolygons(dis)
      st_crs(dis_new)<-st_crs(dis)
      dis<-dis_new
    }else{
      dis_new<-st_cast(st_sfc(dis$Shape), "MULTIPOLYGON")
      dis$Shape<-dis_new
    }
  }
  dis<-st_transform(dis, crs=st_crs(mask))
  saveRDS(dis, sprintf("../Objects/IUCN_Distributions/Birds_MOLL/RAW/%s", item))
}
species_list<-list.files("../Objects/IUCN_Distributions/Birds_MOLL/RAW", pattern="\\.rda")

i=1637
for (i in c(1:length(species_list))){
  print(paste(i, length(species_list)))
  item<-species_list[i]
  f<-sprintf("../Objects/IUCN_Distributions/Birds_MOLL/Fixed/%s", item)
  if (file.exists(f)){
    next()
  }
  dis<-readRDS(sprintf("../Objects/IUCN_Distributions/Birds_MOLL/RAW/%s", item))
  if (nrow(dis)==0){
    next()
  }
  is_valid<-st_is_valid(dis)
  for (j in c(1:length(is_valid))){
    if (is_valid[j]==F){
      print("fixing polygons")
      dis<-st_make_valid(dis)
      break()
    }
  }
  saveRDS(dis, f)
}
species_list<-list.files("../Objects/IUCN_Distributions/Birds_MOLL/RAW", pattern="\\.rda")
overlap_list<-list()
i=8
for (i in c(length(species_list):1)){
  print(paste(i, length(species_list)))
  item<-species_list[i]
  if (!file.exists(sprintf("../Objects/IUCN_Distributions/Birds_MOLL/Fixed/%s", item))){
    next()
  }
  dis<-readRDS(sprintf("../Objects/IUCN_Distributions/Birds_MOLL/Fixed/%s", item))
  f<-sprintf("../Objects/IUCN_Distributions/Birds_MOLL/500km_Overlap/%s", item)
  if (file.exists(f)){
    next()
  }
  if (F){
    plot(grids$geometry, col=grids$color)
    grids$color<-"blue"
    for (i in c(1:nrow(grids))){
      if (length(overlap[[i]])!=0){
        grids[i,]$color<-"red"
      }
    }
    plot(dis$Shape, col="red", add=T)
  }
  overlap<-st_overlaps(grids, dis)
  saveRDS(overlap, f)
}


species_list<-readRDS("../Objects/IUCN_Distributions/areas.rda")
area_threshold<-1e6
i=1
grids$species.richness<-0
grids_full<-grids
grids_subset<-grids
grids_species<-list()
for (i in c(1:nrow(species_list))){
  print(paste(i, nrow(species_list)))
  item<-species_list[i]
  f<-sprintf("../Objects/IUCN_Distributions/Birds_MOLL/500km_Overlap/%s.rda", item$sp)
  if (file.exists(f)){
    overlap<-readRDS(f)
    for (i in c(1:nrow(grids))){
      if (length(overlap[[i]])!=0){
        grids_full[i,]$species.richness<-grids_full[i,]$species.richness+1
        grids_species_item<-item
        grids_species_item$grid_index<-i
        grids_species[[length(grids_species)+1]]<-grids_species_item
        if (item$area<=area_threshold){
          grids_subset[i,]$species.richness<-grids_subset[i,]$species.richness+1
        }
      }
    }
    
  }

}
saveRDS(grids_full, "../Objects/IUCN_Distributions/iucn_richness_full.rda")
saveRDS(grids_subset, "../Objects/IUCN_Distributions/iucn_richness_1km.rda")

grids_subset<-readRDS("../Objects/IUCN_Distributions/iucn_richness_1km.rda")
grids_full<-readRDS("../Objects/IUCN_Distributions/iucn_richness_full.rda")

source("colors.r")
library(ggplot2)
ggplot(grids_full)+geom_sf(aes(fill=species.richness))+
  scale_fill_gradient(low=colors_blue[4], high=colors_red[7])

ggplot(grids_subset)+geom_sf(aes(fill=species.richness))+
  scale_fill_gradient(low=colors_blue[4], high=colors_red[7])

quantile(grids_subset$species.richness)

grids_subset_2<-grids_subset[which((grids_subset$index>=200)&(grids_subset$species.richness>=12)),]

ggplot(grids_subset_2)+geom_sf(aes(fill=species.richness))+
  scale_fill_gradient(low=colors_blue[4], high=colors_red[7])
quantile(grids_subset_2[(grids_subset_2$index>=200),]$species.richness, seq(0, 1, by=0.1))

grids_top1<-data.table(grids_subset_2)
grids_top1$geometry<-NULL

n<-c(12, 24, 36, 50, 64, 78, 96, 140, 198, 713)

grids_top1<-grids_top1[species.richness %in% n]
d <- data.table(grids_top1, key="species.richness")
unique(grids_subset_2$species.richness)
unique(grids_top1$species.richness)
d<-d[, head(.SD, 1), by=species.richness]
index<-d$index
index[index==550]<-512
index[index==563]<-551
index[index==569]<-565
index[index==417]<-434
index[index==418]<-399

picked_grids<-grids_subset[which((grids_subset$index>=200)&(grids_subset$index %in% index)),]
picked_grids$label<-letters[1:10]
picked_grids[which(picked_grids$label=="i"), "index"]
saveRDS(picked_grids, "../Objects/virtual_lands/picked_grids.rda")

picked_grids<-readRDS("../Objects/virtual_lands/picked_grids.rda")
source("colors.r")
p<-ggplot(grids_subset)+geom_sf(aes(fill=species.richness), color=NA)+
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


