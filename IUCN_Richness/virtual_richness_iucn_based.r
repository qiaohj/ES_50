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
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")

if (F){
  
  mask<-raster("../Raster/mask_500km.tif")
  x = read_stars("../Raster/mask_500km.tif")
  grids<-st_as_sf(x[1], as_points = FALSE, merge = FALSE, crs=st_crs(mask))
  st_crs(grids)<-st_crs(mask)
  grids$index<-c(1:nrow(grids))
  i=1
  for (i in 1:nrow(grids)){
    item<-grids[i,]
    folder<-sprintf("../Objects/GRIDS/%d", item$index)
    dir.create(folder, showWarnings = F)
    st_write(item, sprintf("%s/grid.shp", folder), append=F)
  }
}
picked_grids<-readRDS("../Objects/virtual_lands/picked_grids.rda")

species_list<-readRDS("../Objects/IUCN_Distributions/areas.rda")
area_threshold<-2e6
species_list<-species_list[area<=area_threshold]
#6530
for (i in c(6531:nrow(species_list))){
  print(paste(i, nrow(species_list)))
  item<-species_list[i]
  f<-sprintf("../Objects/IUCN_Distributions/Birds_MOLL/500km_Overlap/%s.rda", item$sp)
  if (file.exists(f)){
    overlap<-readRDS(f)
    j=1
    for (j in c(1:nrow(picked_grids))){
      if (length(overlap[[picked_grids[j,]$index]])!=0){
        target<-sprintf("../Objects/GRIDS/%d/%s.tif", picked_grids[j,]$index,  item$sp)
        if (file.exists(target)){
          next()
        }
        shp<-sprintf("../Objects/GRIDS/%d/grid.shp", picked_grids[j,]$index)
        tif<-sprintf("/media/huijieqiao/QNAS/ES50/GEOTIFF/%s.tif", item$sp)
        if (!file.exists(tif)){
          tif<-sprintf("/media/huijieqiao/QNAS/ES50/GEOTIFF_1e6_2e6/%s.tif", item$sp)
        }
        if (!file.exists(tif)){
          next()
        }
        
        gdalwarp(crop_to_cutline=T, dstnodata=-9999, cutline=shp, overwrite=F, 
                 srcfile=tif, dstfile=target)
      }
    }
    
  }
  
}



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
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")

picked_grids<-readRDS("../Objects/virtual_lands/picked_grids.rda")

species_list<-readRDS("../Objects/IUCN_Distributions/areas.rda")
area_threshold<-2e6
species_list<-species_list[area<=area_threshold]

j=1
if (F){
  mask<-"../Raster/ndvi/ndvi_mask_250m.tif"
  for (j in c(1:nrow(grids))){
      tif<-sprintf("../Objects/GRIDS/%d/grid.tif", grids[j,]$index)
      shp<-sprintf("../Objects/GRIDS/%d/grid.shp", grids[j,]$index)
      gdalwarp(crop_to_cutline=T, dstnodata=-9999, cutline=shp, overwrite=F, 
               srcfile=mask, dstfile=tif)
  }
  
}
#stack_dis<-NULL
#div_p_list<-list()
for (j in c(1:nrow(picked_grids))){
  print(j)
  if (file.exists(sprintf("../Objects/GRIDS_S/Diversity/%d.tif", picked_grids[j,]$index))){
    next()
  }
  sp<-list.files(sprintf("../Objects/GRIDS/%d", picked_grids[j,]$index), pattern="\\.tif$", full.names = T)
  sp<-sp[!(grepl("grid\\.tif", sp))]
  grid_mask<-raster(sprintf("../Objects/GRIDS/%d/grid.tif", picked_grids[j,]$index))
  v<-values(grid_mask)
  v[v==255]<--1
  v[v==1]<-0
  values(grid_mask)<-v
  stack_dis<-grid_mask
  item<-sp[1]
  for (item in sp){
    sp_dis<-raster(item)
    v<-values(sp_dis)
    v[v==255]<-0
    v[is.na(v)]<-0
    v[v==1]<-1
    values(sp_dis)<-v
    stack_dis<-stack(stack_dis, sp_dis)
  }
  diversity<-sum(stack_dis)
  v<-values(diversity)
  v[v<=0]<-0
  values(diversity)<-v
  writeRaster(diversity, sprintf("../Objects/GRIDS_S/Diversity/%d.tif", picked_grids[j,]$index), overwrite=T)
  #div_p<-data.table(rasterToPoints(diversity))
  #div_p$label<-picked_grids[j,]$label
  #div_p_list[[length(div_p_list)+1]]<-div_p
}

library(ggpubr)
plist<-list()

for (j in c(1:nrow(picked_grids))){
  print(j)
  div<-raster(sprintf("../Objects/GRIDS_S/Diversity/%d.tif", picked_grids[j,]$index))
  div<-data.table(rasterToPoints(div))
  colnames(div)[3]<-"div"
  div$x<-div$x-extent(div)[1]
  div$y<-div$y-extent(div)[3]
  div$label<-picked_grids[j,]$label
  p<-ggplot(div)+geom_tile(aes(x=x, y=y, fill=div))+
    coord_equal()+
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
  plist[[length(plist)+1]]<-p
  
}

pp<-ggarrange(plotlist=plist, ncol=5, nrow=2, labels=letters[1:10])



ggsave(pp, filename="../Figures/IUCN_Based_VW/diversity.png", width=14, height=6)
