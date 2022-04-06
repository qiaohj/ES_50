library(sf)
library(data.table)
library(raster)
library(scales)
library(ggplot2)
library(ggpubr)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
if (F){
  seeds<-readRDS("../Objects/virtual_lands/virtual_lands_property.rda")
  ndvi<-raster("../Raster/ndvi/mean_ndvi_moll_250m.tif")
  i=1
  seeds$n_ndvi<-0
  for (i in 1:nrow(seeds)){
    print(i)
    if (seeds[i]$n_ndvi>0){
      next()
    }
    
    road_points<-readRDS(sprintf("../Objects/virtual_lands/items/%d/road_points.rda", i))
    if (!("ndvi" %in% colnames(road_points))){
      r<-raster(sprintf("../Objects/virtual_lands/items/%d/road_5km.tif", i))
      min_x<-extent(r)[1]
      min_y<-extent(r)[3]
      road_points$ndvi<-raster::extract(ndvi, data.frame(x=road_points$x+min_x, y=road_points$y+min_y))
      saveRDS(road_points, sprintf("../Objects/virtual_lands/items/%d/road_points.rda", i))
      
    }
    
    seeds[i]$n_ndvi<-nrow(road_points[!is.na(ndvi)])
  }
  saveRDS(seeds, "../Objects/virtual_lands/virtual_lands_property.rda")
}
source("colors.r")
ndvi<-raster("../Raster/ndvi/mean_ndvi_moll_10km.tif")
ndvi_p<-data.table(rasterToPoints(ndvi))
plot(ndvi)
seeds<-readRDS("../Objects/virtual_lands/virtual_lands_property.rda")

points(seeds$x, seeds$y)
hist(seeds$n_no_road_5km)
table(seeds$binds)

land<-raster(sprintf("../Objects/virtual_lands/items/1/road_5km.tif"))
box<-st_as_sfc(st_bbox(land))
plot(box, add=T, border="red")





setorderv(seeds, c("binds", "n_ndvi"), c(1, -1))
selected_seeds<-seeds[, head(.SD, 1), by=binds]
selected_seeds<-selected_seeds[c(seq(1, 100, by=10), 100)]
selected_seeds<-selected_seeds[-6]
selected_seeds$labels<-letters[1:10]
p<-ggplot(ndvi_p)+geom_tile(aes(x=x, y=y, fill=mean_ndvi_moll_10km))+coord_equal()
p<-p+geom_point(data=seeds, aes(x=x, y=y), color=colors_blue[5], size=0.5)
p<-p+geom_point(data=selected_seeds, aes(x=x, y=y), color=colors_red[8], size=1)
p<-p+geom_text(data=selected_seeds, aes(x=x, y=y, label=labels), position = position_dodge(0.9))
p<-p+scale_fill_gradient(low=map_background, high="grey")+
  map_theme
ggsave(p, filename="../Figures/VW/virtual_land_full.png", width=15, height=8)

plot(selected_seeds$x, selected_seeds$y)
plists<-list()
for (i in 1:nrow(selected_seeds)){
  print(i)
  road_points<-readRDS(sprintf("../Objects/virtual_lands/items/%d/road_points.rda", selected_seeds[i]$index))
  px<-ggplot(road_points)+geom_tile(aes(x=x, y=y, fill=type))+
    coord_equal()+labs(title=sprintf("(%s)", selected_seeds[i]$labels), 
                       subtitle=sprintf("%.0f%% on road, %.0f%% in 2km buffer, and %.0f%% in 5km buffer",
                                        selected_seeds[i]$n_road_0km/1600,
                                        selected_seeds[i]$n_road_2km/1600, selected_seeds[i]$n_road_5km/1600))+
    scale_fill_manual(values=road_colors)+
    map_theme
  plists[[length(plists)+1]]<-px
  
}
ptop<-ggarrange(plotlist=plists[c(1:5)], nrow=1, ncol=5)
pbottom<-ggarrange(plotlist=plists[c(6:10)], nrow=1, ncol=5)

pp<-ggarrange(plotlist=list(ptop, p, pbottom), nrow=3, ncol=1, heights=c(1,2,1), widths=c(1,1,1))
ggsave(pp, filename="../Figures/VW/virtual_land_all.png", width=10, height=7)

pp2<-ggarrange(plotlist=plists, nrow=2, ncol=5)
ggsave(pp2, filename="../Figures/VW/virtual_land_items.png", width=20, height=10)



