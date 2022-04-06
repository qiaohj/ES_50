library(raster)
library(data.table)
library(ggplot2)
library(sf)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
effort_distance<-1000
#For random observation
mask<-raster("../Objects/mask.tif")
mask_p<-data.table(rasterToPoints(mask))
observations<-mask_p[between(x, min(x)+effort_distance, max(x)-effort_distance)&
                       between(y, min(y)+effort_distance, max(y)-effort_distance)]
random_observations<-observations[sample(nrow(observations), 1000)]
points<-st_as_sf(random_observations, coords = c("x", "y"))
buffers<-st_buffer(points, effort_distance)
plot(mask, col="lightgrey")
plot(st_geometry(buffers), add=T, border="red")

saveRDS(buffers, "../Objects/virtual_observing_events/random_observing_events.rda")

seeds<-readRDS("../Objects/virtual_lands/centers.rda")
i=500
result<-list()
n_road_0km<-550
n_road_2km<-330
n_road_5km<-110
n_offroad<-10
for (i in c(1:nrow(seeds))){
  print(i)
  folder<-sprintf("../Objects/virtual_lands/items/%d", i)
  road_points<-readRDS(sprintf("%s/road_points.rda", folder))
  road_points_0km<-road_points[road_0km>=0]
  if (nrow(road_points_0km)>n_road_0km){
    observations_road_0km<-road_points_0km[sample(nrow(road_points_0km), n_road_0km)]
  }else{
    observations_road_0km<-road_points_0km[sample(nrow(road_points_0km), n_road_0km, replace=T)]
  }
  
  road_points_2km<-road_points[road_0km<0&road_2km>=0]
  if (nrow(road_points_2km)>n_road_2km){
    observations_road_2km<-road_points_2km[sample(nrow(road_points_2km), n_road_2km)]
  }else{
    observations_road_2km<-road_points_2km[sample(nrow(road_points_2km), n_road_2km, replace=T)]
  }
  
  road_points_5km<-road_points[road_2km<0&road_5km>=0]
  if (nrow(road_points_5km)>n_road_5km){
    observations_road_5km<-road_points_5km[sample(nrow(road_points_5km), n_road_5km)]
  }else{
    observations_road_5km<-road_points_5km[sample(nrow(road_points_5km), n_road_5km, replace=T)]
  }
  
  offroad_points<-road_points[road_5km<0]
  if (nrow(offroad_points)>n_offroad){
    observations_offroad<-offroad_points[sample(nrow(offroad_points), n_offroad)]
  }else{
    observations_offroad<-offroad_points[sample(nrow(offroad_points), n_offroad, replace=T)]
  }
  observations_road_0km$type<-"on road"
  observations_road_2km$type<-"2km buffer"
  observations_road_5km$type<-"5km buffer"
  observations_offroad$type<-"offroad"
  observing_events<-rbindlist(list(observations_road_0km, observations_road_2km, 
                                   observations_road_5km, observations_offroad))
  saveRDS(observing_events, sprintf("%s/observing_events.rda", folder))
  if (F){
    road_points$type<-"offroad"
    road_points[road_0km>=0]$type<-"on road"
    road_points[road_0km<0&road_2km>=0]$type<-"2km buffer"
    road_points[road_2km<0&road_5km>=0]$type<-"5km buffer"
    road_colors<-c("offroad"="white", "on road"="black", "2km buffer"="grey", "5km buffer"="lightgrey")
    colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                           "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    observing_colors<-c("offroad"=colorBlindBlack8[2], "on road"=colorBlindBlack8[3], 
                   "2km buffer"=colorBlindBlack8[4], "5km buffer"=colorBlindBlack8[5])
    points<-st_as_sf(observing_events, coords = c("x", "y"))
    buffers<-st_buffer(points, effort_distance)
    #ggplot()+geom_sf(data=buffers, aes(color=type))
    
    ggplot(road_points)+geom_tile(aes(x=x, y=y, fill=type))+
      geom_sf(data=buffers, aes(color=type))+
      geom_tile(data=road_points[type=="on road"], aes(x=x, y=y, fill=type))+
      scale_fill_manual(values=road_colors)+
      scale_color_manual(values=observing_colors)+
      theme_bw()
    
    random_bufer<-readRDS("../Objects/virtual_observing_events/random_observing_events.rda")
    
    ggplot(road_points)+geom_tile(aes(x=x, y=y, fill=type))+
      geom_sf(data=random_bufer, color=colorBlindBlack8[6])+
      geom_tile(data=road_points[type=="on road"], aes(x=x, y=y, fill=type))+
      scale_fill_manual(values=road_colors)+
      scale_color_manual(values=observing_colors)+
      theme_bw()
  }
}
