library(raster)
library(data.table)
library(ggplot2)
library(sf)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
effort_distance<-1000
random_observing_events<-readRDS("../Objects/virtual_observing_events/random_observing_events.rda")
virtual_species<-readRDS("../Objects/virtual_species/virtual_species.rda")
species_points<-st_as_sf(virtual_species, coords = c("x", "y"))
one_observer<-random_observing_events[1,]
box<-st_bbox(one_observer)
sp_occ<-virtual_species[between(x, box[1], box[3])&between(y, box[2], box[4])]
sp_occ<-sp_occ[rarity<1000]
species_points<-st_as_sf(sp_occ, coords = c("x", "y"))
in_p<-unlist(st_intersects(one_observer, species_points))
sp_occ$status<-"out of range"
sp_occ[in_p]$status<-ifelse(runif(length(in_p), 0, 1)>0.5, "observed", "not observed")
p<-ggplot()+geom_sf(data=one_observer, color=colors_black[9], fill=NA)+
  geom_point(data=sp_occ, aes(x=x, y=y, shape=status))+
  scale_shape_manual(values=c(1, 16, 4), breaks=c("out of range", "observed", "not observed"))+
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
ggsave(p, filename="../Figures/VW/virtual_observing_event.png", width=6, height=6)

mask_index<-349
road_points<-readRDS(sprintf("../Objects/virtual_lands/items/%d/road_points.rda", mask_index))
px<-ggplot(road_points)+geom_tile(aes(x=x, y=y, fill=type))+scale_fill_manual(values=road_colors)+coord_equal()
p1<-px+geom_sf(data=random_observing_events, fill=NA, color=colors_red[4])
p1<-p1+map_theme
ggsave(p1, filename="../Figures/VW/random_virtual_observing_event.png", width=6, height=6)


observing_events<-readRDS(sprintf("../Objects/virtual_lands/items/%d/observing_events.rda", mask_index))


observing_colors<-c("offroad"=colorBlindBlack8[2], "on road"=colorBlindBlack8[3], 
                    "2km buffer"=colorBlindBlack8[4], "5km buffer"=colorBlindBlack8[5])
points<-st_as_sf(observing_events, coords = c("x", "y"))
buffers<-st_buffer(points, effort_distance)
#ggplot()+geom_sf(data=buffers, aes(color=type))

p2<-ggplot(road_points)+geom_tile(aes(x=x, y=y, fill=type))+
  geom_sf(data=buffers, aes(color=type))+
  geom_tile(data=road_points[type=="on road"], aes(x=x, y=y, fill=type))+
  scale_fill_manual(values=road_colors)+
  scale_color_manual(values=observing_colors)+
  map_theme
ggsave(p2, filename="../Figures/VW/road_based_virtual_observing_event.png", width=6, height=6)
p<-ggarrange(plotlist=list(p1, p2), nrow=1, ncol=2)
ggsave(p, filename="../Figures/VW/virtual_observing_events_full.png", width=8, height=4)
