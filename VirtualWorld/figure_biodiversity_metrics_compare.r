library(data.table)
library(ggplot2)
library(raster)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
source("colors.r")
mask_index<-349
random_diversity<-readRDS("../Objects/virtual_observing_events/random_biodiversity_metrics.rda")
random_es<-readRDS("../Objects/virtual_observing_events/random_es_metrics.rda")
road_based_diversity<-readRDS(sprintf("../Objects/virtual_lands/items/%d/biodiversity.rda", mask_index))
road_based_es<-readRDS(sprintf("../Objects/virtual_lands/items/%d/es.rda", mask_index))
real_diversity<-readRDS("../Objects/virtual_observing_events/real_biodiversity_metrics.rda")
real_es<-readRDS("../Objects/virtual_observing_events/real_es_metrics.rda")
road_points<-readRDS(sprintf("../Objects/virtual_lands/items/%d/road_points.rda", mask_index))
resolution<-"1km"

mask<-raster(sprintf("../Objects/masks/mask_%s.tif", resolution))
mask_p<-data.table(rasterToPoints(mask))
#mask_p<-mask_p[between(x, 0, 100000)&between(y, 0, 100000)]
colnames(mask_p)[3]<-"mask"
mask_p_random<-merge(mask_p, random_diversity[res==resolution], by="mask")
mask_p_random$type<-"Random sampling"
p_random<-ggplot()+
  geom_tile(data=mask_p_random, aes(x=x, y=y, fill=species.richness), color="grey")+
  coord_equal()+
  scale_fill_gradient(low=colors_blue[6], high=colors_red[6])+
  map_theme
p_random

mask_p_full<-merge(mask_p, real_diversity[res==resolution], by="mask")
mask_p_full$type<-"Raw data"
p_real<-ggplot()+
  geom_tile(data=mask_p_full, aes(x=x, y=y, fill=species.richness), color="grey")+
  coord_equal()+
  scale_fill_gradient(low=colors_blue[6], high=colors_red[6])+
  map_theme
p_real

mask_p_road<-merge(mask_p, road_based_diversity[res==resolution], by="mask")
mask_p_road$type<-"Road-based sampling"
p_road_based<-ggplot()+
  geom_tile(data=mask_p_road, aes(x=x, y=y, fill=species.richness), color="grey")+
  coord_equal()+
  scale_fill_gradient(low=colors_blue[6], high=colors_red[6])+
  map_theme
p_road_based

mask_p_all<-rbindlist(list(mask_p_full, mask_p_random, mask_p_road))
mask_p_all$type<-factor(mask_p_all$type, levels=c("Raw data", "Random sampling", "Road-based sampling"))
p_all<-ggplot()+
  geom_tile(data=mask_p_all, aes(x=x, y=y, fill=species.richness), color="grey")+
  #geom_text(data=mask_p_all, aes(x=x, y=y, label=species.richness), color="black")+
  coord_equal()+
  scale_fill_gradient(low=colors_blue[6], high=colors_red[6])+
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.background = element_blank())+
  facet_wrap(~type)
ggsave(p_all, filename=sprintf("../Figures/biodiversity_metrics/compare_example_%s.png", resolution),
       width=15, height=5)

p_road<-ggplot(road_points)+geom_tile(aes(x=x, y=y, fill=type))+
  coord_equal()+
  scale_fill_manual(values=road_colors)+
  map_theme
