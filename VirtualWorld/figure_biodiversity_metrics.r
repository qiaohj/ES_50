library(raster)
library(data.table)
library(ggplot2)
library(sf)
library(dplyr)
library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(vegan)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
source("colors.r")
random_biod_se<-readRDS("../Objects/random_sampling_biod_metrics.rda")
random_es_se<-readRDS("../Objects/random_sampling_es_metrics.rda")

biod_se_list<-readRDS("../Objects/biod_metrics.rda")
es_se_list<-readRDS("../Objects/es_metrics.rda")

biod<-biod_se_list[NA_Type=="With NA"]

biod$road_percent<-biod$n_road_5km/1600
biod_se<-biod[, .(species.richness=mean(species.richness), sd_species.richness=sd(species.richness),
                  road_percent=mean(road_percent), sd_road_percent=sd(road_percent)),
              by=list(binds, res)]
random_biod_se$road_percent<-100
p<-ggplot(biod_se)+
  geom_hline(data=random_biod_se[NA_Type=="With NA"], 
             aes(yintercept=species.richness, color=res), linetype=2)+
  geom_ribbon(aes(x=road_percent, 
                  ymin=species.richness-sd_species.richness,
                  ymax=species.richness+sd_species.richness,
                  color=res, fill=res), alpha=0.4)+
  geom_line(aes(x=road_percent, y=species.richness, color=res))+
  
  scale_color_manual(values=colorBlindBlack8[2:8], 
                     breaks=c("100km", "50km", "20km", "10km", "5km", "2km", "1km"))+
  scale_fill_manual(values=colorBlindBlack8[2:8], 
                     breaks=c("100km", "50km", "20km", "10km", "5km", "2km", "1km"))+
  labs(fill="Resolution", color="Resolution", x="coverage of 5km road's buffer (%)", y="Species richness")+
  theme_bw()
ggsave(p, filename="../Figures/biodiversity_metrics/species.richness.png", width=10, height=6)
