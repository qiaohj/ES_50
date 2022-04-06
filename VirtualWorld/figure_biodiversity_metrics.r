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
biod_se_list<-readRDS("../Objects/biod_metrics.rda")
es_se_list<-readRDS("../Objects/es_metrics.rda")

biod<-biod_se_list[NA_Type=="With NA"]

biod$road_percent<-biod$n_road_5km/1600

ggplot(biod)+geom_point(aes(x=road_percent, y=species.richness, color=res))+
  scale_color_manual(values=colorBlindBlack8[2:8], 
                     breaks=c("100km", "50km", "20km", "10km", "5km", "2km", "1km"))+
  labs(fill="Resolution", x="coverage of 5km road's buffer (%)", y="Species richness")+
  theme_bw()
