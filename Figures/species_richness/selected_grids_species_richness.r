library(data.table)
library(raster)
library(ggplot2)
library(ggpubr)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
#grids_full<-readRDS("../Objects/IUCN_Distributions/iucn_richness_full_100km.rda")
grids_full<-data.table(rasterToPoints(raster("../Figures/IUCN_Based_VW/TIFF/IUCN_richness/IUCN_richness_10km.tif")))
colnames(grids_full)[3]<-"species.richness"
source("colors.r")
max_fix<-500
grids_full$fixed_species.richness<-grids_full$species.richness
grids_full[grids_full$fixed_species.richness>max_fix,]$fixed_species.richness<-max_fix
max_richness<-max(grids_full$species.richness)
#hist(grids_full$species.richness)
labs<-as.character(seq(100, max_fix, by=100))
labs[5]<-sprintf(">500 up to %d", max_richness)
picked_grids<-readRDS("../Objects/virtual_lands/picked_grids.rda")
source("colors.r")

p<-ggplot(grids_full)+geom_tile(aes(x=x, y=y, fill=fixed_species.richness), color=NA)+
  geom_sf(data=picked_grids, fill=NA, color="black", linetype=1)+
  geom_sf_text(data=picked_grids, aes(label = c(1:10)), colour = "black", size=2.8)+
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
ggsave(p, filename="../Figures/picked_grids.png", width=12, height=6)

