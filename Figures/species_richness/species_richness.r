library(data.table)
library(raster)
library(ggplot2)
library(ggpubr)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
grids_full<-readRDS("../Objects/IUCN_Distributions/iucn_richness_full_100km.rda")
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


mask<-raster("/media/huijieqiao/SSD_Fast/ES50_eBird/Raster/mask_100km.tif")
all_with_v<-readRDS("../Objects/eBird_Bioclim.rda")
ll_cols<-c("LONGITUDE_rough", "LATITUDE_rough")
ll<-unique(all_with_v[, ..ll_cols])
ll_sf<-st_as_sf(ll, coords=ll_cols, crs=st_crs(4326))
ll_sf_moll<-st_transform(ll_sf, crs=st_crs(mask))
ll_sf_moll$mask_100km<-extract(mask, st_coordinates(ll_sf_moll))
plot(st_geometry(ll_sf_moll), col=ll_sf_moll$mask_100km)
ll_sf_moll<-cbind(ll_sf_moll, ll)
ll_sf_moll$geometry<-NULL
ll_sf_moll<-data.table(ll_sf_moll)
ll_sf_moll_df<-merge(ll_sf_moll, all_with_v, by=c("LATITUDE_rough", "LONGITUDE_rough"))

mask_p<-data.table(rasterToPoints(mask))
species_richness<-ll_sf_moll_df[,.(n_species=length(unique(SCIENTIFIC_NAME)),
                                N_Observation=sum(N)), 
                                by=list(mask_100km)]
species_richness<-merge(species_richness, mask_p, by="mask_100km", all=T)
species_richness<-species_richness[!is.na(mask_100km)]
species_richness[is.na(n_species)]$n_species<-0
species_richness[is.na(N_Observation)]$N_Observation<-0
max_fix<-500
species_richness$fix_n_species<-species_richness$n_species
species_richness[fix_n_species>max_fix]$fix_n_species<-max_fix
labs<-as.character(seq(100, max_fix, by=100))
labs[5]<-sprintf(">500 up to %d", max(species_richness$n_species))
p2<-ggplot(species_richness)+
  geom_sf(data=grids_full, fill=NA, color=NA)+
  geom_tile(aes(x=x, y=y, fill=fix_n_species))+
  theme_bw()+labs(fill="Species richness")+
  scale_fill_gradient(low=colors_blue[4], high=colors_red[7],
                      breaks=seq(100, max_fix, by=100), labels=labs)+
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.background = element_blank(), 
        legend.background = element_rect(fill = "white", color = NA))
p2
hist(species_richness$fix_N_Observation)
max_fix_nb<-1e4
species_richness$fix_N_Observation<-species_richness$N_Observation
species_richness[fix_N_Observation>max_fix_nb]$fix_N_Observation<-max_fix_nb
labs<-as.character(seq(0, max_fix_nb, by=2000))
labs[6]<-sprintf(">%d up to %d", max_fix_nb, max(species_richness$N_Observation))
p3<-ggplot(species_richness)+
  geom_sf(data=grids_full, fill=NA, color=NA)+
  geom_tile(aes(x=x, y=y, fill=fix_N_Observation))+
  #coord_equal()+
  theme_bw()+labs(fill="Number of observations")+
  scale_fill_gradient(low=colors_blue[4], high=colors_red[7],
                      breaks=seq(0, max_fix_nb, by=2000), labels=labs)+
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.background = element_blank(), 
        legend.background = element_rect(fill = "white", color = NA))
p3

pp<-ggarrange(plotlist=list(p, p2, p3), nrow=3, ncol=1, 
              labels = c("(a)", "(b)", "(c)"))
pp
ggsave(pp, filename="../Figures/Full_Species_richness.png", width=6, height=6)
