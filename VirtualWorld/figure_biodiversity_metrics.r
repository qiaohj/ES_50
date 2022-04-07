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
if (F){
  biod_with_na_list<-readRDS("../Objects/biod_with_na_raw.rda")
  biod_with_na_list$road_percent<-biod_with_na_list$n_road_5km/1600
  biod_se_with_na<-biod_with_na_list[, .(
    species.richness=mean(species.richness), sd_species.richness=sd(species.richness),
    simpson=mean(v), sd_simpson=sd(v),
    shannon=mean(shannon), sd_shannon=sd(shannon),
    invsimp=mean(invsimp), sd_invsimp=sd(invsimp),
    unbias.simp=mean(unbias.simp), sd_unbias.simp=sd(unbias.simp),
    J=mean(J), sd_J=sd(J),
    Hill_0=mean(Hill_0), sd_Hill_0=sd(Hill_0),
    Hill_0.25=mean(Hill_0.25), sd_Hill_0.25=sd(Hill_0.25),
    Hill_0.5=mean(Hill_0.5), sd_Hill_0.5=sd(Hill_0.5),
    Hill_1=mean(Hill_1), sd_Hill_1=sd(Hill_1),
    Hill_2=mean(Hill_2), sd_Hill_2=sd(Hill_2),
    Hill_4=mean(Hill_4), sd_Hill_4=sd(Hill_4),
    Hill_8=mean(Hill_8), sd_Hill_8=sd(Hill_8),
    Hill_16=mean(Hill_16), sd_Hill_16=sd(Hill_16),
    Hill_32=mean(Hill_32), sd_Hill_32=sd(Hill_32),
    Hill_64=mean(Hill_64), sd_Hill_64=sd(Hill_64),
    Hill_Inf=mean(Hill_Inf), sd_Hill_Inf=sd(Hill_Inf),
    alpha=mean(alpha), sd_alpha=sd(alpha),
    road_percent=mean(road_percent), sd_road_percent=sd(road_percent)),
                by=list(binds, res)]
  
  biod_without_na_list<-readRDS("../Objects/biod_without_na_raw.rda")
  biod_without_na_list$road_percent<-biod_without_na_list$n_road_5km/1600
  biod_se_without_na<-biod_without_na_list[, .(
    species.richness=mean(species.richness), sd_species.richness=sd(species.richness),
    simpson=mean(v), sd_simpson=sd(v),
    shannon=mean(shannon), sd_shannon=sd(shannon),
    invsimp=mean(invsimp), sd_invsimp=sd(invsimp),
    unbias.simp=mean(unbias.simp), sd_unbias.simp=sd(unbias.simp),
    J=mean(J), sd_J=sd(J),
    Hill_0=mean(Hill_0), sd_Hill_0=sd(Hill_0),
    Hill_0.25=mean(Hill_0.25), sd_Hill_0.25=sd(Hill_0.25),
    Hill_0.5=mean(Hill_0.5), sd_Hill_0.5=sd(Hill_0.5),
    Hill_1=mean(Hill_1), sd_Hill_1=sd(Hill_1),
    Hill_2=mean(Hill_2), sd_Hill_2=sd(Hill_2),
    Hill_4=mean(Hill_4), sd_Hill_4=sd(Hill_4),
    Hill_8=mean(Hill_8), sd_Hill_8=sd(Hill_8),
    Hill_16=mean(Hill_16), sd_Hill_16=sd(Hill_16),
    Hill_32=mean(Hill_32), sd_Hill_32=sd(Hill_32),
    Hill_64=mean(Hill_64), sd_Hill_64=sd(Hill_64),
    Hill_Inf=mean(Hill_Inf), sd_Hill_Inf=sd(Hill_Inf),
    alpha=mean(alpha), sd_alpha=sd(alpha),
    road_percent=mean(road_percent), sd_road_percent=sd(road_percent)),
    by=list(binds, res)]
  saveRDS(biod_se_without_na, "../Objects/biod_se_without_na_raw.rda")
  saveRDS(biod_se_with_na, "../Objects/biod_se_with_na_raw.rda")
}


#biod$road_percent<-biod$n_road_5km/1600
#biod_se<-biod[, .(species.richness=mean(species.richness), sd_species.richness=sd(species.richness),
#                  alpha=mean(alpha), sd_alpha=sd(alpha),
#                  road_percent=mean(road_percent), sd_road_percent=sd(road_percent)),
#              by=list(binds, res)]
random_biod_se$road_percent<-100
p<-ggplot(biod_se_without_na)+
  geom_hline(data=random_biod_se[NA_Type=="Without NA"], 
             aes(yintercept=species.richness, color=res), linetype=2)+
  geom_ribbon(aes(x=road_percent, 
                  ymin=species.richness-sd_species.richness,
                  ymax=species.richness+sd_species.richness,
                  color=NA, fill=res), alpha=0.2)+
  geom_line(aes(x=road_percent, y=species.richness, color=res))+
  
  scale_color_manual(values=colorBlindBlack8[2:8], 
                     breaks=c("100km", "50km", "20km", "10km", "5km", "2km", "1km"))+
  scale_fill_manual(values=colorBlindBlack8[2:8], 
                     breaks=c("100km", "50km", "20km", "10km", "5km", "2km", "1km"))+
  labs(fill="Resolution", color="Resolution", x="coverage of 5km road's buffer (%)", y="Species richness")+
  theme_bw()
p
ggsave(p, filename="../Figures/biodiversity_metrics/species.richness.png", width=10, height=6)

p<-ggplot(biod_se)+
  geom_hline(data=random_biod_se[NA_Type=="With NA"], 
             aes(yintercept=alpha, color=res), linetype=2)+
  geom_ribbon(aes(x=road_percent, 
                  ymin=alpha-sd_alpha,
                  ymax=alpha+sd_alpha,
                  color=res, fill=res), alpha=0.4)+
  geom_line(aes(x=road_percent, y=alpha, color=res))+
  
  scale_color_manual(values=colorBlindBlack8[2:8], 
                     breaks=c("100km", "50km", "20km", "10km", "5km", "2km", "1km"))+
  scale_fill_manual(values=colorBlindBlack8[2:8], 
                    breaks=c("100km", "50km", "20km", "10km", "5km", "2km", "1km"))+
  labs(fill="Resolution", color="Resolution", x="coverage of 5km road's buffer (%)", y="Fisher's alpha")+
  theme_bw()
p
ggsave(p, filename="../Figures/biodiversity_metrics/fisher_alpha.png", width=10, height=6)


es<-es_se_list[available==T]
es$road_percent<-es$n_road_5km/1600
es_se<-es[, .(es=mean(es), sd_es=sd(es),
              road_percent=mean(road_percent), sd_road_percent=sd(road_percent)),
              by=list(binds, res, esNum, available)]
random_es_se$road_percent<-100
p<-ggplot(es_se[available==T&esNum==50])+
  geom_hline(data=random_es_se[available==T&esNum==50], 
             aes(yintercept=es, color=res), linetype=2)+
  geom_ribbon(aes(x=road_percent, 
                  ymin=es-sd_es,
                  ymax=es+sd_es,
                  color=NA, fill=res), alpha=0.4)+
  geom_line(aes(x=road_percent, y=es, color=res))+
  
  scale_color_manual(values=colorBlindBlack8[2:8], 
                     breaks=c("100km", "50km", "20km", "10km", "5km", "2km", "1km"))+
  scale_fill_manual(values=colorBlindBlack8[2:8], 
                    breaks=c("100km", "50km", "20km", "10km", "5km", "2km", "1km"))+
  labs(fill="Resolution", color="Resolution", x="coverage of 5km road's buffer (%)", y="Hurlbert index, ES(50)")+
  theme_bw()

ggsave(p, filename="../Figures/biodiversity_metrics/es50.png", width=10, height=6)

p<-ggplot(es_se[available==T])+
  geom_hline(data=random_es_se[available==T], 
             aes(yintercept=es, color=res), linetype=2)+
  geom_ribbon(aes(x=road_percent, 
                  ymin=es-sd_es,
                  ymax=es+sd_es,
                  color=NA, fill=res), alpha=0.4)+
  geom_line(aes(x=road_percent, y=es, color=res))+
  
  scale_color_manual(values=colorBlindBlack8[2:8], 
                     breaks=c("100km", "50km", "20km", "10km", "5km", "2km", "1km"))+
  scale_fill_manual(values=colorBlindBlack8[2:8], 
                    breaks=c("100km", "50km", "20km", "10km", "5km", "2km", "1km"))+
  labs(fill="Resolution", color="Resolution", x="coverage of 5km road's buffer (%)", y="Hurlbert index, ES(x)")+
  theme_bw()+
  facet_wrap(~esNum, nrow=5, ncol=1, scale="free")
p
ggsave(p, filename="../Figures/biodiversity_metrics/es_all.png", width=6, height=14)
