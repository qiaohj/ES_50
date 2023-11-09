library(ggplot2)
library(data.table)
setwd("/media/huijieqiao/WD10T_12/ES50_eBird/ES_50")
all_metrics_list<-list()
for (sampling_proportion in c(0.1, 0.5, 1)){
  if (sampling_proportion==1){
    all_metrics<-readRDS("../Objects/GRID_Evaluations/all.rda")
  }else{
    all_metrics<-readRDS(sprintf("../Objects/GRID_Evaluations/all_sampling_%.1f.rda", sampling_proportion))
  }
  all_metrics$sampling_proportion<-sampling_proportion
  all_metrics_list[[length(all_metrics_list)+1]]<-all_metrics
}
all_metrics<-rbindlist(all_metrics_list)
all_metrics$sampling_proportion<-all_metrics$sampling_proportion * 1000
all_metrics<-all_metrics[res!="100km"]
all_metrics$res_f<-factor(all_metrics$res, levels=c("1km", "2km", "5km", "10km", "20km", "50km"))
all_metrics[var=="shannon"]$var<-"Shannon"
all_metrics[var=="v"]$var<-"Simpson"
all_metrics[var=="J"]$var<-"Pielou's evenness"
all_metrics[var=="species.richness"]$var<-"Species richness"
all_metrics[var %in% c("es200", "es100", "es50", "es20", "es10")]$var<-toupper(all_metrics[var %in% c("es200", "es100", "es50", "es20", "es10")]$var)

all_metrics$var_f<-factor(all_metrics$var, levels=c("ES10", "ES20", "ES50", "ES100", "ES200", 
                                                    "Hill_0", "Hill_0.25", "Hill_0.5", "Hill_1",
                                                    "Hill_2", "Hill_4", "Hill_8", "Hill_16",
                                                    "Hill_32", "Hill_64", "Hill_Inf", 
                                                    "Shannon", "Simpson", "Pielou's evenness", "Species richness"))

all_metrics[is.na(var_f)]
unique(all_metrics$var)

all_metrics[res=="2km"]
source("colors.r")
if (F){
  colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                         "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  res_colors<-c("1km"=colorBlindBlack8[2], "2km"=colorBlindBlack8[3], 
                "5km"=colorBlindBlack8[4], "10km"=colorBlindBlack8[5],
                "20km"=colorBlindBlack8[6], "50km"=colorBlindBlack8[7],
                "100km"=colorBlindBlack8[8])
  
  p<-ggplot(all_metrics[!is.na(Rsquared_with_na)])+
    geom_smooth(aes(x=var, y=Rsquared_with_na, color=res_f), method="loess")+
    #geom_point(aes(x=var, y=Rsquared_with_na, color=res_f), 
    #           position=position_dodge(width = 0.9))+
    scale_color_manual(values=res_colors, 
                       breaks=c("1km", "2km", "5km", "10km", "20km", "50km"))+
    scale_fill_manual(values=res_colors, 
                      breaks=c("1km", "2km", "5km", "10km", "20km", "50km"))+
    labs(x="Biodiversity metrics", y=expression(R^2), fill="Resolution", color="Resolution")+
    theme_bw()+
    facet_wrap(~sampling_proportion)+
    theme(strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20))
  p
  ggsave(p, filename="../Figures/IUCN_Based_VW/metrics_loess.png", width=30, height=15)
  
  p<-ggplot(all_metrics)+
    geom_smooth(aes(x=var, y=Rsquared_with_na, fill=res_f), method="loess", alpha=0.2)+
    #geom_point(aes(x=var, y=Rsquared_with_na, color=res_f), 
    #           position=position_dodge(width = 0.9))+
    scale_color_manual(values=res_colors, 
                       breaks=c("1km", "2km", "5km", "10km", "20km", "50km"))+
    scale_fill_manual(values=res_colors, 
                      breaks=c("1km", "2km", "5km", "10km", "20km", "50km"))+
    labs(x="Biodiversity metrics", y=expression(R^2), fill="Resolution", color="Resolution")+
    theme_bw()+
    facet_wrap(~grid_index, nrow=2, ncol=5)
  ggsave(p, filename="../Figures/IUCN_Based_VW/metrics_loess_girded.png", width=15, height=10)
  
}

#Figure 15a
all_metrics_se<-all_metrics[!is.na(n_ndvi), .(cor_with_na=mean(cor_with_na, na.rm=T), sd_cor_with_na=sd(cor_with_na, na.rm=T),
                                              cor_without_na=mean(cor_without_na, na.rm=T), sd_cor_without_na=sd(cor_without_na, na.rm=T),
                                              RMSE_with_na=mean(RMSE_with_na, na.rm=T), sd_RMSE_with_na=sd(RMSE_with_na, na.rm=T),
                                              Rsquared_with_na=mean(Rsquared_with_na, na.rm=T), sd_Rsquared_with_na=sd(Rsquared_with_na, na.rm=T),
                                              MAE_with_na=mean(MAE_with_na, na.rm=T), sd_MAE_with_na=sd(MAE_with_na, na.rm=T),
                                              RMSE_without_na=mean(RMSE_without_na, na.rm=T), sd_RMSE_without_na=sd(RMSE_without_na, na.rm=T),
                                              Rsquared_without_na=mean(Rsquared_without_na, na.rm=T), sd_Rsquared_without_na=sd(Rsquared_without_na, na.rm=T),
                                              MAE_without_na=mean(MAE_without_na, na.rm=T), sd_MAE_without_na=sd(MAE_without_na, na.rm=T)),
                            by=list(res_f, var_f)]
all_metrics_se_random<-all_metrics[is.na(n_ndvi), .(cor_with_na=mean(cor_with_na, na.rm=T), sd_cor_with_na=sd(cor_with_na, na.rm=T),
                                                    cor_without_na=mean(cor_without_na, na.rm=T), sd_cor_without_na=sd(cor_without_na, na.rm=T),
                                                    RMSE_with_na=mean(RMSE_with_na, na.rm=T), sd_RMSE_with_na=sd(RMSE_with_na, na.rm=T),
                                                    Rsquared_with_na=mean(Rsquared_with_na, na.rm=T), sd_Rsquared_with_na=sd(Rsquared_with_na, na.rm=T),
                                                    MAE_with_na=mean(MAE_with_na, na.rm=T), sd_MAE_with_na=sd(MAE_with_na, na.rm=T),
                                                    RMSE_without_na=mean(RMSE_without_na, na.rm=T), sd_RMSE_without_na=sd(RMSE_without_na, na.rm=T),
                                                    Rsquared_without_na=mean(Rsquared_without_na, na.rm=T), sd_Rsquared_without_na=sd(Rsquared_without_na, na.rm=T),
                                                    MAE_without_na=mean(MAE_without_na, na.rm=T), sd_MAE_without_na=sd(MAE_without_na, na.rm=T)),
                                   by=list(res_f, var_f)]
all_metrics_se<-all_metrics_se[res_f!="100km"]

colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
res_colors<-c("1km"=colorBlindBlack8[2], "2km"=colorBlindBlack8[3], 
              "5km"=colorBlindBlack8[4], "10km"=colorBlindBlack8[5],
              "20km"=colorBlindBlack8[6], "50km"=colorBlindBlack8[7],
              "100km"=colorBlindBlack8[8])

p<-ggplot(all_metrics_se)+
  geom_errorbar(aes(x=var_f, 
                    ymin=Rsquared_with_na-sd_Rsquared_with_na, 
                    ymax=Rsquared_with_na+sd_Rsquared_with_na, color=res_f),
                position=position_dodge(width = 0.9), width=0.2)+
  geom_point(aes(x=var_f, y=Rsquared_with_na, color=res_f), 
             position=position_dodge(width = 0.9))+
  geom_point(data=all_metrics_se_random, 
             aes(x=var_f, y=Rsquared_with_na, color=res_f), 
             position=position_dodge(width = 0.9), shape=6)+
  labs(x="Biodiversity metrics", y=expression(R^2), fill="Resolution", color="Resolution")+
  scale_color_manual(values=res_colors, 
                     breaks=c("1km", "2km", "5km", "10km", "20km", "50km"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
p
ggsave(p, filename="../Figures/IUCN_Based_VW/metrics_full.png", width=10, height=4)



all_metrics_se<-all_metrics[!is.na(n_ndvi), .(cor_with_na=mean(cor_with_na, na.rm=T), sd_cor_with_na=sd(cor_with_na, na.rm=T),
                                cor_without_na=mean(cor_without_na, na.rm=T), sd_cor_without_na=sd(cor_without_na, na.rm=T),
                                RMSE_with_na=mean(RMSE_with_na, na.rm=T), sd_RMSE_with_na=sd(RMSE_with_na, na.rm=T),
                                Rsquared_with_na=mean(Rsquared_with_na, na.rm=T), sd_Rsquared_with_na=sd(Rsquared_with_na, na.rm=T),
                                MAE_with_na=mean(MAE_with_na, na.rm=T), sd_MAE_with_na=sd(MAE_with_na, na.rm=T),
                                RMSE_without_na=mean(RMSE_without_na, na.rm=T), sd_RMSE_without_na=sd(RMSE_without_na, na.rm=T),
                                Rsquared_without_na=mean(Rsquared_without_na, na.rm=T), sd_Rsquared_without_na=sd(Rsquared_without_na, na.rm=T),
                                MAE_without_na=mean(MAE_without_na, na.rm=T), sd_MAE_without_na=sd(MAE_without_na, na.rm=T)),
                            by=list(res_f, var_f, sampling_proportion)]
all_metrics_se_random<-all_metrics[is.na(n_ndvi), .(cor_with_na=mean(cor_with_na, na.rm=T), sd_cor_with_na=sd(cor_with_na, na.rm=T),
                                              cor_without_na=mean(cor_without_na, na.rm=T), sd_cor_without_na=sd(cor_without_na, na.rm=T),
                                              RMSE_with_na=mean(RMSE_with_na, na.rm=T), sd_RMSE_with_na=sd(RMSE_with_na, na.rm=T),
                                              Rsquared_with_na=mean(Rsquared_with_na, na.rm=T), sd_Rsquared_with_na=sd(Rsquared_with_na, na.rm=T),
                                              MAE_with_na=mean(MAE_with_na, na.rm=T), sd_MAE_with_na=sd(MAE_with_na, na.rm=T),
                                              RMSE_without_na=mean(RMSE_without_na, na.rm=T), sd_RMSE_without_na=sd(RMSE_without_na, na.rm=T),
                                              Rsquared_without_na=mean(Rsquared_without_na, na.rm=T), sd_Rsquared_without_na=sd(Rsquared_without_na, na.rm=T),
                                              MAE_without_na=mean(MAE_without_na, na.rm=T), sd_MAE_without_na=sd(MAE_without_na, na.rm=T)),
                            by=list(res_f, var_f, sampling_proportion)]
all_metrics_se<-all_metrics_se[res_f!="100km"]

colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
res_colors<-c("1km"=colorBlindBlack8[2], "2km"=colorBlindBlack8[3], 
              "5km"=colorBlindBlack8[4], "10km"=colorBlindBlack8[5],
              "20km"=colorBlindBlack8[6], "50km"=colorBlindBlack8[7],
              "100km"=colorBlindBlack8[8])

p<-ggplot(all_metrics_se)+
  geom_errorbar(aes(x=var_f, 
                    ymin=Rsquared_with_na-sd_Rsquared_with_na, 
                    ymax=Rsquared_with_na+sd_Rsquared_with_na, color=res_f),
                position=position_dodge(width = 0.9), width=0.2)+
  geom_point(aes(x=var_f, y=Rsquared_with_na, color=res_f), 
             position=position_dodge(width = 0.9))+
  geom_point(data=all_metrics_se_random, 
             aes(x=var_f, y=Rsquared_with_na, color=res_f), 
             position=position_dodge(width = 0.9), shape=6)+
  labs(x="Biodiversity metrics", y=expression(R^2), fill="Resolution", color="Resolution")+
  scale_color_manual(values=res_colors, 
                     breaks=c("1km", "2km", "5km", "10km", "20km", "50km"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  facet_wrap(~sampling_proportion, nrow=3, ncol=1, strip.position="right")
p
ggsave(p, filename="../Figures/IUCN_Based_VW/metrics_sampling_proportion.png", width=10, height=6)


all_metrics_se<-all_metrics[!is.na(n_ndvi), .(cor_with_na=mean(cor_with_na, na.rm=T), sd_cor_with_na=sd(cor_with_na, na.rm=T),
                                              cor_without_na=mean(cor_without_na, na.rm=T), sd_cor_without_na=sd(cor_without_na, na.rm=T),
                                              RMSE_with_na=mean(RMSE_with_na, na.rm=T), sd_RMSE_with_na=sd(RMSE_with_na, na.rm=T),
                                              Rsquared_with_na=mean(Rsquared_with_na, na.rm=T), sd_Rsquared_with_na=sd(Rsquared_with_na, na.rm=T),
                                              MAE_with_na=mean(MAE_with_na, na.rm=T), sd_MAE_with_na=sd(MAE_with_na, na.rm=T),
                                              RMSE_without_na=mean(RMSE_without_na, na.rm=T), sd_RMSE_without_na=sd(RMSE_without_na, na.rm=T),
                                              Rsquared_without_na=mean(Rsquared_without_na, na.rm=T), sd_Rsquared_without_na=sd(Rsquared_without_na, na.rm=T),
                                              MAE_without_na=mean(MAE_without_na, na.rm=T), sd_MAE_without_na=sd(MAE_without_na, na.rm=T)),
                            by=list(res_f, var_f, grid_index)]
all_metrics_se_random<-all_metrics[is.na(n_ndvi), .(cor_with_na=mean(cor_with_na, na.rm=T), sd_cor_with_na=sd(cor_with_na, na.rm=T),
                                                    cor_without_na=mean(cor_without_na, na.rm=T), sd_cor_without_na=sd(cor_without_na, na.rm=T),
                                                    RMSE_with_na=mean(RMSE_with_na, na.rm=T), sd_RMSE_with_na=sd(RMSE_with_na, na.rm=T),
                                                    Rsquared_with_na=mean(Rsquared_with_na, na.rm=T), sd_Rsquared_with_na=sd(Rsquared_with_na, na.rm=T),
                                                    MAE_with_na=mean(MAE_with_na, na.rm=T), sd_MAE_with_na=sd(MAE_with_na, na.rm=T),
                                                    RMSE_without_na=mean(RMSE_without_na, na.rm=T), sd_RMSE_without_na=sd(RMSE_without_na, na.rm=T),
                                                    Rsquared_without_na=mean(Rsquared_without_na, na.rm=T), sd_Rsquared_without_na=sd(Rsquared_without_na, na.rm=T),
                                                    MAE_without_na=mean(MAE_without_na, na.rm=T), sd_MAE_without_na=sd(MAE_without_na, na.rm=T)),
                                   by=list(res_f, var_f, grid_index)]
all_metrics_se<-all_metrics_se[res_f!="100km"]
all_metrics_se$grid_label<-letters[all_metrics_se$grid_index]
all_metrics_se_random$grid_label<-letters[all_metrics_se_random$grid_index]
all_metrics_se_random[grid_label=="a" & var_f=="ES10" & res_f=="1km"]

colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
res_colors<-c("1km"=colorBlindBlack8[2], "2km"=colorBlindBlack8[3], 
              "5km"=colorBlindBlack8[4], "10km"=colorBlindBlack8[5],
              "20km"=colorBlindBlack8[6], "50km"=colorBlindBlack8[7],
              "100km"=colorBlindBlack8[8])
target_metrics<-c("ES10", 
                  "Hill_Inf", 
                  "Shannon", "Simpson", 
                  "Pielou's evenness", "Species richness")
all_metrics_se$var_f<-as.character(all_metrics_se$var_f)
p<-ggplot(all_metrics_se[var_f %in% target_metrics])+
  geom_errorbar(aes(x=var_f, 
                    ymin=Rsquared_with_na-sd_Rsquared_with_na, 
                    ymax=Rsquared_with_na+sd_Rsquared_with_na, color=res_f),
                position=position_dodge(width = 0.9), width=0.2)+
  geom_point(aes(x=var_f, y=Rsquared_with_na, color=res_f), 
             position=position_dodge(width = 0.9))+
  geom_point(data=all_metrics_se_random[var_f %in% target_metrics], 
             aes(x=var_f, y=Rsquared_with_na, color=res_f), 
             position=position_dodge(width = 0.9), shape=6)+
  labs(x="Biodiversity metrics", y=expression(R^2), fill="Resolution", color="Resolution")+
  scale_color_manual(values=res_colors, 
                     breaks=c("1km", "2km", "5km", "10km", "20km", "50km"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  facet_wrap(~grid_label, nrow=2, ncol=5, strip.position="top")
p
ggsave(p, filename="../Figures/IUCN_Based_VW/metrics_grid_index.png", width=10, height=6)


all_metrics_se2<-all_metrics[!is.na(n_ndvi), .(cor_with_na=mean(cor_with_na, na.rm=T), sd_cor_with_na=sd(cor_with_na, na.rm=T),
                                cor_without_na=mean(cor_without_na, na.rm=T), sd_cor_without_na=sd(cor_without_na, na.rm=T),
                                RMSE_with_na=mean(RMSE_with_na, na.rm=T), sd_RMSE_with_na=sd(RMSE_with_na, na.rm=T),
                                Rsquared_with_na=mean(Rsquared_with_na, na.rm=T), sd_Rsquared_with_na=sd(Rsquared_with_na, na.rm=T),
                                MAE_with_na=mean(MAE_with_na, na.rm=T), sd_MAE_with_na=sd(MAE_with_na, na.rm=T),
                                RMSE_without_na=mean(RMSE_without_na, na.rm=T), sd_RMSE_without_na=sd(RMSE_without_na, na.rm=T),
                                Rsquared_without_na=mean(Rsquared_without_na, na.rm=T), sd_Rsquared_without_na=sd(Rsquared_without_na, na.rm=T),
                                MAE_without_na=mean(MAE_without_na, na.rm=T), sd_MAE_without_na=sd(MAE_without_na, na.rm=T)),
                            by=list(res_f, var_f, grid_index, sampling_proportion)]

all_metrics_se_random<-all_metrics[is.na(n_ndvi), .(cor_with_na=mean(cor_with_na, na.rm=T), sd_cor_with_na=sd(cor_with_na, na.rm=T),
                                               cor_without_na=mean(cor_without_na, na.rm=T), sd_cor_without_na=sd(cor_without_na, na.rm=T),
                                               RMSE_with_na=mean(RMSE_with_na, na.rm=T), sd_RMSE_with_na=sd(RMSE_with_na, na.rm=T),
                                               Rsquared_with_na=mean(Rsquared_with_na, na.rm=T), sd_Rsquared_with_na=sd(Rsquared_with_na, na.rm=T),
                                               MAE_with_na=mean(MAE_with_na, na.rm=T), sd_MAE_with_na=sd(MAE_with_na, na.rm=T),
                                               RMSE_without_na=mean(RMSE_without_na, na.rm=T), sd_RMSE_without_na=sd(RMSE_without_na, na.rm=T),
                                               Rsquared_without_na=mean(Rsquared_without_na, na.rm=T), sd_Rsquared_without_na=sd(Rsquared_without_na, na.rm=T),
                                               MAE_without_na=mean(MAE_without_na, na.rm=T), sd_MAE_without_na=sd(MAE_without_na, na.rm=T)),
                             by=list(res_f, var_f, grid_index, sampling_proportion)]
target_metrics<-c("ES10", 
  "Hill_Inf", 
  "Shannon", "Simpson", 
  "Pielou's evenness", "Species richness")
all_metrics_se2$grid_label<-letters[all_metrics_se2$grid_index]
p<-ggplot(all_metrics_se2[var_f %in% target_metrics])+
  geom_errorbar(aes(x=var_f, 
                    ymin=Rsquared_with_na-sd_Rsquared_with_na, 
                    ymax=Rsquared_with_na+sd_Rsquared_with_na, color=res_f),
                position=position_dodge(width = 0.9), width=0.2)+
  geom_point(aes(x=var_f, y=Rsquared_with_na, color=res_f), 
             position=position_dodge(width = 0.9))+
  geom_point(data=all_metrics_se_random[var_f %in% target_metrics], 
             aes(x=var_f, y=Rsquared_with_na, color=res_f), 
             position=position_dodge(width = 0.9), shape=6)+
  scale_color_manual(values=res_colors, 
                     breaks=c("1km", "2km", "5km", "10km", "20km", "50km"))+
  labs(x="Biodiversity metrics", y=expression(R^2), fill="Resolution", color="Resolution")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  facet_grid(sampling_proportion~grid_label)+
  theme(strip.text.x = element_text(size = 20), 
        strip.text.y = element_text(size = 20),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 20))
p
ggsave(p, filename="../Figures/IUCN_Based_VW/metrics_grided.png", width=30, height=15)

all_metrics_se2<-all_metrics[, .(cor_with_na=mean(cor_with_na, na.rm=T), sd_cor_with_na=sd(cor_with_na, na.rm=T),
                                 cor_without_na=mean(cor_without_na, na.rm=T), sd_cor_without_na=sd(cor_without_na, na.rm=T),
                                 RMSE_with_na=mean(RMSE_with_na, na.rm=T), sd_RMSE_with_na=sd(RMSE_with_na, na.rm=T),
                                 Rsquared_with_na=mean(Rsquared_with_na, na.rm=T), sd_Rsquared_with_na=sd(Rsquared_with_na, na.rm=T),
                                 MAE_with_na=mean(MAE_with_na, na.rm=T), sd_MAE_with_na=sd(MAE_with_na, na.rm=T),
                                 RMSE_without_na=mean(RMSE_without_na, na.rm=T), sd_RMSE_without_na=sd(RMSE_without_na, na.rm=T),
                                 Rsquared_without_na=mean(Rsquared_without_na, na.rm=T), sd_Rsquared_without_na=sd(Rsquared_without_na, na.rm=T),
                                 MAE_without_na=mean(MAE_without_na, na.rm=T), sd_MAE_without_na=sd(MAE_without_na, na.rm=T)),
                             by=list(res_f, var_f, index, n_road_5km)]
all_metrics_se_road<-all_metrics_se2[!is.na(n_road_5km)]
all_metrics_se_random<-all_metrics_se2[is.na(n_road_5km)]
all_metrics_se_road_fig<-all_metrics_se_road[var_f %in% target_metrics]
var_colors<-colorBlindBlack8[2:7]
names(var_colors)<-target_metrics
all_metrics_se_road_fig_sampled<-all_metrics_se_road_fig[sample(nrow(all_metrics_se_road_fig), 1e4)]
p<-ggplot()+
  geom_smooth(data=all_metrics_se_road_fig, 
             aes(x=n_road_5km/16e2, y=Rsquared_with_na, color=var_f), method="loess")+
  geom_point(data=all_metrics_se_road_fig_sampled, 
             aes(x=n_road_5km/16e2, y=Rsquared_with_na, color=var_f), size=0.3)+
  geom_hline(data=all_metrics_se_random[var_f %in% target_metrics], 
             aes(yintercept=Rsquared_with_na, color=var_f), linewidth=0.5, linetype=2)+
  scale_color_manual(values=var_colors, 
                     breaks=target_metrics)+
  labs(x="Road (with 5km buffer) coverage (%)", 
       y=expression(R^2), fill="Biodiversity metrics", 
       color="Biodiversity metrics")+
  facet_wrap(~res_f, nrow=2, ncol=3)+
  theme_bw()+
  #facet_grid(sampling_proportion~res_f)+
  theme(strip.text.x = element_text(size = 20), 
        strip.text.y = element_text(size = 20),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 20))
#p
ggsave(p, filename="../Figures/IUCN_Based_VW/metrics_by_road_coverage_all.png", width=15, height=8)


all_metrics_se2<-all_metrics[, .(cor_with_na=mean(cor_with_na, na.rm=T), sd_cor_with_na=sd(cor_with_na, na.rm=T),
                                 cor_without_na=mean(cor_without_na, na.rm=T), sd_cor_without_na=sd(cor_without_na, na.rm=T),
                                 RMSE_with_na=mean(RMSE_with_na, na.rm=T), sd_RMSE_with_na=sd(RMSE_with_na, na.rm=T),
                                 Rsquared_with_na=mean(Rsquared_with_na, na.rm=T), sd_Rsquared_with_na=sd(Rsquared_with_na, na.rm=T),
                                 MAE_with_na=mean(MAE_with_na, na.rm=T), sd_MAE_with_na=sd(MAE_with_na, na.rm=T),
                                 RMSE_without_na=mean(RMSE_without_na, na.rm=T), sd_RMSE_without_na=sd(RMSE_without_na, na.rm=T),
                                 Rsquared_without_na=mean(Rsquared_without_na, na.rm=T), sd_Rsquared_without_na=sd(Rsquared_without_na, na.rm=T),
                                 MAE_without_na=mean(MAE_without_na, na.rm=T), sd_MAE_without_na=sd(MAE_without_na, na.rm=T)),
                             by=list(res_f, var_f, index, n_road_5km, sampling_proportion)]
all_metrics_se_road<-all_metrics_se2[!is.na(n_road_5km)]
all_metrics_se_random<-all_metrics_se2[is.na(n_road_5km)]
all_metrics_se_road_fig<-all_metrics_se_road[var_f %in% target_metrics]
var_colors<-colorBlindBlack8[2:7]
names(var_colors)<-target_metrics
all_metrics_se_road_fig_sampled<-all_metrics_se_road_fig[sample(nrow(all_metrics_se_road_fig), 1e4)]

p<-ggplot()+
  geom_smooth(data=all_metrics_se_road_fig, 
              aes(x=n_road_5km/16e2, y=Rsquared_with_na, color=var_f), method="loess")+
  geom_point(data=all_metrics_se_road_fig[sample(nrow(all_metrics_se_road_fig), 1e4)], 
             aes(x=n_road_5km/16e2, y=Rsquared_with_na, color=var_f), size=0.3)+
  geom_hline(data=all_metrics_se_random[var_f %in% target_metrics], 
             aes(yintercept=Rsquared_with_na, color=var_f), linewidth=0.5, linetype=2)+
  scale_color_manual(values=var_colors, 
                     breaks=target_metrics)+
  labs(x="Road (with 5km buffer) coverage (%)", 
       y=expression(R^2), fill="Biodiversity metrics", 
       color="Biodiversity metrics")+
  #facet_wrap(~res_f, nrow=2, ncol=3)+
  theme_bw()+
  facet_grid(sampling_proportion~res_f)+
  theme(strip.text.x = element_text(size = 20), 
        strip.text.y = element_text(size = 20),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 20))

ggsave(p, filename="../Figures/IUCN_Based_VW/metrics_by_road_coverage_sampling_proportion.png", width=15, height=8)


all_metrics_se2<-all_metrics[, .(cor_with_na=mean(cor_with_na, na.rm=T), sd_cor_with_na=sd(cor_with_na, na.rm=T),
                                 cor_without_na=mean(cor_without_na, na.rm=T), sd_cor_without_na=sd(cor_without_na, na.rm=T),
                                 RMSE_with_na=mean(RMSE_with_na, na.rm=T), sd_RMSE_with_na=sd(RMSE_with_na, na.rm=T),
                                 Rsquared_with_na=mean(Rsquared_with_na, na.rm=T), sd_Rsquared_with_na=sd(Rsquared_with_na, na.rm=T),
                                 MAE_with_na=mean(MAE_with_na, na.rm=T), sd_MAE_with_na=sd(MAE_with_na, na.rm=T),
                                 RMSE_without_na=mean(RMSE_without_na, na.rm=T), sd_RMSE_without_na=sd(RMSE_without_na, na.rm=T),
                                 Rsquared_without_na=mean(Rsquared_without_na, na.rm=T), sd_Rsquared_without_na=sd(Rsquared_without_na, na.rm=T),
                                 MAE_without_na=mean(MAE_without_na, na.rm=T), sd_MAE_without_na=sd(MAE_without_na, na.rm=T)),
                             by=list(res_f, var_f, index, n_road_5km, grid_index)]
all_metrics_se2$grid_label<-letters[all_metrics_se2$grid_index]
all_metrics_se_road<-all_metrics_se2[!is.na(n_road_5km)]
all_metrics_se_random<-all_metrics_se2[is.na(n_road_5km)]
all_metrics_se_road_fig<-all_metrics_se_road[var_f %in% target_metrics]

var_colors<-colorBlindBlack8[2:7]
names(var_colors)<-target_metrics
all_metrics_se_road_fig_sampled<-all_metrics_se_road_fig[sample(nrow(all_metrics_se_road_fig), 1e4)]


p<-ggplot()+
  geom_smooth(data=all_metrics_se_road_fig, 
              aes(x=n_road_5km/16e2, y=Rsquared_with_na, color=var_f), method="loess")+
  geom_point(data=all_metrics_se_road_fig[sample(nrow(all_metrics_se_road_fig), 1e4)], 
             aes(x=n_road_5km/16e2, y=Rsquared_with_na, color=var_f), size=0.3)+
  geom_hline(data=all_metrics_se_random[var_f %in% target_metrics], 
             aes(yintercept=Rsquared_with_na, color=var_f), linewidth=0.5, linetype=2)+
  scale_color_manual(values=var_colors, 
                     breaks=target_metrics)+
  labs(x="Road (with 5km buffer) coverage (%)", 
       y=expression(R^2), fill="Biodiversity metrics", 
       color="Biodiversity metrics")+
  #facet_wrap(~res_f, nrow=2, ncol=3)+
  theme_bw()+
  facet_grid(grid_label~res_f)+
  theme(strip.text.x = element_text(size = 20), 
        strip.text.y = element_text(size = 20),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 20))

ggsave(p, filename="../Figures/IUCN_Based_VW/metrics_by_road_coverage_grid_index.png", width=15, height=20)

all_metrics_se2<-all_metrics[, .(cor_with_na=mean(cor_with_na, na.rm=T), sd_cor_with_na=sd(cor_with_na, na.rm=T),
                                 cor_without_na=mean(cor_without_na, na.rm=T), sd_cor_without_na=sd(cor_without_na, na.rm=T),
                                 RMSE_with_na=mean(RMSE_with_na, na.rm=T), sd_RMSE_with_na=sd(RMSE_with_na, na.rm=T),
                                 Rsquared_with_na=mean(Rsquared_with_na, na.rm=T), sd_Rsquared_with_na=sd(Rsquared_with_na, na.rm=T),
                                 MAE_with_na=mean(MAE_with_na, na.rm=T), sd_MAE_with_na=sd(MAE_with_na, na.rm=T),
                                 RMSE_without_na=mean(RMSE_without_na, na.rm=T), sd_RMSE_without_na=sd(RMSE_without_na, na.rm=T),
                                 Rsquared_without_na=mean(Rsquared_without_na, na.rm=T), sd_Rsquared_without_na=sd(Rsquared_without_na, na.rm=T),
                                 MAE_without_na=mean(MAE_without_na, na.rm=T), sd_MAE_without_na=sd(MAE_without_na, na.rm=T)),
                             by=list(res_f, var_f, index, n_road_5km, grid_index, sampling_proportion)]
all_metrics_se2$grid_label<-letters[all_metrics_se2$grid_index]
#all_metrics_se2<-all_metrics_se2[!is.na(res_f)]
all_metrics_se_road<-all_metrics_se2[!is.na(n_road_5km)]
all_metrics_se_random<-all_metrics_se2[is.na(n_road_5km)]
all_metrics_se_road_fig<-all_metrics_se_road[var_f %in% target_metrics]
var_colors<-colorBlindBlack8[2:7]
names(var_colors)<-target_metrics
sp=100
for (sp in c(100, 500, 1000)){
  print(sp)
  item1<-all_metrics_se_road_fig[sampling_proportion==sp]
  item2<-all_metrics_se_random[sampling_proportion==sp]
  p<-ggplot()+
    geom_smooth(data=item1, 
                aes(x=n_road_5km/16e2, y=Rsquared_with_na, color=var_f), method="loess")+
    geom_point(data=item1[sample(nrow(item1), 1e1)], 
               aes(x=n_road_5km/16e2, y=Rsquared_with_na, color=var_f), size=0.3)+
    geom_hline(data=item2[var_f %in% target_metrics], 
               aes(yintercept=Rsquared_with_na, color=var_f), size=0.5, linetype=2)+
    scale_color_manual(values=var_colors, 
                       breaks=target_metrics)+
    ggtitle(sprintf("Number of sampling events: %d", sp))+
    labs(x="Road (with 5km buffer) coverage (%)", y=expression(R^2), color="Biodiversity metrics")+
    facet_grid(grid_label~res_f)+
    theme_bw()+
    theme(strip.text.x = element_text(size = 20), 
          strip.text.y = element_text(size = 20),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 20))
  
  #p
  ggsave(p, filename=sprintf("../Figures/IUCN_Based_VW/metrics_by_road_coverage_grided_sampling_%d.png", sp), 
         width=15, height=20)
}


###not accounting the unsampled cells

all_metrics_se<-all_metrics[!is.na(n_ndvi), .(cor_with_na=mean(cor_with_na, na.rm=T), sd_cor_with_na=sd(cor_with_na, na.rm=T),
                                              cor_without_na=mean(cor_without_na, na.rm=T), sd_cor_without_na=sd(cor_without_na, na.rm=T),
                                              RMSE_with_na=mean(RMSE_with_na, na.rm=T), sd_RMSE_with_na=sd(RMSE_with_na, na.rm=T),
                                              Rsquared_with_na=mean(Rsquared_with_na, na.rm=T), sd_Rsquared_with_na=sd(Rsquared_with_na, na.rm=T),
                                              MAE_with_na=mean(MAE_with_na, na.rm=T), sd_MAE_with_na=sd(MAE_with_na, na.rm=T),
                                              RMSE_without_na=mean(RMSE_without_na, na.rm=T), sd_RMSE_without_na=sd(RMSE_without_na, na.rm=T),
                                              Rsquared_without_na=mean(Rsquared_without_na, na.rm=T), sd_Rsquared_without_na=sd(Rsquared_without_na, na.rm=T),
                                              MAE_without_na=mean(MAE_without_na, na.rm=T), sd_MAE_without_na=sd(MAE_without_na, na.rm=T)),
                            by=list(res_f, var_f)]
all_metrics_se_random<-all_metrics[is.na(n_ndvi), .(cor_with_na=mean(cor_with_na, na.rm=T), sd_cor_with_na=sd(cor_with_na, na.rm=T),
                                                    cor_without_na=mean(cor_without_na, na.rm=T), sd_cor_without_na=sd(cor_without_na, na.rm=T),
                                                    RMSE_with_na=mean(RMSE_with_na, na.rm=T), sd_RMSE_with_na=sd(RMSE_with_na, na.rm=T),
                                                    Rsquared_with_na=mean(Rsquared_with_na, na.rm=T), sd_Rsquared_with_na=sd(Rsquared_with_na, na.rm=T),
                                                    MAE_with_na=mean(MAE_with_na, na.rm=T), sd_MAE_with_na=sd(MAE_with_na, na.rm=T),
                                                    RMSE_without_na=mean(RMSE_without_na, na.rm=T), sd_RMSE_without_na=sd(RMSE_without_na, na.rm=T),
                                                    Rsquared_without_na=mean(Rsquared_without_na, na.rm=T), sd_Rsquared_without_na=sd(Rsquared_without_na, na.rm=T),
                                                    MAE_without_na=mean(MAE_without_na, na.rm=T), sd_MAE_without_na=sd(MAE_without_na, na.rm=T)),
                                   by=list(res_f, var_f)]
all_metrics_se<-all_metrics_se[res_f!="100km"]

colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
res_colors<-c("1km"=colorBlindBlack8[2], "2km"=colorBlindBlack8[3], 
              "5km"=colorBlindBlack8[4], "10km"=colorBlindBlack8[5],
              "20km"=colorBlindBlack8[6], "50km"=colorBlindBlack8[7],
              "100km"=colorBlindBlack8[8])

p<-ggplot(all_metrics_se)+
  geom_errorbar(aes(x=var_f, 
                    ymin=Rsquared_without_na-sd_Rsquared_without_na, 
                    ymax=Rsquared_without_na+sd_Rsquared_without_na, color=res_f),
                position=position_dodge(width = 0.9), width=0.2)+
  geom_point(aes(x=var_f, y=Rsquared_without_na, color=res_f), 
             position=position_dodge(width = 0.9))+
  geom_point(data=all_metrics_se_random, 
             aes(x=var_f, y=Rsquared_without_na, color=res_f), 
             position=position_dodge(width = 0.9), shape=6)+
  labs(x="Biodiversity metrics", y=expression(R^2), fill="Resolution", color="Resolution")+
  scale_color_manual(values=res_colors, 
                     breaks=c("1km", "2km", "5km", "10km", "20km", "50km"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
p
ggsave(p, filename="../Figures/IUCN_Based_VW/metrics_full_unsampled.png", width=10, height=4)



all_metrics_se<-all_metrics[!is.na(n_ndvi), .(cor_with_na=mean(cor_with_na, na.rm=T), sd_cor_with_na=sd(cor_with_na, na.rm=T),
                                              cor_without_na=mean(cor_without_na, na.rm=T), sd_cor_without_na=sd(cor_without_na, na.rm=T),
                                              RMSE_with_na=mean(RMSE_with_na, na.rm=T), sd_RMSE_with_na=sd(RMSE_with_na, na.rm=T),
                                              Rsquared_with_na=mean(Rsquared_with_na, na.rm=T), sd_Rsquared_with_na=sd(Rsquared_with_na, na.rm=T),
                                              MAE_with_na=mean(MAE_with_na, na.rm=T), sd_MAE_with_na=sd(MAE_with_na, na.rm=T),
                                              RMSE_without_na=mean(RMSE_without_na, na.rm=T), sd_RMSE_without_na=sd(RMSE_without_na, na.rm=T),
                                              Rsquared_without_na=mean(Rsquared_without_na, na.rm=T), sd_Rsquared_without_na=sd(Rsquared_without_na, na.rm=T),
                                              MAE_without_na=mean(MAE_without_na, na.rm=T), sd_MAE_without_na=sd(MAE_without_na, na.rm=T)),
                            by=list(res_f, var_f, sampling_proportion)]
all_metrics_se_random<-all_metrics[is.na(n_ndvi), .(cor_with_na=mean(cor_with_na, na.rm=T), sd_cor_with_na=sd(cor_with_na, na.rm=T),
                                                    cor_without_na=mean(cor_without_na, na.rm=T), sd_cor_without_na=sd(cor_without_na, na.rm=T),
                                                    RMSE_with_na=mean(RMSE_with_na, na.rm=T), sd_RMSE_with_na=sd(RMSE_with_na, na.rm=T),
                                                    Rsquared_with_na=mean(Rsquared_with_na, na.rm=T), sd_Rsquared_with_na=sd(Rsquared_with_na, na.rm=T),
                                                    MAE_with_na=mean(MAE_with_na, na.rm=T), sd_MAE_with_na=sd(MAE_with_na, na.rm=T),
                                                    RMSE_without_na=mean(RMSE_without_na, na.rm=T), sd_RMSE_without_na=sd(RMSE_without_na, na.rm=T),
                                                    Rsquared_without_na=mean(Rsquared_without_na, na.rm=T), sd_Rsquared_without_na=sd(Rsquared_without_na, na.rm=T),
                                                    MAE_without_na=mean(MAE_without_na, na.rm=T), sd_MAE_without_na=sd(MAE_without_na, na.rm=T)),
                                   by=list(res_f, var_f, sampling_proportion)]
all_metrics_se<-all_metrics_se[res_f!="100km"]

colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
res_colors<-c("1km"=colorBlindBlack8[2], "2km"=colorBlindBlack8[3], 
              "5km"=colorBlindBlack8[4], "10km"=colorBlindBlack8[5],
              "20km"=colorBlindBlack8[6], "50km"=colorBlindBlack8[7],
              "100km"=colorBlindBlack8[8])

p<-ggplot(all_metrics_se)+
  geom_errorbar(aes(x=var_f, 
                    ymin=Rsquared_without_na-sd_Rsquared_without_na, 
                    ymax=Rsquared_without_na+sd_Rsquared_without_na, color=res_f),
                position=position_dodge(width = 0.9), width=0.2)+
  geom_point(aes(x=var_f, y=Rsquared_without_na, color=res_f), 
             position=position_dodge(width = 0.9))+
  geom_point(data=all_metrics_se_random, 
             aes(x=var_f, y=Rsquared_without_na, color=res_f), 
             position=position_dodge(width = 0.9), shape=6)+
  labs(x="Biodiversity metrics", y=expression(R^2), fill="Resolution", color="Resolution")+
  scale_color_manual(values=res_colors, 
                     breaks=c("1km", "2km", "5km", "10km", "20km", "50km"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  facet_wrap(~sampling_proportion, nrow=3, ncol=1, strip.position="right")
p
ggsave(p, filename="../Figures/IUCN_Based_VW/metrics_sampling_proportion_unsampled.png", width=10, height=6)


all_metrics_se<-all_metrics[!is.na(n_ndvi), .(cor_with_na=mean(cor_with_na, na.rm=T), sd_cor_with_na=sd(cor_with_na, na.rm=T),
                                              cor_without_na=mean(cor_without_na, na.rm=T), sd_cor_without_na=sd(cor_without_na, na.rm=T),
                                              RMSE_with_na=mean(RMSE_with_na, na.rm=T), sd_RMSE_with_na=sd(RMSE_with_na, na.rm=T),
                                              Rsquared_with_na=mean(Rsquared_with_na, na.rm=T), sd_Rsquared_with_na=sd(Rsquared_with_na, na.rm=T),
                                              MAE_with_na=mean(MAE_with_na, na.rm=T), sd_MAE_with_na=sd(MAE_with_na, na.rm=T),
                                              RMSE_without_na=mean(RMSE_without_na, na.rm=T), sd_RMSE_without_na=sd(RMSE_without_na, na.rm=T),
                                              Rsquared_without_na=mean(Rsquared_without_na, na.rm=T), sd_Rsquared_without_na=sd(Rsquared_without_na, na.rm=T),
                                              MAE_without_na=mean(MAE_without_na, na.rm=T), sd_MAE_without_na=sd(MAE_without_na, na.rm=T)),
                            by=list(res_f, var_f, grid_index)]
all_metrics_se_random<-all_metrics[is.na(n_ndvi), .(cor_with_na=mean(cor_with_na, na.rm=T), sd_cor_with_na=sd(cor_with_na, na.rm=T),
                                                    cor_without_na=mean(cor_without_na, na.rm=T), sd_cor_without_na=sd(cor_without_na, na.rm=T),
                                                    RMSE_with_na=mean(RMSE_with_na, na.rm=T), sd_RMSE_with_na=sd(RMSE_with_na, na.rm=T),
                                                    Rsquared_with_na=mean(Rsquared_with_na, na.rm=T), sd_Rsquared_with_na=sd(Rsquared_with_na, na.rm=T),
                                                    MAE_with_na=mean(MAE_with_na, na.rm=T), sd_MAE_with_na=sd(MAE_with_na, na.rm=T),
                                                    RMSE_without_na=mean(RMSE_without_na, na.rm=T), sd_RMSE_without_na=sd(RMSE_without_na, na.rm=T),
                                                    Rsquared_without_na=mean(Rsquared_without_na, na.rm=T), sd_Rsquared_without_na=sd(Rsquared_without_na, na.rm=T),
                                                    MAE_without_na=mean(MAE_without_na, na.rm=T), sd_MAE_without_na=sd(MAE_without_na, na.rm=T)),
                                   by=list(res_f, var_f, grid_index)]
all_metrics_se<-all_metrics_se[res_f!="100km"]
all_metrics_se$grid_label<-letters[all_metrics_se$grid_index]
all_metrics_se_random$grid_label<-letters[all_metrics_se_random$grid_index]
all_metrics_se_random[grid_label=="a" & var_f=="ES10" & res_f=="1km"]

colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
res_colors<-c("1km"=colorBlindBlack8[2], "2km"=colorBlindBlack8[3], 
              "5km"=colorBlindBlack8[4], "10km"=colorBlindBlack8[5],
              "20km"=colorBlindBlack8[6], "50km"=colorBlindBlack8[7],
              "100km"=colorBlindBlack8[8])
target_metrics<-c("ES10", 
                  "Hill_Inf", 
                  "Shannon", "Simpson", 
                  "Pielou's evenness", "Species richness")
all_metrics_se$var_f<-as.character(all_metrics_se$var_f)
p<-ggplot(all_metrics_se[var_f %in% target_metrics])+
  geom_errorbar(aes(x=var_f, 
                    ymin=Rsquared_without_na-sd_Rsquared_without_na, 
                    ymax=Rsquared_without_na+sd_Rsquared_without_na, color=res_f),
                position=position_dodge(width = 0.9), width=0.2)+
  geom_point(aes(x=var_f, y=Rsquared_without_na, color=res_f), 
             position=position_dodge(width = 0.9))+
  geom_point(data=all_metrics_se_random[var_f %in% target_metrics], 
             aes(x=var_f, y=Rsquared_without_na, color=res_f), 
             position=position_dodge(width = 0.9), shape=6)+
  labs(x="Biodiversity metrics", y=expression(R^2), fill="Resolution", color="Resolution")+
  scale_color_manual(values=res_colors, 
                     breaks=c("1km", "2km", "5km", "10km", "20km", "50km"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  facet_wrap(~grid_label, nrow=2, ncol=5, strip.position="top")
p
ggsave(p, filename="../Figures/IUCN_Based_VW/metrics_grid_index_unsampled.png", width=10, height=6)


all_metrics_se2<-all_metrics[!is.na(n_ndvi), .(cor_with_na=mean(cor_with_na, na.rm=T), sd_cor_with_na=sd(cor_with_na, na.rm=T),
                                               cor_without_na=mean(cor_without_na, na.rm=T), sd_cor_without_na=sd(cor_without_na, na.rm=T),
                                               RMSE_with_na=mean(RMSE_with_na, na.rm=T), sd_RMSE_with_na=sd(RMSE_with_na, na.rm=T),
                                               Rsquared_with_na=mean(Rsquared_with_na, na.rm=T), sd_Rsquared_with_na=sd(Rsquared_with_na, na.rm=T),
                                               MAE_with_na=mean(MAE_with_na, na.rm=T), sd_MAE_with_na=sd(MAE_with_na, na.rm=T),
                                               RMSE_without_na=mean(RMSE_without_na, na.rm=T), sd_RMSE_without_na=sd(RMSE_without_na, na.rm=T),
                                               Rsquared_without_na=mean(Rsquared_without_na, na.rm=T), sd_Rsquared_without_na=sd(Rsquared_without_na, na.rm=T),
                                               MAE_without_na=mean(MAE_without_na, na.rm=T), sd_MAE_without_na=sd(MAE_without_na, na.rm=T)),
                             by=list(res_f, var_f, grid_index, sampling_proportion)]

all_metrics_se_random<-all_metrics[is.na(n_ndvi), .(cor_with_na=mean(cor_with_na, na.rm=T), sd_cor_with_na=sd(cor_with_na, na.rm=T),
                                                    cor_without_na=mean(cor_without_na, na.rm=T), sd_cor_without_na=sd(cor_without_na, na.rm=T),
                                                    RMSE_with_na=mean(RMSE_with_na, na.rm=T), sd_RMSE_with_na=sd(RMSE_with_na, na.rm=T),
                                                    Rsquared_with_na=mean(Rsquared_with_na, na.rm=T), sd_Rsquared_with_na=sd(Rsquared_with_na, na.rm=T),
                                                    MAE_with_na=mean(MAE_with_na, na.rm=T), sd_MAE_with_na=sd(MAE_with_na, na.rm=T),
                                                    RMSE_without_na=mean(RMSE_without_na, na.rm=T), sd_RMSE_without_na=sd(RMSE_without_na, na.rm=T),
                                                    Rsquared_without_na=mean(Rsquared_without_na, na.rm=T), sd_Rsquared_without_na=sd(Rsquared_without_na, na.rm=T),
                                                    MAE_without_na=mean(MAE_without_na, na.rm=T), sd_MAE_without_na=sd(MAE_without_na, na.rm=T)),
                                   by=list(res_f, var_f, grid_index, sampling_proportion)]
target_metrics<-c("ES10", 
                  "Hill_Inf", 
                  "Shannon", "Simpson", 
                  "Pielou's evenness", "Species richness")
all_metrics_se2$grid_label<-letters[all_metrics_se2$grid_index]
p<-ggplot(all_metrics_se2[var_f %in% target_metrics])+
  geom_errorbar(aes(x=var_f, 
                    ymin=Rsquared_without_na-sd_Rsquared_without_na, 
                    ymax=Rsquared_without_na+sd_Rsquared_without_na, color=res_f),
                position=position_dodge(width = 0.9), width=0.2)+
  geom_point(aes(x=var_f, y=Rsquared_without_na, color=res_f), 
             position=position_dodge(width = 0.9))+
  geom_point(data=all_metrics_se_random[var_f %in% target_metrics], 
             aes(x=var_f, y=Rsquared_without_na, color=res_f), 
             position=position_dodge(width = 0.9), shape=6)+
  scale_color_manual(values=res_colors, 
                     breaks=c("1km", "2km", "5km", "10km", "20km", "50km"))+
  labs(x="Biodiversity metrics", y=expression(R^2), fill="Resolution", color="Resolution")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  facet_grid(sampling_proportion~grid_label)+
  theme(strip.text.x = element_text(size = 20), 
        strip.text.y = element_text(size = 20),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 20))
p
ggsave(p, filename="../Figures/IUCN_Based_VW/metrics_grided_unsampled.png", width=30, height=15)

all_metrics_se2<-all_metrics[, .(cor_with_na=mean(cor_with_na, na.rm=T), sd_cor_with_na=sd(cor_with_na, na.rm=T),
                                 cor_without_na=mean(cor_without_na, na.rm=T), sd_cor_without_na=sd(cor_without_na, na.rm=T),
                                 RMSE_with_na=mean(RMSE_with_na, na.rm=T), sd_RMSE_with_na=sd(RMSE_with_na, na.rm=T),
                                 Rsquared_with_na=mean(Rsquared_with_na, na.rm=T), sd_Rsquared_with_na=sd(Rsquared_with_na, na.rm=T),
                                 MAE_with_na=mean(MAE_with_na, na.rm=T), sd_MAE_with_na=sd(MAE_with_na, na.rm=T),
                                 RMSE_without_na=mean(RMSE_without_na, na.rm=T), sd_RMSE_without_na=sd(RMSE_without_na, na.rm=T),
                                 Rsquared_without_na=mean(Rsquared_without_na, na.rm=T), sd_Rsquared_without_na=sd(Rsquared_without_na, na.rm=T),
                                 MAE_without_na=mean(MAE_without_na, na.rm=T), sd_MAE_without_na=sd(MAE_without_na, na.rm=T)),
                             by=list(res_f, var_f, index, n_road_5km)]
all_metrics_se_road<-all_metrics_se2[!is.na(n_road_5km)]
all_metrics_se_random<-all_metrics_se2[is.na(n_road_5km)]
all_metrics_se_road_fig<-all_metrics_se_road[var_f %in% target_metrics]
var_colors<-colorBlindBlack8[2:7]
names(var_colors)<-target_metrics
all_metrics_se_road_fig_sampled<-all_metrics_se_road_fig[sample(nrow(all_metrics_se_road_fig), 1e4)]
p<-ggplot()+
  geom_smooth(data=all_metrics_se_road_fig, 
              aes(x=n_road_5km/16e2, y=Rsquared_without_na, color=var_f), method="loess")+
  geom_point(data=all_metrics_se_road_fig_sampled, 
             aes(x=n_road_5km/16e2, y=Rsquared_without_na, color=var_f), size=0.3)+
  geom_hline(data=all_metrics_se_random[var_f %in% target_metrics], 
             aes(yintercept=Rsquared_without_na, color=var_f), linewidth=0.5, linetype=2)+
  scale_color_manual(values=var_colors, 
                     breaks=target_metrics)+
  labs(x="Road (with 5km buffer) coverage (%)", 
       y=expression(R^2), fill="Biodiversity metrics", 
       color="Biodiversity metrics")+
  facet_wrap(~res_f, nrow=2, ncol=3)+
  theme_bw()+
  #facet_grid(sampling_proportion~res_f)+
  theme(strip.text.x = element_text(size = 20), 
        strip.text.y = element_text(size = 20),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 20))
#p
ggsave(p, filename="../Figures/IUCN_Based_VW/metrics_by_road_coverage_all_unsampled.png", width=15, height=8)


all_metrics_se2<-all_metrics[, .(cor_with_na=mean(cor_with_na, na.rm=T), sd_cor_with_na=sd(cor_with_na, na.rm=T),
                                 cor_without_na=mean(cor_without_na, na.rm=T), sd_cor_without_na=sd(cor_without_na, na.rm=T),
                                 RMSE_with_na=mean(RMSE_with_na, na.rm=T), sd_RMSE_with_na=sd(RMSE_with_na, na.rm=T),
                                 Rsquared_with_na=mean(Rsquared_with_na, na.rm=T), sd_Rsquared_with_na=sd(Rsquared_with_na, na.rm=T),
                                 MAE_with_na=mean(MAE_with_na, na.rm=T), sd_MAE_with_na=sd(MAE_with_na, na.rm=T),
                                 RMSE_without_na=mean(RMSE_without_na, na.rm=T), sd_RMSE_without_na=sd(RMSE_without_na, na.rm=T),
                                 Rsquared_without_na=mean(Rsquared_without_na, na.rm=T), sd_Rsquared_without_na=sd(Rsquared_without_na, na.rm=T),
                                 MAE_without_na=mean(MAE_without_na, na.rm=T), sd_MAE_without_na=sd(MAE_without_na, na.rm=T)),
                             by=list(res_f, var_f, index, n_road_5km, sampling_proportion)]
all_metrics_se_road<-all_metrics_se2[!is.na(n_road_5km)]
all_metrics_se_random<-all_metrics_se2[is.na(n_road_5km)]
all_metrics_se_road_fig<-all_metrics_se_road[var_f %in% target_metrics]
var_colors<-colorBlindBlack8[2:7]
names(var_colors)<-target_metrics
all_metrics_se_road_fig_sampled<-all_metrics_se_road_fig[sample(nrow(all_metrics_se_road_fig), 1e4)]

p<-ggplot()+
  geom_smooth(data=all_metrics_se_road_fig, 
              aes(x=n_road_5km/16e2, y=Rsquared_without_na, color=var_f), method="loess")+
  geom_point(data=all_metrics_se_road_fig[sample(nrow(all_metrics_se_road_fig), 1e4)], 
             aes(x=n_road_5km/16e2, y=Rsquared_without_na, color=var_f), size=0.3)+
  geom_hline(data=all_metrics_se_random[var_f %in% target_metrics], 
             aes(yintercept=Rsquared_without_na, color=var_f), linewidth=0.5, linetype=2)+
  scale_color_manual(values=var_colors, 
                     breaks=target_metrics)+
  labs(x="Road (with 5km buffer) coverage (%)", 
       y=expression(R^2), fill="Biodiversity metrics", 
       color="Biodiversity metrics")+
  #facet_wrap(~res_f, nrow=2, ncol=3)+
  theme_bw()+
  facet_grid(sampling_proportion~res_f)+
  theme(strip.text.x = element_text(size = 20), 
        strip.text.y = element_text(size = 20),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 20))

ggsave(p, filename="../Figures/IUCN_Based_VW/metrics_by_road_coverage_sampling_proportion_unsampled.png", width=15, height=8)


all_metrics_se2<-all_metrics[, .(cor_with_na=mean(cor_with_na, na.rm=T), sd_cor_with_na=sd(cor_with_na, na.rm=T),
                                 cor_without_na=mean(cor_without_na, na.rm=T), sd_cor_without_na=sd(cor_without_na, na.rm=T),
                                 RMSE_with_na=mean(RMSE_with_na, na.rm=T), sd_RMSE_with_na=sd(RMSE_with_na, na.rm=T),
                                 Rsquared_with_na=mean(Rsquared_with_na, na.rm=T), sd_Rsquared_with_na=sd(Rsquared_with_na, na.rm=T),
                                 MAE_with_na=mean(MAE_with_na, na.rm=T), sd_MAE_with_na=sd(MAE_with_na, na.rm=T),
                                 RMSE_without_na=mean(RMSE_without_na, na.rm=T), sd_RMSE_without_na=sd(RMSE_without_na, na.rm=T),
                                 Rsquared_without_na=mean(Rsquared_without_na, na.rm=T), sd_Rsquared_without_na=sd(Rsquared_without_na, na.rm=T),
                                 MAE_without_na=mean(MAE_without_na, na.rm=T), sd_MAE_without_na=sd(MAE_without_na, na.rm=T)),
                             by=list(res_f, var_f, index, n_road_5km, grid_index)]
all_metrics_se2$grid_label<-letters[all_metrics_se2$grid_index]
all_metrics_se_road<-all_metrics_se2[!is.na(n_road_5km)]
all_metrics_se_random<-all_metrics_se2[is.na(n_road_5km)]
all_metrics_se_road_fig<-all_metrics_se_road[var_f %in% target_metrics]

var_colors<-colorBlindBlack8[2:7]
names(var_colors)<-target_metrics
all_metrics_se_road_fig_sampled<-all_metrics_se_road_fig[sample(nrow(all_metrics_se_road_fig), 1e4)]


p<-ggplot()+
  geom_smooth(data=all_metrics_se_road_fig, 
              aes(x=n_road_5km/16e2, y=Rsquared_without_na, color=var_f), method="loess")+
  geom_point(data=all_metrics_se_road_fig[sample(nrow(all_metrics_se_road_fig), 1e4)], 
             aes(x=n_road_5km/16e2, y=Rsquared_without_na, color=var_f), size=0.3)+
  geom_hline(data=all_metrics_se_random[var_f %in% target_metrics], 
             aes(yintercept=Rsquared_without_na, color=var_f), linewidth=0.5, linetype=2)+
  scale_color_manual(values=var_colors, 
                     breaks=target_metrics)+
  labs(x="Road (with 5km buffer) coverage (%)", 
       y=expression(R^2), fill="Biodiversity metrics", 
       color="Biodiversity metrics")+
  #facet_wrap(~res_f, nrow=2, ncol=3)+
  theme_bw()+
  facet_grid(grid_label~res_f)+
  theme(strip.text.x = element_text(size = 20), 
        strip.text.y = element_text(size = 20),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 20))

ggsave(p, filename="../Figures/IUCN_Based_VW/metrics_by_road_coverage_grid_index_unsampled.png", width=15, height=20)

all_metrics_se2<-all_metrics[, .(cor_with_na=mean(cor_with_na, na.rm=T), sd_cor_with_na=sd(cor_with_na, na.rm=T),
                                 cor_without_na=mean(cor_without_na, na.rm=T), sd_cor_without_na=sd(cor_without_na, na.rm=T),
                                 RMSE_with_na=mean(RMSE_with_na, na.rm=T), sd_RMSE_with_na=sd(RMSE_with_na, na.rm=T),
                                 Rsquared_with_na=mean(Rsquared_with_na, na.rm=T), sd_Rsquared_with_na=sd(Rsquared_with_na, na.rm=T),
                                 MAE_with_na=mean(MAE_with_na, na.rm=T), sd_MAE_with_na=sd(MAE_with_na, na.rm=T),
                                 RMSE_without_na=mean(RMSE_without_na, na.rm=T), sd_RMSE_without_na=sd(RMSE_without_na, na.rm=T),
                                 Rsquared_without_na=mean(Rsquared_without_na, na.rm=T), sd_Rsquared_without_na=sd(Rsquared_without_na, na.rm=T),
                                 MAE_without_na=mean(MAE_without_na, na.rm=T), sd_MAE_without_na=sd(MAE_without_na, na.rm=T)),
                             by=list(res_f, var_f, index, n_road_5km, grid_index, sampling_proportion)]
all_metrics_se2$grid_label<-letters[all_metrics_se2$grid_index]
#all_metrics_se2<-all_metrics_se2[!is.na(res_f)]
all_metrics_se_road<-all_metrics_se2[!is.na(n_road_5km)]
all_metrics_se_random<-all_metrics_se2[is.na(n_road_5km)]
all_metrics_se_road_fig<-all_metrics_se_road[var_f %in% target_metrics]
var_colors<-colorBlindBlack8[2:7]
names(var_colors)<-target_metrics
sp=100
for (sp in c(100, 500, 1000)){
  print(sp)
  item1<-all_metrics_se_road_fig[sampling_proportion==sp]
  item2<-all_metrics_se_random[sampling_proportion==sp]
  p<-ggplot()+
    geom_smooth(data=item1, 
                aes(x=n_road_5km/16e2, y=Rsquared_without_na, color=var_f), method="loess")+
    geom_point(data=item1[sample(nrow(item1), 1e1)], 
               aes(x=n_road_5km/16e2, y=Rsquared_without_na, color=var_f), size=0.3)+
    geom_hline(data=item2[var_f %in% target_metrics], 
               aes(yintercept=Rsquared_without_na, color=var_f), size=0.5, linetype=2)+
    scale_color_manual(values=var_colors, 
                       breaks=target_metrics)+
    ggtitle(sprintf("Number of sampling events: %d", sp))+
    labs(x="Road (with 5km buffer) coverage (%)", y=expression(R^2), color="Biodiversity metrics")+
    facet_grid(grid_label~res_f)+
    theme_bw()+
    theme(strip.text.x = element_text(size = 20), 
          strip.text.y = element_text(size = 20),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 20))
  
  #p
  ggsave(p, filename=sprintf("../Figures/IUCN_Based_VW/metrics_by_road_coverage_grided_sampling_%d_unsampled.png", sp), 
         width=15, height=20)
}
