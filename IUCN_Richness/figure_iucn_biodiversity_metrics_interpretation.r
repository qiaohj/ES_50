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
library(entropart)
library(ggpubr)
library(caret)
library(raster)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")

picked_grids<-readRDS("../Objects/virtual_lands/picked_grids.rda")
grid_index<-4
base<-sprintf("../Objects/GRIDS_S/%s", picked_grids[grid_index,]$index)

virtual_species<-readRDS(sprintf("../Objects/GRIDS_S/species_points_%d.rda", picked_grids[grid_index,]$index))
virtual_species<-virtual_species[iucn_category %in% c("Critically Endangered", "Endangered",
                                                      "Least Concern", "Near Threatened", "Vulnerable")]
#species_points<-st_as_sf(virtual_species, coords = c("x", "y"))
seeds<-readRDS("../Objects/virtual_lands/centers.rda")
#coms<-expand.grid(mask=c("100km", "50km", "20km", "10km", "5km", "2km", "1km"),
#                  es=c(200, 100, 50, 20, 10), stringsAsFactors = F)

#seeds<-seeds[sample(nrow(seeds), nrow(seeds))]
#100 LC, 40 NT, 20 Vu, 10 En, CR
sample_probability_Least_Concern<-1
sample_probability_Near_Threatened<-0.4
sample_probability_Vulnerable<-0.2
sample_probability_Endangered<-0.1
sample_probability_Critically_Endangered<-0.1
probabilities<-list("Least Concern"=sample_probability_Least_Concern,
                    "Vulnerable"=sample_probability_Vulnerable,
                    "Near Threatened"=sample_probability_Near_Threatened,
                    "Endangered"=sample_probability_Endangered,
                    "Critically Endangered"=sample_probability_Critically_Endangered)
if (F){
  source("colors.r")
  mask<-raster("../Objects/masks/mask_10km.tif")
  mask_p<-data.table(rasterToPoints(mask))
  
  folder_full<-sprintf("%s/full", base)
  biodiv_full<-readRDS(sprintf("%s/biodiversity.rda", folder_full))
  es_full<-readRDS(sprintf("%s/es.rda", folder_full))
  biodiv_full_10km<-biodiv_full[res=="10km"]
  biodiv_full_10km<-merge(biodiv_full_10km, mask_p, by.x="mask", by.y="mask_10km")
  es10_full_10km<-es_full[res=="10km"&esNum==10]
  es10_full_10km$mask<-as.numeric(es10_full_10km$cell)
  biodiv_full_10km<-merge(biodiv_full_10km, es10_full_10km, by="mask", all=T)
  var<-"es"
  
  plist<-list()
  biodiv_full_10km$type<-"RAW"
  biodiv_full_10km$sample_size<-0
  plist[[length(plist)+1]]<-biodiv_full_10km
  if (F){
    p_full<-ggplot(biodiv_full_10km)+
      geom_tile(aes_string(x="x", y="y", fill=var))+
      geom_text(aes_string(x="x", y="y", label="label"), size=5)+
      #ggtitle(title)+
      coord_equal()+
      scale_fill_gradient(low=colors_blue[6], high=colors_red[6])+
      map_theme
    p_full
  }
  
  for (sampling_proportion in c(1, 0.5, 0.1)){
    i<-349
    folder<-sprintf("%s/%d", base, i)
    if (sampling_proportion==1){
      biodiv<-readRDS(sprintf("%s/biodiversity.rda", folder))
      es<-readRDS(sprintf("%s/es.rda", folder))
      biodiv_random<-readRDS(sprintf("%s/biodiversity.rda", sprintf("%s/random_sampling", base)))
      es_random<-readRDS(sprintf("%s/es.rda", sprintf("%s/random_sampling", base)))
    }else{
      biodiv<-readRDS(sprintf("%s/biodiversity_sampling_%.1f.rda", folder, sampling_proportion))
      es<-readRDS(sprintf("%s/es_%.1f.rda", folder, sampling_proportion))
      biodiv_random<-readRDS(sprintf("%s/biodiversity_%.1f.rda", sprintf("%s/random_sampling", base), sampling_proportion))
      es_random<-readRDS(sprintf("%s/es_%.1f.rda", sprintf("%s/random_sampling", base), sampling_proportion))
      
    }
    biodiv_10km<-biodiv[res=="10km"]
    biodiv_10km<-merge(biodiv_10km, mask_p, by.x="mask", by.y="mask_10km")
    es10_10km<-es[res=="10km"&esNum==10]
    es10_10km$mask<-as.numeric(es10_10km$cell)
    biodiv_10km<-merge(biodiv_10km, es10_10km, by="mask", all=T)
    biodiv_10km$type<-"road-based"
    biodiv_10km$sample_size<-sampling_proportion * 1000
    biodiv_random_10km<-biodiv_random[res=="10km"]
    biodiv_random_10km<-merge(biodiv_random_10km, mask_p, by.x="mask", by.y="mask_10km")
    es10_random_10km<-es_random[res=="10km"&esNum==10]
    es10_random_10km$mask<-as.numeric(es10_random_10km$cell)
    biodiv_random_10km<-merge(biodiv_random_10km, es10_random_10km, by="mask", all=T)
    biodiv_random_10km$type<-"random"
    biodiv_random_10km$sample_size<-sampling_proportion * 1000
    
    
    plist[[length(plist)+1]]<-biodiv_10km
    
    plist[[length(plist)+1]]<-biodiv_random_10km
  }
  plist<-rbindlist(plist)
  
  colnames(plist)
  metrics_title<-c("Hurlbert's index (ES10)", 
                    "Hill number (Inf)", 
                    "Shannon", "Simpson", 
                    "Pielou's evenness", "Species richness")
  metrics_name<-c("es10", "hill.inf", "shannon", "simpson", "pielou.evenness", "species.richness")
  target_metrics<-c("es", "Hill_Inf", "shannon", "v", "J", "species.richness")
  j=2
  for (j in 1:length(target_metrics)){
    var<-target_metrics[j]
    name<-metrics_name[j]
    title<-metrics_title[j]
    if (var=="species.richness"){
      plist$label<-as.character(plist$species.richness)
    }else{
      plist$label<-sprintf("%.2f", pull(plist[, ..var]))
    }
    if (var=="Hill_Inf"){
      size1=2.5
      size2=1.2
    }else{
      size1=3
      size2=2
    }
    
    plist$check<-plist[, ..var]
    plist[is.na(check)]
    item<-plist[!is.na(check)]
    item_full<-item[type=="RAW"]
    s<-100
    item$cor<-""
    item$r2<-""
    for (s in unique(item$sample_size)){
      item_sub<-item[sample_size==s]
      item_sub_random<-item_sub[type=="random"]
      cor_random<-cor(item_full[mask %in% item_sub_random$mask, ..var], item_sub_random[, ..var])
      r2_random<-postResample(item_full[mask %in% item_sub_random$mask, ..var], item_sub_random[, ..var])
      item[sample_size==s&type=="random"]$cor<-sprintf("ρ = %.2f", cor_random)
      item[sample_size==s&type=="random"]$r2<-sprintf("R^2 = %.2f", r2_random[2])
      
      item_sub_road<-item_sub[type=="road-based"]
      cor_road<-cor(item_full[mask %in% item_sub_road$mask, ..var], item_sub_road[, ..var])
      r2_road<-postResample(item_full[mask %in% item_sub_road$mask, ..var], item_sub_road[, ..var])
      item[sample_size==s&type=="road-based"]$cor<-sprintf("ρ = %.2f", cor_road)
      item[sample_size==s&type=="road-based"]$r2<-sprintf("R^2 = %.2f", r2_road[2])
      
      
    }
    
    p_full<-ggplot(item[type=="RAW"])+geom_tile(aes_string(x="x", y="y", fill=var))+
      geom_text(aes_string(x="x", y="y", label="label"), size=size1)+
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
            panel.background = element_blank(), 
            legend.background = element_rect(fill = "white", color = NA),
            #panel.border = element_blank(),
            legend.position="none")
    
    data_cor<-item[type!="RAW"]
    cols<-c("type", "sample_size", "cor", "r2")
    data_cor<-unique(data_cor[, ..cols])
    item[is.na(Hill_Inf)]
    item[label=="NA"]
    p_raw<-ggplot(item[type!="RAW"])+geom_tile(aes_string(x="x", y="y", fill=var))+
      geom_text(aes_string(x="x", y="y", label="label"), size=size2)+
      coord_equal()+
      scale_fill_gradient(low=colors_blue[6], high=colors_red[6])+
      geom_text(data=data_cor, aes(x=45000, y=-4000, label=r2))+
      theme(axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "white", color = NA), 
            panel.background = element_blank(), 
            legend.background = element_rect(fill = "white", color = NA),
            #panel.border = element_blank(),
            legend.position="none")+
      facet_grid(type~sample_size)
    p_raw
    p<-ggarrange(plotlist = list(p_full, p_raw), nrow=1, ncol=2, widths=c(1, 1.5), labels=c("(a)", "(b)"))
    p<-annotate_figure(p, top = text_grob(title, face = "bold", size = 14))
    ggsave(p, filename=sprintf("../Figures/biodiversity_metrics/examples/%s_%s_grid_index_%d_sample_%d.png", 
                               name, "10km", grid_index, i), width=10, height=5)
  }
  
}

if (F){
  p_raw<-ggplot(biodiv_10km)+geom_tile(aes_string(x="x", y="y", fill=var))+
    geom_text(aes_string(x="x", y="y", label="label"), size=5)+
    coord_equal()+
    scale_fill_gradient(low=colors_blue[6], high=colors_red[6])+
    map_theme
  p_raw
}
