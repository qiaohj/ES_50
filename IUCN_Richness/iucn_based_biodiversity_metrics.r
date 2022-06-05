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
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
if (F){
  #Calculate the number of species per category per land
  j=1
  picked_grids<-readRDS("../Objects/virtual_lands/picked_grids.rda")
  full<-list()
  for (j in c(1:10)){
    print(j)
    virtual_species<-readRDS(sprintf("../Objects/GRIDS_S/species_points_%d.rda", picked_grids[j,]$index))
    species_points<-st_as_sf(virtual_species, coords = c("x", "y"))
    species_points$geometry<-NULL
    species_points<-data.table(species_points)
    species_points$grid_index<-picked_grids[j,]$index
    species_points$index<-j
    full[[length(full)+1]]<-species_points
  }
  full<-rbindlist(full)
  saveRDS(full, "../Objects/GRIDS_S/all_species_points_all_grid.rda")
  species_detail<-full[, .(Area=.N), by=list(sp, iucn_category, grid_index, index)]
  saveRDS(species_detail, "../Objects/GRIDS_S/species_details_per_grid.rda")
  species_se<-full[, .(N_species=length(unique(sp)), Area=.N), by=list(iucn_category, grid_index, index)]
  
  saveRDS(species_se, "../Objects/GRIDS_S/N_species_per_category_per_grid.rda")
  
  species_detail<-readRDS("../Objects/GRIDS_S/species_details_per_grid.rda")
  species_detail_se<-species_detail[, .(mean_Area=mean(Area), sd_Area=sd(Area)), by=list(iucn_category, grid_index, index)]
  species_se<-readRDS("../Objects/GRIDS_S/N_species_per_category_per_grid.rda")
  species_se<-merge(species_se, species_detail_se, by=c("iucn_category", "grid_index", "index"))
  species_se[is.na(sd_Area)]$sd_Area<-0
  saveRDS(species_se, "../Objects/GRIDS_S/N_species_per_category_per_grid.rda")
  species_se<-species_se[iucn_category %in% c("Critically Endangered", "Endangered",
                                              "Least Concern", "Near Threatened", "Vulnerable")]
  ggplot(species_se)+geom_point(aes(x=grid_index, y=N_species, fill=iucn_category))
  
  p1<-ggplot(species_se, aes(x=letters[index], y=N_species))+
    geom_bar(aes(fill=iucn_category), stat="identity", position="dodge")+
    theme_bw()+labs(x="Land index", y="Number of species", fill="Category")+
    #scale_y_log10()+
    theme(axis.text.x=element_blank(), axis.title.x = element_blank())
  p2<-ggplot(species_se, aes(x=letters[index], y=Area))+
    geom_bar(aes(fill=iucn_category), stat="identity", position="dodge")+
    theme_bw()+labs(x="Land index", y="Total distribution area", fill="Category")+
    #scale_y_log10()+
    theme(axis.text.x=element_blank(), axis.title.x = element_blank())
  
  p3<-ggplot(species_se)+
    geom_bar(aes(x=letters[index], y=mean_Area, fill=iucn_category), 
             stat="identity", position=position_dodge())+
    theme_bw()+labs(x="Land index", y="Average distribution area", fill="Category")
    #geom_errorbar(aes(x=factor(grid_index), ymin=mean_Area-sd_Area, ymax=mean_Area+sd_Area), 
    #              position=position_dodge(.9), width=0.2)
  p3
  
  
  p<-ggarrange(plotlist=list(p1, p2, p3), nrow=3, ncol=1, common.legend = TRUE, legend="right")
  p
  ggsave(p, filename="../Figures/IUCN_Based_VW/species_details_per_grid.png", width=8, height=6)
}


add_location<-function(indices, location, type){
  location$metric<-indices
  location$type<-type
  location
}
full_mask<-readRDS("../Objects/full_mask.rda")

picked_grids<-readRDS("../Objects/virtual_lands/picked_grids.rda")
args = commandArgs(trailingOnly=TRUE)
grid_index=1
grid_index<-as.numeric(args[1])
if (is.na(grid_index)){
  grid_index<-4
}
sampling_proportion<-as.numeric(args[2])
if (is.na(sampling_proportion)){
  sampling_proportion<-0.1
}
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

i<-349
for (i in c(1:nrow(seeds))){
  #for (i in c(1:400)){
  folder<-sprintf("%s/%d", base, i)
  if (sampling_proportion==1){
    target<-sprintf("%s/biodiversity.rda", folder)
    f_es<-sprintf("%s/es.rda", folder)
  }else{
    target<-sprintf("%s/biodiversity_sampling_%.1f.rda", folder, sampling_proportion)
    f_es<-sprintf("%s/es_%.1f.rda", folder, sampling_proportion)
  }
  if (file.exists(target)){
    if (file.size(target)>100){
      next()
    }
  }
  saveRDS(NULL, target)
  f<-sprintf("%s/observations.rda", folder)
  
  occ_index<-readRDS(f)
  if (sampling_proportion!=1){
    occ_index<-occ_index[sample(length(occ_index), length(occ_index)*sampling_proportion)]
  }
  occ_index<-unlist(occ_index)
  occ<-virtual_species[ID %in% occ_index]
  occ_list<-list()
  for (cate in unique(occ$iucn_category)){
    occ_item<-occ[iucn_category==cate]
    occ_item<-occ_item[sample(nrow(occ_item), nrow(occ_item) * probabilities[[cate]])]
    occ_list[[length(occ_list)+1]]<-occ_item
  }
  occ<-rbindlist(occ_list)
  if (F){
    table(occ$iucn_category)
    table(occ_list$iucn_category)
  }
  if (nrow(occ)==0){
    next()
  }
  j=1
  biodiversity_list<-list()
  es_list<-list()
  for (res in c("100km", "50km", "20km", "10km", "5km", "2km", "1km")){
    print(paste("Grid:", grid_index, "seeds:", i, "/", nrow(seeds), "@", res, "sampling proportion:", sampling_proportion))
    occ_se<-occ[, .(N=.N), by=c(sprintf("mask_%s", res), "sp", "sp_id")]
    colnames(occ_se)[1]<-"mask"
    if (!file.exists(f_es)){
      for (es_threshold in c(200, 100, 50, 20, 10)){
        
        esNum<-es_threshold
        es = occ_se %>%
          group_split(mask) %>%
          map(~ .x %>%
                group_by(sp_id) %>%
                summarize(occCount = sum(N))
          ) %>%
          map(~ deframe(.x)) %>%
          modify_if(~ length(.x) <= esNum, ~ NA) %>% # run only if more than 50 species
          modify_if(~ !anyNA(.x), ~ Hurlbert(.x, esNum)) %>%
          map(~ unname(.x)) %>%
          flatten_dbl()
        
        cell = occ_se %>%
          group_split(mask) %>%
          map(~ .x %>% mutate(mask = as.character(mask))) %>%
          map_chr(~ unique(.x$mask))
        
        es50Table = tibble(cell,es)
        es50Table$res<-res
        es50Table$esNum<-es_threshold
        es_list[[length(es_list)+1]]<-es50Table
      }
    }
    if (F){
      occ_se<-rbind(occ_se, data.frame(mask=999999, sp="SP_1_3", sp_id=1, N=0))
    }
    t_m<-pivot_wider(occ_se, id_cols=mask, names_from = sp, values_from=N, values_fill=0)
    
    d2<-t_m[,-1]
    location<-t_m$mask
    biodiversity_metrics<-data.frame(mask=location, res=res, metrics="simpson",
                                     v = diversity(d2, "simpson"))
    
    biodiversity_metrics$shannon<-diversity(d2, "shannon")
    biodiversity_metrics$invsimp<-diversity(d2, "invsimpson")
    #biodiversity_metrics$unbias.simp<-rarefy(d2, 100) - 1
    #biodiversity_metrics$alpha <- fisher.alpha(d2)
    biodiversity_metrics$species.richness <- specnumber(d2) ## rowSums(BCI > 0) does the same...
    biodiversity_metrics$J <- biodiversity_metrics$shannon/log(biodiversity_metrics$species.richness)
    hill<-renyi(d2, hill=T)
    for (k in 1:length(hill)){
      biodiversity_metrics[,sprintf("Hill_%s", names(hill)[k])]<-hill[[k]]
    }
    biodiversity_list[[length(biodiversity_list)+1]]<-biodiversity_metrics
    
    
  }
  if (!file.exists(f_es)){
    es_list<-rbindlist(es_list)
    saveRDS(es_list, f_es)
  }
  biodiversity_list<-rbindlist(biodiversity_list)
  
  saveRDS(biodiversity_list, target)
  
  
  if (F){
    effort_distance<-1000
    road_points<-readRDS(sprintf("%s/road_points.rda", folder))
    observing_events<-readRDS(sprintf("%s/observing_events.rda", folder))
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
      geom_tile(data=occ[sample(nrow(occ), 10000)], aes(x=x, y=y), color="red")+
      scale_fill_manual(values=road_colors)+
      scale_color_manual(values=observing_colors)+
      theme_bw()
    
  }
}


