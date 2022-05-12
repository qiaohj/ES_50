library(raster)
library(rgdal)
library(rgeos)
library(MASS)
library(cluster)
library(data.table)
library(sf)
library(fasterize)
library(rmapshaper)
library(stars)
library(gdalUtilities)
library(ggplot2)
#library(tidyverse)


setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
if (F){
  r<-raster("../Raster/bioclim/wc2.1_10m_bio_1.tif")
  r_1_degree<-projectRaster(r, res=c(1, 1), crs=crs(r))
  plot(r_1_degree)
  v<-values(r_1_degree)
  no_na<-!is.na(v)
  v[no_na]<-c(1:length(no_na[no_na==T]))
  values(r_1_degree)<-v
  writeRaster(r_1_degree, "../Raster/mask_1_degree.tif")
}

if (F){
  mask<-raster("../Raster/mask_1_degree.tif")
  x = read_stars("../Raster/mask_1_degree.tif")
  grids<-st_as_sf(x[1], as_points = FALSE, merge = FALSE, crs=st_crs(mask))
  st_crs(grids)<-st_crs(mask)
  grids$index<-c(1:nrow(grids))
  
  species_list<-list.files("../Objects/IUCN_Distributions/Birds_RAW", pattern="\\.rda")
  overlap_list<-list()
  i=100
  species_list<-species_list[sample(length(species_list), length(species_list))]
  for (i in c(1:length(species_list))){
    print(paste(i, length(species_list)))
    item<-species_list[i]
    if (!file.exists(sprintf("../Objects/IUCN_Distributions/Birds_RAW/RAW/%s", item))){
      next()
    }
    
    f<-sprintf("../Objects/IUCN_Distributions/Birds_RAW/1_degree_Overlap/%s", item)
    if (file.exists(f)){
      print("skip")
      next()
    }
    saveRDS(NULL, f)
    dis<-readRDS(sprintf("../Objects/IUCN_Distributions/Birds_RAW/%s", item))
    
    overlap<-st_overlaps(grids, dis)
    if (F){
      bbox<-st_as_sfc(st_bbox(dis))
      grid_index<-unlist(st_overlaps(bbox, grids))
      if ((length(grid_index)>=10)&(length(grid_index)<=20)){
        adsf
        ggplot()+geom_sf(data=grids[grid_index,], fill=NA)+geom_sf(data=dis, fill=NA)
      }
    }
    saveRDS(overlap, f)
  }
  
  
  species_list<-readRDS("../Objects/IUCN_Distributions/areas.rda")
  iucn_status<-data.table(read.csv("../Tables/redlist_species_data_3752fd69-a2ab-4550-9648-6dd9736eaa78/simple_summary.csv", stringsAsFactors = F))
  cols<-c("scientificName", "orderName", "familyName", "redlistCategory")
  iucn_status<-iucn_status[, ..cols]
  order<-"PASSERIFORMES"
  order_list<-iucn_status[orderName==order]
  unique(iucn_status$redlistCategory)
  grids$species.richness<-0
  grids_in_order<-grids
  grids_out_oder<-grids
  grids_endanger<-grids
  grids_endanger_and_in_order<-grids
  grids_endanger_and_out_order<-grids
  i=1
  grids_full<-grids
  done_sp_list<-c()
  for (i in c(1:nrow(iucn_status))){
    print(paste(i, nrow(iucn_status)))
    item<-iucn_status[i]
    
    if (item$scientificName %in% done_sp_list){
      print("skip")
      next()
    }
    f<-sprintf("../Objects/IUCN_Distributions/Birds_RAW/1_degree_Overlap/%s.rda", 
               item$scientificName)
    if (file.exists(f)){
      
      overlap<-readRDS(f)
      if (is.null(overlap)){
        print("skip null")
        next()
      }
      names(overlap)<-paste(c(1:nrow(grids)), "_", sep="")
      x<-unlist(overlap, use.names = T)
      
      if (length(x)>0){
        grid_index<-names(x)
        grid_index<-sapply(strsplit(grid_index,"_"), `[`, 1)
        grid_index<-unique(as.numeric(grid_index))
        grids_full[grid_index,]$species.richness<-grids_full[grid_index,]$species.richness+1  
        grid_index<-names(x)
        grid_index<-sapply(strsplit(grid_index,"_"), `[`, 1)
        grid_index<-unique(as.numeric(grid_index))
        if (item$orderName==order){
          grids_in_order[grid_index,]$species.richness<-grids_in_order[grid_index,]$species.richness+1  
        }else{
          grids_out_oder[grid_index,]$species.richness<-grids_out_oder[grid_index,]$species.richness+1  
        }
        if (item$redlistCategory %in% c("Critically Endangered", "Endangered")){
          grids_endanger[grid_index,]$species.richness<-grids_endanger[grid_index,]$species.richness+1  
          if (item$orderName==order){
            grids_endanger_and_in_order[grid_index,]$species.richness<-
              grids_endanger_and_in_order[grid_index,]$species.richness+1  
          }else{
            grids_endanger_and_out_order[grid_index,]$species.richness<-
              grids_endanger_and_out_order[grid_index,]$species.richness+1  
          }
        }
      }
      
      done_sp_list<-c(done_sp_list, item$sp)
    }
    
  }
  saveRDS(grids_full, "../Objects/IUCN_Distributions/iucn_richness_full_1_degree.rda")
  saveRDS(grids_in_order, "../Objects/IUCN_Distributions/iucn_richness_passeriformes_1_degree.rda")
  saveRDS(grids_out_oder, "../Objects/IUCN_Distributions/iucn_richness_not_passeriformes_1_degree.rda")
  saveRDS(grids_endanger, "../Objects/IUCN_Distributions/iucn_richness_engdangered_1_degree.rda")
  saveRDS(grids_endanger_and_in_order, 
          "../Objects/IUCN_Distributions/iucn_richness_endangered_passeriformes_1_degree.rda")
  saveRDS(grids_endanger_and_out_order, 
          "../Objects/IUCN_Distributions/iucn_richness_not_endangered_passeriformes_1_degree.rda")
  
}
grids_full<-readRDS("../Objects/IUCN_Distributions/iucn_richness_full_1_degree.rda")
grids_in_order<-readRDS("../Objects/IUCN_Distributions/iucn_richness_passeriformes_1_degree.rda")
grids_out_oder<-readRDS("../Objects/IUCN_Distributions/iucn_richness_not_passeriformes_1_degree.rda")
grids_endanger<-readRDS("../Objects/IUCN_Distributions/iucn_richness_engdangered_1_degree.rda")
grids_endanger_and_in_order<-readRDS("../Objects/IUCN_Distributions/iucn_richness_endangered_passeriformes_1_degree.rda")
grids_endanger_and_out_order<-readRDS("../Objects/IUCN_Distributions/iucn_richness_not_endangered_passeriformes_1_degree.rda")

picked_grids<-readRDS("../Objects/virtual_lands/picked_grids.rda")
source("colors.r")
max_fix<-500
st_write(grids_full, "../Shape/IUCN_Richness/IUCN_Richness_1_Degree.shp")
grids_full<-st_transform(grids_full, crs=st_crs(picked_grids))
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
ggsave(p, filename="../Figures/IUCN_Based_VW/species.richness_1_degree.png", width=10, height=8)



p<-ggplot(grids_full)+geom_sf(aes(fill=species.richness), color=NA)+
  geom_sf(data=picked_grids, fill=NA, color="black")+
  geom_sf_text(data=picked_grids, aes(label = label), colour = "black", size=3)+
  scale_fill_gradient(low=colors_blue[4], high=colors_red[7])+
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
ggsave(p, filename="../Figures/IUCN_Based_VW/picked_grids_1_degree.png", width=10, height=8)



max_fix<-100
#grids_in_order<-st_transform(grids_in_order, crs=st_crs(picked_grids))
grids_in_order$fixed_species.richness<-grids_in_order$species.richness
grids_in_order[grids_in_order$fixed_species.richness>max_fix,]$fixed_species.richness<-max_fix
max_richness<-max(grids_in_order$species.richness)
#hist(grids_in_order$species.richness)
labs<-as.character(seq(0, max_fix, by=20))
labs[6]<-sprintf(">100 up to %d", max_richness)
p<-ggplot(grids_in_order)+geom_sf(aes(fill=fixed_species.richness), color=NA)+
  scale_fill_gradient(low=colors_blue[4], high=colors_red[7],
                      breaks=seq(0, max_fix, by=20), labels=labs)+
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
ggsave(p, filename="../Figures/IUCN_Based_VW/iucn_richness_passeriformes_1_degree.png", width=10, height=8)

hist(grids_out_oder$species.richness)
max_fix<-100
#grids_out_oder<-st_transform(grids_out_oder, crs=st_crs(picked_grids))
grids_out_oder$fixed_species.richness<-grids_out_oder$species.richness
grids_out_oder[grids_out_oder$fixed_species.richness>max_fix,]$fixed_species.richness<-max_fix
max_richness<-max(grids_out_oder$species.richness)
labs<-as.character(seq(0, max_fix, by=20))
labs[6]<-sprintf(">100 up to %d", max_richness)
p<-ggplot(grids_out_oder)+geom_sf(aes(fill=fixed_species.richness), color=NA)+
  scale_fill_gradient(low=colors_blue[4], high=colors_red[7],
                      breaks=seq(0, max_fix, by=20), labels=labs)+
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
ggsave(p, filename="../Figures/IUCN_Based_VW/iucn_richness_no_passeriformes_1_degree.png", width=10, height=8)

hist(grids_endanger$species.richness)
max_fix<-5
grids_endanger$fixed_species.richness<-grids_endanger$species.richness
grids_endanger[grids_endanger$fixed_species.richness>max_fix,]$fixed_species.richness<-max_fix
max_richness<-max(grids_endanger$species.richness)
labs<-as.character(seq(0, max_fix, by=1))
labs[6]<-sprintf(">5 up to %d", max_richness)
p<-ggplot(grids_endanger)+geom_sf(aes(fill=fixed_species.richness), color=NA)+
  scale_fill_gradient(low=colors_blue[4], high=colors_red[7],
                      breaks=seq(0, max_fix, by=1), labels=labs)+
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
ggsave(p, filename="../Figures/IUCN_Based_VW/iucn_richness_endanger_1_degree.png", width=10, height=8)
