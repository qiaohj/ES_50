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

setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
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
for (i in c(1:nrow(iucn_status))){
  print(paste(i, nrow(iucn_status)))
  item<-iucn_status[i]
  
  f<-sprintf("../Objects/IUCN_Distributions/Birds_MOLL/100km_Overlap/%s.rda", 
             gsub(" ", "_", item$scientificName))
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
  }
  
}
saveRDS(grids_in_order, "../Objects/IUCN_Distributions/iucn_richness_passeriformes_100km.rda")
saveRDS(grids_out_oder, "../Objects/IUCN_Distributions/iucn_richness_not_passeriformes_100km.rda")
saveRDS(grids_endanger, "../Objects/IUCN_Distributions/iucn_richness_engdangered_100km.rda")
saveRDS(grids_endanger_and_in_order, 
        "../Objects/IUCN_Distributions/iucn_richness_endangered_passeriformes_100km.rda")
saveRDS(grids_endanger_and_out_order, 
        "../Objects/IUCN_Distributions/iucn_richness_not_endangered_passeriformes_100km.rda")

max_fix<-100

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
ggsave(p, filename="../Figures/IUCN_Based_VW/iucn_richness_passeriformes_100km.png", width=10, height=8)

hist(grids_out_oder$species.richness)
max_fix<-100
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
ggsave(p, filename="../Figures/IUCN_Based_VW/iucn_richness_no_passeriformes_100km.png", width=10, height=8)

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
ggsave(p, filename="../Figures/IUCN_Based_VW/iucn_richness_endanger_100km.png", width=10, height=8)
