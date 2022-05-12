library(data.table)
library(ggplot2)
library(caret)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
picked_grids<-readRDS("../Objects/virtual_lands/picked_grids.rda")
selected_seeds<-readRDS("../Objects/selected_seeds.rda")
grid_index<-1
var<-"species.richness"
res<-"10km"
if (F){
  all_points_list<-list()
  
  for (grid_index in c(1:10)){
    full<-raster(sprintf("../Objects/GRIDS_TIF/%d/%s/%s_%s.tif", picked_grids[grid_index,]$index, "full", res, var))
    full_p<-data.table(rasterToPoints(full))
    colnames(full_p)[3]<-"v"
    full_p$type<-"FULL"
    full_p$grid_label<-picked_grids[grid_index,]$label
    full_p$grid_index<-picked_grids[grid_index,]$index
    full_p$res<-res
    full_p$var<-var
    full_raw<-raster(sprintf("../Objects/GRIDS_TIF/%d/%s/%s_%s.tif", picked_grids[grid_index,]$index, "full", "1km", var))
    full_raw_p<-data.table(rasterToPoints(full_raw))
    colnames(full_raw_p)[3]<-"v"
    full_raw_p$type<-"RAW"
    full_raw_p$grid_label<-picked_grids[grid_index,]$label
    full_raw_p$grid_index<-picked_grids[grid_index,]$index
    full_raw_p$res<-res
    full_raw_p$var<-var
    
    for (sampling_proportion in c(0.1, 0.5, 1)){
      if (sampling_proportion==1){
        random<-raster(sprintf("../Objects/GRIDS_TIF/%d/%s/%s_%s.tif", picked_grids[grid_index,]$index, "random_sampling", res, var))
      }else{
        random<-raster(sprintf("../Objects/GRIDS_TIF/%d_sampling_%.1f/%s/%s_%s.tif", 
                               picked_grids[grid_index,]$index, sampling_proportion, "random_sampling", res, var))
      }
      random_p<-data.table(rasterToPoints(random))
      colnames(random_p)[3]<-"v"
      random_p$type<-"Random sampling"
      random_p$grid_label<-picked_grids[grid_index,]$label
      random_p$grid_index<-picked_grids[grid_index,]$index
      random_p$res<-res
      random_p$var<-var
      all_points<-rbindlist(list(full_p, full_raw_p, random_p))
      all_points$n_road_5km<-0
      all_points$binds<-NA
      all_points$seed_index<--1
      all_points$seed_label<-""
      road_based<-list()
      seed_i=1
      for (seed_i in 1:nrow(selected_seeds)){
        print(paste(res, var, grid_index, "/", 10, seed_i, "/", nrow(selected_seeds), 
                    sampling_proportion))
        if (sampling_proportion==1){
          r<-raster(sprintf("../Objects/GRIDS_TIF/%d/%d/%s_%s.tif", picked_grids[grid_index,]$index, 
                            selected_seeds[seed_i]$index, res, var))
        }else{
          r<-raster(sprintf("../Objects/GRIDS_TIF/%d_sampling_%.1f/%d/%s_%s.tif", 
                            picked_grids[grid_index,]$index, sampling_proportion,
                            selected_seeds[seed_i]$index, res, var))
        }
        r_p<-data.table(rasterToPoints(r))
        colnames(r_p)[3]<-"v"
        r_p$type<-sprintf("Road based No.%d", selected_seeds[seed_i]$index)
        r_p$grid_label<-picked_grids[grid_index,]$label
        r_p$grid_index<-picked_grids[grid_index,]$index
        r_p$res<-res
        r_p$var<-var
        r_p$n_road_5km<-selected_seeds[seed_i]$n_road_5km
        r_p$binds<-selected_seeds[seed_i]$binds
        r_p$seed_index<-selected_seeds[seed_i]$index
        r_p$seed_label<-selected_seeds[seed_i]$labels
        
        road_based[[length(road_based)+1]]<-r_p
      }
      road_based<-rbindlist(road_based)
      all_points<-rbindlist(list(all_points, road_based))
      all_points$sampling_proportion<-sampling_proportion
      all_points_list[[length(all_points_list)+1]]<-all_points
    }
  }
  all_points_full<-rbindlist(all_points_list)
  saveRDS(all_points_full, "../Figures/IUCN_Based_VW/all_points_full.rda")
}
all_points_full<-readRDS("../Figures/IUCN_Based_VW/all_points_full.rda")

all_points_full<-all_points_full[between(x, 0, 1e5)&between(y, 0, 1e5)]
#for raw richness
all_points_sub<-all_points_full[type=="RAW"&sampling_proportion==1]
source("colors.r")
p<-ggplot(all_points_sub)+geom_tile(aes(x=x, y=y, fill=v))+
  coord_equal()+
  scale_fill_gradient(low=colors_blue[6], high=colors_red[6])+
  map_theme+
  facet_wrap(~grid_label, nrow=2, ncol=5)
p
ggsave(p, filename="../Figures/IUCN_Based_VW/raw_richness.png", width=10, height=5)

#for full richness
all_points_sub<-all_points_full[type=="FULL"&sampling_proportion==1]
p<-ggplot(all_points_sub)+geom_tile(aes(x=x, y=y, fill=v))+
  geom_text(aes(x=x, y=y, label=v), size=2)+
  coord_equal()+
  scale_fill_gradient(low=colors_blue[6], high=colors_red[6])+
  map_theme+
  #facet_grid(sampling_proportion~grid_label)
  facet_wrap(~grid_label, nrow=2, ncol=5)
p
ggsave(p, filename="../Figures/IUCN_Based_VW/full_richness.png", width=10, height=5)


#for random sampling richness
all_points_sub<-all_points_full[type=="Random sampling"]
p<-ggplot(all_points_sub)+geom_tile(aes(x=x, y=y, fill=v))+
  geom_text(aes(x=x, y=y, label=v), size=4)+
  coord_equal()+
  scale_fill_gradient(low=colors_blue[6], high=colors_red[6])+
  map_theme+
  facet_grid(sampling_proportion~grid_label)+
  theme(strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20))
p
ggsave(p, filename="../Figures/IUCN_Based_VW/random_sampling_richness.png", width=30, height=15)

postResample(all_points_full[type=="FULL"&grid_label==g_label]$v, 
             all_points_full[type=="Random sampling"&grid_label==g_label]$v)
postResample(all_points_full[type=="FULL"&grid_label==g_label]$v, 
             all_points_full[type=="Road based No.256"&grid_label==g_label]$v)
postResample(2, 4)

#for road_based sampling richness
g_label<-"d"

all_points_sub<-all_points_full[!is.na(binds)]
p<-ggplot(all_points_sub[grid_label==g_label])+
  geom_tile(aes(x=x, y=y, fill=v))+
  geom_text(aes(x=x, y=y, label=v), size=4)+
  coord_equal()+
  scale_fill_gradient(low=colors_blue[6], high=colors_red[6])+
  map_theme+
  facet_grid(sampling_proportion~seed_label)+
  theme(strip.text.x = element_text(size = 20), strip.text.y = element_text(size = 20))
p
ggsave(p, filename=sprintf("../Figures/IUCN_Based_VW/road_based_grid_%s_richness.png", g_label), width=30, height=15)


