library(data.table)
library(ggplot2)
library(caret)
library(raster)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
picked_grids<-readRDS("../Objects/virtual_lands/picked_grids.rda")
seeds<-readRDS("../Objects/virtual_lands/virtual_lands_property.rda")

grid_index=10
args = commandArgs(trailingOnly=TRUE)
grid_index=1
grid_index<-as.numeric(args[1])
if (is.na(grid_index)){
  grid_index<-5
}
var<-"species.richness"
res<-"10km"

es<-paste("es", c(200, 100, 50, 20, 10), sep="")
hill<-paste("Hill", c(0, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, "Inf"), sep="_")

vars<-c(es, hill, "shannon", "v", "J", "species.richness")
res<-c("1km", "2km", "5km", "10km", "20km", "50km", "100km")

sampling_proportion<-0.1
sampling_proportion<-as.numeric(args[2])
if (is.na(sampling_proportion)){
  sampling_proportion<-0.1
}

coms<-data.table(expand.grid(var=vars, res=res, gird_index=grid_index, stringsAsFactors = F))
i=2
all_metrics<-list()
for (i in 1:nrow(coms)){
  com<-coms[i]
  full<-raster(sprintf("../Objects/GRIDS_TIF/%d/%s/%s_%s.tif", 
                       picked_grids[com$gird_index,]$index, "full", com$res, com$var))
  full_p<-data.table(rasterToPoints(full))
  full_p<-full_p[between(x, 0, 1e5)&between(y, 0, 1e5)]
  colnames(full_p)[3]<-"v1"
  if (sampling_proportion==1){
    random<-raster(sprintf("../Objects/GRIDS_TIF/%d/%s/%s_%s.tif", 
                           picked_grids[com$gird_index,]$index, "random_sampling", com$res, com$var))
  }else{
    random<-raster(sprintf("../Objects/GRIDS_TIF/%d_sampling_%.1f/%s/%s_%s.tif", 
                           picked_grids[com$gird_index,]$index, sampling_proportion, 
                           "random_sampling", com$res, com$var))
  }
  
  random_p<-data.table(rasterToPoints(random))
  random_p<-random_p[between(x, 0, 1e5)&between(y, 0, 1e5)]
  colnames(random_p)[3]<-"v2"
  if (com$var=="v"){
    na_v<-1
  }else{
    na_v<-0
  }
  ppp<-merge(full_p, random_p, by=c("x", "y"), all=T)
  ppp[is.na(v2)]$v2<-na_v
  cor_with_na<-cor(ppp$v1, ppp$v2)
  cor_without_na<-cor(ppp[v2!=na_v]$v1, ppp[v2!=na_v]$v2)
  postResample_with_na<-postResample(ppp$v1, ppp$v2)
  postResample_without_na<-postResample(ppp[v2!=na_v]$v1, ppp[v2!=na_v]$v2)
  
  
  random_item<-data.table(cor_with_na=cor_with_na, cor_without_na=cor_without_na,
                          RMSE_with_na=postResample_with_na[1],
                          Rsquared_with_na=postResample_with_na[2],
                          MAE_with_na=postResample_with_na[3],
                          RMSE_without_na=postResample_without_na[1],
                          Rsquared_without_na=postResample_without_na[2],
                          MAE_without_na=postResample_without_na[3])
  
  seed_m_list<-list()
  for (seed_i in 1:nrow(seeds)){
    print(paste(com$var, com$res, com$gird_index, seed_i, nrow(seeds), sampling_proportion))
    item<-seeds[seed_i]
    if (seed_i==3671){
      #next()
    }
    if (sampling_proportion==1){
      r<-raster(sprintf("../Objects/GRIDS_TIF/%d/%d/%s_%s.tif", picked_grids[grid_index,]$index, 
                        seeds[seed_i]$index, com$res, com$var))
    }else{
      r<-raster(sprintf("../Objects/GRIDS_TIF/%d_sampling_%.1f/%d/%s_%s.tif", 
                        picked_grids[grid_index,]$index, sampling_proportion,
                        seeds[seed_i]$index, com$res, com$var))
    }
    r_p<-data.table(rasterToPoints(r))
    r_p<-r_p[between(x, 0, 1e5)&between(y, 0, 1e5)]
    colnames(r_p)[3]<-"v2"
    
    ppp<-merge(full_p, r_p, by=c("x", "y"), all=T)
    ppp[is.na(v2)]$v2<-na_v
    
    item$cor_with_na<-cor(ppp$v1, ppp$v2)
    item$cor_without_na<-cor(ppp[v2!=na_v]$v1, ppp[v2!=na_v]$v2)
    postResample_with_na<-postResample(ppp$v1, ppp$v2)
    item$RMSE_with_na<-postResample_with_na[1]
    item$Rsquared_with_na<-postResample_with_na[2]
    item$MAE_with_na<-postResample_with_na[3]
    postResample_without_na<-postResample(ppp[v2!=na_v]$v1, ppp[v2!=na_v]$v2)
    item$RMSE_without_na<-postResample_without_na[1]
    item$Rsquared_without_na<-postResample_without_na[2]
    item$MAE_without_na<-postResample_without_na[3]
    seed_m_list[[length(seed_m_list)+1]]<-item
  }
  seed_m_list<-rbindlist(seed_m_list)
  all_items<-rbindlist(list(random_item, seed_m_list), fill=T)
  all_items$res<-com$res
  all_items$grid_index<-com$gird_index
  all_items$var<-com$var
  all_metrics[[length(all_metrics)+1]]<-all_items
}

if (sampling_proportion==1){
  saveRDS(all_metrics, sprintf("../Objects/GRID_Evaluations/%d.rda", picked_grids[grid_index,]$index))
}else{
  saveRDS(all_metrics, sprintf("../Objects/GRID_Evaluations/%d_sampling_%.1f.rda", 
                               picked_grids[grid_index,]$index, sampling_proportion))
}

if (F){
  sampling_proportion<-0.1
  picked_grids<-readRDS("../Objects/virtual_lands/picked_grids.rda")
  
  grid_index<-1
  all_metrics_list<-list()
  for (grid_index in c(1:nrow(picked_grids))){
    print(grid_index)
    if (sampling_proportion==1){
      all_metrics<-readRDS(sprintf("../Objects/GRID_Evaluations/%d.rda", picked_grids[grid_index,]$index))
    }else{
      all_metrics<-readRDS(sprintf("../Objects/GRID_Evaluations/%d_sampling_%.1f.rda", 
                                   picked_grids[grid_index,]$index, sampling_proportion))
      
    }
    all_metrics<-rbindlist(all_metrics)
    all_metrics_list[[length(all_metrics_list)+1]]<-all_metrics
  }
  all_metrics_list<-rbindlist(all_metrics_list)
  if (sampling_proportion==1){
    saveRDS(all_metrics_list, "../Objects/GRID_Evaluations/all.rda")
  }else{
    saveRDS(all_metrics_list, sprintf("../Objects/GRID_Evaluations/all_sampling_%.1f.rda", 
                                      sampling_proportion))
  }
  if (F){
    ggplot(all_metrics[!is.na(index)])+geom_boxplot(aes(x=var, 
                                                        y=Rsquared_with_na, 
                                                        color=res))+
      geom_point(data=all_metrics[is.na(index)], aes(x=var, 
                                                     y=Rsquared_with_na, 
                                                     color=res, group=res), 
                 shape=8, position=position_dodge(width=1))
    
    ggplot(all_metrics)+geom_boxplot(aes(x=var, 
                                         y=Rsquared_without_na, 
                                         color=res))
    
    ggplot(all_metrics)+geom_boxplot(aes(x=var, 
                                         y=cor_with_na, 
                                         color=res))
  }
}