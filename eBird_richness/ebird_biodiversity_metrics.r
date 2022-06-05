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
setDTthreads(6)
if (F){
  all_df<-readRDS("../Objects/ebird_richness_resolutions.rda")
  res<-"10km"
  es_threshold<-200
  for (res in c("100km", "50km", "20km", "10km", "5km", "2km", "1km")){
    occ_se<-all_df[, .(N=sum(N)), by=c(sprintf("mask_%s", res), "SCIENTIFIC_NAME")]
    colnames(occ_se)[1]<-"mask"
    f_es<-sprintf("../Objects/eBird_Biodiversity/es_%s.rda", res)
    if (!file.exists(f_es)){
      es_list<-list()
      for (es_threshold in c(200, 100, 50, 20, 10)){
        print(sprintf("ES %d on %s", es_threshold, res))
        esNum<-es_threshold
        es = occ_se %>%
          group_split(mask) %>%
          map(~ .x %>%
                group_by(SCIENTIFIC_NAME) %>%
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
      saveRDS(es_list, f_es)
      
      #t_m2<-pivot_wider(occ_se, id_cols=mask, names_from = SCIENTIFIC_NAME, 
      #                 values_from=N, values_fill=0)
      occ_se<-occ_se[!is.na(mask)]
      setkeyv(occ_se, "mask")
      mask_ids<-unique((occ_se$mask))
      mask_ids<-data.table(id=c(1:length(mask_ids)), mask_id=mask_ids)
      species<-unique((occ_se$SCIENTIFIC_NAME))
      t_m<-data.table(matrix(data = 0, nrow=nrow(mask_ids), 
                       ncol=length(species)))
      colnames(t_m)<-species
      sp_i=1
      for (sp_i in c(1:length(species))){
        sp<-species[sp_i]
        print(paste(sp_i, length(species), sp))
        occ_item<-occ_se[SCIENTIFIC_NAME==sp]
        
        t_m[mask_ids[mask_id %in% occ_item$mask]$id,(sp):=occ_item$N]
        if (F){
          cols<-c("mask", sp)
          t_m[mask_ids[mask_id %in% occ_item$mask]$id, ..cols]
        }
      }
      t_m$mask<-mask_ids$mask_id
      #t_m<-t_m[unique(occ_se$mask)]
      if (F){
        cols<-c("mask", "Gyps himalayensis")
        head(t_m[`Gyps himalayensis`>100, ..cols], 10)
        t_m2%>%filter(mask %in% head(t_m[`Gyps himalayensis`>100, ..cols], 10)$mask)%>%select(mask, `Gyps himalayensis`)
      }
      location<-t_m$mask
      t_m$mask<-NULL
      d2<-t_m
      #location<-mask_ids
      biodiversity_metrics<-data.frame(mask=location, res=res, metrics="simpson",
                                       simpson = diversity(d2, "simpson"))
      
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
      saveRDS(biodiversity_metrics, sprintf("../Objects/eBird_Biodiversity/biodiversity_%s.rda", res))
    }
  }
}

source("colors.r")
#for (res in c("100km", "50km", "20km", "10km", "5km", "2km", "1km")){
for (res in c("10km", "5km")){
  print(res)
  f_es<-sprintf("../Objects/eBird_Biodiversity/es_%s.rda", res)
  es<-readRDS(f_es)
  mask<-raster(sprintf("../Raster/mask_%s.tif", res))
  mask_p<-data.table(rasterToPoints(mask))
  colnames(mask_p)[3]<-"mask"
  es_i<-1
  for (es_i in 1:5){
    es_threshold<-c(200, 100, 50, 20, 10)[es_i]
    item<-data.table(es[[es_i]])
    item$mask<-as.numeric(item$cell)
    item<-merge(item, mask_p, by.x="mask", by.y="mask", all=T)
    item<-item[!is.na(mask)]
    
    p<-ggplot(item)+geom_tile(aes_string(x="x", y="y", fill="es"))+
      coord_equal()+
      scale_fill_gradient(low=colors_blue[6], high=colors_red[6])+
      labs(fill=sprintf("ES %d", es_threshold))+
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
            legend.background = element_rect(fill = "white", color = NA)
      )
    ggsave(p, filename=sprintf("../Figures/eBird_Biodiversity_Metrics/PNG/%s_%s.png",
                               sprintf("es%d", es_threshold), res), width=8, height=5)
    r<-mask
    vv<- values(r)
    vv[!is.na(vv)]<-item$es
    values(r)<-vv
    writeRaster(r, sprintf("../Figures/eBird_Biodiversity_Metrics/TIFF/%s_%s.tif",
                           sprintf("es%d", es_threshold), res), 
                overwrite=T)
  }
  biodiversity<-readRDS(sprintf("../Objects/eBird_Biodiversity/biodiversity_%s.rda", res))
  biodiversity<-data.table(biodiversity)
  vars<-colnames(biodiversity)[c(4, 5, 7:19)]
  biodiversity<-merge(biodiversity, mask_p, by="mask", all=T)
  biodiversity<-biodiversity[!is.na(mask)]
  v<-"J"
  for (v in vars){
    if (v=="J"){
      title<-"pielou.evenness"
    }else{
      title<-v
    }
    p<-ggplot(biodiversity)+geom_tile(aes_string(x="x", y="y", fill=v))+
      coord_equal()+
      scale_fill_gradient(low=colors_blue[6], high=colors_red[6])+
      labs(fill=title)+
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
            legend.background = element_rect(fill = "white", color = NA)
            )
    ggsave(p, filename=sprintf("../Figures/eBird_Biodiversity_Metrics/PNG/%s_%s.png",
                               title, res), width=8, height=5)
    r<-mask
    vv<- values(r)
    vv[!is.na(vv)]<-pull(biodiversity[, ..v])
    values(r)<-vv
    writeRaster(r, sprintf("../Figures/eBird_Biodiversity_Metrics/TIFF/%s_%s.tif",
                           title, res), 
                overwrite=T)
    
  }
}
