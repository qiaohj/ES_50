library(sf)
library(data.table)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
birdlife<-readRDS("../Shape/Birdlife_202101_fixed.rda")
ecoregion<-sf::st_read(dsn = "../Shape/Ecoregions2017", 
                    layer = "Ecoregions2017")

species<-unique(birdlife$binomial)
sp<-species[1]
sp_region_all<-list()
for (sp in species){
  print(sp)
  target<-sprintf("../Tables/Species_ecoregion/%s.rda", sp)
  if (file.exists(target)){
    next()
  }
  saveRDS(NULL, target)
  sp_layer<-birdlife[which(birdlife$binomial==sp),]
  index<-st_intersects(sp_layer, ecoregion)
  overlapped<-ecoregion[unlist(index),]
  if (F){
    plot(st_geometry(overlapped))
    plot(st_geometry(sp_layer), add=T, col="red")
  }
  sp_region<-data.table(scientific_name=sp, 
                        ECO_NAME=overlapped$ECO_NAME,
                        BIOME_NUM=overlapped$BIOME_NUM,
                        BIOME_NAME=overlapped$BIOME_NAME,
                        REALM=overlapped$REALM,
                        ECO_BIOME_=overlapped$ECO_BIOME_,
                        ECO_ID=overlapped$ECO_ID,
                        NNH=overlapped$NNH,
                        NNH_NAME=overlapped$NNH_NAME)
  
  saveRDS(sp_region, target)
  sp_region_all[[sp]]<-sp_region
}

sp_region_all<-list()
for (sp in species){
  print(sp)
  target<-sprintf("../Tables/Species_ecoregion/%s.rda", sp)
  sp_region<-readRDS(target)
  sp_region_all[[sp]]<-sp_region
}
sp_region_all<-rbindlist(sp_region_all)
saveRDS(sp_region_all, "../Tables/birdlife_ecoregion.rda")
