library(sf)
library(data.table)
library(gdalUtilities)

setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
sf_use_s2(FALSE)
birdlife<-readRDS("../Shape/Birdlife_202101_fixed.rda")
birdlife_checklist<-data.table(binomial=birdlife$binomial,
                               presence=birdlife$presence,
                               origin=birdlife$origin,
                               seasonal=birdlife$seasonal,
                               Shape_Area=birdlife$Shape_Area)
splist<-unique(birdlife_checklist$binomial)
sp<-splist[1]
for (sp in splist){
  item<-birdlife[which(birdlife$binomial==sp),]
  print(sp)
  if (file.exists(sprintf("../Objects/IUCN_Distributions/Birds_RAW/%s.rda", sp))){
    next()
  }
  saveRDS(item, sprintf("../Objects/IUCN_Distributions/Birds_RAW/%s.rda", sp))
}
