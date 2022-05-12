library(data.table)
library(raster)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
splist<-list.files("/media/huijieqiao/QNAS/ES50/IUCN_1km_Tables", pattern="\\.rda")
sp<-splist[1]
richness_list<-list()

for (i in c(1:length(splist))){
  sp<-splist[i]
  print(paste(sp, i, "/", length(splist)))
  df<-readRDS(sprintf("/media/huijieqiao/QNAS/ES50/IUCN_1km_Tables/%s", sp))
  if (is.null(df)){
    next()
  }
  if (nrow(df)==0){
    next()
  }
  richness_list[[sp]]<-df
}