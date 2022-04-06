library(raster)
library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
rarities<-c(3, 9, 27, 81, 243, 729, 2187, 6561, 19683, 59049)
rep<-100
mask<-raster("../Objects/mask.tif")
mask_p<-data.table(rasterToPoints(mask))
all_species<-list()
for (r in c(1:rep)){
  for (rarity in rarities){
    points<-mask_p[sample(nrow(mask_p), rarity)]
    points$rep<-r
    points$rarity<-rarity
    all_species[[length(all_species)+1]]<-points
  }
}
all_species<-rbindlist(all_species)
saveRDS(all_species, "../Objects/virtual_species/virtual_species.rda")
if (F){
  p<-ggplot(mask_p)+#geom_tile(aes(x=x, y=y), color="lightgrey")+
    geom_tile(data=all_species, aes(x=x, y=y, fill=factor(rep)))+
    coord_equal()+
    facet_wrap(~rarity, nrow=2)+theme_bw()+
    labs(fill="Species No.")+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position="none")
  ggsave(p, filename="../Figures/VW/virtual_species.png", 
         width=15, height=6)
}