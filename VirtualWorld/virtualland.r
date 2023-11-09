library(sf)
library(data.table)
library(raster)
library(scales)
setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")
road<-raster("../Raster/road/road.tif")
road2km<-raster("../Raster/road/road_2km.tif")
road5km<-raster("../Raster/road/road_5km.tif")
build2km<-raster("../Raster/build/build_2km.tif")
build5km<-raster("../Raster/build/build_5km.tif")
build<-raster("../Raster/build/build.tif")
if (F){
  seeds<-data.table(x=runif(1e4, extent(build)[1]+radius, extent(build)[2]-radius),
                    y=runif(1e4, extent(build)[3]+radius, extent(build)[4]-radius))
  seeds$build<-extract(build, seeds)
  seeds<-data.table(seeds)
  seeds1<-seeds[!is.na(build)]
  seeds2<-seeds[!is.na(build)]
  seeds3<-seeds[!is.na(build)]
  seeds4<-seeds[!is.na(build)]
  
  useable_seeds<-rbindlist(list(seeds1, seeds2, seeds3, seeds4))
  plot(seeds1$x, seeds1$y)
  points(seeds2$x, seeds2$y, col="red", pch=2)
  points(seeds3$x, seeds3$y, col="blue", pch=3)
  points(seeds4$x, seeds4$y, col="green", pch=4)
  saveRDS(useable_seeds, "../Objects/virtual_lands/centers.rda")
}
if (F){
 
  seeds<-data.table(x=runif(1e4, extent(ndvi)[1]+radius, extent(ndvi)[2]-radius),
                    y=runif(1e4, extent(ndvi)[3]+radius, extent(ndvi)[4]-radius))
  seeds$ndvi<-extract(ndvi, seeds)
  seeds<-data.table(seeds)
  seeds1<-seeds[!is.na(ndvi)]
  
  useable_seeds<-readRDS("../Objects/virtual_lands/centers.rda")
  plot(useable_seeds$x, useable_seeds$y)
  points(seeds1$x, seeds1$y, col="red", pch=2)
  useable_seeds<-rbindlist(list(useable_seeds, seeds1), fill=T)
  saveRDS(useable_seeds, "../Objects/virtual_lands/centers.rda")
  
}

radius<-50000


seeds<-readRDS("../Objects/virtual_lands/centers.rda")
i=1
for (i in c(1:nrow(seeds))){
  print(i)
  folder<-sprintf("../Objects/virtual_lands/items/%d", i)
  if (dir.exists(folder)){
    next()
  }else{
    dir.create(folder, showWarnings = F)
  }
  center<-seeds[i]
  box<-c(center$x-radius, center$x+radius,
         center$y-radius, center$y+radius)
  land<-crop(build, box)
  
  land_2km<-crop(build2km, box)
  land_5km<-crop(build5km, box)
  road_item<-crop(road, box)
  road_item_2km<-crop(road2km, box)
  road_item_5km<-crop(road5km, box)
  writeRaster(land, sprintf("%s/build.tif", folder), overwrite=T)
  writeRaster(land_2km, sprintf("%s/build_2km.tif", folder), overwrite=T)
  writeRaster(land_5km, sprintf("%s/build_5km.tif", folder), overwrite=T)
  writeRaster(road_item, sprintf("%s/road.tif", folder), overwrite=T)
  writeRaster(road_item_2km, sprintf("%s/road_2km.tif", folder), overwrite=T)
  writeRaster(road_item_5km, sprintf("%s/road_5km.tif", folder), overwrite=T)
  
  png(filename = sprintf("%s/png/%d_road.png", "../Objects/virtual_lands", i),
      width = 1000, height = 1000)
  v_road_5km<-values(road_item_5km)
  v_road_5km[v_road_5km==65535]<-NA
  v_road_5km[!is.na(v_road_5km)]<-1
  values(road_item_5km)<-v_road_5km
  plot(road_item_5km, col="lightgrey", xaxt='n', yaxt='n', ann=FALSE, asp=1, axes=F, legend=F)
  
  v_road_2km<-values(road_item_2km)
  v_road_2km[v_road_2km==65535]<-NA
  v_road_2km[!is.na(v_road_2km)]<-1
  values(road_item_2km)<-v_road_2km
  plot(road_item_2km, col=alpha("grey", 0.8), add=T, xaxt='n', yaxt='n', ann=FALSE, asp=1, axes=F, legend=F)
  
  v_road<-values(road_item)
  v_road[v_road==0]<-NA
  v_road[!is.na(v_road)]<-1
  values(road_item)<-v_road
  plot(road_item, col=alpha("black", 1), add=T, xaxt='n', yaxt='n', ann=FALSE, asp=1, axes=F, legend=F)
  dev.off()
  
  png(filename = sprintf("%s/png/%d_build.png", "../Objects/virtual_lands", i),
      width = 1000, height = 1000)
  v_land_5km<-values(land_5km)
  v_land_5km[v_land_5km==65535]<-NA
  v_land_5km[!is.na(v_land_5km)]<-1
  values(land_5km)<-v_land_5km
  plot(land_5km, col="lightgrey", xaxt='n', yaxt='n', ann=FALSE, asp=1, axes=F, legend=F)
  
  v_land_2km<-values(land_2km)
  v_land_2km[v_land_2km==65535]<-NA
  v_land_2km[!is.na(v_land_2km)]<-1
  values(land_2km)<-v_land_2km
  plot(land_2km, col=alpha("grey", 0.8), add=T, xaxt='n', yaxt='n', ann=FALSE, asp=1, axes=F, legend=F)
  
  v_land<-values(land)
  v_land[v_land==0]<-NA
  v_land[!is.na(v_land)]<-1
  values(land)<-v_land
  plot(land, col=alpha("black", 1), add=T, xaxt='n', yaxt='n', ann=FALSE, asp=1, axes=F, legend=F)
  dev.off()
}


seeds<-readRDS("../Objects/virtual_lands/centers.rda")
i=1
result<-list()
for (i in c(1:nrow(seeds))){
  print(i)
  folder<-sprintf("../Objects/virtual_lands/items/%d", i)
  land<-raster(sprintf("%s/build.tif", folder))
  land_2km<-raster(sprintf("%s/build_2km.tif", folder))
  land_5km<-raster(sprintf("%s/build_5km.tif", folder))
  road_item<-raster(sprintf("%s/road.tif", folder))
  road_item_2km<-raster(sprintf("%s/road_2km.tif", folder))
  road_item_5km<-raster(sprintf("%s/road_5km.tif", folder))
  
  v_road_5km<-values(road_item_5km)
  n_no_road_5km<-length(v_road_5km[v_road_5km==65535])
  n_no_land_5km<-length(v_road_5km[is.na(v_road_5km)])
  v_road_5km[v_road_5km==65535]<-NA
  n_road_5km<-length(v_road_5km[!is.na(v_road_5km)])
  
  v_road_2km<-values(road_item_2km)
  n_no_road_2km<-length(v_road_2km[v_road_2km==65535])
  n_no_land_2km<-length(v_road_2km[is.na(v_road_2km)])
  v_road_2km[v_road_2km==65535]<-NA
  n_road_2km<-length(v_road_2km[!is.na(v_road_2km)])
  
  v_road<-values(road_item)
  n_no_road_0km<-length(v_road[v_road==0])
  n_no_land_0km<-length(v_road[is.na(v_road)])
  v_road[v_road==0]<-NA
  n_road_0km<-length(v_road[!is.na(v_road)])
  
  v_build_5km<-values(land_5km)
  n_no_build_5km<-length(v_build_5km[v_build_5km==65535])
  n_no_land_5km<-length(v_build_5km[is.na(v_build_5km)])
  v_build_5km[v_build_5km==65535]<-NA
  n_build_5km<-length(v_build_5km[!is.na(v_build_5km)])
  
  v_build_2km<-values(land_2km)
  n_no_build_2km<-length(v_build_2km[v_build_2km==65535])
  n_no_land_2km<-length(v_build_2km[is.na(v_build_2km)])
  v_build_2km[v_build_2km==65535]<-NA
  n_build_2km<-length(v_build_2km[!is.na(v_build_2km)])
  
  v_build<-values(land)
  n_no_build_0km<-length(v_build[v_build==0])
  n_no_land_0km<-length(v_build[is.na(v_build)])
  v_build[v_build==0]<-NA
  n_build_0km<-length(v_build[!is.na(v_build)])
  
  df_item<-data.table(n_no_road_0km=n_no_road_0km, n_road_0km=n_road_0km,
                      n_no_road_2km=n_no_road_2km, n_road_2km=n_road_2km,
                      n_no_road_5km=n_no_road_5km, n_road_5km=n_road_5km,
                      n_no_build_0km=n_no_build_0km, n_build_0km=n_build_0km,
                      n_no_build_2km=n_no_build_2km, n_build_2km=n_build_2km,
                      n_no_build_5km=n_no_build_5km, n_build_5km=n_build_5km,
                      n_no_land_0km=n_no_land_0km, n_no_land_2km=n_no_land_2km,
                      n_no_land_5km=n_no_land_5km,
                      index=i, x=seeds[i]$x, y=seeds[i]$y)
  result[[i]]<-df_item
  
}
result<-rbindlist(result)
result
saveRDS(result, "../Objects/virtual_lands/virtual_lands_property.rda")
if (F){
  result<-readRDS("../Objects/virtual_lands/virtual_lands_property.rda")
  hist(result$n_no_road_2km)
  ggplot(result$n_road_2km)
  result$road_0km_p<-result$n_road_0km/160000
  result$road_2km_p<-result$n_road_2km/160000
  result$road_5km_p<-result$n_road_5km/160000
  result$road_0km_int<-round(result$road_0km_p * 100)
  result$road_2km_int<-round(result$road_2km_p * 100)
  result$road_5km_int<-round(result$road_5km_p * 100)
  result_se_0km<-result[, .(N=.N), by=list(road_0km_int)]
  colnames(result_se_0km)[1]<-"bin"
  result_se_0km$buffer_size<-"0KM"
  
  result_se_2km<-result[, .(N=.N), by=list(road_2km_int)]
  colnames(result_se_2km)[1]<-"bin"
  result_se_2km$buffer_size<-"2KM"
  
  result_se_5km<-result[, .(N=.N), by=list(road_5km_int)]
  colnames(result_se_5km)[1]<-"bin"
  result_se_5km$buffer_size<-"5KM"
  result_se<-rbindlist(list(result_se_0km, result_se_2km, result_se_5km))
  p<-ggplot(result_se)+geom_line(aes(x=bin, y=N, color=buffer_size))+
    scale_y_sqrt()+
    theme_bw()+
    labs(x="Road coverage (%)", y="Number of virtual lands", color="Buffer size")+
    scale_color_manual(values=c("0KM"=colors_red[8],
                                "2KM"=colors_green[8],
                                "5KM"=colors_blue[8]))
  ggsave(p, filename="../Figures/road_coverage.png", width=8, height=4)
}
hist(result$n_road_2km)
hist(result$n_road_5km)

cuts<-seq(0, 160000, by=1600)
result$binds<-Hmisc::cut2(result$n_road_5km, cuts=cuts)
table(result$binds)
length(unique(result$binds))
b<-unique(result$binds)[1]
random_land<-list()
for (b in unique(result$binds)){
  items<-result[binds==b]
  sub_items<-items[sample(nrow(items), 10)]
  random_land[[length(random_land)+1]]<-sub_items
}
random_land<-rbindlist(random_land)
saveRDS(random_land, "../Objects/virtual_lands/random_1000_lands.rda")

mask<-raster("../Objects/mask.tif")
values(mask)<-0
plot(mask)
extent(mask)<-c(0, 1e5, 0, 1e5)
values(mask)<-c(1:16e4)
plot(mask)
writeRaster(mask, "../Objects/mask.tif", overwrite=T)
mask_p<-data.table(rasterToPoints(mask))

if (F){
  library(ggplot2)
  random_land<-readRDS("../Objects/virtual_lands/random_1000_lands.rda")
  bind<-unique(random_land$binds)[1]
  road_points_all<-list()
  for (bind in unique(random_land$binds)){
    print(length(road_points_all))
    item<-random_land[binds==bind][1]
    i<-item$index
    folder<-sprintf("../Objects/virtual_lands/items/%d", i)
    road_item<-raster(sprintf("%s/road.tif", folder))
    road_item_2km<-raster(sprintf("%s/road_2km.tif", folder))
    road_item_5km<-raster(sprintf("%s/road_5km.tif", folder))
    road_points<-data.table(rasterToPoints(road_item_5km))
    road_points[road_5km==65535]$road_5km<--1
    road_points$road_2km<--1
    road_points[road_5km!=-1]$road_2km<-
      raster::extract(road_item_2km, 
                      data.frame(
                        x=road_points[road_5km!=-1]$x,
                        y=road_points[road_5km!=-1]$y))
    road_points[road_2km==65535]$road_2km<--1
    road_points$road_0km<--1
    road_points[road_2km!=-1]$road_0km<-
      raster::extract(road_item, 
                      data.frame(
                        x=road_points[road_2km!=-1]$x,
                        y=road_points[road_2km!=-1]$y))
    road_points[road_0km==0]$road_0km<--1
    road_points$type<-"offroad"
    road_points[road_0km>=0]$type<-"on road"
    road_points[road_0km<0&road_2km>=0]$type<-"2km buffer"
    road_points[road_2km<0&road_5km>=0]$type<-"5km buffer"
    road_points$x<-road_points$x-extent(road_item)[1]
    road_points$y<-road_points$y-extent(road_item)[3]
    road_points$road_bind<-bind
    road_points_all[[length(road_points_all)+1]]<-road_points
  }
  road_points_all<-rbindlist(road_points_all)
  road_colors<-c("offroad"="white", "on road"="black", "2km buffer"="grey", "5km buffer"="lightgrey")
  p<-ggplot(road_points_all)+geom_tile(aes(x=x, y=y, fill=type))+
    coord_equal()+
    scale_fill_manual(values=road_colors)+
    theme_bw()+
    facet_wrap(~road_bind, nrow=10, ncol=10)+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position="none")
  ggsave(p, filename="../Figures/VW/virtual_land.png", width=16, height=16)
}


seeds<-readRDS("../Objects/virtual_lands/centers.rda")
i=1
for (i in c(1:nrow(seeds))){
  print(i)
  folder<-sprintf("../Objects/virtual_lands/items/%d", i)
  road_item<-raster(sprintf("%s/road.tif", folder))
  road_item_2km<-raster(sprintf("%s/road_2km.tif", folder))
  road_item_5km<-raster(sprintf("%s/road_5km.tif", folder))
  road_points<-data.table(rasterToPoints(road_item_5km))
  road_points[road_5km==65535]$road_5km<--1
  road_points$road_2km<--1
  road_points[road_5km!=-1]$road_2km<-
    raster::extract(road_item_2km, 
                    data.frame(
                      x=road_points[road_5km!=-1]$x,
                      y=road_points[road_5km!=-1]$y))
  road_points[road_2km==65535]$road_2km<--1
  road_points$road_0km<--1
  road_points[road_2km!=-1]$road_0km<-
    raster::extract(road_item, 
                    data.frame(
                      x=road_points[road_2km!=-1]$x,
                      y=road_points[road_2km!=-1]$y))
  road_points[road_0km==0]$road_0km<--1
  road_points$type<-"offroad"
  road_points[road_0km>=0]$type<-"on road"
  road_points[road_0km<0&road_2km>=0]$type<-"2km buffer"
  road_points[road_2km<0&road_5km>=0]$type<-"5km buffer"
  road_points$x<-road_points$x-extent(road_item)[1]
  road_points$y<-road_points$y-extent(road_item)[3]
  saveRDS(road_points, sprintf("%s/road_points.rda", folder))
}
