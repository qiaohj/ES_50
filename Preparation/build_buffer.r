library(sf)
library(raster)

setwd("/media/huijieqiao/SSD_Fast/ES50_eBird/ES_50")

#cp GHS_BUILT_LDS2014_GLOBE_R2018A_54009_250_V2_0.tif 1.tif
#gdal_edit.py -unsetnodate 1.tif
#gdal_calc.py -A 1.tif --outfile=2.tif --calc="numpy.where(A<=0, 0, A)" --NoDataValue=0
#gdal_proximity.py 2.tif 2_2km.tif -distunits PIXEL -maxdist 8
#gdal_proximity.py 2.tif 2_5km.tif -distunits PIXEL -maxdist 20
#mv 2.tif build.tif
#mv 2_2km.tif build_2km.tif
#mv 2_5km.tif build_5km.tif