## GENERAL INFORMATION ##

This repository contains the R code and data associated with the manuscript titled 'Measuring metrics: what diversity indicators are most appropriate for different forms of data biases.'

The authors are:
Huijie Qiao, Michael Orr, Alice C. Hughes

Please contact [Huijie Qiao](huijieqiao@gmail.com) and [Alice C. Hughes](ach_conservation3@hotmail.com) for any queries.

## Data requirement ##
- [Birdlife species distribution data](http://datazone.birdlife.org)
- [eBird data](https://download.ebird.org/ebd/prepackaged/ebd_relDec-2021.tar)
- [GRIP global roads database](https://www.globio.info/download-grip-dataset)
- [GHS built-up grid](https://ghsl.jrc.ec.europa.eu/ghs_bu2019.php)
- [Global ecoregion layer](https://ecoregions.appspot.com)

## Overall structure ##

The main functions were saved in four folders (Preparation, IUCN_Richness, eBird_richness, and VirtualWorld)
### Folder [Preparation](https://github.com/qiaohj/ES_50/tree/master/Preparation): ###
All the scripts in it were used to prepare the necessary datasets for the following analysis.
  - __Group 1__: Handling the Birdlife species distribution data ([Birdlife](http://datazone.birdlife.org))
    - [birdlife_checklist.r](https://github.com/qiaohj/ES_50/blob/master/Preparation/birdlife_checklist.r): Extracting the checklist from [Birdlife](http://datazone.birdlife.org).
    - [birdlife_checklist_by_country.r](https://github.com/qiaohj/ES_50/blob/master/Preparation/birdlife_checklist_by_country.r): Extracting the country-by-country checklists from [Birdlife](http://datazone.birdlife.org).
    - [birdlife_checklist_by_realm.r](https://github.com/qiaohj/ES_50/blob/master/Preparation/birdlife_checklist_by_realm.r): Extracting the realm-by-realm checklists from [Birdlife](http://datazone.birdlife.org).
    - [interact_birdlife_ecoregion.r](https://github.com/qiaohj/ES_50/blob/master/Preparation/interact_birdlife_ecoregion.r): Calculating the interaction between [Birdlife](http://datazone.birdlife.org) and [Global ecoregion layer](https://ecoregions.appspot.com).
  - __Group 2__: Handling the [eBird](https://download.ebird.org/ebd/prepackaged/ebd_relDec-2021.tar) database
    - [ebird_checklist.r](https://github.com/qiaohj/ES_50/blob/master/Preparation/ebird_checklist.r): Extracting the checklist from [eBird data](https://download.ebird.org/ebd/prepackaged/ebd_relDec-2021.tar).
    - [ebird_checklist_by_country_and_realm.r](https://github.com/qiaohj/ES_50/blob/master/Preparation/ebird_checklist_by_country_and_realm.r): Extracting the country-by-country checklist and realm-by-realm checklists from [eBird data](https://download.ebird.org/ebd/prepackaged/ebd_relDec-2021.tar).
    - [ebird_checklist_by_realm.r](https://github.com/qiaohj/ES_50/blob/master/Preparation/ebird_checklist_by_realm.r): Extracting the realm-by-realm mchecklist from [eBird data](https://download.ebird.org/ebd/prepackaged/ebd_relDec-2021.tar).
  - __Group 3__: Handling the [GRIP global roads database](https://www.globio.info/download-grip-dataset) and [GHS built-up grid](https://ghsl.jrc.ec.europa.eu/ghs_bu2019.php)
    - [road_buffer.r](https://github.com/qiaohj/ES_50/blob/master/Preparation/road_buffer.r): Creating 2km and 5km buffers for the [road layer](https://www.globio.info/download-grip-dataset).
    - [build_buffer.r](https://github.com/qiaohj/ES_50/blob/master/Preparation/build_buffer.r): Creating 2km and 5km buffers for the road layer (https://ghsl.jrc.ec.europa.eu/ghs_bu2019.php).
    - [road_build_distance.r](https://github.com/qiaohj/ES_50/blob/master/Preparation/road_build_distance.r): Determining the relationships between  [eBird](https://download.ebird.org/ebd/prepackaged/ebd_relDec-2021.tar) and the buffered road and building layers generated above.
  - __Others__: Other functions
    - [colors.r](https://github.com/qiaohj/ES_50/blob/master/Preparation/colors.r): Defining the common colors used to generate the figures.
    - [completeness.r](https://github.com/qiaohj/ES_50/blob/master/Preparation/completeness.r): Combining all the results above to count the point coverage and species coverage (in relation to [Birdlife data](http://datazone.birdlife.org)) 

## Mapping Birdlife-based richness patterns ##
### Folder [IUCN_Richness](https://github.com/qiaohj/ES_50/tree/master/IUCN_Richness): ###
- For Birdlife-based species richness, we downloaded the range maps via [http://datazone.birdlife.org/](http://datazone.birdlife.org) (accessed date: Jan, 10th 2022). We used four steps to generate the richness maps at different resolutions (1km, 2km, 5km, 10km, 20km, 50km and 100km) (Figure 1). 
  1) We generated seven gridded masks (GM) of the world at the seven resolutions. We then calculated the biodiversity metrics of each grid in each GM (Figure 1). The code to implement the function is in lines 13-82 in ‘[distribution_polygon_2_raster_30s.r](https://github.com/qiaohj/ES_50/blob/master/IUCN_Richness/distribution_polygon_2_raster_30s.r)’ 
  2) We reformatted the Birdlife range map to a raster map at 30s (~1km) resolution, species by species, then projected them at the 1km resolution with Mollweide equal-area projection (lines 119-226 in ‘[distribution_polygon_2_raster_30s.r](https://github.com/qiaohj/ES_50/blob/master/IUCN_Richness/distribution_polygon_2_raster_30s.r)’).
  3) We converted the Birdlife range map of each species generated in step two (1km with Mollweide projection) into a table, extracted the index for each cell in the range map from the 1km-resolution mask generated in step one, and merged it with MAR to get the index of mask in all resolution. The code is provided in lines 4-39 in ‘[iucn_richness_moll.r](https://github.com/qiaohj/ES_50/blob/master/IUCN_Richness/iucn_richness_moll.r).’
  4) Because the total number of distribution cells (in 1km resolution) of all the species in Birdlife was larger than the representable integers (~2×109) in R and caused an integer overflow exception which stopped us from calculating the species richness in one step, we had to separate the species into 26 groups based on the started letter of its Latin name, calculate the species richness individually (lines 5-59 in ‘iucn_richness_resolutions.r’), and summed up them (26 tables) to get the final Birdlife-based species richness in seven resolutions (lines 66-192 in ‘[iucn_richness_resolutions.r](https://github.com/qiaohj/ES_50/blob/master/IUCN_Richness/iucn_richness_resolutions.r)’).  
