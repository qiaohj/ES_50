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

The main functions were saved in four folders ([Preparation](https://github.com/qiaohj/ES_50/tree/master/Preparation), [IUCN_Richness](https://github.com/qiaohj/ES_50/tree/master/IUCN_Richness), [eBird_richness](https://github.com/qiaohj/ES_50/tree/master/eBird_richness), and [VirtualWorld](https://github.com/qiaohj/ES_50/tree/master/VirtualWorld)). 
### Folder [Preparation](https://github.com/qiaohj/ES_50/tree/master/Preparation) ###
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
### Folder [IUCN_Richness](https://github.com/qiaohj/ES_50/tree/master/IUCN_Richness) ###
- For Birdlife-based species richness, we downloaded the range maps via [http://datazone.birdlife.org/](http://datazone.birdlife.org) (accessed date: Jan, 10th 2022). We used four steps to generate the richness maps at different resolutions (1km, 2km, 5km, 10km, 20km, 50km and 100km) (Figure 1a). 
  1) We generated seven gridded masks (GM) of the world at the seven resolutions. We then calculated the biodiversity metrics of each grid in each GM (Figure 1). The code to implement the function is in lines 13-82 in ‘[distribution_polygon_2_raster_30s.r](https://github.com/qiaohj/ES_50/blob/master/IUCN_Richness/distribution_polygon_2_raster_30s.r)’ 
  2) We reformatted the Birdlife range map to a raster map at 30s (~1km) resolution, species by species, then projected them at the 1km resolution with Mollweide equal-area projection (lines 119-226 in ‘[distribution_polygon_2_raster_30s.r](https://github.com/qiaohj/ES_50/blob/master/IUCN_Richness/distribution_polygon_2_raster_30s.r)’).
  3) We converted the Birdlife range map of each species generated in step two (1km with Mollweide projection) into a table, extracted the index for each cell in the range map from the 1km-resolution mask generated in step one, and merged it with MAR to get the index of mask in all resolution. The code is provided in lines 4-39 in ‘[iucn_richness_moll.r](https://github.com/qiaohj/ES_50/blob/master/IUCN_Richness/iucn_richness_moll.r).’
  4) Because the total number of distribution cells (in 1km resolution) of all the species in Birdlife was larger than the representable integers (~2×109) in R and caused an integer overflow exception which stopped us from calculating the species richness in one step, we had to separate the species into 26 groups based on the started letter of its Latin name, calculate the species richness individually (lines 5-59 in ‘iucn_richness_resolutions.r’), and summed up them (26 tables) to get the final Birdlife-based species richness in seven resolutions (lines 66-192 in ‘[iucn_richness_resolutions.r](https://github.com/qiaohj/ES_50/blob/master/IUCN_Richness/iucn_richness_resolutions.r)’).  

## Mapping eBird-based richness patterns ##
### Folder [eBird_richness](https://github.com/qiaohj/ES_50/tree/master/eBird_richness) ###
- For eBird-based species richness, we downloaded the full eBird database via [https://download.ebird.org/ebd/prepackaged/ebd_relDec-2021.tar](https://download.ebird.org/ebd/prepackaged/ebd_relDec-2021.tar) (accessed date: Jan, 25th 2022), extracted the observation records from 2010 to 2019, and calculated the species richness (Fig. 1b) and number of observations (Fig. 1c) in the same tile as above (seven resolutions and Mollweide projection). We used two steps to generate eBird-based richness.
  1) Because there were ~one billion records in eBird, we split it into subsets by country and province/state (eBird-P, line 6-81 in ‘[ebird_checklist.r](https://github.com/qiaohj/ES_50/blob/master/Preparation/ebird_checklist.r)’). 
  2) To reduce potential errors in the eBird data, we used a realm appropriateness approach to remove any occurrences that were outside of their expected realms. First, we determined species-realm relationships (SRR) by finding the interactions between the Birdlife range map and realm layer (downloaded via [https://ecoregions.appspot.com/](https://ecoregions.appspot.com), accessed date: Jan, 27th 2022). Then we removed all the records which were out of the realm(s) that a given species should be from (eBird-P). The code for SRR is in ‘[interact_birdlife_ecoregion.r](https://github.com/qiaohj/ES_50/blob/master/eBird_richness/interact_birdlife_ecoregion.r)’ and we used ‘[birdlife_checklist_by_realm.r](https://github.com/qiaohj/ES_50/blob/master/eBird_richness/birdlife_checklist_by_realm.r)(https://github.com/qiaohj/ES_50/blob/master/eBird_richness/birdlife_checklist_by_realm.r)’ to create the species checklist per realm.
  3) We projected the datasets above at 1km with a Mollweide projection (lines 23-34), extracted the index of the 1km-resolution mask for each record (line 35), merged the dataset with GM (line 39), and calculated the species richness and number of observations per cell at a 1km resolution (line 40 in ‘[ebird_richness_resolutions.r](https://github.com/qiaohj/ES_50/blob/master/eBird_richness/ebird_richness_resolutions.r)’).
  4) Based on the results in step 2, we calculated the species richness and number of observations for seven resolutions. The code is lines 51-152 in ‘[ebird_richness_resolutions.r](https://github.com/qiaohj/ES_50/blob/master/eBird_richness/ebird_richness_resolutions.r).’

<img src=https://user-images.githubusercontent.com/572353/183867599-478592cc-2814-4ba1-ae5a-d4d70ac61f2e.png width=100>

  __Figure 1__ The species richness and number of observations from the different data sources. (a) Birdlife range map-based species richness, (b) eBird based species richness and (c) number of observations from eBird records at a 100km resolution.
  
## Creating virtual world ##
### Folder [VirtualWorld](https://github.com/qiaohj/ES_50/tree/master/VirtualWorld) ###
#### Simulating virtual species ranges (as polygons) ####
- In [virtual_richness_iucn_based.r](https://github.com/qiaohj/ES_50/blob/master/eBird_richness/virtual_richness_iucn_based.r) we chose ten random, 500km-by-500km boxes based on the [Birdlife-based species richness map] generated in iucn_richness.r (https://github.com/qiaohj/ES_50/blob/master/IUCN_Richness/iucn_richness.r) (Figure 2a), and reconstructed species richness based on 1km resolution maps using Birdlife by restacking the polygons (Figure. 2b). 

  (a)<img src=https://user-images.githubusercontent.com/572353/183873156-abdc36c1-10b4-401d-8524-fae31b558e96.png width=100>
  (b)<img src=https://user-images.githubusercontent.com/572353/183873190-80e8271c-9229-4fb0-aedd-5e3e4a4268bd.png width=100>

  __Figure 2__. (a) Simulated virtual species richness, (b) Simulated virtual species richness in 1km resolution.


#### Simulating certain biases (i.e. those identified in different regions) and subsampling these ranges ####
- To simulate sampling bias, we chose 5027 random global points as the centres of (Figure 3) and cropped boxes of road patterns as an agent of sample distribution (Figure 4). The code can be found in ‘[virtualland.r](https://github.com/qiaohj/ES_50/blob/master/VirtualWorld/virtualland.r).’ Then we created 100, 500, and 1000 random observing events and 100, 500, and 1000 road-based observing events with the code ‘[virtualobserve.r](https://github.com/qiaohj/ES_50/blob/master/VirtualWorld/virtualobserve.r).’ 
  1) Random observing behavior: For each virtual area, we created 100, 500, and 1000 random observation events. The localities of observation events were the same among all the virtual lands (Figure 5a, Figure 6).
  2) Road-based observing behavior: For each virtual area, we created 100, 500, and 1000 observing events. 55% of them were on the road, 33% of them within 2km of the road, 11% within 5km buffer of the road, and 1% were outside of the buffer of the road (Figure 5b).
- Then we calculated the observed species based on the species richness map generated in step above and the simulated observing behaviours described above. We assumed that different species have differing probabilities of being observed. The observability of species was based on IUCN threat status. The species in Least Concern (LC) group has 100% probability to be observed if its distribution was in any of observing behaviours above. The observable probability decreased to 40% in Near Threatened (NT) group, 20% in Vulnerable (VU), and 10% Critically Endangered (CR) and Endangered (EN). The code to implement this function is in ‘[iucn_based_observation.r](https://github.com/qiaohj/ES_50/blob/master/IUCN_Richness/iucn_based_observation.r).’
<img src=https://user-images.githubusercontent.com/572353/183874351-7ebdd79f-278b-4ccd-b711-4feaa40790c7.png width=100>

  __Figure 3__ creating the virtual lands based on the real-world road map. (5027 sites)

<img src=https://user-images.githubusercontent.com/572353/183874488-56f7b6c6-7656-47a9-bfb8-b06e38349ba0.png width=100>

  __Figure 4__ 10 of 5027 random virtual lands with different road coverage gradient.
  
<img src=https://user-images.githubusercontent.com/572353/183874588-db60d64d-2cf7-4958-8f80-dcd74b02ceae.png width=100>

  __Figure 5__ 1000, 500 and 100 random observing events (a, c, and e) and 1000, 500 and 100 road-based observing events (b, d, and e). 55% of them (in blue) are on the road, 33% of them (in green) are on the 2km buffer of the road, 11% (in yellow) are on the 5km buffer of the road, and 1% (in brown) are outside of the buffer of the road.

  (a)<img src=https://user-images.githubusercontent.com/572353/183874697-734289d8-3eaa-4d96-b3eb-9c7eaf19b62e.png width=100>
  (b)<img src=https://user-images.githubusercontent.com/572353/183874788-96489618-76a3-4696-8ee3-a4ae31793679.png width=100>
  
  __Figure 6__ Plot based richness

  
## Apply the diversity metrics to the different scenarios ##
  - After all the steps above, we generated 150,810 virtual scenarios by 10 biodiversity patterns × 5028 (1 random sampling + 5027 road-based sampling behaviours) × 3 sampling strengths (100, 500 and 1000). For each virtual scenario, we calculated Hurlbert’s index (ES10, 20, 50 100 and 200) with the ‘entropart’ package (Marcon & Herault 2015), Shannon’s index, Simpson’s index, Hill numbers (Rényi diversity, scales are 0, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, and infinity; Hill 1973) and Pielou’s index with ‘vegan 2.6-2’ in R (Oksanen et al., 2013). We also applied the metrics above to the eBird data to map the global biodiversity metrics at multiple resolutions ([ebird_biodiversity_metrics.r](https://github.com/qiaohj/ES_50/blob/master/eBird_richness/ebird_biodiversity_metrics.r )).
  - Code list:
    - [iucn_based_biodiversity_metrics.r](https://github.com/qiaohj/ES_50/blob/master/IUCN_Richness/iucn_based_biodiversity_metrics.r)
    - [iucn_based_random_sampling_biodiversity_metrics.r](https://github.com/qiaohj/ES_50/blob/master/IUCN_Richness/iucn_based_random_sampling_biodiversity_metrics.r)
    - [iucn_based_full_biodiversity_metrics.r](https://github.com/qiaohj/ES_50/blob/master/IUCN_Richness/iucn_based_full_biodiversity_metrics.r)
    - [ebird_biodiversity_metrics.r](https://github.com/qiaohj/ES_50/blob/master/eBird_richness/ebird_biodiversity_metrics.r)

## Mapping richness indices ##
  - We mapped out diversity from eBird data based on each index at resolutions of 1, 2, 5, 10, 20, and 50km. 
  - Code:
    - Following this, given that each index has a different numerical scale, each was reclassified into 20 bins with equal divisions for both 5 and 10km resolutions (using the reclassify function in ArcMap 10.8), to be the equivalent of 5% value intervals and enable a more direct comparison. We then quantified the coverage area of each of the 20 bins in each metric and resolution.
### Projecting richness ###
   - To project richness beyond sampled sites, we used MaxEnt to model richness based on various climatic drivers for the world, and climate plus landcover drivers for regions, to map each “richness band” for equally weighted richness. Firstly, we downloaded a number of bioclimatic and other climate variables (see supplemental source link file) at 5km and 10km resolutions, and the 5km resolution data was partitioned to realm using the [Ecoregion 2017 data](https://ecoregions.appspot.com/) giving Palearctic, Afrotropical, Nearctic, Neotropical, North Asia, South Asia and Australasia realms (Oceania was excluded as the realm was too small). Richness indices were then averaged for both 1) all metrics and 2) those found as good and reasonable (all except Hurlbert’s index and Pielou’s), and one using all metrics averaged. These were then converted to point form at both resolutions giving whole integers, and values of over 16 merged for the 5km analysis because of small sample size. These were then exported to CSV form, and run in Maxent using default parameters and five replicates, with each richness value treated as the equivalent of a species. The averages of the five thresholded replicates were used, and a 10-percentile training presence threshold used to classify “suitable” and “unsuitable” to give binary maps for each richness level. These were then reclassified to give values of “0” and the appropriate richness level (e.g for a model richness value of 10 areas above the threshold would have a value of 10). For each scenario (global at 10km, with good and all richness metrics, and then per realm at 5km with good and all richness metrics) we then used the raster mosaic function in ArcMap 10.8, then using the maximum value to develop two maps of richness for each scenario (good and all richness metrics). We then used the mosaic function to merge the good vs all metric maps, both using the sum and the mean function to map out richness for each region and resolution.
