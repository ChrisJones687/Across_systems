### Data preparation for foot-and-mouth epidemic data at the zip-code level in the USA ###
library(raster)
library(sp)
library(rgdal)
library(dplyr)
cattle <- read.csv("G:/My Drive/EEID/Foot and Mouth Disease/fmd_zipcode.csv")

## United States zip codes
## read in data
zip <- readOGR("G:/My Drive/EEID/Foot and Mouth Disease/tl_2015_us_zcta510.shp")
zip$Zip.Code <- zip$ZCTA5CE10
## merge cattle data with zip code shapefiles
zipcode_cattle <- merge(zip, cattle, by = "Zip.Code")
## create raster
plot(zipcode_cattle)
zip_cattle <- rasterize(zipcode_cattle, )