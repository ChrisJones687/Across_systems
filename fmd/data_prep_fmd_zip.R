### Data preparation for foot-and-mouth epidemic data at the zip-code level in the USA ###
library(raster)
library(sp)
library(rgdal)
library(dplyr)
cattle_zip <- read.csv("G:/My Drive/EEID/Foot and Mouth Disease/fmd_zipcode.csv")
cattle_county <- read.csv("G:/My Drive/EEID/Foot and Mouth Disease/us_county_cattle.csv")
## United States zip codes
## set up nlcd data
nlcd2016 <- raster("G:/My Drive/EEID/Foot and Mouth Disease/NLCD_2016_Land_Cover_L48_20190424.img")
nlcd2016[nlcd2016 == 81] <- 1
nlcd2016[nlcd2016 == 71] <- 1
nlcd2016[nlcd2016 > 1] <- 0
nlcd_3000m_pasture <- aggregate(nlcd2016, fact = 100, fun = 'sum')
writeRaster(nlcd_3000m_pasture, "G:/My Drive/EEID/West Nile Virus/nlcd_3000m.img")
nlcd_3000m_pasture <- raster("G:/My Drive/EEID/West Nile Virus/nlcd_3000m.img")

## ZIP CODE
## read in data
zip <- readOGR("G:/My Drive/EEID/Foot and Mouth Disease/tl_2015_us_zcta510.shp")
zip$Zip.Code <- zip$ZCTA5CE10
## merge cattle data with zip code shapefiles
zipcode_cattle <- merge(zip, cattle_zip, by = "Zip.Code")
## distribute cattle across zip code region
zipcode_cattle$cattle <- as.numeric(zipcode_cattle$cattle)
zipcode_cattle$ALAND10 <- as.numeric(zipcode_cattle$ALAND10)
zipcode_cattle$cattle <- zipcode_cattle$cattle/zipcode_cattle$ALAND10
zipcode_cattle$cattle <- as.character(zipcode_cattle$cattle)
zipcode_cattle$cattle[is.na(zipcode_cattle$cattle)] <- 0
zipcode_cattle$cattle <- as.numeric(zipcode_cattle$cattle)
## create raster
zip_cattle <- rasterize(zipcode_cattle, nlcd_3000m, field = "cattle", fun = 'last')
writeRaster(zip_cattle, "G:/My Drive/EEID/Foot and Mouth Disease/zip_cattle.tif")
zip_cattle

## COUNTY
us_counties <- readOGR("/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/us_lower_48_counties.shp")
us_counties$NAME <- us_counties$County
## merge cattle data with county shapefiles
counties_cattle <- merge(us_counties, cattle_county, by = "NAME")
#us_counties$area_of_pasture <- extract(nlcd_3000m_pasture, us_counties, fun = sum)
#writeOGR(COcounties, "/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/colorado_counties_pasturecattle.shp")
us_counties_cattle <- read.csv("/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/colorado.csv")
names(us_counties_cattle)[1] <- "NAME"
COcounties <- COcounties[order(COcounties$NAME),]
COcounties$cattle <- colorado_cattle$cattle
COcounties$NAME <- as.character(COcounties$NAME)
colorado_cattle$NAME <- as.character(colorado_cattle$NAME)
COcounties2 <- left_join(COcounties@data, colorado_cattle, by = 'NAME')

