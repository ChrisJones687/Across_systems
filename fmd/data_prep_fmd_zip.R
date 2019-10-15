### Data preparation for foot-and-mouth epidemic data at the zip-code level in the USA ###
library(raster)
library(sp)
library(rgdal)
library(dplyr)
library(rgeos)
cattle_zip <- read.csv("G:/My Drive/EEID/Foot and Mouth Disease/fmd_zipcode.csv")
cattle_county <- read.csv("G:/My Drive/EEID/Foot and Mouth Disease/us_county_cattle.csv")
## UNITED STATES
## set up nlcd data
nlcd2016 <- raster("C:/Users/lantzra/Desktop/NLCD_2016_Land_Cover_L48_20190424.img")
us_counties <- readOGR("G:/My Drive/EEID/Foot and Mouth Disease/us_lower_48_counties.shp")
us_counties <- spTransform(us_counties, CRSobj = crs(nlcd2016))
us_nlcd <- crop(nlcd2016, us_counties)
us_nlcd <- mask(us_nlcd, us_counties)
us_nlcd[us_nlcd == 81] <- 1
us_nlcd[us_nlcd == 71] <- 1
us_nlcd[us_nlcd > 1] <- 0
nlcd_3000m_pasture <- aggregate(us_nlcd, fact = 100, fun = 'sum')
writeRaster(nlcd_3000m_pasture, "G:/My Drive/EEID/Foot and Mouth Disease/nlcd_3000m.img")
nlcd_3000m_pasture <- raster("G:/My Drive/EEID/Foot and Mouth Disease/nlcd_3000m.img")

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
us_counties <- readOGR("G:/My Drive/EEID/Foot and Mouth Disease/us_lower_48_counties.shp")
us_counties$NAME <- us_counties$County
virginia <- states[states$STATE_NAME == 'Virginia',]
virginia <- spTransform(virginia, CRSobj = crs(nlcd2016))
## merge cattle data with county shapefiles
counties_cattle <- merge(us_counties, cattle_county, by = "NAME")
#us_counties$area_of_pasture <- extract(nlcd_3000m_pasture, us_counties, fun = sum)
#writeOGR(COcounties, "/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/colorado_counties_pasturecattle.shp")
VA_counties_cattle <- read.csv("G:/My Drive/EEID/Foot and Mouth Disease/virginia_county_cattle.csv")
names(VA_counties_cattle)[2] <- "NAME"
VA_counties <- VA_counties[order(VA_counties$NAME),]
VA_counties$cattle <- VA_counties_cattle$cattle
VA_counties$NAME <- as.character(VA_counties$NAME)
VA_counties$NAME <- as.character(VA_counties_cattle$NAME)
VA_counties2 <- merge(VA_counties, VA_counties_cattle, by = 'NAME')


## STATE
## set up nlcd data by state
states <- readOGR("G:/My Drive/EEID/Foot and Mouth Disease/us_lower_48_states.shp")
virginia <- states[states$STATE_NAME == 'Virginia',]
virginia <- spTransform(virginia, CRSobj = crs(nlcd2016))
virginia_nlcd <- crop(nlcd2016, virginia)
virginia_nlcd <- mask(virginia_nlcd, virginia)
virginia_nlcd[virginia_nlcd == 81] <- 1
virginia_nlcd[virginia_nlcd == 71] <- 1
virginia_nlcd[virginia_nlcd > 1] <- 0
VA_nlcd_3000m_pasture <- aggregate(virginia_nlcd, fact = 100, fun = 'sum')
writeRaster(VA_nlcd_3000m_pasture, "G:/My Drive/EEID/Foot and Mouth Disease/VA_nlcd_3000m.img")
VA_nlcd_3000m_pasture <- raster("G:/My Drive/EEID/Foot and Mouth Disease/states/VA_nlcd_3000m.img")

## ZIP CODE
## read in data
zip <- readOGR("G:/My Drive/EEID/Foot and Mouth Disease/tl_2015_us_zcta510.shp")
zip <- spTransform(zip, CRSobj = crs(nlcd2016))
VA_zip <- crop(zip, virginia)
VA_zip$Zip.Code <- VA_zip$ZCTA5CE10
## merge cattle data with zip code shapefiles
zipcode_cattle <- merge(VA_zip, cattle_zip, by = "Zip.Code")
## distribute cattle across zip code region
zipcode_cattle$cattle <- as.numeric(zipcode_cattle$cattle)
zipcode_cattle$ALAND10 <- as.numeric(zipcode_cattle$ALAND10)
zipcode_cattle$ALAND10 <- sqrt(zipcode_cattle$ALAND10)
zipcode_cattle$cattle <- as.character(zipcode_cattle$cattle)
zipcode_cattle$cattle[is.na(zipcode_cattle$cattle)] <- 0
zipcode_cattle$cattle <- as.numeric(zipcode_cattle$cattle)
zipcode_cattle$cattle <- zipcode_cattle$cattle/zipcode_cattle$ALAND10
## create raster
zip_cattle <- rasterize(zipcode_cattle, VA_nlcd_3000m_pasture, field = "cattle", fun = 'last')
plot(zip_cattle)
writeRaster(zip_cattle, "G:/My Drive/EEID/Foot and Mouth Disease/VA_zip_cattle.tif", overwrite = TRUE)
zip_cattle

## COUNTY
us_counties <- readOGR("G:/My Drive/EEID/Foot and Mouth Disease/us_lower_48_counties.shp")
VA_counties <- crop(us_counties, virginia)
VA_counties$NAME <- VA_counties$County
# merge cattle data with county shapefiles
VA_counties$area_of_pasture <- extract(VA_nlcd_3000m_pasture, VA_counties, fun = sum)
writeOGR(VA_zip_cattle, "G:/My Drive/EEID/Foot and Mouth Disease/VA_nlcd_3000m_pasture")
VA_counties_cattle <- read.csv("G:/My Drive/EEID/Foot and Mouth Disease/VA_county_cattle.csv")
#virginia_cattle <- counties_cattle[counties_cattle$State == 'Virginia',]
names(virginia_cattle)[2] <- "NAME"
VA_counties_cattle <- merge(VA_counties, cattle_county, by = "NAME")
VAcounties <- COcounties[order(COcounties$NAME),]
COcounties$cattle <- colorado_cattle$cattle
COcounties$NAME <- as.character(COcounties$NAME)
colorado_cattle$NAME <- as.character(colorado_cattle$NAME)
COcounties2 <- left_join(COcounties@data, colorado_cattle, by = 'NAME')



