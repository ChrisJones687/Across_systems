library(raster)
library(sp)
library(rgdal)
library(dplyr)
library(fuzzyjoin)
fmd <- read.csv("H:/My Drive/EEID/Foot and Mouth Disease/FMD dummy.csv")

nlcd2016 <- raster("H:/My Drive/EEID/Foot and Mouth Disease/NLCD_2016_Land_Cover_L48_20190424.img")
states <- readOGR("H:/My Drive/EEID/Foot and Mouth Disease/us_lower_48_states.shp")
texas <- states[states$STATE_NAME == 'Texas',]
texas <- spTransform(texas, CRSobj = crs(nlcd2016))

texas_nlcd <- crop(nlcd2016, texas)
texas_nlcd <- mask(texas_nlcd, texas)
texas_nlcd[texas_nlcd != c(81,71)] <-0
writeRaster(texas_nlcd81, "H:/My Drive/EEID/Foot and Mouth Disease/texas_pasture.tif")

### Colorado
nlcd2016 <- raster("H:/My Drive/EEID/Foot and Mouth Disease/NLCD_2016_Land_Cover_L48_20190424.img")
states <- readOGR("H:/My Drive/EEID/Foot and Mouth Disease/us_lower_48_states.shp")
colorado <- states[states$STATE_NAME == 'Colorado',]
colorado <- spTransform(colorado, CRSobj = crs(nlcd2016))

colorado_nlcd <- crop(nlcd2016, colorado)
colorado_nlcd <- mask(colorado_nlcd, colorado)
colorado_nlcd[colorado_nlcd == 81] <- 1
colorado_nlcd[colorado_nlcd == 71] <- 1
colorado_nlcd[colorado_nlcd > 1] <- 0

writeRaster(colorado_nlcd, "H:/My Drive/EEID/Foot and Mouth Disease/colorado_pasture.tif")
colorado_nlcd_300m <- aggregate(colorado_nlcd, fact = 10, fun = 'sum')
writeRaster(colorado_nlcd_300m, "H:/My Drive/EEID/Foot and Mouth Disease/colorado_pasture_300m.tif")

### Colorado counties
counties <- readOGR("H:/My Drive/EEID/Foot and Mouth Disease/us_lower_48_counties.shp")
COcounties <- counties[counties$STATE_NAME == 'Colorado',]
colorado_nlcd_300m <- raster("H:/My Drive/EEID/Foot and Mouth Disease/colorado_pasture_300m.tif")
COcounties <- spTransform(COcounties, CRSobj = crs(colorado_nlcd_300m))
COcounties$area_of_pasture <- extract(colorado_nlcd_300m, COcounties, fun = sum)
# COcounties_pasture <- extract(colorado_nlcd_300m, COcounties, fun = sum)
writeOGR(COcounties, "H:/My Drive/EEID/Foot and Mouth Disease/colorado_counties_pasturecattle.shp")
colorado_cattle <- read.csv("H:/My Drive/EEID/Foot and Mouth Disease/colorado.csv")
names(colorado_cattle)[1] <- "NAME"
COcounties <- COcounties[order(COcounties$NAME),]
COcounties2 <- left_join(COcounties@data, colorado_cattle, by = 'NAME')
COcounties$cattle <- colorado_cattle$cattle
COcounties$NAME <- as.character(COcounties$NAME)
colorado_cattle$NAME <- as.character(colorado_cattle$NAME)

COcounties$cattleperarea <- COcounties$cattle/COcounties$area_of_pasture
COcounties$cattleperarea
plot(colorado_nlcd)
