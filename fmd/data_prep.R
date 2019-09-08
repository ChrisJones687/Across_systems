### Data preparation for state cattle data at the county level ###
library(raster)
library(sp)
library(rgdal)
library(dplyr)
library(fuzzyjoin)
fmd <- read.csv("/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/FMD dummy.csv")

### Texas (state)
nlcd2016 <- raster("/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/NLCD_2016_Land_Cover_L48_20190424.img")
states <- readOGR("/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/us_lower_48_states.shp")
texas <- states[states$STATE_NAME == 'Texas',]
texas <- spTransform(texas, CRSobj = crs(nlcd2016))

texas_nlcd <- crop(nlcd2016, texas)
texas_nlcd <- mask(texas_nlcd, texas)
texas_nlcd[texas_nlcd != c(81,71)] <-0
writeRaster(texas_nlcd81, "/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/texas_pasture.tif")

### Colorado (state)
nlcd2016 <- raster("/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/NLCD_2016_Land_Cover_L48_20190424.img")
states <- readOGR("/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/us_lower_48_states.shp")
colorado <- states[states$STATE_NAME == 'Colorado',]
colorado <- spTransform(colorado, CRSobj = crs(nlcd2016))

colorado_nlcd <- crop(nlcd2016, colorado)
colorado_nlcd <- mask(colorado_nlcd, colorado)
colorado_nlcd[colorado_nlcd == 81] <- 1
colorado_nlcd[colorado_nlcd == 71] <- 1
colorado_nlcd[colorado_nlcd > 1] <- 0

writeRaster(colorado_nlcd, "/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/colorado_pasture.tif")
colorado_nlcd_300m <- aggregate(colorado_nlcd, fact = 10, fun = 'sum')
writeRaster(colorado_nlcd_300m, "/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/colorado_pasture_300m.tif")

### Colorado (counties)
counties <- readOGR("/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/us_lower_48_counties.shp")
COcounties <- counties[counties$STATE_NAME == 'Colorado',]
colorado_nlcd_300m <- raster("/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/colorado_pasture_300m.tif")
COcounties <- spTransform(COcounties, CRSobj = crs(colorado_nlcd_300m))
COcounties$area_of_pasture <- extract(colorado_nlcd_300m, COcounties, fun = sum)
# COcounties_pasture <- extract(colorado_nlcd_300m, COcounties, fun = sum)
#writeOGR(COcounties, "/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/colorado_counties_pasturecattle.shp")
colorado_cattle <- read.csv("/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/colorado.csv")
names(colorado_cattle)[1] <- "NAME"
COcounties <- COcounties[order(COcounties$NAME),]
COcounties$cattle <- colorado_cattle$cattle
COcounties$NAME <- as.character(COcounties$NAME)
colorado_cattle$NAME <- as.character(colorado_cattle$NAME)
COcounties2 <- left_join(COcounties@data, colorado_cattle, by = 'NAME')

# Distribute cattle across the county area
COcounties$cattle <- as.numeric(COcounties$cattle)
COcounties$area_of_pasture <- as.numeric(COcounties$area_of_pasture)
COcounties$cattleperarea <- COcounties$cattle/COcounties$area_of_pasture
# Average number of cattle in 30 x 30 m of pasture/grassland in the county
COcounties$cattleperarea
plot(colorado_nlcd_300m)
plot(COcounties, add = TRUE)

# Extract the values for the number of cattle for each county
for (i in 1:64) {
ex <- extract(colorado_nlcd_300m, COcounties[i,], cellnumbers = TRUE)
ex <- data.frame(ex)
ex <- ex$cell
colorado_nlcd_300m[ex] <- ceiling(colorado_nlcd_300m[ex]*COcounties$cattleperarea[i]*100)
}
colorado_nlcd_300m[colorado_nlcd_300m==0] <- NA
colorado_nlcd_300m <- mask(colorado_nlcd_300m, colorado)
plot(colorado_nlcd_300m)

# Write out host, total hosts, and infected files
writeRaster(colorado_nlcd_300m, "/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/host.tif")
colorado_nlcd_300m[colorado_nlcd_300m >=0] <- maxValue(colorado_nlcd_300m)
writeRaster(colorado_nlcd_300m, "/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/total_hosts.tif")
host <- raster("/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/host.tif")
host[host==27]
infected <- host
infected[infected<27] <- 0
infected[infected==27] <- 1
infected[infected==0] <- NA
infected[infected ==1]
plot(infected)
writeRaster(infected, "/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/infected.tif")
