library(raster)
library(sp)
library(rgdal)
fmd <- read.csv("/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/FMD dummy.csv")

nlcd2016 <- raster("/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/NLCD_2016_Land_Cover_L48_20190424.img")
states <- readOGR("/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/us_lower_48_states.shp")
texas <- states[states$STATE_NAME == 'Texas',]
texas <- spTransform(texas, CRSobj = crs(nlcd2016))

texas_nlcd <- crop(nlcd2016, texas)
texas_nlcd <- mask(texas_nlcd, texas)
texas_nlcd[texas_nlcd != c(81,71)] <-0
writeRaster(texas_nlcd81, "/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/texas_pasture.tif")

### Colorado
nlcd2016 <- raster("/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/NLCD_2016_Land_Cover_L48_20190424.img")
states <- readOGR("/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/us_lower_48_states.shp")
colorado <- states[states$STATE_NAME == 'Colorado',]
colorado <- spTransform(colorado, CRSobj = crs(nlcd2016))

colorado_nlcd <- crop(nlcd2016, colorado)
colorado_nlcd <- mask(colorado_nlcd, colorado)
colorado_nlcd[colorado_nlcd != c(81,71)] <-0
writeRaster(colorado_nlcd81, "/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/colorado_pasture.tif")

### Colorado counties
counties <- readOGR("/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/COUNTY DATA.shp")
COcounties <- counties[counties$STATE_NAME == 'Colorado']

COcounties_pasture <- extract(colorado_nlcd300m, COcounties, fun = 'sum')
colorado_cattle <- read.csv("/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/colorado.csv")

