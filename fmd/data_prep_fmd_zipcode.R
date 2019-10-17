### Evenly distribute cattle in pasture by zip codes for FMD
library(raster)
library(rgdal)
library(rgeos)
library(dplyr)
## Virginia (zip code)
# Read in data files
zip <- readOGR("G:/My Drive/EEID/Foot and Mouth Disease/tl_2015_us_zcta510.shp")
us_zip <- read.csv("G:/My Drive/EEID/Foot and Mouth Disease/uszips.csv")
zip$GEOID10 <- as.character(zip$GEOID10)
us_zip$GEOID10 <- as.character(us_zip$GEOID10)
us_zip2 <- left_join(zip@data, us_zip, by = 'GEOID10')


#zip <- merge(zip@data, us_zip, by = "GEOID10")
#zip@data$NAME <- as.character(zip@data$NAME)
#zip@data[zip@data == "NA"] <- "NONE"
#virginia_zip <- zip[zip$State == 'VIRGINIA',]


#virginia_zip <- read.csv("G:/My Drive/EEID/Foot and Mouth Disease/virginia_zipcodes.csv")
virginia_nlcd_3000m <- raster("G:/My Drive/EEID/Foot and Mouth Disease/states/VA_nlcd_3000m.img")
#states <- readOGR("G:/My Drive/EEID/Foot and Mouth Disease/us_lower_48_states.shp")
#virginia <- states[states$STATE_NAME == 'Virginia',]
virginia_zip <- spTransform(virginia_zip, CRSobj = crs(virginia_nlcd_3000m))
virginia <- spTransform(virginia, CRSobj = crs(virginia_nlcd_3000m))
zip <- as(zip, "SpatialPolygons")
VA_zip <- mask(zip, virginia)
VA_zip <- spTransform(VA_zip, CRSobj = crs(virginia_nlcd_3000m))
virginia_cattle <- read.csv("G:/My Drive/EEID/Foot and Mouth Disease/fmd_zipcode.csv")

# Organize and reorder data
names(virginia_cattle)[2] <- "NAME"
VA_zip <- VA_zip[order(VA_zip$NAME),]
VA_zip$cattle <- virginia_cattle$cattle
VA_zip$area <- virginia_cattle$area
VA_zip$cattle_per_area <- virginia_cattle$cattle_per_area
VA_zip$NAME <- as.character(VA_zip$NAME)
virginia_cattle$NAME <- as.character(virginia_cattle$NAME)
VA_zip$area <- as.numeric(VA_zip$area)

# Assign cattle for each raster cell based on county data
cell_num <- Which(virginia_nlcd_3000m==0, cells = TRUE)
cell_num <- as.numeric(cell_num)
for (i in 1:134) {
  ex <- extract(virginia_nlcd_3000m, VA_zip[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  virginia_nlcd_3000m[ex] <- ceiling(virginia_nlcd_3000m[ex]*VA_zip$cattle_per_area[i])
}
states <- readOGR("G:/My Drive/EEID/Foot and Mouth Disease/us_lower_48_states.shp")
virginia <- states[states$STATE_NAME == 'Virginia',]
virginia <- spTransform(virginia, CRSobj = crs(virginia_nlcd_3000m))
virginia_nlcd_3000m <- mask(virginia_nlcd_3000m, virginia)
writeRaster(virginia_nlcd_3000m, "C:/Users/lantzra/Desktop/VA_nlcd_3000m_cattle.img", overwrite = TRUE)
# Update raster so the cells without pasture have zero cattle
VA_nlcd_3000m_cattle <- raster("C:/Users/lantzra/Desktop/VA_nlcd_3000m_cattle.img")
update(VA_nlcd_3000m_cattle, v=rep(0, 18923), cell=cell_num)
VA_nlcd_3000m_cattle <- mask(VA_nlcd_3000m_cattle, virginia)
writeRaster(VA_nlcd_3000m_cattle, "G:/My Drive/EEID/Foot and Mouth Disease/states/VA_nlcd_3000m_cattle.img", overwrite = TRUE)
VA_nlcd_3000m_cattle <- raster("G:/My Drive/EEID/Foot and Mouth Disease/states/VA_nlcd_3000m_cattle.img")

# Write out host, total hosts, and infected files
writeRaster(VA_nlcd_3000m_cattle, "G:/My Drive/EEID/Foot and Mouth Disease/states/virginia/host.tif", overwrite = TRUE)
VA_nlcd_3000m_cattle[VA_nlcd_3000m_cattle >=0] <- maxValue(VA_nlcd_3000m_cattle)
writeRaster(VA_nlcd_3000m_cattle, "G:/My Drive/EEID/Foot and Mouth Disease/states/virginia/total_hosts.tif", overwrite = TRUE)
host <- raster("G:/My Drive/EEID/Foot and Mouth Disease/states/virginia/host.tif")
host[host==2120]
infected <- host
infected[infected<2120] <- 0
infected[infected==2120] <- 1
infected[infected==0] <- NA
infected[infected==1]
plot(infected)
writeRaster(infected, "G:/My Drive/EEID/Foot and Mouth Disease/states/virginia/infected.tif")
