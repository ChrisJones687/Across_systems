### Evenly distribute cattle in pasture by counties for FMD
library(raster)
library(rgdal)
## Virginia (counties)
# Read in data files
counties <- readOGR("G:/My Drive/EEID/Foot and Mouth Disease/us_lower_48_counties.shp")
VA_counties <- counties[counties$STATE_NAME == 'Virginia',]
virginia_nlcd_3000m <- raster("G:/My Drive/EEID/Foot and Mouth Disease/states/VA_nlcd_3000m.img")
VA_counties <- spTransform(VA_counties, CRSobj = crs(virginia_nlcd_3000m))
virginia_cattle <- read.csv("G:/My Drive/EEID/Foot and Mouth Disease/virginia_county_cattle.csv")

# Organize and reorder data
names(virginia_cattle)[2] <- "NAME"
VA_counties <- VA_counties[order(VA_counties$NAME),]
VA_counties$cattle <- virginia_cattle$cattle
VA_counties$area <- virginia_cattle$area
VA_counties$cattle_per_area <- virginia_cattle$cattle_per_area
VA_counties$NAME <- as.character(VA_counties$NAME)
virginia_cattle$NAME <- as.character(virginia_cattle$NAME)
VA_counties$area <- as.numeric(VA_counties$area)

# Assign cattle for each raster cell based on county data
cell_num <- Which(virginia_nlcd_3000m==0, cells = TRUE)
cell_num <- as.numeric(cell_num)
for (i in 1:134) {
  ex <- extract(virginia_nlcd_3000m, VA_counties[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  virginia_nlcd_3000m[ex] <- ceiling(virginia_nlcd_3000m[ex]*VA_counties$cattle_per_area[i])
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
writeRaster(infected, "G:/My Drive/EEID/Foot and Mouth Disease/states/virginia/infected.tif", overwrite = TRUE)
