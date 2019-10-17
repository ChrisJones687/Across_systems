### 2001 UK epidemic model in Cumbria
library(raster)
library(sf)
library(rgdal)

# Read in data
cumbria_all_farms <- read.csv("G:/My Drive/EEID/Foot and Mouth Disease/cumbria/cumbria_all_farms.csv")
cumbria_infected_farms <- read.csv("G:/My Drive/EEID/Foot and Mouth Disease/cumbria/cumbria_uk.csv")
uk_counties <- readOGR("G:/My Drive/EEID/Foot and Mouth Disease/cumbria/bdline_essh_gb/Data/GB/county_region.shp")
uk_raster <- raster("G:/My Drive/EEID/Foot and Mouth Disease/cumbria/minisc_gb/minisc_gb/data/RGB_TIF_compressed/MiniScale_(mono)_R21.tif")

# Organize data
cumbria_all_farms$x <- as.numeric(cumbria_all_farms$x)
cumbria_all_farms$y <- as.numeric(cumbria_all_farms$y)
cumbria_all_farms$cattle <- as.numeric(cumbria_all_farms$cattle)
#uk_counties <- spTransform(uk_counties, CRSobj = crs(uk_raster))
# Crop UK files to Cumbria boundary
cumbria <- uk_counties[uk_counties$NAME == 'Cumbria County',]
cumbria_raster <- crop(uk_raster, cumbria)
cumbria_raster <- mask(cumbria_raster, cumbria)
cumbria_farm_locations <- cbind(cumbria_all_farms$x, cumbria_all_farms$y)
cumbria_cattle <- cbind(cumbria_all_farms$cattle)
# Create raster of farm locations and cattle in Cumbria
cumbria_all_farms_cattle <- rasterize(cumbria_farm_locations, cumbria_raster, cumbria_cattle, fun = sum)
cumbria_all_farms_cattle[cumbria_all_farms_cattle==0] <- NA
writeRaster(cumbria_all_farms_cattle, "G:/My Drive/EEID/Foot and Mouth Disease/cumbria/cumbria_all_farms_cattle.img", overwrite = TRUE)

# Write out host, total hosts, and infected files
writeRaster(cumbria_all_farms_cattle, "G:/My Drive/EEID/Foot and Mouth Disease/cumbria/host.tif", overwrite = TRUE)
cumbria_all_farms_cattle[cumbria_all_farms_cattle >=0] <- maxValue(cumbria_all_farms_cattle)
writeRaster(cumbria_all_farms_cattle, "G:/My Drive/EEID/Foot and Mouth Disease/cumbria/total_hosts.tif", overwrite = TRUE)
host <- raster("G:/My Drive/EEID/Foot and Mouth Disease/cumbria/host.tif")
host[host==1593]
infected <- host
infected[infected<1593] <- 0
infected[infected==1593] <- 1
infected[infected==0] <- NA
infected[infected==1]
plot(infected)
writeRaster(infected, "G:/My Drive/EEID/Foot and Mouth Disease/cumbria/infected.tif", overwrite = TRUE)

# Create infected years file for calibrate function
# Organize data
cumbria_infected_farms$x <- as.numeric(cumbria_infected_farms$x)
cumbria_infected_farms$y <- as.numeric(cumbria_infected_farms$y)
cumbria_infected_farms$cattle <- as.numeric(cumbria_infected_farms$cattle)
#uk_counties <- spTransform(uk_counties, CRSobj = crs(uk_raster))
# Crop UK files to Cumbria boundary
cumbria <- uk_counties[uk_counties$NAME == 'Cumbria County',]
cumbria_raster <- crop(uk_raster, cumbria)
cumbria_raster <- mask(cumbria_raster, cumbria)
cumbria_farm_locations <- cbind(cumbria_infected_farms$x, cumbria_infected_farms$y)
#cumbria_cattle <- cbind(cumbria_infected_farms$cattle)
# Create raster of farm locations and cattle in Cumbria
cumbria_infected_farms_cattle <- rasterize(cumbria_farm_locations, cumbria_raster)
cumbria_infected_farms_cattle[cumbria_infected_farms_cattle==0] <- NA
writeRaster(cumbria_infected_farms_cattle, "G:/My Drive/EEID/Foot and Mouth Disease/cumbria/cumbria_infected_farms_cattle.img", overwrite = TRUE)
# Write out infected years file
writeRaster(cumbria_infected_farms_cattle, "G:/My Drive/EEID/Foot and Mouth Disease/cumbria/infected_2001.tif", overwrite = TRUE)

