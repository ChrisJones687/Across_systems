library("raster")
library("sp")
library("readxl")


#####Hyslop

num_cols <- 45
num_rows <- 3
resolution <- 2.5
host <- matrix(100, nrow = num_rows, ncol = num_cols)
host <- raster(host, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
plot(host)
writeRaster(host, "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/Across_systems/wheat stripe rust/Hyslop/Hyslop_host.tif")

num_cols <- 45
num_rows <- 3
resolution <- 2.5
total <- matrix(100, nrow = num_rows, ncol = num_cols)
total <- raster(total, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
plot(total)
writeRaster(total, "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/Across_systems/wheat stripe rust/Hyslop/Hyslop_total.tif")


num_cols <- 45
num_rows <- 3
resolution <- 2.5
infected <- matrix(0, nrow = num_rows, ncol = num_cols)
infected <- raster(infected, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
infected[2,23]<-1
plot(infected)
writeRaster(infected, "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/Across_systems/wheat stripe rust/Hyslop/Hyslop_infected.tif", overwrite=TRUE)

###Hyslop culling
### single culling treatment
num_cols <- 45
num_rows <- 3
resolution <- 2.5
singlecull_treat <- matrix(0, nrow = num_rows, ncol = num_cols)

singlecull_treat <- raster(singlecull_treat, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
singlecull_treat[c(1:3),c(22:24)]<-1
plot(singlecull_treat)

writeRaster(singlecull_treat, "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/Across_systems/wheat stripe rust/Hyslop/singlecull_treat.tif")

### larger culling treatment
num_cols <- 45
num_rows <- 3
resolution <- 2.5
singlecull_treat <- matrix(0, nrow = num_rows, ncol = num_cols)

singlecull_treat <- raster(singlecull_treat, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
singlecull_treat[c(1:3),c(20:26)]<-1
plot(singlecull_treat)

writeRaster(singlecull_treat, "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/Across_systems/wheat stripe rust/Hyslop/largercull_treat.tif")



####Fungicide treatments
###Fungicide protection of the entire plot

num_cols <- 45
num_rows <- 3
resolution <- 2.5
Fungicide_Protection_entire <- matrix(1, nrow = num_rows, ncol = num_cols)
Fungicide_Protection_entire <- raster(Fungicide_Protection_entire, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
plot(Fungicide_Protection_entire)
writeRaster(host, "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/Across_systems/wheat stripe rust/Hyslop/Fungicide_Protection_entire.tif")


#####Mixture

num_cols <- 96
num_rows <- 32
resolution <- 2.5
host <- matrix(25, nrow = num_rows, ncol = num_cols)
host <- raster(host, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
plot(host)
writeRaster(host, "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/Across_systems/wheat stripe rust/mix_host.tif")

num_cols <- 96
num_rows <- 32
resolution <- 2.5
total <- matrix(100, nrow = num_rows, ncol = num_cols)
total <- raster(total, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
plot(total)
writeRaster(total, "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/Across_systems/wheat stripe rust/mix_total.tif")


num_cols <- 96
num_rows <- 32
resolution <- 2.5
infected <- matrix(0, nrow = num_rows, ncol = num_cols)
infected <- raster(infected, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
infected[c(16,17),c(8,9)]<-100
plot(infected)
writeRaster(infected, "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/Across_systems/wheat stripe rust/mix_infected.tif")

#####Pure

num_cols <- 96
num_rows <- 32
resolution <- 2.5
host <- matrix(100, nrow = num_rows, ncol = num_cols)
host <- raster(host, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
plot(host)
writeRaster(host, "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/Across_systems/wheat stripe rust/Pure_host.tif")

num_cols <- 96
num_rows <- 32
resolution <- 2.5
total <- matrix(100, nrow = num_rows, ncol = num_cols)
total <- raster(total, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
plot(total)
writeRaster(total, "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/Across_systems/wheat stripe rust/Pure_total.tif")


num_cols <- 96
num_rows <- 32
resolution <- 2.5
infected <- matrix(0, nrow = num_rows, ncol = num_cols)
infected <- raster(infected, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
infected[c(16,17),c(8,9)]<-1
plot(infected)
writeRaster(infected, "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/Across_systems/wheat stripe rust/Pure_infected.tif", overwrite = TRUE)

## set for actual infections_mixture
num_cols <- 96
num_rows <- 32
resolution <- 2.5
infected_w1 <- matrix(0, nrow = num_rows, ncol = num_cols)
infected_w1 <- raster(infected_w1, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
plot(infected_w1)
infected_w2 <- infected_w3 <- infected_w4 <- infected_w5 <- infected_w1
real_wsr_infections <- read_excel('Mixture_Interpolation.xlsx', sheet= 'week5')


for (i in 8:(num_cols-7)){
  infected_w5[seq(1,nrow(infected),1), i]<- real_wsr_infections$Disease[i]
}
plot(infected_w5)
infected_w5[seq(1,nrow(infected),1), c(1:7)]<- 0
infected_w5[seq(1,nrow(infected),1), c((num_cols-7):num_cols)]<- 0
plot(infected_w5)
infected_weeks <- stack(infected_w1, infected_w2, infected_w3, infected_w4, infected_w5)
infected_weeks[is.na(infected_weeks)] <- 0
plot(infected_weeks)
writeRaster(infected_weeks, "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/Across_systems/wheat stripe rust/real_infected.tif")


## set for actual infections_Pure
num_cols <- 96
num_rows <- 32
resolution <- 2.5
infected_w1 <- matrix(0, nrow = num_rows, ncol = num_cols)
infected_w1 <- raster(infected_w1, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
plot(infected_w1)
infected_w2 <- infected_w3 <- infected_w4 <- infected_w5 <- infected_w1
real_wsr_infections <- read_excel('Pure_Interpolation.xlsx', sheet= 'week5')


for (i in 8:(num_cols-7)){
  infected_w5[seq(1,nrow(infected),1), i]<- real_wsr_infections$Disease[i]
}
plot(infected_w5)
infected_w5[seq(1,nrow(infected),1), c(1:7)]<- 0
infected_w5[seq(1,nrow(infected),1), c((num_cols-7):num_cols)]<- 0
plot(infected_w5)


infected_weeks <- stack(infected_w1, infected_w2, infected_w3, infected_w4, infected_w5)
infected_weeks[is.na(infected_weeks)] <- 0
plot(infected_weeks)
writeRaster(infected_weeks, "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/Across_systems/wheat stripe rust/real_infected for Purelines.tif")



###Raster file for masking

num_cols <- 96
num_rows <- 32
resolution <- 2.5
mask <- matrix(0, nrow = num_rows, ncol = num_cols)
mask <- raster(mask, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
plot(mask)
mask[seq(1,nrow(infected),1), c(1:7)]<- NA
mask[seq(1,nrow(infected),1), c((num_cols-15):num_cols)]<- NA


writeRaster(mask, "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/Across_systems/wheat stripe rust/mask.tif", overwrite=TRUE )





## set for actual infections raw code
num_cols <- 96
num_rows <- 32
resolution <- 2.5
infected_w1 <- matrix(0, nrow = num_rows, ncol = num_cols)
infected_w1 <- raster(infected_w1, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
plot(infected_w1)
infected_w2 <- infected_w3 <- infected_w4 <- infected_w5 <- infected_w1
real_wsr_infections <- read_excel('Mixture_Interpolation.xlsx', sheet= 'week1')


for (i in 8:(num_cols-7)){
  infected_w1[seq(1,nrow(infected),1), i]<- real_wsr_infections$Disease[i]
}
plot(infected_w1)
infected_w1[seq(1,nrow(infected),1), c(1:7)]<- 0
infected_w1[seq(1,nrow(infected),1), c((num_cols-7):num_cols)]<- 0
plot(infected_w1)
infected_w1[1,num_cols-9]
infected_weeks <- stack(infected_w1)


s <- seq(8, num_cols-8, 1)

#########Stat after calibration###########
params_pure_lines<-read.csv("params_pure_lines.csv")

table(params_pure_lines$reproductive_rate)/nrow(params_pure_lines)*100
table(params_pure_lines$natural_distance_scale)/nrow(params_pure_lines)*100
