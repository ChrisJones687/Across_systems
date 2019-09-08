library(doParallel)
library(raster)
library(ggplot2)
infected_file <- "C:/Users/cmjone25/Desktop/Across_systems/wheat stripe rust/Hyslop/Hyslop_infected.tif"
host_file <- "C:/Users/cmjone25/Desktop//Across_systems/wheat stripe rust/Hyslop/Hyslop_host.tif"
total_plants_file <- "C:/Users/cmjone25/Desktop//Across_systems/wheat stripe rust/Hyslop/Hyslop_total.tif"
temperature_file <- ""
temperature_coefficient_file <- ""
precipitation_coefficient_file <-""
use_lethal_temperature <- FALSE
temp <- FALSE
precip <- FALSE
season_month_start <- 1
season_month_end <- 1
time_step <- "month"
start_time <- 1961
end_time <- 2019
lethal_temperature <- 0
lethal_temperature_month <- 1
random_seed <- 42
reproductive_rate <- 0.55
treatments_file <- ""
treatment_years <- c(0)
treatment_month <- 12
treatment_method <- "ratio"
management <- FALSE
mortality_on <- FALSE
mortality_rate <- 0
mortality_time_lag <- 0
percent_natural_dispersal <- 1.0
natural_kernel_type <- "cauchy"
anthropogenic_kernel_type <- "cauchy"
natural_distance_scale <- 10
anthropogenic_distance_scale <- 0.0
natural_dir <- "W"
natural_kappa <- 0.15
anthropogenic_dir <- "NONE"
anthropogenic_kappa <- 0

host <- raster(host_file)

core_count <- 3
cl <- makeCluster(core_count)
registerDoParallel(cl)

infected_stack <- foreach::foreach(i = 1:1000, .combine = c, .packages = c("raster", "PoPS"), .export = ls(globalenv())) %dopar% {
  data <- PoPS::pops(infected_file, host_file, total_plants_file, 
                     temp, temperature_coefficient_file, 
                     precip, precipitation_coefficient_file, 
                     time_step, reproductive_rate,
                     season_month_start, season_month_end, 
                     start_time, end_time, 
                     use_lethal_temperature, temperature_file,
                     lethal_temperature, lethal_temperature_month,
                     mortality_on, mortality_rate, mortality_time_lag, 
                     management, treatment_years, treatments_file,
                     treatment_method, treatment_month,
                     percent_natural_dispersal,
                     natural_kernel_type, anthropogenic_kernel_type,
                     natural_distance_scale, anthropogenic_distance_scale,
                     natural_dir, natural_kappa, 
                     anthropogenic_dir, anthropogenic_kappa,
                     random_seed = NULL)
  
  disease_severity <- raster::stack(lapply(1:length(data$infected), function(i) host))
  
  for (q in 1:raster::nlayers(disease_severity)) {
    disease_severity[[q]] <- data$infected[[q]]
  }
  
  number_infected <- data$number_infected
  spread_rate <- data$rates
  infected_area <- data$area_infected
  data <- list(disease_severity, number_infected, infected_area, spread_rate)
}

stopCluster(cl)
probability_runs <- infected_stack[seq(1,length(infected_stack),4)]
number_infected_runs <- infected_stack[seq(2,length(infected_stack),4)]
area_infected_runs <- infected_stack[seq(3,length(infected_stack),4)]
spread_rate_runs <- infected_stack[seq(4,length(infected_stack),4)]

prediction <- probability_runs[[1]]
prediction[prediction > 0] <- 0

for (i in 1:length(probability_runs)) {
  prediction <- prediction + probability_runs[[i]]
}

probability <- (prediction/(length(probability_runs)))
plot(probability)

probability_end <- probability[[58]]

data <- data.frame(distance = seq(0,110,2.5), disease_severity = 0)
for (i in 1:ncol(probability_end)) {
  data$disease_severity[i] <- mean(probability_end[seq(1,nrow(probability_end),1), i])
  data$disease_severity_sd[i] <- sd(probability_end[seq(1,nrow(probability_end),1), i])
}

p <- ggplot(data, aes(x=distance, y=disease_severity)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=disease_severity-disease_severity_sd, ymax=disease_severity+disease_severity_sd), width=.2,
                position=position_dodge(0.05))
p
