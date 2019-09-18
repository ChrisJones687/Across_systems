library(PoPS)
library(raster)

infected_file <- "H:/My Drive/EEID/Wheat Stripe Rust/PureLine/Pure_infected.tif"
host_file <- "H:/My Drive/EEID/Wheat Stripe Rust/PureLine/Pure_host.tif"
total_plants_file <- "H:/My Drive/EEID/Wheat Stripe Rust/PureLine/Pure_total.tif"
temperature_file <- ""
temperature_coefficient_file <- ""
precipitation_coefficient_file <-""
use_lethal_temperature <- FALSE
temp <- FALSE
precip <- FALSE
season_month_start <- 1
season_month_end <- 7
time_step <- "month"
start_time <- 2015
end_time <- 2019
lethal_temperature <- 0
lethal_temperature_month <- 1
random_seed <- 42
reproductive_rate <- .5
treatments_file <- ""
treatment_years <- c(2015)
treatment_month <- 4
treatment_method <- "ratio"
management <- FALSE
mortality_on <- FALSE
mortality_rate <- 0
mortality_time_lag <- 0
percent_natural_dispersal <- 1.0
natural_kernel_type <- "cauchy"
anthropogenic_kernel_type <- "cauchy"
natural_distance_scale <- 7
anthropogenic_distance_scale <- 0.0
natural_dir <- "NONE"
natural_kappa <- 0
anthropogenic_dir <- "NONE"
anthropogenic_kappa <- 0

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


head (data)
plot(raster(data$infected[[1]]))
plot(raster(host_file))
inf <- raster(host_file)
inf[] <- data$infected[[5]]
plot(inf)

real_infected<-stack("H:/My Drive/EEID/Wheat Stripe Rust/PureLine/real_infected for Purelines.tif")
plot(real_infected)
plot(real_infected[[5]])

diff <- real_infected[[5]] - inf[[1]]
plot(diff)

#########Stat for calibration###########
params_pure_lines<-read.csv("H:/My Drive/EEID/Wheat Stripe Rust/Parameter_values/params_pure_lines.csv")
head(params_pure_lines, 6)
table(params_pure_lines$reproductive_rate)/nrow(params_pure_lines)*100
table(params_pure_lines$natural_distance_scale)/nrow(params_pure_lines)*100

# 