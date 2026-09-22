#setwd()
library(tidyverse)
library(zoo)
library(sf)
library(terra)
library(exactextractr)

#load functions
source("functions.R")

# load data from HOSTRADA  ---------------------------------------------------------------
#data obtained from https://www.dwd.de/DE/leistungen/uhi/info_methodic/01_hostrada.html 
#preprocessed to be aggregated to district-level based on hourly population-weighted averages

##mean temp from HOSTRADA
mean_temp <- read_csv("data/hostrada_mean_tas_pop_weighted_2005_2025.csv")

##max temp from HOSTRADA
max_temp <- read_csv("data/hostrada_max_tas_pop_weighted_2005_2025.csv")

##dew point from HOSTRADA
hum <- read_csv("data/hostrada_mean_tdew_pop_weighted_2005_2025.csv")

#adjust format
mean_temp <- mean_temp %>% pivot_longer(cols = 2:ncol(mean_temp),
                            names_to = "date",
                            values_to = "mean_temp") 
max_temp <- max_temp %>% pivot_longer(cols = 2:ncol(max_temp),
                            names_to = "date",
                            values_to = "max_temp") 
hum <- hum %>% pivot_longer(cols = 2:ncol(hum),
                            names_to = "date",
                            values_to = "dew_point") 

## join
temp_dat <- left_join(mean_temp, max_temp, by=c("krs_code","date"))
temp_dat <- left_join(temp_dat, hum, by=c("krs_code","date"))

#rename
temp_dat <- temp_dat %>% 
  rename(AGS_N3_23 = krs_code) %>%
  mutate(AGS_N3_23 = as.integer(AGS_N3_23),
         date = as.Date(date),
         year = year(date),
         month = month(date))


# load reference temp data ------------------------------------------------
##load ref data from EOBS so we can calculate percentiles based on reference period
#obtained from: https://cds.climate.copernicus.eu/datasets/insitu-gridded-observations-europe?tab=overview

#aggregate to Germany districts through population-weighted averaging
# load shapefile for German districs (obtained from https://www.bkg.bund.de/)
shape_data <- st_read("data/vg5000_ebenen_1231/VG5000_KRS.shp")

# shapefile is in meters but temperature file is long/langitude
# transform shapefile
new_shp <- st_transform(shape_data, crs = "+proj=longlat +datum=WGS84")

## load human settlement data (1x1km grid, -200 = NA, 2015)
#obtained from https://human-settlement.emergency.copernicus.eu/ghs_pop2023.php 
HSL <- terra::rast("Data/GHS_POP_E2015_GLOBE_R2023A_54009_1000_V1_0.tif")

# transform shapefile in native Mollweide so it matches HSL
new_shp_moll <- st_transform(new_shp, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m")
e_moll       <- terra::ext(vect(new_shp_moll))       
HSL_crop     <- terra::crop(HSL, e_moll)
HSL_GER      <- project(HSL_crop, "EPSG:4326", method = "bilinear")

#-200 is NA
HSL_GER[HSL_GER == -200] <- NA

# aggregate meteorological information to district level
metric_list <- vector(mode = "list")

for (metric in c("tg", "hu")) {
  for (yrs in c( "1950-1964", "1965-1979","1980-1994")){

    version <- ifelse(metric == "hu", '_v33.0e.nc', '_v30.0e.nc')
    metric_list[[paste0(metric,"_",yrs)]] <- preprocess(file_path=paste0('data/', metric,
                                                                          '_ens_mean_0.1deg_reg_',
                                                                          yrs,version),
                                                        #measure=metric,
                                                        shape_file=new_shp)
}}

temp_ref <- do.call(cbind, metric_list)

#rm(metric_list);gc()

#attach AGS information from shapefile
temp_ref$AGS <- shape_data$AGS

#move AGS to front
temp_ref <- temp_ref[,c(ncol(temp_ref),
                        1:(ncol(temp_ref)-1))]

#put in right format to join with hospitalization data
temp_ref <- temp_ref %>%
  pivot_longer(
    cols = -1,  
    names_pattern = "^(\\w{2}).*(.{10})$", #first 2 and last 10 characters
    names_to = c("measurement","date"),
    values_to = c("temperature")) %>%
  pivot_wider(
    names_from = "measurement",
    values_from = "temperature")

##set reference period df (1950 to 1990, month 6 to 8)
temp_ref <- temp_ref %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         year = year(date),
         AGS = as.integer(AGS),
         month = month(date))  %>%
  rename(AGS_N3_23 = AGS) %>%
  filter(year >= 1961 & year <= 1990 & month %in% c(6:8))

# calculate heat index ----------------------------------------------------
#see functions.R for more information
comp_heatind <- heat_index(temperature = temp_dat$mean_temp,
                           dewpoint = temp_dat$dew_point,
                           dp = TRUE, dp_f = FALSE, t_f = FALSE)
#rename for clarity
comp_heatind <- comp_heatind %>% rename(apparent_temp = at_c,
                                        heat_index    = hi_c)

#join with temperature data 
temp_dat <- cbind(temp_dat, comp_heatind[,c("heat_index", "apparent_temp")])

##same for ref data but here we have relative humidity not dew point
comp_heatindref <- heat_index(temperature = temp_ref$tg,
                           rh = temp_ref$hu,
                           dp = FALSE, rh_original = FALSE, t_f = FALSE)
#rename for clarity
comp_heatindref <- comp_heatindref %>% rename(apparent_temp = at_c,
                                        heat_index    = hi_c)

#join with temperature data 
temp_ref <- cbind(temp_ref, comp_heatindref[,c("heat_index", "apparent_temp")])

# calculate cutoffs -------------------------------------------------------
#using the metric that were coded here
#inspo from 
#https://github.com/haskellcraigz/TEE-dataset/tree/main/code/02_metricconstruction
#https://www.dwd.de/DE/service/lexikon/Functions/glossar.html?lv3=624852&lv2=101094
## heat wave according to DWD temp over 98th percentile

# global temperature cut-offs
#DWD mentions 28degree as a cutoff, include that there
hottemp_val <- c(20, 24, 28, 30, 40)
hottemp_val_perc <- c(.90, .95, .98, .99)

#we dont need years before 2004
temp_aggr <- temp_dat %>%
  filter(year > 2004)

## calculate global cut offs
for (i in 1:length(hottemp_val)) {
  #mean temp
  temp_aggr$heatday <- ifelse(temp_aggr$mean_temp > hottemp_val[i], 1, 0)
  names(temp_aggr)[names(temp_aggr) == "heatday"] <- paste0("heatday_meanabove", as.character(hottemp_val[i]))

  #max temp
  temp_aggr$heatday <- ifelse(temp_aggr$max_temp > hottemp_val[i], 1, 0)
  names(temp_aggr)[names(temp_aggr) == "heatday"] <- paste0("heatday_maxabove", as.character(hottemp_val[i]))
  
  #heat index
  temp_aggr$heatday <- ifelse(temp_aggr$heat_index > hottemp_val[i], 1, 0)
  names(temp_aggr)[names(temp_aggr) == "heatday"] <- paste0("heatday_heatindexabove", as.character(hottemp_val[i]))
  
  #apparent temp
  temp_aggr$heatday <- ifelse(temp_aggr$apparent_temp > hottemp_val[i], 1, 0)
  names(temp_aggr)[names(temp_aggr) == "heatday"] <- paste0("heatday_apparenttempabove", as.character(hottemp_val[i]))
}


## calculate percentile cut offs
for (i in 1:length(hottemp_val_perc)) {
  temp <- temp_ref %>%
    group_by(AGS_N3_23) %>% 
    mutate(threshold_mean = quantile(tg, hottemp_val_perc[i], na.rm=TRUE),
           threshold_hi = quantile(heat_index, hottemp_val_perc[i], na.rm=TRUE),
           AGS_N3_23 = as.integer(AGS_N3_23)) %>% 
    ungroup() %>% 
    select(AGS_N3_23, threshold_mean, threshold_hi) %>% 
    distinct()
  
  temp_aggr <- left_join(temp_aggr, temp, by=c("AGS_N3_23"))
  
  #mean temp
  temp_aggr$heatday <- ifelse(temp_aggr$mean_temp > temp_aggr$threshold_mean, 1, 0)
  names(temp_aggr)[names(temp_aggr) == "heatday"] <- paste0("heatday_meanabove",  as.character(hottemp_val_perc[i]), "thperc")
  
  #heat index
  temp_aggr$heatday <- ifelse(temp_aggr$heat_index > temp_aggr$threshold_mean, 1, 0)
  names(temp_aggr)[names(temp_aggr) == "heatday"] <- paste0("heatday_heatindexabove", as.character(hottemp_val_perc[i]), "thperc")
  
  #rename thresholds so we can keep them in the dataset
  names(temp_aggr)[names(temp_aggr) == "threshold_mean"] <- paste0("threshold_meanabove", as.character(hottemp_val_perc[i]), "thperc")
  names(temp_aggr)[names(temp_aggr) == "threshold_hi"] <- paste0("threshold_hiabove", as.character(hottemp_val_perc[i]), "thperc")
  
  rm(temp)
  }

### save file 
write_csv(temp_aggr, file = "data/weather_data_Germany_HOSTRAD_hithreshold.csv")


