#setwd()
library(tidyverse)
library(lubridate)
library(data.table)
library(readxl)

load("data/weekcounts_byyear_v2.RData")

## the end categories of each year have very low counts 
## likely because of the holidays, ppl are either released before holidays or after holidays (in next year)
check_N <- weekcounts_v2 %>%
  group_by(dat_aufn_3day_cat) %>%
  summarise(case = mean(N_hospitalization, na.rm = T)) 

#keep only 3-day categories below cat 119 (119 corresponds to 23/24.12.,
# so now we are excluding the holiday period)
weekcounts_v2 <- subset(weekcounts_v2, weekcounts_v2$dat_aufn_3day_cat < 119)

# add INKAR ---------------------------------------------------------------
INKAR <- read.csv("data/INKAR_indicators_complete.csv")

weekcounts_INKAR <- left_join(weekcounts_v2, INKAR, by = c("AGS_N3_23","year"))

summary(weekcounts_INKAR) #no new missingness introduced

# Create dayofyear from 3-day group
weekcounts_INKAR$dayofyear_begin <- (weekcounts_INKAR$dat_aufn_3day_cat * 3) + 1
weekcounts_INKAR$dayofyear_end <- (weekcounts_INKAR$dat_aufn_3day_cat * 3) + 3

# Create calendar_date by adding dayofyear to January 1st of each year
weekcounts_INKAR$date_begin <- as.Date(paste0(weekcounts_INKAR$year, "-01-01")) + (weekcounts_INKAR$dayofyear_begin - 1)
weekcounts_INKAR$date_end <- as.Date(paste0(weekcounts_INKAR$year, "-01-01")) + (weekcounts_INKAR$dayofyear_end - 1)

# add GISD ----------------------------------------------------------------
#obtained from https://zenodo.org/records/14781119 
GISD <- read_excel("data/00_GISD_Bund.xlsx", sheet = "Kreis")

GISD <- GISD %>% mutate(AGS_N3_23 = as.numeric(region_id)) %>%
  select(year:AGS_N3_23)

weekcounts_INKAR <- left_join(weekcounts_INKAR, GISD, by=c("AGS_N3_23", "year"))

# add temperature ---------------------------------------------------------
##temporally reshape hospitalization data to daily so it is easier to deal with
hosp_daily <- weekcounts_INKAR %>%
  mutate(date_end = if_else(
    year(date_end) != year(date_begin),   # crosses into next year
    as.Date(paste0(year(date_begin), "-12-31")), # force to 31 Dec
    date_end)) %>%
  rowwise() %>%
  mutate(date = list(seq(date_begin, date_end, by = "day"))) %>%
  unnest(date)

#check if there's duplicates
check <- hosp_daily %>% group_by(AGS_N3_23, date) %>% summarise(n = n()) %>% filter(n > 1)

# read in temperature information
temp <- read_csv("data/weather_data_Germany_HOSTRAD_hithreshold.csv")

#calculate lags
temp <- temp %>%
  group_by(AGS_N3_23) %>%
  mutate(across(
    .cols = c("mean_temp", "heat_index", "apparent_temp"), 
    .fns = list(
      lag_day1 = ~lag(., 1),
      lag_day2 = ~lag(., 2),
      lag_day3 = ~lag(., 3),
      lag_day4 = ~lag(., 4),
      lag_day5 = ~lag(., 5),
      lag_day6 = ~lag(., 6),
      lag_day7 = ~lag(., 7),
      lag_day8 = ~lag(., 8),
      lag_day9 = ~lag(., 9),
      lag_day10 = ~lag(., 10))),
    across(
      .cols = c("dew_point"),
      .fns = list(
        lag_day1 = ~lag(., 1),
        lag_day2 = ~lag(., 2),
        lag_day3 = ~lag(., 3)),
      .names = "{.col}_{.fn}")) %>%
  ungroup()

# Convert to data.table to make merging everything faster
setDT(temp)

# Make sure both have date as Date class
temp[, date := as.Date(date)]

# Key on date for fast join
setkey(temp, AGS_N3_23, date)

# Left join in place
hosp_daily <- temp[hosp_daily, on = .(AGS_N3_23, date)]

#check if there's duplicates
check <- distinct(hosp_daily)

rm(check, temp); gc()

# add air pollution data  ---------------------------------------------------------------
#obtained from https://sites.wustl.edu/acag/surface-pm2-5/#V4.EU.03 
load("Data/PM25_month_95to2023.RData")

#rename
PM25.dat <- PM25.dat %>%
  rename(AGS_N3_23 = AGS) %>%
  mutate(AGS_N3_23 = as.integer(AGS_N3_23))

# Convert to data.table
setDT(PM25.dat)

# Key on date for fast join
setkey(PM25.dat, AGS_N3_23, month, year)

#create some more variables
hosp_daily <- hosp_daily %>%
  mutate(month = month(date),
         DOW = weekdays(date),
         weekend = ifelse(DOW %in% c("Saturday", "Sunday"), 1, 0))

# Left join in place
hosp_daily <- PM25.dat[hosp_daily, on = .(AGS_N3_23, year, month)]

# Aggregate back to 3-day level -------------------------------------------
#clean
rm(INKAR, 
  GISD, weekcounts_INKAR,
   weekcounts_v2, PM25.dat);gc()

#specify what to do which each variable
#take last value here so mean_temp in 3-day category format corresponds to
#temperature of last day of three day window
#therefore current temp, lag1, and lag2 describe temperature profile
#of each 3-day observation window
covariates <- c("mean_temp","max_temp", "dew_point","heat_index", "apparent_temp")

#count occurrence
count_occurence <- c(names(hosp_daily)[grepl("heatday_", names(hosp_daily))], "weekend")

#take unique (doesn't change)
uniques <- c(names(hosp_daily)[grepl("threshold", names(hosp_daily))])

#take first value of these covariates (numbers are the same
## across all three days because we artificially extended 3-day data to daily format)
covariates_fixed_temp <- c("N_hospitalization","N_population",
                           "Auslanderanteil","Schutzsuchende.an.Bevolkerung", "Schutzsuchende.an.Bevolkerung_corr", "Schutzsuchende.an.auslandischer.Bevolkerung","Asylbewerber","Einwohnerdichte","Einwohner.65.Jahre.und.alter",                 
                           "Einwohner.unter.6.Jahre","Frauenanteil","Empfanger.von.Pflegegeld","Arbeitslose","Pkw.Dichte","Waldflache","Wasserflache","Erholungsflache","Freiflache",
                           "Wohnflache", "Krankenhausbetten","Neubauwohnungen.je.Einwohner","Pflegebedurftige" ,"Nahversorgung.Supermarkt.Discounter","Entfernung.zum.Hausarzt","Nahversorgung.OV.Haltestelle",
                           "Geborene", "Arzte", "gisd_score","gisd_5","gisd_k","gisd_10", "PM25",
                           "Schulabganger.ohne.Abschluss","Monatliches.Haushaltseinkommen","Bruttoverdienst", "Schuldnerquote","Steuereinnahmen","SGB.II...Quote","Beschaftigtenquote","Beschaftigte.am.WO.ohne.Berufsabschluss" ,
                           "Beschaftigte.am.WO.mit.akademischen.Abschluss","settlement", "Lebenserwartung",
                           "DOW", "month")    

#anything imputed
imp_or_transf <- names(hosp_daily)[grepl("_imp|_sqrt|_log", names(hosp_daily))] 
covariates_fixed <- c(covariates_fixed_temp, imp_or_transf)

#lags
lags <- names(hosp_daily)[grepl("lag", names(hosp_daily))] 

#check if there are any duplicate rows
check <- hosp_daily %>% group_by(AGS_N3_23, date) %>% summarise(n = n()) %>% filter( n > 1)
rm(check);gc()

hosp_aggr <- hosp_daily[, c(
  # first value of covariates_fixed
  lapply(.SD[, ..covariates_fixed], first),
  # sum of count_occurence
  lapply(.SD[, ..count_occurence], sum),
  # last value of covariates
  lapply(.SD[, ..covariates], last),
  ## take the first one that is not NA
  lapply(.SD[, ..uniques], function(x) x[which(!is.na(x))[1]]),
  # last value of lags
  lapply(.SD[, ..lags], last)),
  by = .(AGS_N3_23, dat_aufn_3day_cat, year)]

#remove missing hospitalization information
hosp_aggr <- hosp_aggr[!is.na(hosp_aggr$N_hospitalization),]

#keep only summer months
hosp_aggr <- hosp_aggr[as.numeric(as.character(hosp_aggr$month)) >= 6 & as.numeric(as.character(hosp_aggr$month)) < 9,]

#get corrected population counts to calculate hosp rate
#121 is the number of categories that exist per year (121 to 122)
hosp_aggr$N_population.corr <- hosp_aggr$N_population/121
hosp_aggr$hosp_rate_1000 <- hosp_aggr$N_hospitalization / hosp_aggr$N_population.corr * 1000

write.csv(hosp_aggr, file="data/weekcounts_climate_INKAR_1607.csv")