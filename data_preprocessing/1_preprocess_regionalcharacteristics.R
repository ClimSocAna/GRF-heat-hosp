#setwd()
library(tidyverse)
library(readxl)
library(stringi)
library(zoo)

# AGS harmonization file: includes updated AGS codes for 2005-2023 -------------------------------------------
#load updated harm file
harm_vb_Kreise <- read.csv(file = "data/harmonized_vb_Kreise_2005to2023.txt")

#for those who do not have an observation in 2011, copy the observation from 2010
rows_to_add <- harm_vb_Kreise %>%
  filter(!has_2011, year == 2010) %>%
  mutate(year = 2011)

# Combine the original data with the new rows
harm_vb_ags_unique_update <- rbind(harm_vb_Kreise, rows_to_add)
harm_vb_ags_unique_update <- harm_vb_ags_unique_update[order(harm_vb_ags_unique_update$year,
                                                             harm_vb_ags_unique_update$pat_ags5), ]

# data from INKAR --------------------------------------------------------------
#obtained from https://www.inkar.de/
#load INKAR
INKAR <- read.csv("data/INKAR_indicators.csv", dec=",", fill=T, sep=";",
                  header=F)

#downloaded births and practitioner density in separate file
INKAR_add <- read.csv("data/INKAR_birth_arzte_05to23.csv", dec=",", fill=T, sep=";",
                      header=F)

#add added indicators
INKAR <- cbind(INKAR, INKAR_add[,4:ncol(INKAR_add)])

#update col names to include year
colnames(INKAR) <- paste0(INKAR[1,],"_",INKAR[2,]) 
colnames(INKAR) <- stringi::stri_trans_general(colnames(INKAR), "Latin-ASCII")

#remove first 2 rows because they do not contain data
INKAR <- INKAR[-c(1,2),]

#some variables contain the German decimal systems, correct:
INKAR[] <- cbind(INKAR[,1:3],
                 lapply(INKAR[,4:ncol(INKAR)], function(x) gsub("\\.", "", x)))
INKAR[] <- cbind(INKAR[,1:3],
                 lapply(INKAR[,4:ncol(INKAR)], function(x) as.numeric(gsub(",", ".", x))))

#transform into long format
INKAR_long <- INKAR %>%
  rename(AGS_N3_23 = Kennziffer_) %>%
  pivot_longer(cols = -c(AGS_N3_23, Raumeinheit_, Aggregat_),   
               names_to = c("Indicator", "year"),  
               names_sep = "_") %>% 
  pivot_wider(names_from = "Indicator", values_from = value) %>%
  mutate(year = as.integer(year))

summary(INKAR_long)

## car density per 1000 and number of new apartments both have negative values
# practitioner density has a lot of zero's
#set negative values to NA since it's not plausible
#because it describes the number of cars or new apartments in a given year divided by population
#this way it'll be filled up later when we're imputing
INKAR_long$`Pkw-Dichte` <- ifelse(INKAR_long$`Pkw-Dichte` < 0, NA, INKAR_long$`Pkw-Dichte`)

INKAR_long$`Neubauwohnungen je Einwohner` <- ifelse(INKAR_long$`Neubauwohnungen je Einwohner` < 0,
                                                    NA, INKAR_long$`Neubauwohnungen je Einwohner`)

## for Schutzsuchende, we need to redistribute the prevalence across district 
## because the data for Kassel, Landkreis are included in Kassel, Stadt
## the data for district Spree-Neiße included in district Cottbus
## all Saarland district included in Kreis Saarlouis

## Kassel Landkreis is 6633, Kassel, Stadt is 6611
## Spree Neiße ist 12071, cottbus is 12052
## Sarlouis is 10044, Saarland is 10041, 10042, 10043, 10044, 10045, 10046
INKAR_long <- INKAR_long %>% 
  mutate(Schutzsuchende.an.Bevolkerung_temp = ifelse(AGS_N3_23 == "10044", 
                                                     `Schutzsuchende an Bevolkerung`/6,
                                                     `Schutzsuchende an Bevolkerung`/2),
         Schutzsuchende.an.Bevolkerung_corr = ifelse(AGS_N3_23 %in% c("10041", "10042", "10043", "10044", "10045", "10046"),
                                                     Schutzsuchende.an.Bevolkerung_temp[AGS_N3_23 == "10044"],
                                                     ifelse(AGS_N3_23 %in% c("06633", "06611"),
                                                            Schutzsuchende.an.Bevolkerung_temp[AGS_N3_23 == "06611"],
                                                            ifelse(AGS_N3_23 %in% c("12071", "12052"),
                                                                   Schutzsuchende.an.Bevolkerung_temp[AGS_N3_23 == "12052"], 
                                                                   `Schutzsuchende an Bevolkerung`)))) %>%
  select(-Schutzsuchende.an.Bevolkerung_temp)

#check which indicators have missingness
cols_with_na <- colnames(INKAR_long)[colSums(is.na(INKAR_long)) > 0]
cols_with_na

#check which years are missing
INKAR_tot <- INKAR_long %>% 
  mutate(across(-c(1:3), ~ as.numeric(as.character(.)))) %>% 
  group_by(year) %>% 
  summarise(across(-c(1:3), ~ mean(.))) %>%
  ungroup()

#save those in seperate dataframe so we can fill in missingness
INKAR_tofix <- INKAR_long[,c("AGS_N3_23","year",cols_with_na)]

#rename everything to ..._imp so it's clear that these are imputed
INKAR_tofix <- INKAR_tofix %>% 
  rename_with(~ paste0(.x, "_imp"), -c(1:2))

#fill up values with either previous or later value (previous is preferred)
INKAR_tofix <- INKAR_tofix %>%
  group_by(AGS_N3_23) %>%
  fill(3:ncol(INKAR_tofix), .direction = "downup") %>%
  ungroup()

##merge back with the rest
INKAR_tot <- left_join(INKAR_long, INKAR_tofix, by=c("AGS_N3_23","year"))

#code as factor
INKAR_tot$AGS_N3_23 <- as.factor(as.numeric(INKAR_tot$AGS_N3_23))

# Settlement data --------------------------------------------------------------
## is available from 2017 onwards from the BSSR
# https://www.bbsr.bund.de/BBSR/DE/forschung/raumbeobachtung/Raumabgrenzungen/downloads/archiv/download-referenzen.html

#read in each year seperate and save the correct variables
dat17 <- read.csv("data/BSSR settlement/siedlungsstrukt-kreistypen-2017.csv",
                  sep=";")
colnames(dat17) <- dat17[1,]
dat17 <- dat17[-c(1,2),c(1,2,18,19)]

colnames(dat17) <- c("KRS", "KRS_NAME","KTU","KTU_NAME")
dat17$KTU <- as.integer(dat17$KTU)
dat17$KRS <- as.integer(dat17$KRS)
dat17$year <- 2017

dat18 <- read_excel("data/BSSR settlement/siedlungsstrukt-kreistypen-2018.xlsx")
colnames(dat18) <- dat18[1,]
dat18 <- dat18[-c(1,2),c(1,2,14,15)]

colnames(dat18) <- c("KRS", "KRS_NAME","KTU","KTU_NAME")

dat18$KTU <- as.integer(dat18$KTU)
dat18$KRS <- as.integer(dat18$KRS)
dat18$year <- 2018

dat19 <- read.csv("data/BSSR settlement/siedlungsstrukt-kreistypen-2019.csv",
                  sep=";")
colnames(dat19) <- c("KRS", "KRS_NAME","KTU","KTU_NAME")
dat19$year <- 2019

dat20 <- read.csv("data/BSSR settlement/siedlungsstrukt-kreistypen-2020.csv",
                  sep=";")
colnames(dat20) <- c("KRS", "KRS_NAME","KTU","KTU_NAME")
dat20$year <- 2020

dat1 <- bind_rows(dat17, dat18, dat19, dat20)

## 2021 to 2023 are in different format
dat21 <- read_excel("data/BSSR settlement/raumgliederungen-referenzen-2021.xlsx", sheet = "Kreisreferenz")
dat22 <- read_excel("data/BSSR settlement/raumgliederungen-referenzen-2022.xlsx", sheet = "Kreisreferenz")
dat23 <- read_excel("data/BSSR settlement/raumgliederungen-referenzen-2023.xlsx", sheet = "Kreisreferenz")

dat21 <- dat21[-1,c(1,2,19,20)]
colnames(dat21) <- c("KRS", "KRS_NAME","KTU","KTU_NAME")
dat22 <- dat22[-1,c(1,2,19,20)]
colnames(dat22) <- c("KRS", "KRS_NAME","KTU","KTU_NAME")
dat23 <- dat23[-1,c(1,2,43,44)]
colnames(dat23) <- c("KRS", "KRS_NAME","KTU","KTU_NAME")

dat21$year <- 2021
dat22$year <- 2022
dat23$year <- 2023

dat2 <- bind_rows(dat21, dat22, dat23)
dat2 <- dat2 %>% mutate(KRS = as.integer(KRS), KTU = as.integer(KTU))

dat <- bind_rows(dat1, dat2)

dat$settlement <- factor(dat$KTU, labels = c("kreisfreie Großstadt", "Städtischer Kreis",
                                             "Ländlicher Kreis mit Verdichtungsansätzen",
                                             "Dünn besiedelter ländlicher Kreis"))

#edit AGS to be Kreisschlüssel
dat$AGS_N3_23 <- gsub('.{3}$', '', dat$KRS)

#join the harmonized AGS codes
settlement_dat <- dat %>%
  rename("pat_ags5" = "AGS_N3_23") %>%
  mutate(pat_ags5 = as.integer(pat_ags5)) %>%
  left_join(harm_vb_ags_unique_update, by=c("year","pat_ags5"))

## check if there are some AGS that are coded wrong 
check_NAs <- settlement_dat[is.na(settlement_dat$AGS_N3_23),]
AGS_to_fix <- unique(check_NAs$pat_ags5) 
years_to_fix <- check_NAs %>% distinct(pat_ags5, year)

## duplicates?
settlement_dat |>
  summarise(n = n(), .by = c(AGS_N3_23, year)) |>
  filter(n > 1)

#fix it
settlement_dat <- settlement_dat %>%
  group_by(AGS_N3_23, year) %>%
  summarise(settlement = unique(settlement)) %>%
  ungroup()

settlement_dat$AGS_N3_23 <- as.factor(settlement_dat$AGS_N3_23)

##join
INKAR_tot <- left_join(INKAR_tot, settlement_dat[,c("AGS_N3_23","settlement","year")],
                       by = c("AGS_N3_23","year"))
rm(dat, dat1, dat17, dat18, dat19, dat20, dat21, dat22, dat23, dat2);gc()

# Life expectancy data --------------------------------------------------------------
#load in data
LE_95to17 <- readxl::read_excel("data/INKAR_LE_Kreise_Geschlecht_1995_2017.xlsx", sheet = "Daten")
LE_17to22 <- readxl::read_excel("data/DE_LE_Kreise_Geschl_2017-2022.xlsx")

#preprocessing to make it easier to work with
#clean up colnames
colnames(LE_95to17) <- sub("\\..*", "", colnames(LE_95to17))

#update col names to include year
colnames(LE_95to17) <- paste0(colnames(LE_95to17),"_",LE_95to17[1,]) 

#remove first row and reorder because some variables do not change over time
LE_95to17 <- LE_95to17[-1,c(1:3,73:75,4:72,76:144)]

#transform into long format
LE_95to17_long <- LE_95to17 %>%
  select(-Raumeinheit_NA) %>%
  rename(pat_ags5 = Kennziffer_NA) %>%
  pivot_longer(cols = -c(pat_ags5, Aggregat_NA,`Veränderung Lebenserwartung_2017`,`Veränderung Lebenserwartung Männer_2017`,`Veränderung Lebenserwartung Frauen_2017`),   # Columns to pivot (exclude ID)
               names_to = c("Indicator", "year"),  # Split column names
               names_sep = "_") %>% 
  pivot_wider(names_from = "Indicator", values_from = value) %>%
  mutate(year = as.integer(year),
         pat_ags5 = as.numeric(pat_ags5)) %>%
  filter(year >= 2005)

#keep only total Lebenserwartung
LE_95to17_long <- LE_95to17_long[,c(1,2,6,7,10)]

#check if Kreise are harmonized
#Eisenach 16066 needs to be included in Wartburgkreis (16063)
#subset harmonization file 
harm_subset <- harm_vb_ags_unique_update[harm_vb_ags_unique_update$pat_ags5 == "16063" 
                                  | harm_vb_ags_unique_update$AGS_N3_23 == "16063"
                                  | harm_vb_ags_unique_update$AGS_N3_23 == "16066",]

#join with dataset
LE_95to17_long <- left_join(LE_95to17_long, harm_subset, by=c("pat_ags5","year"))

#fix AGS column
LE_95to17_long$AGS_N3_23 <- ifelse(is.na(LE_95to17_long$AGS_N3_23), LE_95to17_long$pat_ags5,
                                   LE_95to17_long$AGS_N3_23)

which(is.na(LE_95to17_long$AGS_N3_23)) #no NAs
length(unique(LE_95to17_long$AGS_N3_23)) #400counties

check_duplicates <- LE_95to17_long %>%
  count(AGS_N3_23, year) %>%
  filter(n > 1)

#we have duplicates for 16063 and 16066, take the mean LE here
LE_95to17_long <- LE_95to17_long %>%
  group_by(AGS_N3_23, year) %>%
  summarize(Lebenserwartung = mean(as.numeric(Lebenserwartung)),
            `Restlebenserwartung der 60-Jährigen` = mean(as.numeric(`Restlebenserwartung der 60-Jährigen`))) %>%
  ungroup()

## check other dataset from 2017
check_AGS <- LE_17to22[(LE_17to22$Region %in% harm_vb_ags_unique_update$AGS_N3_23) == F,]

#restructure and rename
LE_17to22 <- LE_17to22 %>%
  filter(Sex == "b") %>%
  rename(AGS_N3_23 = Region,
         year = Year,
         life_exp = ex) %>%
  mutate(Indicator = ifelse(Age==0, "Lebenserwartung","Restlebenserwartung ab 65")) %>%
  select(-Age) %>%
  pivot_wider(names_from = "Indicator", values_from = "life_exp") #we want each indicator to have its own column

LE_17to22_long <- LE_17to22 %>%
  slice(rep(1:n(), each = 3)) %>%
  mutate(year = rep(rep(c(2017:2022), 2),200)) %>%
  select(-Sex) %>%
  filter(year > 2017) #bc we have the calculations from BSPP 

LE_17to22_long$AGS_N3_23 <- as.character(LE_17to22_long$AGS_N3_23)

#join both LE datasets
LE <- rbind(LE_95to17_long[,1:3], LE_17to22_long[,1:3])

#code as factor
LE$AGS_N3_23 <- as.factor(LE$AGS_N3_23)

##join
INKAR_tot <- left_join(INKAR_tot, LE[,c("AGS_N3_23","year","Lebenserwartung")],
                       by = c("AGS_N3_23","year"))

# fill in life expectancy and settlement
# sanity check to make sure only years are missing that are not in LE dataset not some AGS problem
length(which(is.na(INKAR_tot$Lebenserwartung[INKAR_tot$year>2004 & INKAR_tot$year <=2023])))
length(which(is.na(INKAR_tot$settlement[INKAR_tot$year>=2017 & INKAR_tot$year <=2023])))


#fill in missing values
INKAR_filled <- INKAR_tot %>%
  mutate(Lebenserwartung_imp = Lebenserwartung,
         settlement_imp = settlement) %>%
  group_by(AGS_N3_23) %>%
  fill(c(Lebenserwartung_imp, settlement_imp), .direction = "downup") %>%
  ungroup()

# check duplicates
#none
check_duplicates <- INKAR_filled %>%
  count(AGS_N3_23, year) %>%
  filter(n > 1)

# save final dataset 
write.csv(INKAR_filled, file = "data/INKAR_indicators_complete.csv")

