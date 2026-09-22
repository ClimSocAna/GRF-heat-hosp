#setwd()

library(dplyr)
library(data.table)
library(tidyr)
library(readxl)

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

# hospitalization counts --------------------------------------------------
#merge the years into one
#we have seperate outcomes for aggregated weekcounts for Berlin
weekcounts <- weekcounts_Berlin <- vector(mode = "list",length = length(2005:2023))

for (d in 2005:2023){
   weekcounts[[d-2004]] <- readxl::read_excel(paste0("data/drg_daycounts_",d,"_anonym.xlsx"))
   weekcounts[[d-2004]]$pat_ags5 <- as.numeric(weekcounts[[d-2004]]$pat_ags5)
   
   ##Berlin codes were received seperately
   weekcounts_Berlin[[d-2004]] <- readxl::read_excel(paste0("data/Berlin_dayCounts_",d,"_anonym.xlsx"))
   weekcounts_Berlin[[d-2004]]$pat_ags5 <- as.numeric(weekcounts_Berlin[[d-2004]]$pat_ags5_grouped)
   }

#rbind
weekcounts <- do.call(rbind.data.frame, weekcounts)
weekcounts_Berlin <- do.call(rbind.data.frame, weekcounts_Berlin)

#check sample size
sample_size <- weekcounts %>% group_by(year_according_to_aufndat) %>% summarise(counts = sum(as.integer(count_var), na.rm = T))

## weekcounts has city region codes 11001 to 11012 for Berlin, set to 11000 and then remove
uniques <- weekcounts %>% group_by(pat_ags5) %>% summarise(n=n()) %>% filter(pat_ags5 >=11000)
weekcounts$pat_ags5 <- ifelse(weekcounts$pat_ags5 %in% c(11000:11012, 11100), 11000, weekcounts$pat_ags5) 

#remove Berlin
weekcounts_noBer <- subset(weekcounts, weekcounts$pat_ags5 != 11000)

#add only new berlin codes
weekcounts <- bind_rows(weekcounts_noBer, weekcounts_Berlin)

#check if it worked 
uniques2 <- weekcounts %>% group_by(pat_ags5) %>% summarise(n=n()) %>% filter(pat_ags5 >=11000)

#- 11100 is still in the sample so aggregate that into it
weekcounts$pat_ags5 <- ifelse(weekcounts$pat_ags5 == 11100, 11000, weekcounts$pat_ags5) 

#remove first zero from AGS5 code so we can merge it with the harmonized one
weekcounts$pat_ags5 <- as.numeric(weekcounts$pat_ags5)
weekcounts$count_var <- as.integer(weekcounts$count_var) #sets xxx to NA

#correct year variable
weekcounts$year <- weekcounts$year_according_to_aufndat

#aggregate across districs
weekcounts <- weekcounts %>% group_by(pat_ags5, dat_aufn_3day_cat, year) %>% 
  summarise(count = sum(count_var)) %>%
  ungroup()

##get sample size after exclusion due to data security
sample_size <- weekcounts %>% group_by(year) %>% summarise(counts = sum(count, na.rm = T))

#join the harmonized AGS codes code to the weekcounts
weekcounts <- left_join(weekcounts, harm_vb_ags_unique_update, by=c("year","pat_ags5"))

## some AGS coded wrong? -- all good
check_NAs <- weekcounts[is.na(weekcounts$AGS_N3_23),]
AGS_to_fix <- unique(check_NAs$pat_ags5) 
years_to_fix <- check_NAs %>% distinct(pat_ags5, year)

gc()

#just so we know which ones need to be changed: check the different AGSs
length(unique(weekcounts$pat_ags5))
length(unique(weekcounts$AGS_N3_23)) #74 counties less in 2023 

## aggregate because of the harmonization some AGS were merged into one another
weekcounts <- weekcounts %>%
  group_by(AGS_N3_23, dat_aufn_3day_cat, year) %>% 
  summarise(count = sum(count)) %>%
  ungroup()

# combine with population counts -------------------------------------------------------
#load pop count data by year, age groups, gender and AGS5
#obtained from: https://genesis.destatis.de/datenbank/online/statistic/12411/table/12411-0015 
#skip first 4 because they just have information about the data
pop_counts_byAGS5 <- fread("data/populationcounts_2005to2023.csv",skip = 4, fill=T, na.strings = "-")

#remove the last few rows because they just have some information about the data
pop_counts_byAGS5 <- pop_counts_byAGS5[-c(479:nrow(pop_counts_byAGS5)),]
pop_counts_byAGS5 <- as.data.frame(pop_counts_byAGS5)

#remove all the e columns
pop_counts_byAGS5[2,1] <- "pat_ags5"
pop_counts_byAGS5[2,2] <- "Name"
pop_counts_byAGS5 <- pop_counts_byAGS5[, !grepl("^\\s*$", as.character(pop_counts_byAGS5[2, ]))]

#set column names (currently in row 2)
colnames(pop_counts_byAGS5) <- pop_counts_byAGS5[2,]

#remove first two rows because that information is now in the column header
pop_counts_byAGS5 <- pop_counts_byAGS5[-c(1,2),]

#pivot
pop_counts_byAGS5 <- pop_counts_byAGS5 %>%
  pivot_longer(cols = 3:21,
               names_to = "year",
               values_to = "pop_count")

#put datasets in correct format so we can merge them and there is no confusion what level is what sex
pop_counts_byAGS5$year       <- as.numeric(sub("31.12.", "", pop_counts_byAGS5$year))
pop_counts_byAGS5$pat_ags5   <- as.integer(pop_counts_byAGS5$pat_ags5)

#add new AGS codes 
pop_counts_byAGS5 <- left_join(pop_counts_byAGS5, harm_vb_Kreise[,c("pat_ags5", "year", "AGS_N3_23")], by=c("year","pat_ags5"))

#aggregate by year and district
pop_counts_aggr <- pop_counts_byAGS5 %>%
  group_by(year, AGS_N3_23) %>%
  summarise(pop_counts = sum(as.numeric(pop_count))) %>%
  ungroup()

## attach to weekcounts
weekcounts_w.popcounts <- left_join(weekcounts, pop_counts_aggr, by = c("year", "AGS_N3_23"))

##check NAs for pop counts 
check_NAs.pop <- weekcounts_w.popcounts %>% filter(is.na(pop_counts))
#no missings

#rename
weekcounts_w.popcounts <- weekcounts_w.popcounts %>%
  rename(N_hospitalization = count,
         N_population = pop_counts) 

#check duplicates
check <- weekcounts_w.popcounts %>%
  group_by(AGS_N3_23,year,dat_aufn_3day_cat) %>%
  summarise(counts = n()) %>%
  filter(counts > 1)

weekcounts_w.popcounts2 <- distinct(weekcounts_w.popcounts)
nrow(weekcounts_w.popcounts)
nrow(unique(weekcounts_w.popcounts))
#none

#save
weekcounts_v2 <- weekcounts_w.popcounts
save(weekcounts_v2, file="data/weekcounts_byyear_v2.RData")
