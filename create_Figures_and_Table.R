#setwd("")

library(sf)
library(tidyverse)
library(tableone)
library(grf)
library(patchwork)
library(fixest)
library(ggcorrplot)
library(colorspace)

#load function
source("functions.R")

# data prep -------------------------------------------------------
##read in data
dat <-  read.csv("toy_data.csv")

#heatwave definition
dat <- dat %>%
  mutate(dayofyear = (dat_aufn_3day_cat * 3) + 1,
         date = as.Date(paste0(year, "-01-01")) + (dayofyear - 1)) %>%
  arrange(AGS_N3_23, date) %>%
  group_by(AGS_N3_23) %>%
  mutate(
    heatdaylag_heatindexabove0.98thperc = rowSums(across(
      .cols = c(heat_index_lag_day3, heat_index_lag_day4, heat_index_lag_day5),
      .fns = ~ as.integer(. > threshold_hiabove0.98thperc)
    )),
    heatdaylag2_heatindexabove0.98thperc = rowSums(across(
      .cols = c(heat_index_lag_day6, heat_index_lag_day7, heat_index_lag_day8),
      .fns = ~ as.integer(. > threshold_hiabove0.98thperc)
    )),
    heatday_counter = heatday_heatindexabove0.98thperc + heatdaylag_heatindexabove0.98thperc) %>%
  ungroup()

dat$heatwave_yes <- ifelse(dat$heatday_heatindexabove0.98thperc > 0, 1,0)

#make sure AGS is a factor
dat$AGS_N3_23 <- as.factor(dat$AGS_N3_23)

# Create dayofyear from 3-day group
dat$dayofyear <- (dat$dat_aufn_3day_cat * 3) + 1

# Create calendar_date by adding dayofyear to January 1st of each year
dat$date <- as.Date(paste0(dat$year, "-01-01")) + (dat$dayofyear - 1)

## Read in shapefile
shape_data <- st_read("data/vg5000_ebenen_1231/VG5000_KRS.shp")
shape_data <- rename(shape_data, AGS_N3_23=AGS)
shape_data$AGS_N3_23 <- as.factor(as.integer(shape_data$AGS_N3_23))

#EMs
#for table 1
effect_modifiers <- c('Auslanderanteil', 'Schutzsuchende.an.Bevolkerung_corr_imp',
                      'Einwohnerdichte_imp', 'Einwohner.65.Jahre.und.alter',
                      'Einwohner.unter.6.Jahre', 'Frauenanteil',
                      "Geborene_imp", 
                      'Empfanger.von.Pflegegeld_imp', 'Pkw.Dichte_imp',
                      'Waldflache_imp', 'Wasserflache_imp', 'Freiflache_imp', 'Wohnflache_imp',
                      'Krankenhausbetten_imp', 'Neubauwohnungen.je.Einwohner_imp',
                      'Pflegebedurftige_imp', 
                      'Lebenserwartung_imp',
                      "gisd_score", 'PM25',"settlement_imp")

#actually used in grf
effect_modifiers_imp <- c('Auslanderanteil', 'Schutzsuchende.an.Bevolkerung_corr_imp',
                          'Einwohner.65.Jahre.und.alter',
                          'Einwohner.unter.6.Jahre', 'Frauenanteil',
                          'Empfanger.von.Pflegegeld_imp', 'Pkw.Dichte_imp',
                          'Waldflache_imp', 'Wasserflache_imp', 'Wohnflache_imp',
                          'Krankenhausbetten_imp', 'Neubauwohnungen.je.Einwohner_imp',
                          'Pflegebedurftige_imp', 
                          "gisd_score",
                          'PM25',
                          'settlement_impvery_rural',
                          'settlement_impvery_urban',
                          'settlement_imprural',
                          'settlement_impurban')

# Table 1: Descriptive Table -------------------------------------------------------

##subset information that I want in my Table
table_dat2 <- dat[,c(
  #outcome
  "hosp_rate_1000", 
  #exposure
  "threshold_hiabove0.98thperc", 
  "heatwave_yes",
  #effect modifiers
  effect_modifiers)]

#assign nicer names
names(table_dat2) <- c("Weekly_emergency_hospitalization_rate_per10000",
                       "98th_percentile_temperature_threshold",
                       "heat_day_prevalence", 
                       'migrants_perc', 'population_seeking_protection_perc',
                       'population_per_sqkm', 'population_above65_perc',
                       'population_below6_perc', 'females_perc',
                       'births_per_1000', 
                       'care_allowance_recipients_perc', 'cars_per1000',
                       'forest_perc', 'water_bodies_perc', 'openspace_perc', 'livingspace_sqm_pp',
                       'hospital_beds_per1000', 'new_apartments_per1000',
                       'population_care_needs_perc', 'life_expectancy_at_birth',
                       'GISD', 'PM25', "degree_of_urbanization")


#create table 1
descr_tab <- CreateTableOne(data = table_dat2[, !names(table_dat2) %in% "degree_of_urbanization"], 
                            factorVars = "heat_day_prevalence",
                            addOverall = F)
descr_tab

#adapt format so we can join it with other tables
descr_tab <- as.data.frame(print(descr_tab, quote = FALSE, noSpaces = TRUE, printToggle = FALSE))
descr_tab$Variable <- rownames(descr_tab)
rownames(descr_tab) <- NULL
descr_tab <- descr_tab[, c("Variable", setdiff(names(descr_tab), "Variable"))]

#calcuate degree of urbanization seperately
## because we want the mode on a district level
## not the 3-day categories falling into specific category
#fix factor levels
dat$degree_of_urbanization <- factor(dat$settlement_imp,
                                     levels = c("Dünn besiedelter ländlicher Kreis", "Ländlicher Kreis mit Verdichtungsansätzen", "Städtischer Kreis", "kreisfreie Großstadt"),
                                     labels = c("very rural", "rural", "urban", "very urban"))

settle <- dat %>%
  count(AGS_N3_23, degree_of_urbanization) %>%
  group_by(AGS_N3_23) %>%
  slice_max(degree_of_urbanization, n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  #frequency table of districts by their modal settlement type
  count(degree_of_urbanization) %>%
  mutate(pct = n / sum(n) * 100,
         Overall= paste0(n, " (", pct, ")")) %>%
  rename(Variable = degree_of_urbanization) %>%
  select(c(Variable, Overall))

descr_tab <- rbind(descr_tab[-1,], settle)

#join the other information
add_inf <- data.frame(
  Variable = c("Number_of_cases", "N_of_districts", "N_of_3daycats", "Year_range", "Month_range"),
  Overall = c(sum(dat$N_hospitalization), 
              length(unique(dat$AGS_N3_23)),
              length(unique(dat$dat_aufn_3day_cat)),
              paste(range(as.numeric(as.character(dat$year))), collapse = "-"),
              paste(range(as.numeric(as.character(dat$month))), collapse = "-")),
  stringsAsFactors = FALSE)

descr_tab <- rbind(add_inf, descr_tab[-1,])

#order variable names
order <- c('Number_of_cases', 'N_of_districts', 'N_of_3daycats', 'Year_range',
           'Month_range', 'Weekly_emergency_hospitalization_rate_per10000 (mean (SD))',
           '98th_percentile_temperature_threshold (mean (SD))', 'heat_day_prevalence = 1 (%)',
           
           ##demographic
           
           'migrants_perc (mean (SD))', 'population_seeking_protection_perc (mean (SD))',
           'population_per_sqkm (mean (SD))', 'population_above65_perc (mean (SD))', 
           'population_below6_perc (mean (SD))', 'females_perc (mean (SD))',
           'births_per_1000 (mean (SD))', 'GISD (mean (SD))',
           
           ##health and social
           
           'care_allowance_recipients_perc (mean (SD))', 'hospital_beds_per1000 (mean (SD))',
           'population_care_needs_perc (mean (SD))', 'cars_per1000 (mean (SD))',
           'life_expectancy_at_birth (mean (SD))',
           
           ##natural and built environment
           'forest_perc (mean (SD))', 'water_bodies_perc (mean (SD))', 'openspace_perc (mean (SD))',
           'livingspace_sqm_pp (mean (SD))', 'new_apartments_per1000 (mean (SD))', 
           'PM25 (mean (SD))',
           'very rural', 'rural', 'urban', 'very urban')

#variable as factor
descr_tab <- descr_tab %>%
  mutate(Variable = factor(Variable, levels = order)) %>%
  arrange(Variable)

write.csv(descr_tab, file="Tables/Descriptive_table1.csv")

# Figure 1: individual CATE predictions ----------------------------------------------------------
#load causal forest
load("Models/GRF_heatindex_perc_98th_exclCovidno_10k.RData")
CATE1 <- predict(tau.forest)
CATE1$check <- "perc98th\n(main analysis)"
CATE1$AGS_N3_23 <- dat$AGS_N3_23

load("Models/GRF_heatindex_lag1_exclCovidno_10k.RData")
CATE2 <- predict(tau.forest)
CATE2$check <- "lag 1"
CATE2$AGS_N3_23 <- dat$AGS_N3_23

load("Models/GRF_heatindex_lag2_exclCovidno_10k.RData")
CATE3 <- predict(tau.forest)
CATE3$check <- "lag 2"
CATE3$AGS_N3_23 <- dat$AGS_N3_23

#get predictions
CATE_i <- bind_rows(CATE1, CATE2, CATE3)

## aggregate to district
CATE_AGS <- CATE_i %>%
  group_by(AGS_N3_23, check) %>%
  summarise(nj = n(), 
            mean_cate = mean(predictions), 
            q25 = quantile(predictions, .25),
            q50 = quantile(predictions, .50),
            q75 = quantile(predictions, .75)) %>%
  ungroup() %>%
  #create state variable with correct coding
  mutate(Bundesland = as.factor(ifelse(nchar(as.character(AGS_N3_23)) == 5, 
                                       substr(as.character(AGS_N3_23), 1, 2), 
                                       substr(as.character(AGS_N3_23), 1, 1))),
         Bundesland = factor(Bundesland, 
                             levels=c("1", "2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"),
                             labels=c(
                               "SH","HH",  "NI", "HB",
                               "NW", "HE", "RP",
                               "BW", "BY", "SL", 
                               "BE","BB", "MV", "SN",
                               "ST", "TH")))


CATE_AGS$check <- factor(CATE_AGS$check,  
                         levels = c("perc98th\n(main analysis)",
                                    "lag 1" , "lag 2"),
                         labels = c("perc98th\n(main analysis)",
                                    "short-term lag (days 3-5)", "extended lag (days 6-8)"))

##add some spacers in the plot so the city states are better visible
# Define which states are city states (single-district)
stadtstaaten <- c("HH", "HB", "BE")

# Create spacer rows after each Stadtstaat
spacers <- CATE_AGS %>%
  group_by(Bundesland, check) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    AGS_N3_23 = NA,
    nj = NA_real_,
    mean_cate = NA_real_,
    q10 = NA_real_, q25 = NA_real_, q50 = NA_real_,
    q75 = NA_real_, q90 = NA_real_,
    is_spacer = TRUE
  )

ranks_98 <- CATE_AGS %>%
  filter(check == "perc98th\n(main analysis)") %>%
  mutate(is_spacer = FALSE) %>%
  bind_rows(spacers) %>%
  arrange(Bundesland, desc(q50)) %>%
  mutate(rank2 = row_number()) %>%
  select(AGS_N3_23, rank2)          

# join ranks back onto all check values for the same district
CATE_AGS <- CATE_AGS %>%
  left_join(ranks_98, by = "AGS_N3_23")

p <- CATE_AGS %>%
  filter(check != "perc98th\n(main analysis)") %>%
  ggplot(aes(x = rank2, color = Bundesland, group = Bundesland)) +
  geom_errorbar(aes(ymin = q25, ymax = q75), alpha = .75, na.rm = TRUE) +
  geom_point(aes(y = q50), na.rm = TRUE) +
  geom_point(aes(y = mean_cate), shape = 0, na.rm = TRUE) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_minimal() +
  scale_color_viridis_d() +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  guides(color = guide_legend(title = "State code", nrow = 2, byrow = TRUE)) +
  labs(x = "Districts", y = "CATE predictions") + 
  facet_wrap(~check, nrow=3)

ggsave(file="Figures/CATE_ind_bystate_wlags.pdf", p, device = cairo_pdf, width = 10, height = 7)

p <- CATE_AGS %>%
  filter(check == "perc98th\n(main analysis)") %>%
  ggplot(aes(x = rank2, color = Bundesland, group = Bundesland)) +
  geom_errorbar(aes(ymin = q25, ymax = q75), alpha = .75, na.rm = TRUE) +
  geom_point(aes(y = q50), na.rm = TRUE) +
  geom_point(aes(y = mean_cate), shape = 0, na.rm = TRUE) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_minimal() +
  scale_color_viridis_d() +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  guides(color = guide_legend(title = "State code", nrow = 2, byrow = TRUE)) +
  labs(x = "Districts", y = "CATE predictions")

##add map in same colors on the side 
plot_map <- left_join(shape_data, CATE_AGS, by="AGS_N3_23")

map_p <- plot_map %>%
  filter(check == "perc98th\n(main analysis)") %>%
  ggplot() +
  geom_sf(aes(fill = Bundesland), linewidth = 0.05) +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "none")

#combine them
map_p <- map_p + 
  theme(panel.background = element_rect(fill = "white"))

p <- p + 
  theme(plot.margin = margin(0, 27, 0, 0, unit = "mm")) +  
  inset_element(map_p, left = 0.93, bottom = 0.55, right = 1.15, top = 1,
                align_to = "plot")

ggsave(file="Figures/CATE_ind_bystate.pdf", p, device = cairo_pdf, width = 10, height = 5)

# Figure 2: put the (most common Q) map  ----------------------------------------------------------
## count occurence of quantiles by district and show on map
#load ind CATEs
CATE_q_i <- read.csv("Tables/Quantile_ind.ATE_GRF_heat_defperc_98th_exclCovidno.csv")
CATE_q_i$check <- "heat event window (days 0-2)"
CATE_q_i2 <- read.csv("Tables/Quantile_ind.ATE_GRF_heat_deflag1_exclCovidno.csv")
CATE_q_i2$check <- "short-term lag (days 3-5)"
CATE_q_i3 <- read.csv("Tables/Quantile_ind.ATE_GRF_heat_deflag2_exclCovidno.csv")
CATE_q_i3$check <-  "extended lag (days 6-8)"

CATE_i <- bind_rows(CATE_q_i, CATE_q_i2, CATE_q_i3)

#count occurences of CATE per district
CATE_qs <- CATE_i %>%
  mutate(AGS_N3_23 = as.factor(AGS_N3_23),
         check = factor(check, 
                        levels = c("heat event window (days 0-2)",
                                   "short-term lag (days 3-5)", "extended lag (days 6-8)"))) %>%
  group_by(AGS_N3_23, check) %>%
  mutate(obs_distr = n()) %>%
  ungroup() %>%
  group_by(AGS_N3_23, ranking, check) %>%
  reframe(N_occurence = n(), 
          perc_occurence = N_occurence/obs_distr * 100) %>%
  distinct() %>%
  ungroup()

#if some rows are missing fill them with zeros
CATE_qs <- CATE_qs %>%
  complete(AGS_N3_23, check, ranking = 1:4,
           fill = list(perc_occurence = 0,
                       N_occurence = 0))

#assign ranking
CATE_qs$ranking <- factor(CATE_qs$ranking, levels = c("1","2","3","4"),
                          labels = c("Q1: Moderate decrease",
                                     "Q2: Minor decrease",
                                     "Q3: Minor increase",
                                     "Q4: Moderate increase"))

# CATE QUANT MAP in one map
#add max rank variable
plot_map <- CATE_qs %>%
  group_by(AGS_N3_23, check) %>%
  mutate(max_rank = ranking[which.max(perc_occurence)]) 

plot_map <- left_join(shape_data, plot_map, by="AGS_N3_23")

#color palette
palette <- c('#fee6ce','#fdae6b','#f16913','#a63603')

map_quart_p_comb1 <- plot_map %>%
  filter(check %in% c("heat event window (days 0-2)")) %>%
  ggplot() +
  geom_sf(aes(fill = max_rank), color = "#555555", linewidth = 0.05) +
  facet_wrap(~check) +
  theme_minimal() +
  scale_fill_manual(values = palette, name = "",
                    guide = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size=18,face = "bold"),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "bottom",
        legend.text = element_text(size=13),
        legend.title = element_text()) +
  guides(fill = guide_legend(nrow=1) )

map_quart_p_comb2 <- plot_map %>%
  filter(check %in% c( "short-term lag (days 3-5)", "extended lag (days 6-8)")) %>%
  ggplot() +
  geom_sf(aes(fill = max_rank), color = "#555555", linewidth = 0.05) +
  facet_wrap(~check, nrow = 2) +
  theme_minimal() +
  scale_fill_manual(values = palette, name = "",
                    guide = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size=18,face = "bold"),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "bottom",
        legend.text = element_text(size=13),
        legend.title = element_text()) +
  guides(fill = guide_legend(nrow=1) )

map_quart_p_comb1 <- map_quart_p_comb1 +
  scale_x_continuous(expand = expansion(mult = 0)) +
  scale_y_continuous(expand = expansion(mult = 0))

map_quart_p_comb2 <- map_quart_p_comb2 +
  scale_x_continuous(expand = expansion(mult = 0)) +
  scale_y_continuous(expand = expansion(mult = 0))

p <- map_quart_p_comb1 + map_quart_p_comb2 +
  plot_layout(guides = "collect", widths = c(1, 1)) &
  theme(legend.position = "bottom",
        legend.text = element_text(),
        legend.title = element_text(),
        plot.margin = margin(0, 0, 0, 0)) 

ggsave("Figures/quartile_map_lagscomb.pdf", p, device = cairo_pdf,  width = 10, height = 10)

# Suppl Figure: Detailed quartile maps ----------------------------------------------------------

#quartile map
map_quart_p <- plot_map %>%
  ggplot() +
  geom_sf(aes(fill = perc_occurence), color="#323232", linewidth = 0.05) +
  theme_minimal() +
  scale_fill_distiller(palette = "BuPu", direction=-1,
                       name = "% of 3-day categories\nin ATE quartile") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size=11),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "bottom") +
  facet_grid(check~ranking)


ggsave("Figures/Quartile_map_detailed_wlags.pdf", map_quart_p,  device = cairo_pdf,  width = 13, height = 9)

# Figure 3 & 4: VIM and heterogeneity indicator profiles ------------------------------------------------
#load results
#covariance structure from quantile grf
ATE_quant_main <- read.csv("Tables/Quantile_ATE_cov.str_GRF_heat_defperc_98th_exclCovidno.csv")
ATE_quant_main <- rename(ATE_quant_main, variable=effect_modifiers)

ATE_quant_main$ranking <- factor(ATE_quant_main$ranking, levels = c("Q1","Q2","Q3","Q4"),
                                 labels = c("Q1: moderate decrease",
                                            "Q2: minor decrease",
                                            "Q3: minor increase",
                                            "Q4: moderate increase"))

#variable importance frm grf
VI_LOO_main <- read.csv("Tables/GRF_TE_VIM_GRF_heat_defperc_98th_exclCovidno.csv")

#assign effect modifiers to each domain
domain <- ifelse(effect_modifiers_imp %in% c("Auslanderanteil","Schutzsuchende.an.Bevolkerung_corr_imp",
                                             "Einwohner.65.Jahre.und.alter","Einwohner.unter.6.Jahre",
                                             "Frauenanteil","gisd_score"), "Demographic",
                 ifelse(effect_modifiers_imp %in% c("Empfanger.von.Pflegegeld_imp","Pkw.Dichte_imp",
                                                    "Krankenhausbetten_imp", "Pflegebedurftige_imp",
                                                    "Lebenserwartung_imp"), "Health & Social",
                        ifelse(effect_modifiers_imp %in% c("Waldflache_imp","Wasserflache_imp",                                               
                                                           "Wohnflache_imp" ,"Neubauwohnungen.je.Einwohner_imp", 
                                                           "PM25","settlement_impurban", 
                                                           "settlement_impvery_rural",
                                                           "settlement_imprural",
                                                           "settlement_impvery_urban"), "Environment", NA)))
domain <- as.data.frame(cbind(domain, effect_modifiers_imp))
domain <- rename(domain, variable=effect_modifiers_imp)

#join to both datasets
ATE_quant_main <- left_join(ATE_quant_main, domain, by="variable")
VI_LOO_main <- left_join(VI_LOO_main, domain, by="variable")

#get the VIM ranking
var_order <- VI_LOO_main %>%
  mutate(domain = factor(domain, levels = c("Demographic", "Health & Social", "Environment"))) %>%
  arrange(desc(tevim_s)) %>%
  mutate(vim_rank = row_number()) %>%
  select(variable, vim_rank)

# join VIM rank, sort rows by domain then importance
ATE_quant_main <- ATE_quant_main %>%
  left_join(var_order, by = c("variable")) %>%
  mutate(variable = factor(variable, levels = var_order$variable),
         domain =  factor(domain, levels = c("Demographic", "Health & Social", "Environment")),
         avg = round(avg, 2),
         stderr = round(stderr, 2))  

VI_LOO_main <- VI_LOO_main %>%
  left_join(var_order, by = c("variable")) |>
  mutate(variable = factor(variable, levels = var_order$variable),
         domain =  factor(domain, 
                          levels = c("Demographic", "Health & Social",
                                     "Environment")),
         ub_scaled = tevim_s + 1.96*std_err_s,
         lb_scaled = tevim_s - 1.96*std_err_s,) 

#update names
levels(VI_LOO_main$variable) <- levels(ATE_quant_main$variable) <- c('% seeking protection',
                                                                     '% below age 6', 'PM 2.5 in [ug/m3]',
                                                                     'Living space pp in sqm',
                                                                     'Cars per 1,000','% above age 65','% migrants',
                                                                     'GISD','Hospital beds\nper 1,000',
                                                                     '% forest',
                                                                     
                                                                     '% women','% water bodies',
                                                                     '% with care needs',
                                                                     'New apartments\nper 1,000\n',
                                                                     '% receiving\ncare allowance',
                                                                     
                                                                     "Rural", "Very urban", 'Urban',"Very rural")

# define a color per domain
domain_col <- c("#B22272","#005eb8","#636B05")

#VIM
bar_width <- 0.7

p_v <- VI_LOO_main  %>%
  ggplot(aes(x = tevim_s, y = variable, fill=domain)) +
  geom_col() +
  scale_y_discrete(limits = rev, drop = FALSE) +
  scale_x_continuous(limits = c(-0.0007,0.04))+
  scale_fill_manual(values = domain_col, name="Domain")+
  theme_minimal() +
  theme(
    axis.title.y  = element_blank(),
    axis.text.y   = element_text(),
    strip.text.y  = element_blank(),
    legend.position = "bottom") +
  labs(x = "Variable Importance (scaled)",
       y = NULL, title = NULL)

p_v
ggsave("Figures/VIM.pdf", p_v,  width = 7, height = 7)

#make indicator if show distribution or not
ATE_quant_top10 <- ATE_quant_main %>%
  filter(vim_rank <= 10) 

ATE_quant_top10$variable <- droplevels(ATE_quant_top10$variable)

#dim down the palette a little
hsl <- as(hex2RGB(palette), "HLS")
hsl@coords[, "S"] <- hsl@coords[, "S"] * 0.95
palette_dim <- hex(as(hsl, "RGB"))

p_h <- ATE_quant_top10 %>%
  ggplot() +
  geom_tile(aes(x = factor(ranking), y = variable, fill = scaling),
            color = "white", linewidth = .3) +
  geom_text(aes(x = factor(ranking), y = variable, label = labels,
                color = scaling > 1.5),
            size = 3) +
  scale_color_manual(values = c("black", "white"), guide = "none") +
  scale_fill_gradient(low = palette_dim[1], high = palette_dim[4], name = "Scaled mean") +
  scale_y_discrete(limits = rev, drop = FALSE) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.x  = element_text(size = 8),
    plot.title = element_text(hjust = -1),
    legend.position = "none"
  ) +
  labs(x = "CATE Quartile", y = NULL)

p_h
ggsave("Figures/heatmap.pdf", p_h,  width = 7, height = 7)

# Supplement: Figure showing heat thresholds ------------------------------------------

#get threshold per district and year
thresholds <- dat %>%
  group_by(AGS_N3_23,year) %>%
  summarise(threshold_mean = round(unique(threshold_hiabove0.98thperc), 2),
            days_above_threshold = sum(heatwave_yes),
            n = n()) %>%
  ungroup() %>%
  mutate(perc_days_above_threshold = days_above_threshold/n*100) 

#merge with shapefile
thresholds <- left_join(shape_data, thresholds, by="AGS_N3_23")

## map
#threshold
p1 <- thresholds %>%
  ggplot() +
  geom_sf(aes(fill = threshold_mean), linewidth = 0.05) +
  theme_minimal() +
  scale_fill_viridis_c(option="rocket", direction = -1,
                       name = "Heat Day Threshold") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    legend.position = "bottom") 

#%days over threshold
p2 <- thresholds %>%
  ggplot() +
  geom_sf(aes(fill = perc_days_above_threshold)) +
  theme_minimal() +
  scale_fill_viridis_c(option= "cividis", direction = -1,
                       name = "% of days per year\nabove heat day threshold") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    legend.position = "bottom") 

#combine
p <- (p1 + p2) + 
  plot_annotation(title = 'Heat day threshold and number of 3-day categories above threshold\n by district across all years')

ggsave("Figures/Descriptive_plot_heat_threshold.pdf", device = cairo_pdf, p, width = 8, height = 6)

# Supplement: Propensity Score Diagnostics --------------------------------
#fit exposure model
fit_exp <- feglm(heatwave_yes ~ 1 | AGS_N3_23 + year + month,
                 family = "binomial",
                 data = dat,
                 cluster = ~ AGS_N3_23)

## calculate stable inverse probability weights
W_hat <- predict(fit_exp)
W <- dat$heatwave_yes

IPW_stab <- ifelse(W == 1,
                   mean(W) / W_hat,
                   (1 - W) / (1 - W_hat))

#put all in df
plot.df <- data.frame(W_hat = predict(fit_exp),
                      heatday = as.factor(dat$heatwave_yes),
                      IPW_stab = ifelse(W == 1,
                                        mean(W) / W_hat,
                                        (1 - W) / (1 - W_hat)))

#plot overlap
p1 <- ggplot(plot.df, aes(x = W_hat, fill = heatday)) + 
  geom_histogram(aes(y=after_stat(density)),
                 alpha = 0.5, position = "identity", bins = 30) +
  scale_fill_manual(values=c("steelblue", "darkred")) + 
  labs(title="A: Overlap assumption.",
       x="Propensity score") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = abs)

#plot covariate balance plot before and after weighting
#select covariates
C <- dat %>%
  select(all_of(c("AGS_N3_23", "year", "month"))) %>%
  mutate(AGS_N3_23 = as.numeric(AGS_N3_23),
         across(where(is.factor), ~ as.numeric(as.character(.))))

plot.df2 <- data.frame(value = as.vector(as.matrix(C)),
                       variable = colnames(C)[rep(1:ncol(C), each = nrow(C))],
                       Z = as.factor(W),
                       IPW_stab = IPW_stab)
#rename
plot.df2$variable <- factor(plot.df2$variable, 
                            levels = c("AGS_N3_23", "year", "month"),
                            labels = c("district", "year", "month"))

#balance before weighting
p1a <- ggplot(plot.df2, aes(x = value, fill = Z)) +
  geom_histogram(aes(y=after_stat(density)),
                 alpha = 0.5, position = "identity", bins = 30) +
  scale_fill_manual(values=c("steelblue", "darkred")) + 
  labs(title="B: Original Covariate Balance")+ 
  theme_minimal() +
  theme(legend.position = "None") +
  facet_wrap( ~ variable, nrow = 8, scales="free")

#balance after weighting
p2 <- ggplot(plot.df2, aes(x = value, weight = IPW_stab, fill = Z)) +
  geom_histogram(aes(y=after_stat(density)),
                 alpha = 0.5, position = "identity", bins = 30) +
  scale_fill_manual(values=c("steelblue", "darkred")) + 
  labs(title="C: Covariate Balance after Weighting")+ 
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap( ~ variable, nrow = 8, scales="free")

p_cov <- (p1a+p2) +
  plot_layout(guides = "collect") +
  plot_annotation(title = "Covariate Balance before and after weighting") &
  theme(legend.position = "bottom")

#combine all
p_tot <- p1/p_cov +
  plot_annotation(title = 'Propensity score diagnostics.')

ggsave("Figures/check_cov_balance.pdf", p_tot, width= 8, height= 10)

# Supplement: check correlation of features -------------------------------------------------------
#get effect modifiers from data
EMs <- dat[,c(effect_modifiers)]

#create model matrix
EMs <- as.data.frame(model.matrix(~0+., data=EMs))

#update names 
names(EMs) <- c('migrants_perc', 'ppl_seeking_prot_perc',
                'pop_per_sqkm', 'pop_above65_perc',
                'pop_below6_perc', 'females_perc',
                'births_per_1000', 
                'care_allowance_rec_perc', 'cars_per1000',
                'forest_perc', 'water_bodies_perc', 'openspace_perc', 'livingspace_sqm_pp',
                'hospital_beds_per1000', 'new_apartments_per1000',
                'pop_care_needs_perc', 'life_exp_birth',
                'GISD','PM25', 'very rural', "very urban", "rural", "urban")
#plot correlations
EM_corr <- EMs %>%
  cor() %>% 
  ggcorrplot(type="lower", lab=TRUE, lab_size=3)

ggsave("Figures/Corr_matrix_features.pdf", EM_corr, width = 18, height = 12)

