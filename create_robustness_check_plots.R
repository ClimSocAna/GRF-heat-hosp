#setwd("")

library(sf)
library(tidyverse)
library(grf)
library(patchwork)
library(colorspace)

#load functions
source("functions.R")

# data prep -------------------------------------------------------
dat <- read.csv("data/weekcounts_climate_INKAR_1607.csv")

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

#redefine heatwave to be official definition of heatwave (3 days)
dat$heatwave_yes_hw <- ifelse(dat$heatday_counter >= 3, 1, 0) 

# Create dayofyear from 3-day group
dat$dayofyear <- (dat$dat_aufn_3day_cat * 3) + 1

# Create calendar_date by adding dayofyear to January 1st of each year
dat$date <- as.Date(paste0(dat$year, "-01-01")) + (dat$dayofyear - 1)

# Read the shapefile
shape_data <- st_read("data/vg5000_ebenen_1231/VG5000_KRS.shp")
shape_data <- rename(shape_data, AGS_N3_23=AGS)
shape_data$AGS_N3_23 <- as.factor(as.integer(shape_data$AGS_N3_23))

#EMs
effect_modifiers_imp <- c('Auslanderanteil', 'Schutzsuchende.an.Bevolkerung_corr_imp',
                          'Einwohner.65.Jahre.und.alter',
                          'Einwohner.unter.6.Jahre', 'Frauenanteil',
                          'Empfanger.von.Pflegegeld_imp', 'Pkw.Dichte_imp',
                          'Waldflache_imp', 'Wasserflache_imp', 'Wohnflache_imp',
                          'Krankenhausbetten_imp', 'Neubauwohnungen.je.Einwohner_imp',
                          'Pflegebedurftige_imp', 
                          "gisd_score",
                          'PM25',
                          "settlement_impurban", 
                          "settlement_impvery_rural",
                          "settlement_imprural",
                          "settlement_impvery_urban")
#for covid plots
dat2 <- dat[as.numeric(as.character(dat$year)) <= 2019,]

## create a df with the names of each robustness check
#and what needs to be included so we can iterate through
checks <- data.frame(check= c("heatday_98th", "heatwave", "abs24", 
                              "heatday_98th", "lag 1", "lag 2",
                              "heatday_98th", "exclCovid"),
                     sens_anal = c(rep("heat_def",3),
                                   rep("lags", 3),
                                   rep("no_covid",2)))

# plot ATE across robustness checks ----------------------------------------------------------
## load the causal forests but only keep the CATE predictions and dr.scores predictions

#get estimates for main analysis
load("Models/GRF_heatindex_perc_98th_exclCovidno_10k.RData")

dat$heatwave_yes <- ifelse(dat$heatday_heatindexabove0.98thperc > 0, 1, 0) 
prev_hw = mean(dat$heatwave_yes) 
stab_weights =  (dat$heatwave_yes - tau.forest$W.hat) * (prev_hw * (1 - prev_hw)) / (tau.forest$W.hat * (1 - tau.forest$W.hat))

CATE1 <- data.frame(t(average_treatment_effect(tau.forest, debiasing.weights = stab_weights)))
CATE1$check <- "perc98th\n(main analysis)"

#get estimates for heatwave definition
load("Models/GRF_heatindex_heatwave_exclCovidno_10k.RData")

#prep for calculating scores
prev_hw = mean(dat$heatwave_yes_hw)
stab_weights =  (dat$heatwave_yes_hw - tau.forest$W.hat) * (prev_hw * (1 - prev_hw)) / (tau.forest$W.hat * (1 - tau.forest$W.hat))

CATE2 <- data.frame(t(average_treatment_effect(tau.forest, debiasing.weights = stab_weights)))
CATE2$check <- "heatwave"

#get estimates for lag 1 before
load("Models/GRF_heatindex_lag1_exclCovidno_10k.RData")

#prep for calculating scores
dat$heatwave_yes <- ifelse(dat$heatdaylag_heatindexabove0.98thperc > 0, 1, 0) 
prev_hw = mean(dat$heatwave_yes) 
stab_weights =  (dat$heatwave_yes - tau.forest$W.hat) * (prev_hw * (1 - prev_hw)) / (tau.forest$W.hat * (1 - tau.forest$W.hat))

CATE3 <- data.frame(t(average_treatment_effect(tau.forest, debiasing.weights = stab_weights)))
CATE3$check <- "lag 1"

#get estimates for absolute heat cutoff
load("Models/GRF_heatindex_abs_24_exclCovidno_10k.RData")

#prep for calculating scores
dat$heatwave_yes <- ifelse(dat$heatday_heatindexabove24 > 0, 1, 0) 
prev_hw = mean(dat$heatwave_yes) 
stab_weights =  (dat$heatwave_yes - tau.forest$W.hat) * (prev_hw * (1 - prev_hw)) / (tau.forest$W.hat * (1 - tau.forest$W.hat))

CATE4 <- data.frame(t(average_treatment_effect(tau.forest, debiasing.weights = stab_weights)))
CATE4$check <- "abs24"

#get estimates for lag 3-6 days before
load("Models/GRF_heatindex_lag2_exclCovidno_10k.RData")

#prep for calculating scores
dat$heatwave_yes <- ifelse(dat$heatdaylag2_heatindexabove0.98thperc > 0, 1, 0)
prev_hw = mean(dat$heatwave_yes)
stab_weights =  (dat$heatwave_yes - tau.forest$W.hat) * (prev_hw * (1 - prev_hw)) / (tau.forest$W.hat * (1 - tau.forest$W.hat))

CATE5 <- data.frame(t(average_treatment_effect(tau.forest, debiasing.weights = stab_weights)))
CATE5$check <- "lag 2"

#get estimates for analysis excluding covid
load("Models/GRF_heatindex_perc_98th_exclCovidyes_10k.RData")

#prep for calculating scores
dat2$heatwave_yes <- ifelse(dat2$heatday_heatindexabove0.98thperc > 0, 1, 0) 
prev_hw = mean(dat2$heatwave_yes) 
stab_weights =  (dat2$heatwave_yes - tau.forest$W.hat) * (prev_hw * (1 - prev_hw)) / (tau.forest$W.hat * (1 - tau.forest$W.hat))

CATE6 <- data.frame(t(average_treatment_effect(tau.forest, debiasing.weights = stab_weights)))
CATE6$check <- "exclCovid"

#combine all
CATE <- bind_rows(CATE1, CATE2, CATE3, CATE4, CATE5, CATE6)
CATE <- CATE %>%
  mutate(lower = estimate - 1.96*std.err,
         upper = estimate + 1.96*std.err,
         check = factor(check,
                        levels=c("perc98th\n(main analysis)", "abs24", 
                                 "heatwave",
                                 "lag 1", "lag 2",
                                 "exclCovid"),
                        labels=c("perc98th\n(main analysis, lag 0)", "abs24", 
                                 "heatwave",
                                 "short-term lag (days 3-5)", "extended lag (days 6-8)",
                                 "exclCovid"))) 

CATE <- CATE[,c("check","estimate", "lower", "upper")]

write.csv(CATE, file="Tables/check_ATEs.csv")

# plot CATE estimates across robustness checks ---------------------------------------------------------
#load results
CATE_q <- read.csv("Tables/Quantile_ATE_GRF_heat_defperc_98th_exclCovidno.csv")
CATE_q$check <- "heatday_98th"
CATE_q1 <- read.csv("Tables/Quantile_ATE_GRF_heat_defabs_24_exclCovidno.csv")
CATE_q1$check <- "abs24"
CATE_q2 <- read.csv("Tables/Quantile_ATE_GRF_heat_deflag1_exclCovidno.csv")
CATE_q2$check <- "lag 1"
CATE_q3 <- read.csv("Tables/Quantile_ATE_GRF_heat_deflag2_exclCovidno.csv")
CATE_q3$check <- "lag 2"
CATE_q4 <- read.csv("Tables/Quantile_ATE_GRF_heat_defheatwave_exclCovidno.csv")
CATE_q4$check <- "heatwave"
CATE_q5 <- read.csv("Tables/Quantile_ATE_GRF_heat_defperc_98th_exclCovidyes.csv")
CATE_q5$check <- "exclCovid"

#combine
CATE_q <- bind_rows(CATE_q, CATE_q1, CATE_q2, CATE_q3, CATE_q4, CATE_q5)

#get confidence intervals
CATE_q <- CATE_q %>% 
  mutate(lower=estimate - 1.96*std.err,
         upper=estimate + 1.96*std.err, 
         label = paste0(round(estimate, 2),
                        " (95%CI:", round(lower, 2),
                        ";" , round(upper, 2),
                        ")"))

#attach check df so we can loop
CATE_q <- left_join(CATE_q, checks, by ="check")

#make sure everything is ordered correctly
CATE_q <- CATE_q %>%
  mutate(check = factor(check, 
                        levels = c("heatday_98th", "abs24","lag 1","lag 2" ,"heatwave", "exclCovid"),
                        labels = c("perc98th\n(main analysis, lag 0)", "abs24",
                                   "short-term lag (days 3-5)", "extended lag (days 6-8)",
                                   "heatwave", "exclCovid")))
plot_list <- vector(mode = "list")

#loop through and save
for (sens in unique(checks$sens_anal)) {
  
  plot_list[[sens]] <- CATE_q %>%
    filter(sens_anal == sens) %>% 
    ggplot() +
    aes(x = ranking, y = estimate, color=check) +
    geom_point(position=position_dodge(0.2)) +
    geom_errorbar(aes(ymin=estimate-1.96*std.err, ymax=estimate+1.96*std.err),
                  width=.2, position=position_dodge(0.2)) +
    geom_hline(yintercept = 0) +
    theme_minimal() +
    theme(legend.position="bottom") +
    guides(color= guide_legend(title = "check")) +
    labs(y="ATE", x="Quartile")
  
  ggsave(paste0("Figures/check_qunatile_ATEs_", sens, ".pdf"), plot_list[[sens]],
         width = 7, height= 5)
  
}

# plot Quartile maps across robustness checks ---------------------------------------------------------
#load individual CATEs
CATE_q_i <- read.csv("Tables/Quantile_ind.ATE_GRF_heat_defperc_98th_exclCovidno.csv")
CATE_q_i$check <- "heatday_98th"
CATE_q_i1 <- read.csv("Tables/Quantile_ind.ATE_GRF_heat_defabs_24_exclCovidno.csv")
CATE_q_i1$check <- "abs24"
CATE_q_i2 <- read.csv("Tables/Quantile_ind.ATE_GRF_heat_defheatwave_exclCovidno.csv")
CATE_q_i2$check <- "heatwave"
CATE_q_i3 <- read.csv("Tables/Quantile_ind.ATE_GRF_heat_defperc_98th_exclCovidyes.csv")
CATE_q_i3$check <- "exclCovid"

CATE_i <- bind_rows(CATE_q_i, CATE_q_i1, CATE_q_i2, CATE_q_i3)

#count occurences of CATE per district
CATE_qs <- CATE_i %>%
  mutate(AGS_N3_23 = as.factor(AGS_N3_23)) %>%
  group_by(AGS_N3_23, check) %>%
  mutate(obs_distr = n()) %>%
  ungroup() %>%
  group_by(AGS_N3_23, ranking, check) %>%
  reframe(N_occurence = n(), 
          perc_occurence = N_occurence/obs_distr * 100) %>%
  distinct() %>%
  ungroup()

#if some rows are missing it means that there are no occurences
# so we can fill them with zeros
CATE_qs <- CATE_qs %>%
  complete(AGS_N3_23, check, ranking = 1:4,
           fill = list(perc_occurence = 0,
                       N_occurence = 0))

#make sure factor ordering is correct
CATE_qs$ranking <- factor(CATE_qs$ranking, levels = c("1","2","3","4"),
                          labels = c("Q1: Moderate decrease",
                                     "Q2: Minor decrease",
                                     "Q3: Minor increase",
                                     "Q4: Moderate increase"))

#join with shape data
plot_map <- left_join(shape_data, CATE_qs, by="AGS_N3_23")

## get common occurence of CATE quartile per district
#add max rank variable
plot_map <- plot_map %>%
  mutate(ranking = factor(ranking, labels=c("Q1","Q2","Q3","Q4"))) %>%
  group_by(AGS_N3_23, check) %>%
  mutate(max_rank = ranking[which.max(perc_occurence)],
         check_perc = ifelse(max(perc_occurence)>35, "over 35%", "under 35%")) 

#color palette most common occurence of CATE quartile per district
palette_u <- c('#fee6ce','#fdae6b','#f16913','#a63603')

#attach check df so we can loop
plot_map <- left_join(plot_map, checks, by ="check")

#make sure everything is ordered correctly
plot_map <- plot_map %>%
  mutate(check = factor(check,
                        levels = c("heatday_98th", "abs24","heatwave", "exclCovid"),
                        labels = c("perc98th\n(main analysis, lag 0)", "abs24","heatwave","exclCovid")))

plot_list2 <- vector(mode = "list")

#loop through and save
for (sens in c("heat_def", "no_covid")) {

  ## plot most commonly occuring CATEs
  plot_list2[[sens]] <- plot_map %>%
    filter(sens_anal == sens) %>% 
    ggplot() +
    geom_sf(aes(fill = max_rank), color="#555555", linewidth = 0.05) +
    theme_minimal() +
    scale_fill_manual(values=palette_u, name = "") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          strip.text = element_text(size=11),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          legend.position = "bottom") +
    facet_wrap(~check)
  
  ggsave(paste0("Figures/check_", sens, "_Quartile_map_red.pdf"), device=cairo_pdf, plot_list2[[sens]],
         width = 7, height = 6)  
}

# plot VIM & covariate distribution  ------------------------------------------------
# feature importance plot
VI <- read.csv("Tables/GRF_TE_VIM_GRF_heat_deflag1_exclCovidno.csv")
VI$check <- "lag 1"
VI2 <- read.csv("Tables/GRF_TE_VIM_GRF_heat_defheatwave_exclCovidno.csv")
VI2$check <- "heatwave"
VI3 <- read.csv("Tables/GRF_TE_VIM_GRF_heat_defabs_24_exclCovidno.csv")
VI3$check <- "abs24"
VI4 <- read.csv("Tables/GRF_TE_VIM_GRF_heat_defperc_98th_exclCovidno.csv")
VI4$check <- "heatday_98th"
VI5 <- read.csv("Tables/GRF_TE_VIM_GRF_heat_deflag2_exclCovidno.csv")
VI5$check <- "lag 2"
VI6 <- read.csv("Tables/GRF_TE_VIM_GRF_heat_defperc_98th_exclCovidyes.csv")
VI6$check <- "exclCovid"

#combine
VI <- bind_rows(VI4, VI2, VI, VI3, VI5, VI6)

##covariate distribution
ATE_quant <- read.csv("Tables/Quantile_ATE_cov.str_GRF_heat_deflag1_exclCovidno.csv")
ATE_quant$check <- "lag 1"
ATE_quant2 <- read.csv("Tables/Quantile_ATE_cov.str_GRF_heat_defheatwave_exclCovidno.csv")
ATE_quant2$check <- "heatwave"
ATE_quant3 <- read.csv("Tables/Quantile_ATE_cov.str_GRF_heat_defabs_24_exclCovidno.csv")
ATE_quant3$check <- "abs24"
ATE_quant4 <- read.csv("Tables/Quantile_ATE_cov.str_GRF_heat_defperc_98th_exclCovidno.csv")
ATE_quant4$check <- "heatday_98th"
ATE_quant5 <- read.csv("Tables/Quantile_ATE_cov.str_GRF_heat_deflag2_exclCovidno.csv")
ATE_quant5$check <- "lag 2"
ATE_quant6 <- read.csv("Tables/Quantile_ATE_cov.str_GRF_heat_defperc_98th_exclCovidyes.csv")
ATE_quant6$check <- "exclCovid"

#combine
ATE_quant <- bind_rows(ATE_quant, ATE_quant2, ATE_quant3, ATE_quant4, ATE_quant5, ATE_quant6)
ATE_quant <- ATE_quant %>%
  mutate(variable=effect_modifiers)

ATE_quant$ranking <- factor(ATE_quant$ranking, levels = c("Q1","Q2","Q3","Q4"),
                            labels = c("Q1: moderate decrease",
                                       "Q2: minor decrease",
                                       "Q3: minor increase",
                                       "Q4: moderate increase"))

#create df to match Ems with domain
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

#join
VI_LOO <- left_join(VI, domain, by="variable")
ATE_quant <- left_join(ATE_quant, domain, by="variable")

#get the VIM ranking
var_order <- VI_LOO %>%
  mutate(domain = factor(domain,
                         levels = c("Demographic", "Health & Social", "Environment"))) %>%
  filter(check == "heatday_98th") %>%
  arrange(desc(tevim_s)) %>%
  pull(variable)

# join VIM rank, sort rows by domain then importance
#get confidence intervals
VI_LOO <- VI_LOO %>%
  mutate(
    variable = factor(variable, levels = var_order),
    lower = tevim_s - 1.96 * std_err,
    upper = tevim_s + 1.96 * std_err)

#order variable by var_order
ATE_quant <- ATE_quant %>%
  mutate(variable = factor(variable, levels = var_order),
         domain =  factor(domain, levels = c("Demographic", "Health & Social", "Environment")))  

#update names (make sure the levels are still in that order)
levels(VI_LOO$variable) <- levels(ATE_quant$variable) <- c('% seeking protection',
                                                           '% below age 6', 'PM 2.5 in [ug/m3]',
                                                           'Living space pp in sqm',
                                                           'Cars per 1,000','% above age 65','% migrants',
                                                           'GISD','Hospital beds\nper 1,000',
                                                           '% forest',
                                                           
                                                           '% women','% water bodies',
                                                           '% with care needs',
                                                           'New apartments\nper 1,000',
                                                           '% receiving\ncare allowance',
                                                           
                                                           "Rural", "Very urban", 'Urban',"Very rural")


# plot VIM and covariate distribution seperately (to not overcrowd the plot)
#attach check df so we can loop
VI_LOO <- left_join(VI_LOO, checks, by ="check")
ATE_quant <- left_join(ATE_quant, checks, by ="check")

#make sure check variables are ordered correctly
VI_LOO <- VI_LOO %>%
  mutate(check = factor(check, 
                        levels =  c("heatday_98th", "abs24","lag 1","lag 2" ,"heatwave","exclCovid"),
                        labels =  c("perc98th\n(main analysis, lag 0)",  "absolute cut-off",
                                    "short-term lag (days 3-5)", "extended lag (days 6-8)", "heatwave", "excl. Covid yrs")))
ATE_quant <- ATE_quant %>%
  mutate(check = factor(check, 
                        levels =  c("heatday_98th", "abs24","lag 1","lag 2" ,"heatwave","exclCovid"),
                        labels =  c("perc98th\n(main analysis, lag 0)",  "absolute cut-off",
                                    "short-term lag (days 3-5)", "extended lag (days 6-8)", "heatwave", "excl. Covid yrs")))


#dim down the palette a little
palette <- c('#fee6ce','#fdae6b','#f16913','#a63603')
hsl <- as(hex2RGB(palette), "HLS")
hsl@coords[, "S"] <- hsl@coords[, "S"] * 0.95
palette_dim <- hex(as(hsl, "RGB"))

#color per domain
# define a color per domain
domain_col <- c("#B22272","#005eb8","#636B05")
alphas  <- c(1, 0.5, 0.1)

#empty list
plot_list3 <- vector(mode = "list")

bar_width <- 0.7

#loop through and save
for (sens in unique(checks$sens_anal)) {

  plot_list3[[paste0(sens,"_VIM")]] <- VI_LOO  %>%
    filter(sens_anal == sens) %>% 
    ggplot(aes(x = tevim_s, y = variable, 
               color = domain, fill = domain, alpha = check,
               group=interaction(domain, check))) +
    geom_col(position = position_dodge2(width = 0.8), width = 0.7) +
    scale_y_discrete(limits = rev, drop = FALSE) +
    scale_x_continuous(limits = c(-0.0007,0.28))+
    scale_fill_manual(values = domain_col, name="Domain")+
    scale_color_manual(values = domain_col, name="Domain")+
    scale_alpha_manual(values = alphas) +
    guides(fill = guide_legend(override.aes = list(alpha = 1))) +
    theme_minimal() +
    theme(
      axis.title.y  = element_blank(),
      axis.text.y   = element_text(),
      strip.text.y  = element_blank(),
      legend.position = "bottom",
      legend.box = "vertical") +
    labs(x = "Variable Importance (scaled)",
         y = NULL, title = NULL)
  
  #save
  ggsave(paste0("Figures/check_VIM_", sens, ".pdf"),
         plot_list3[[paste0(sens,"_VIM")]],  width = 7, height = 6)
  ggsave(paste0("Figures/check_VIM_", sens, ".svg"),
         plot_list3[[paste0(sens,"_VIM")]],  width = 7, height = 6)

}

#indicator values across quartiles
#get top ten by check

for (check1 in c("absolute cut-off", "short-term lag (days 3-5)", "extended lag (days 6-8)",
                 "heatwave", "excl. Covid yrs")) {
  
  top10 <- VI_LOO %>% 
    filter(check == check1) %>%
    arrange(desc(tevim_s)) %>%
    slice_max(tevim_s, n = 10) %>%
    pull(variable)
  
  ATE_quant_top10 <- ATE_quant %>%
    filter(check == check1 & variable %in% top10)
  
    ATE_quant_top10$variable <- factor(ATE_quant_top10$variable,
                                       levels=top10)
    
    plot_list3[[paste0(check1,"_cov.distr")]] <- ATE_quant_top10 %>%
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
        legend.position = "none"
      ) +
      labs(title=paste0(check1), x = "CATE Quartile", y = NULL)
    
    ggsave(paste0("Figures/check_covstr_", check1, ".pdf"),
           plot_list3[[paste0(check1,"_cov.distr")]],  width = 7, height = 7)
    ggsave(paste0("Figures/check_covstr_", check1, ".svg"),
           plot_list3[[paste0(check1,"_cov.distr")]],  width = 7, height = 7)
}
