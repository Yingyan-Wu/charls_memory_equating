# 2 equating method for memory scores
# Created by Yingyan Wu and Yuan Zhang
# Mar.14.2023

#---- Package loading + options ----
rm(list = ls())

if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')
}

p_load("here", "readr", "tidyverse", "magrittr", "plyr", "haven", "labelled",
       "gtsummary", "equate", "twang", "spatstat", "survey", "moments")
options(scipen = 999)
#---- function ----
source(here::here("scripts", "functions.R"))
source(here::here("scripts", "equating_cog_charls","functions_weight_dev.R"))

#---- Load the datasets ----
load(here::here("data", "analysis_data", "charls_equating_samp_151820.RData"))

#---- 1. Restrict to a calibration group ----
# Restrict to a “calibration group” that attends all three waves and
# This is a highly selected, “biased” sample.
harmonized_addw5_selected %>% select(contains("age_y")) %>% summary()

harmonized_long_151820 %>% filter(age_y >= 45 & age_y < 90) %>%
  select(contains("age_y"), wave) %>% split(.$wave) %>% map(summary)

charls_selected <- harmonized_long_151820 %>%
  na.omit() %>%
  filter(age_y >= 45 & age_y < 90 & raeduc_c_v2 >= 4 & raeduc_c_v2 != 10) %>%
  mutate(treat_1820 = case_when(wave %in% c("4", "5") ~ 1,
                                wave == "3" ~ 0),
        treat_18 = case_when(wave == "4" ~ 1,
                              wave == "3" ~ 0),
         treat_20 = case_when(wave == "5" ~ 1,
                              wave == "3" ~ 0),
         work_status = case_when(lbrf_c %in% c(1, 2) ~ 1, # Agriculture employed
                                 lbrf_c %in% c(3, 4, 5) ~ 2, # Non-agriculture employed
                                 lbrf_c == 6 ~ 3, # Unemployed
                                 lbrf_c == 7 ~ 4, # retired
                                 lbrf_c == 8 ~ 5 # Never work
         ),
         unweight = 1) %>%
  mutate(across(c("childhood_edu", "hukou_impute", "work_status"), factor))

table(charls_selected$childhood_edu, useNA = "ifany")
table(charls_selected$mstat, useNA = "ifany")

#---- 2a. Generate weights ----
# Generate weights, to ensure the age/education distributions are the same 
# in each study wave. This will remove the aging and education effects from the 
# equating algorithm. You can generate weights using any approach, here
# we used IOW, IPW also works.

##---- IOW ----
###---- Setup ----
charls_covbal <- charls_selected %>%
  remove_labels() %>%
  remove_val_labels() %>%
  as.data.frame()

selected_vars <- colnames(charls_covbal %>% 
                            select(female, childhood_edu, rural, mstat,
                                   work_status, age_y))
order_var <- c("age_y", "rural", "female", "mstat", 
               "childhood_edu:2", "childhood_edu:3", "childhood_edu:1", 
               "work_status:1", "work_status:2", "work_status:3", 
               "work_status:4", "work_status:5")

###---- Unweighted ----
unw_test <- checkcovbal(charls_covbal, "unweight", "treat_1820")
unw_test$covbal_plot

###---- pS ----
pS <- sum(charls_covbal$treat_1820)/nrow(charls_covbal)
# P(wave 2018/2020)

##---- weight ----
###---- Weight 1: education + age ----
charls_covbal %<>% cbind(
  weight_dev(charls_covbal, "treat_1820 ~ childhood_edu + age_y", "1",
             "treat_1820", "pS"))

charls_covbal %>%
  ggplot() +
  geom_density(aes(x = pS_Z1, linetype = factor(treat_1820), weight = unweight)) +
  scale_linetype_manual(name = "Sample", values = c("0" = 1, "1" = 2),
                        labels = c("Wave 2015", "Wave 2018/2020")) +
  labs(x = "Propensity score", y = "Density") + theme_bw()

# Covbalance plot
w1_test <- checkcovbal(charls_covbal, "SW1_twang_trunc99", "treat_1820")
covbal_comp_plot(w1_test$covbal_data, unw_test$covbal_data,
                 "2015", "2018/2020")

###---- Weight 2: education + age + work ----
charls_covbal %<>% cbind(
  weight_dev(charls_covbal, 
             "treat_1820 ~ childhood_edu + age_y + work_status", "2",
             "treat_1820", "pS"))

charls_covbal %>%
  ggplot() +
  geom_density(aes(x = pS_Z2, linetype = factor(treat_1820), weight = unweight)) +
  scale_linetype_manual(name = "Sample", values = c("0" = 1, "1" = 2),
                        labels = c("Wave 2015", "Wave 2018/2020")) +
  labs(x = "Propensity score", y = "Density") + theme_bw()

# Covbalance plot
w2_test <- checkcovbal(charls_covbal, "SW2_twang_trunc99", "treat_1820")
covbal_comp_plot(w2_test$covbal_data, unw_test$covbal_data,
                 "2015", "2018/2020")

###---- Weight 3: education + age + work + female ----
charls_covbal %<>% cbind(
  weight_dev(charls_covbal, 
             "treat_1820 ~ childhood_edu + age_y + work_status + female", "3",
             "treat_1820", "pS"))

charls_covbal %>%
  ggplot() +
  geom_density(aes(x = pS_Z3, linetype = factor(treat_1820), weight = unweight)) +
  scale_linetype_manual(name = "Sample", values = c("0" = 1, "1" = 2),
                        labels = c("Wave 2015", "Wave 2018/2020")) +
  labs(x = "Propensity score", y = "Density") + theme_bw()

# Covbalance plot
w3_test <- checkcovbal(charls_covbal, "SW3_twang_trunc99", "treat_1820")
covbal_comp_plot(w3_test$covbal_data, unw_test$covbal_data,
                 "2015", "2018/2020") +
  scale_shape_manual(values = c(21, 16), labels = c("Unweighted", "Weighted")) +
  theme(legend.position = "right")

###---- Weight 4: education + age + work + female*rural ----
charls_covbal %<>% cbind(
  weight_dev(charls_covbal, 
             "treat_1820 ~ childhood_edu + age_y + work_status + female*rural", "4",
             "treat_1820", "pS"))

charls_covbal %>%
  ggplot() +
  geom_density(aes(x = pS_Z4, linetype = factor(treat_1820), weight = unweight)) +
  scale_linetype_manual(name = "Sample", values = c("0" = 1, "1" = 2),
                        labels = c("Wave 2015", "Wave 2020")) +
  labs(x = "Propensity score", y = "Density") + theme_bw()

# Covbalance plot
w4_test <- checkcovbal(charls_covbal, "SW4_twang_trunc99", "treat_1820")
covbal_comp_plot(w4_test$covbal_data, unw_test$covbal_data,
                 "2015", "2018/2020")

###---- Weight 4b: education + age + work_status*rural + female*rural----
charls_covbal %<>% cbind(
  weight_dev(charls_covbal, 
             "treat_1820 ~ childhood_edu + age_y + work_status*rural + female*rural", 
             "4b", "treat_1820", "pS"))

charls_covbal %>%
  ggplot() +
  geom_density(aes(x = pS_Z4b, linetype = factor(treat_1820), weight = unweight)) +
  scale_linetype_manual(name = "Sample", values = c("0" = 1, "1" = 2),
                        labels = c("Wave 2015", "Wave 2020")) +
  labs(x = "Propensity score", y = "Density") + theme_bw()

# Covbalance plot
w4b_test <- checkcovbal(charls_covbal, "SW4b_twang_trunc99", "treat_1820")
covbal_comp_plot(w4b_test$covbal_data, unw_test$covbal_data,
                 "2015", "2018/2020")  +
  scale_shape_manual(values = c(21, 16), labels = c("Unweighted", "Weighted")) +
  theme(legend.position = "right")

###---- Weight 4c: education*age + work*rural + female*rural ----
charls_covbal %<>% cbind(
  weight_dev(charls_covbal, 
             "treat_1820 ~ childhood_edu*age_y + work_status*rural + female*rural", 
             "4c", "treat_1820", "pS"))

charls_covbal %>%
  ggplot() +
  geom_density(aes(x = pS_Z4c, linetype = factor(treat_1820), weight = unweight)) +
  scale_linetype_manual(name = "Sample", values = c("0" = 1, "1" = 2),
                        labels = c("Wave 2015", "Wave 2018/020")) +
  labs(x = "Propensity score", y = "Density") + theme_bw()

# Covbalance plot
w4c_test <- checkcovbal(charls_covbal, "SW4c_twang_trunc99", "treat_1820")
covbal_comp_plot(w4c_test$covbal_data, unw_test$covbal_data,
                 "2015", "2018/2020")

###---- Weight 4d: education*age + work*rural + female*rural + age*rural ----
charls_covbal %<>% cbind(
  weight_dev(charls_covbal, 
             "treat_1820 ~ childhood_edu*age_y + work_status*rural + female*rural +
             age_y*rural", "4d",
             "treat_1820", "pS"))

charls_covbal %>%
  ggplot() +
  geom_density(aes(x = pS_Z4d, linetype = factor(treat_1820), weight = unweight)) +
  scale_linetype_manual(name = "Sample", values = c("0" = 1, "1" = 2),
                        labels = c("Wave 2015", "Wave 2020")) +
  labs(x = "Propensity score", y = "Density") + theme_bw()

# Covbalance plot
w4d_test <- checkcovbal(charls_covbal, "SW4d_twang_trunc99", "treat_1820")
covbal_comp_plot(w4d_test$covbal_data, unw_test$covbal_data,
                 "2015", "2018/2020")

##---- save final weights covariate balance data ----
save(unw_test, w4b_test,
     file = here::here("data", "analysis_data", "eqt_covbal_data_2015_20182020.RData"))

#----2b. Check distribution for weighted 2018 and 2015 ----
# Unweighted 2015
r3freqim_IOW <- freqtab(charls_covbal[charls_covbal$wave == "3", "imrc_cleaned"])
r3freqdl_IOW <- freqtab(charls_covbal[charls_covbal$wave == "3", "dlrc_cleaned"])
# Weighted 2018/2020
r45freqim_IOW <- charls_covbal %>%
  filter(wave %in% c("4", "5")) %>%
  dplyr::count(imrc_cleaned, wt = SW4b_twang_trunc99) %>%
  as.freqtab()

r45freqdl_IOW <- charls_covbal %>%
  filter(wave %in% c("4", "5")) %>%
  dplyr::count(dlrc_cleaned, wt = SW4b_twang_trunc99) %>%
  as.freqtab()

#---- 3. Perform the equating ----
##---- Immediate recall ----
# Wave 4
eqim_linear_IOW_15_1820 <- equate(r45freqim_IOW, r3freqim_IOW, type = "l")
eqim_mean_IOW_15_1820 <- equate(r45freqim_IOW, r3freqim_IOW, type = "mean")
eqim_eqp_IOW_15_1820 <- equate(r45freqim_IOW, r3freqim_IOW, type = "equipercentile")

eqim_linear_IOW_15_1820
eqim_mean_IOW_15_1820
eqim_eqp_IOW_15_1820$concordance

plot(eqim_eqp_IOW_15_1820, eqim_mean_IOW_15_1820, eqim_linear_IOW_15_1820)

##---- Delayed recall ----
# Wave 4
eqdl_linear_IOW_15_1820 <- equate(r45freqdl_IOW, r3freqdl_IOW, type = "l")
eqdl_mean_IOW_15_1820 <- equate(r45freqdl_IOW, r3freqdl_IOW, type = "mean")
eqdl_eqp_IOW_15_1820 <- equate(r45freqdl_IOW, r3freqdl_IOW, type = "equipercentile")

eqdl_linear_IOW_15_1820
eqdl_mean_IOW_15_1820
eqdl_eqp_IOW_15_1820$concordance

plot(eqdl_eqp_IOW_15_1820, eqdl_mean_IOW_15_1820, eqdl_linear_IOW_15_1820)

##---- eqt plots ----
# png(here::here("output", "figures", "figure4_eqt_plot_lump20182020.png"),
#     width = 7, height = 5, units = "in", res = 300)
pdf(here::here("output", "figures", "figure4_eqt_plot_lump20182020.pdf"),
    width = 7, height = 5)
# tiff(here::here("output", "figures", "figure4_eqt_plot_lump20182020.tiff"),
#     width = 7, height = 5, units = "in", res = 600)
par(mfrow=c(1,2), mar=c(4, 4, 2, 2), xaxs='i', yaxs='i')
plot(eqim_eqp_IOW_15_1820, xlab = "Original Score (Wave 2018/2020)",
     ylab = "Equated Score", legendtext = "Equipercentile",
     morepars = list(ylim = c(0, 10)))
title(main = "Immediate word recall score", line = 1)
mtext("A", side = 3, line = 0.5, cex = 1, adj = -0.28, font = 2)
plot(eqdl_eqp_IOW_15_1820, xlab = "Original Score (Wave 2018/2020)",
     ylab = "Equated Score", legendtext = "Equipercentile",
     morepars = list(ylim = c(0, 10)))
title(main = "Delayed word recall score", line = 1)
mtext("B", side = 3, line = 0.5, cex = 1, adj = -0.28, font = 2)
dev.off()
# graphics.off()

# Table 
equated_score_table <- 
  rbind(eqim_eqp_IOW_15_1820$concordance %>% mutate(Score = "Immediate"), 
      eqdl_eqp_IOW_15_1820$concordance %>% mutate(Score = "Delayed")) %>%
  select(Score, scale, yx) %>%
  set_colnames(c("Score", "Original score", "Wave 2018/2020 Equated score")) %>%
  mutate_if(is.numeric, round, 2)

writexl::write_xlsx(
  equated_score_table,
  path = here::here("output", "tables", "table3_lumping_equating.xlsx"))

##---- Checked the distribution in the calibration group! ----
charls_covbal %<>%
  # Wave 2018
  left_join(eqim_eqp_IOW_15_1820$concordance %>% 
              dplyr::rename(imrc_cleaned = scale, r4imrc_eqt = yx) %>%
              select(contains("imrc")) %>%
              mutate(wave = "4"),
            by = c("wave", "imrc_cleaned")) %>%
  left_join(eqdl_eqp_IOW_15_1820$concordance %>% 
              dplyr::rename(dlrc_cleaned = scale, r4dlrc_eqt = yx) %>%
              select(contains("dlrc")) %>%
              mutate(wave = "4"),
            by = c("wave", "dlrc_cleaned")) %>%
  # Wave 2020
  left_join(eqim_eqp_IOW_15_1820$concordance %>% 
              dplyr::rename(imrc_cleaned = scale, r5imrc_eqt = yx) %>%
              select(contains("imrc")) %>%
              mutate(wave = "5"),
            by = c("wave", "imrc_cleaned")) %>%
  left_join(eqdl_eqp_IOW_15_1820$concordance %>% 
              dplyr::rename(dlrc_cleaned = scale, r5dlrc_eqt = yx) %>%
              select(contains("dlrc")) %>%
              mutate(wave = "5"),
            by = c("wave", "dlrc_cleaned"))

# top and bottom coded
charls_covbal %<>%
  mutate(across(contains("eqt"), 
                function(x) case_when(x > 10 ~ 10, x < 0 ~ 0, TRUE ~ x),
                .names = "{sub('eqt', 'eqt_adj', col)}"))

#raw score 3
charls_covbal %>% filter(wave == "3") %>%
  select(contains("_cleaned")) %>%
  apply(., 2, function(x) list(proc_sum(x, more = T), kurtosis(x)))
# kurtosis: r3imrc: 3.343741; r3dlrc: 2.614592

# eqted
charls_covbal %>% filter(wave == "4") %>% 
  select(contains(c("r4imrc", "r4dlrc"))) %>%
  apply(., 2, function(x) list(proc_sum(x, more = T), kurtosis(x)))

charls_covbal %>% filter(wave == "5") %>% 
  select(contains(c("r5imrc", "r5dlrc"))) %>%
  apply(., 2, function(x) list(proc_sum(x, more = T), kurtosis(x)))
# distribution very similar between ee and top/bottom coded ee scores
# unadjusted (top/bottom coded) closer to the raw score 3
# Do not top/bottom code dlrc

#---- 4. Apply the equipercentile equating algorithm to the full sample in the focal group -----
for (w in 4:5){
  for (test in c("im", "dl")){
    temp <- get(paste0("eq", test, "_eqp_IOW_15_1820"))$concordance %>%
      dplyr::rename(!!paste0("r", w, test, "rc_cleaned") := scale, 
                    !!paste0("r", w, test, "rc_eqt") := yx) %>%
        select(-se)
    assign(paste0("eqt_", w, "_", test), temp)
  }
}

harmonized_addw5_selected %<>%
  left_join(eqt_4_im, by = "r4imrc_cleaned") %>%
  left_join(eqt_4_dl, by = "r4dlrc_cleaned") %>%
  left_join(eqt_5_im, by = "r5imrc_cleaned") %>%
  left_join(eqt_5_dl, by = "r5dlrc_cleaned")

# Sanity check
with(harmonized_addw5_selected, table(r4imrc_cleaned, r4imrc_eqt, useNA = "ifany"))
with(harmonized_addw5_selected, table(r4dlrc_cleaned, r4dlrc_eqt, useNA = "ifany"))
with(harmonized_addw5_selected, table(r5imrc_cleaned, r5imrc_eqt, useNA = "ifany"))
with(harmonized_addw5_selected, table(r5dlrc_cleaned, r5dlrc_eqt, useNA = "ifany"))

#---- Save the dataset  ----
# only ID and eqt scores
eqt_tib <- harmonized_addw5_selected %>%
  select(ID, contains("eqt"))
save(eqt_tib, file = here::here("data", "analysis_data", "eqt_rc_05012024.RData"))
# Save harmonized_addw5_selected
save(harmonized_addw5_selected, 
     file = here::here("data", "analysis_data", "charls_equating_samp_eqt_151820_05012024.RData"))
