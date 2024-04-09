# Data export for equating method
# Created by: Yingyan Wu
# Mar.13.2023

# For the specific missing characters in stata, the data was read using 
# the package `readstata13` and checked the distribution of refused responses
# and no response for the word recall scores
# The code for reading data using readstata13 was commented out below.

#---- Package loading + options ----
rm(list = ls())

if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')
}

p_load("here", "readr", "tidyverse", "magrittr", "plyr", "haven", "labelled",
       "glue", "gtsummary")

#---- function ----
source(here::here("scripts", "functions.R"))

#---- Load the datasets ----
##----harmonized ----
harmonized_variables <- c("ID", "householdID", "pnc", "communityID",
                          "ID_w1", "householdID_w1", 
                          # Weights
                          paste0("r", 1:4, "wtrespb"),
                          paste0("r", 1:4, "agey"),
                          "ragender",
                          paste0("h", 1:4, "rural"),
                          paste0("r", 1:4, "hukou"),
                          "raeduc_c",
                          paste0("r", 1:4, "mstat"),
                          paste0("r", 1:4, "lbrf_c"), # labor force status
                          paste0("r", 1:4, "imrc"), # immediate word recall
                          paste0("r", 1:4, "dlrc") # delayed word recall
)

harmonized <- read_dta(here::here("data", "raw_data", "harmonized",
                                  "H_CHARLS_D_Data.dta"),
                       col_select = all_of(harmonized_variables))

# harmonized <- read.dta13(here::here("data", "raw_data", "harmonized",
#                                     "H_CHARLS_D_Data.dta"),
#                          missing.type = TRUE,
#                          select.cols = all_of(harmonized_variables))

#---- Core data for cleaning the imrc and dlrc ----
# Adapted from Mingfei Dong's data reading script, 
# Crystal's reading result snippet from make_figures.R in exposure_trajectories
# and the IDRE website
# https://stats.idre.ucla.edu/r/codefragments/read_multiple/
getwd()
folders <- list.files(path = here::here("data", "raw_data"), full.names = TRUE)
datasets <- c("health_status_and_functioning", "cognition",
              "demographic_background", "work_retirement","sample_infor", "weights")

folder_index <- c(6, 7, 8) # 2015, 2018, 2020 waves only

for (i in folder_index){
  # get all file names
  files <- list.files(path = folders[i], full.names = TRUE)
  files <- files[str_detect(files, regex(glue_collapse(datasets, sep="|"), 
                                         ignore_case = T))]
  # Read in as list
  list <- lapply(files, read_dta)
  # list <- lapply(files, read.dta13)
  names(list) <- gsub(".*/(.*)\\..*", "\\1", files)
  # Assign names for each wave's list
  assign(paste0("wave", which(folder_index == i)+2), list)
}

rm(list)

##----join the datasets ----
# Waves
charls_w3 <- join_all(wave3, 
                      # by = c("ID", "householdID", "communityID"),
                      type = "left", match = "all") %>%
  rename_with(.cols = !c("ID", "householdID", "communityID"), 
              function(x){paste0("w3_", x)})
# colnames(charls_w3)

charls_w4 <- join_all(wave4, 
                      # by = c("ID", "householdID", "communityID"),
                      type = "left", match = "all") %>%
  rename_with(.cols = !c("ID", "householdID", "communityID"), 
              function(x){paste0("w4_", x)})
# colnames(charls_w4)

charls_w5 <- join_all(wave5, 
                      # by = c("ID", "householdID", "communityID"),
                      type = "left", match = "all") %>%
  rename_with(.cols = !c("ID", "householdID", "communityID"), 
              function(x){paste0("w5_", x)})
# colnames(charls_w5)

#---- New harmonized dataset ----
harmonized_addw5 <- harmonized %>%
  full_join(charls_w5 %>% select(ID), by = "ID")

#---- Age cleaning ----
# wave 5 age cleaning
age_w5 <- charls_w5 %>% select(ID, contains("w5_ba003"), contains("xrage"),
                               w5_imonth, w5_iyear) %>%
  mutate(w5_iwd = as.Date(paste0(w5_imonth, "/1/", w5_iyear), "%m/%d/%Y"),
         w5_rabdate = case_when(
           w5_ba003_2 == -1 ~
             as.Date(paste0("1/1/", w5_ba003_1), "%m/%d/%Y"),
           w5_ba003_3 == -1 ~
             as.Date(paste0(w5_ba003_2, "/1/", w5_ba003_1), "%m/%d/%Y"),
           !is.na(w5_ba003_3) ~
             as.Date(paste0(w5_ba003_2, "/", w5_ba003_3, "/", w5_ba003_1), 
                     "%m/%d/%Y")),
         r5agey = round(interval(start = w5_rabdate, 
                                 end = w5_iwd) / 
                          duration(num = 1, units = "years")))
harmonized_addw5 %<>%
  left_join(age_w5 %>% select(ID, r5agey), by = "ID")

age_y <- harmonized_addw5 %>% dplyr::select(contains("agey"), -contains("LHagey")) %>% 
  apply(., 1, impute_ages, survey_years = c(2011, 2013, 2015, 2018, 2020)) %>% t()

harmonized_addw5[, paste0("r", 1:5, "age_y")] <- age_y
rm(age_y)

harmonized_addw5 %<>%
  select(-contains("agey"),)

# Sanity check
harmonized_addw5 %>% select(contains("age_y")) %>%
  apply(., 2, proc_sum, more = T)

# Cross checked with birthdate and age in raw dataset, (r4age_y is the same)
# using r4age_y to impute r5age_y and ignore xrage

# test <- charls %>% select(ID, r4agey_v3, rabdate_v2)
# save(test, here::here("test.RData"))
# load(here::here("test.RData"))
# age_w5 %<>%
#   left_join(test, by = "ID")
# with(age_w5, table(r4agey_v3 == r4age_y, useNA = "ifany")) # all the same

#---- Gender ----
# wave 5 gender
gender_w5 <- charls_w5 %>%
  select(ID, w5_ba001, w5_ba002) %>%
  mutate(w5_gender = 
           case_when(w5_ba001 == w5_ba002|is.na(w5_ba002) ~ as.numeric(w5_ba001),
                     w5_ba001 != w5_ba002 ~ NA))

harmonized_addw5 %<>% 
  left_join(gender_w5, by = "ID")
# # Check harmonized ragender and w5_gender
# with(harmonized_addw5, table(w5_gender == ragender, useNA = "ifany"))
# harmonized_addw5 %>%
#   filter(w5_gender != ragender) %>% 
#   select(ID, contains("age"), contains("gender"), w5_ba001, w5_ba002) %>%
#   view()
# Use harmonized version of gender
harmonized_addw5 %<>%
  mutate(ragender_v2 = case_when(!is.na(ragender) ~ as.numeric(ragender),
                                 !is.na(w5_gender) ~ as.numeric(w5_gender)))

# # Sanity check
# with(harmonized_addw5, table(ragender_v2, ragender, useNA = "ifany"))
# harmonized_addw5 %>%
#   filter(is.na(ragender_v2)) %>% 
#   select(ID, contains("age"), contains("gender"), w5_ba001, w5_ba002) %>%
#   view()

harmonized_addw5 %<>%
  mutate(female = case_when(ragender_v2 == 1 ~ 0,
                            ragender_v2 == 2 ~ 1))
harmonized_addw5 %<>%
  select(-ragender, -w5_ba001, -w5_ba002, -ragender_v2)

#---- Marital status ----
with(charls_w5, table(w5_ba011, w5_ba012, useNA = "ifany"))

charls_w5 %<>%
  mutate(r5mstat = case_when(w5_ba012 == 1 ~ 3, # partnered
                             w5_ba011 %in% c(1, 2) ~ w5_ba011,
                             w5_ba011 %in% c(3, 4) ~ (w5_ba011 + 1),
                             w5_ba011 %in% c(5, 6) ~ (w5_ba011 + 2)))
with(charls_w5, table(r5mstat, w5_ba011, useNA = "ifany"))
with(charls_w5, table(r5mstat, w5_ba012, useNA = "ifany"))

harmonized_addw5 %<>%
  left_join(charls_w5 %>% select(ID, r5mstat), by = "ID")

harmonized_addw5 %<>%
  mutate(r3mstat = case_when(r3mstat %in% c(1, 3) ~ 1,
                             r3mstat %in% c(4, 5, 7, 8) ~ 0),
         r4mstat = case_when(r4mstat %in% c(1, 3) ~ 1,
                             r4mstat %in% c(4, 5, 7, 8) ~ 0),
         r5mstat = case_when(r5mstat %in% c(1, 3) ~ 1,
                             r5mstat %in% c(4, 5, 7, 8) ~ 0))

# with(harmonized_addw5, table(r3mstat, r4mstat, useNA = "ifany"))
# with(harmonized_addw5, table(r3mstat, r5mstat, useNA = "ifany"))

#---- Early life edu ----
# Wave 5
with(charls_w5, table(w5_ba010, w5_ba010_1, useNA = "ifany"))

charls_w5 %<>%
  mutate(r5educ_c = case_when(w5_ba010 %in% c(10, 11) ~ 10, # master + phd
                              TRUE ~ w5_ba010))
with(charls_w5, table(r5educ_c, w5_ba010, useNA = "ifany"))
with(charls_w5, table(r5educ_c, w5_ba010_1, useNA = "ifany"))

harmonized_addw5 %<>%
  left_join(charls_w5 %>% select(ID, r5educ_c, w5_ba010_1), by = "ID")

with(harmonized_addw5, table(r5educ_c, raeduc_c, useNA = "ifany"))
# Harmonized
harmonized_addw5 %<>%
  mutate(raeduc_c_v2 = case_when(!is.na(raeduc_c) ~ as.numeric(raeduc_c),
                                 is.na(raeduc_c) ~ r5educ_c)) %>%
  # with(harmonized_addw5, table(raeduc_c_v2, w5_ba010_1, useNA = "ifany"))
  mutate(childhood_edu = case_when(raeduc_c_v2 == 1 ~ 0,
                                   raeduc_c_v2 %in% c(2, 3, 4) ~ 1,
                                   raeduc_c_v2 %in% c(5, 6, 7) ~ 2,
                                   raeduc_c_v2 %in% c(8, 9, 10) ~ 3))
harmonized_addw5 %<>%
  select(-raeduc_c, -r5educ_c)

#---- labor force ----
# Adapted the harmonized code from wave1-wave4 harmonized data by g2aging
w5_labor_force_mat <- charls_w5 %>%
  select(ID, contains(c("w5_fa", "w5_fb", "w5_fc", "w5_fd", "w5_ff", "w5_fg", 
                        "w5_fh")), 
         w5_zworking, w5_xworking, w5_zrretired, w5_xgetjob, w5_xquitjob) %>%
  left_join(harmonized_addw5 %>% select(ID, contains("lbrf_c")), 
            by = "ID")
w5_labor_force_mat[w5_labor_force_mat == 999 | w5_labor_force_mat == 997 |
                     w5_labor_force_mat == 995 |
                     w5_labor_force_mat == -1] <- NA
summary(w5_labor_force_mat)

w5_labor_force_mat %<>%
  mutate(r5nowork = case_when(
    w5_fa001 == 2 & w5_fa004 == 2 & 
      (w5_fa005 == 2| (w5_fa005 == 1 & w5_fa007 == 2 & w5_fa008 == 2)) ~ 1,
    is.na(w5_fa001) & is.na(w5_fa004) & is.na(w5_fa005) &
      is.na(w5_fa007) & is.na(w5_fa008) ~ NA_real_,
    TRUE ~ 0),
    r5lbrf_c = case_when(
      # agricultural, employed
      w5_fa001 == 1 & w5_fa002_s2 == 2 ~ 1, 
      # agricultural, self-employed
      w5_fa001 == 1 & w5_fa002_s1 == 1 ~ 2, 
      # non-agricultural, employed
      w5_fa010 == 1 | w5_fa011 == 1 | w5_fg004 != 1 ~ 3, 
      # non-agricultural, self-employed
      w5_fa010 == 2 | w5_fa011 == 2 ~ 4, 
      # non-agricultural, family business
      w5_fa010 == 3 | w5_fa011 == 3 ~ 5,
      # unemployed: not working, stopped working and looking for work
      (r5nowork == 1 & 
         (((w5_fa019 != 6 | is.na(w5_fa019)) & w5_ff003 == 1) | w5_ff004 == 3)) |
        w5_xquitjob == 1 | (w5_zworking == 1 & w5_zrretired == 0) ~ 6, 
      # retired: # not working, has worked, and not looking for work
      (r5nowork == 1 & 
         ((w5_fa019 == 6 | w5_fa005 == 1 | !is.na(w5_fa019) | w5_ff001 > 0 | 
             !is.na(w5_fa016_1) | w5_fh001 == 1) | w5_ff005 == 3)) |
        w5_zrretired == 1 ~ 7,
      # never worked: no work currently and no recreational work, not looking for work, 
      # not retired in previous wave, never work in previous wave
      w5_zworking == 2 | 
        (w5_zrretired == 0 & (r5nowork == 1 & w5_ff002 == 2 & w5_ff003 == 2 & 
                                is.na(w5_fh001))) ~ 8, 
      # Supplement values from other related variables
      # employed: add recreational work and last year worked to employed
      w5_ff002 == 1 | (!is.na(w5_ff001_min) | !is.na(w5_ff001_max)) ~ 3, 
      # Supplement values from previous waves
      # unemployed: previous unemployed, currently not working
      r5nowork == 1 & (r4lbrf_c == 6 | (is.na(r4lbrf_c) & r3lbrf_c == 6) |
                         (is.na(r4lbrf_c) & is.na(r3lbrf_c) & r2lbrf_c == 6) |
                         (is.na(r4lbrf_c) & is.na(r3lbrf_c) & is.na(r2lbrf_c) &
                            r1lbrf_c == 6)) ~ 6, 
      # retired: previous retired, currently not working
      r5nowork == 1 & (r4lbrf_c == 7 | (is.na(r4lbrf_c) & r3lbrf_c == 7) |
                         (is.na(r4lbrf_c) & is.na(r3lbrf_c) & r2lbrf_c == 7) |
                         (is.na(r4lbrf_c) & is.na(r3lbrf_c) & is.na(r2lbrf_c) &
                            r1lbrf_c == 7)) ~ 7, 
      # never worked: previous never worked, currently not working
      r5nowork == 1 & (r4lbrf_c == 8 | (is.na(r4lbrf_c) & r3lbrf_c == 8) |
                         (is.na(r4lbrf_c) & is.na(r3lbrf_c) & r2lbrf_c == 8) |
                         (is.na(r4lbrf_c) & is.na(r3lbrf_c) & is.na(r2lbrf_c) &
                            r1lbrf_c == 8)) ~ 8,
      # unemployed: currently not working, retirement not proceed, previous employed
      r5nowork == 1 & w5_fh001 == 2 & 
        (r4lbrf_c < 6| r3lbrf_c < 6 | r2lbrf_c < 6 | r1lbrf_c < 6) ~ 6,
      # never worked: all the auxiliary variables implemented they were not working
      # missing r1-r4 labor force harmonized values
      r5nowork == 1 & 
        ((w5_zworking == 0 | is.na(w5_zworking)) & 
           w5_xworking == 0 & (w5_zrretired == 0 | is.na(w5_zrretired)) &
           w5_xgetjob == 0 & w5_xquitjob == 0) ~ 8,
      TRUE ~ NA_real_))

# # Sanity check
# with(w5_labor_force_mat, table(r5lbrf_c, useNA = "ifany"))
# # Check never work people
# a <- w5_labor_force_mat %>% filter(r5lbrf_c == 8)
# colSums(!is.na(a))[colSums(!is.na(a)) != 0]
# view(a)
# 
# b <- w5_labor_force_mat %>% filter(is.na(r5lbrf_c) & r5nowork == 1)
# colSums(!is.na(b))[colSums(!is.na(b)) != 0]
# view(b)

harmonized_addw5 %<>%
  left_join(w5_labor_force_mat %>% select(ID, r5lbrf_c), by = "ID")
harmonized_addw5 %>% select(contains("lbrf_c")) %>%
  apply(., 2, table, useNA = "ifany")

#---- hukou ----
# Wave 5 hukou
with(charls_w5, table(w5_ba009, useNA = "ifany"))
harmonized_addw5 %<>%
  left_join(charls_w5 %>% select(ID, w5_ba009), by = "ID")

harmonized_addw5 %<>%
  mutate(r2hukou_impute = case_when(is.na(r2hukou) ~ r1hukou,
                                    !is.na(r2hukou) ~ r2hukou),
         r3hukou_impute = case_when(is.na(r3hukou) ~ r2hukou_impute,
                                    !is.na(r3hukou) ~ r3hukou),
         r4hukou_impute = case_when(is.na(r4hukou) ~ r3hukou_impute,
                                    !is.na(r4hukou) ~ r4hukou),
         r5hukou_impute = case_when(is.na(w5_ba009) ~ r4hukou_impute,
                                    !is.na(w5_ba009) ~ as.numeric(w5_ba009)))
# # sanity check
# with(harmonized_addw5, table(w5_ba009, r5hukou_impute, useNA = "ifany"))
# with(harmonized_addw5, table(r4hukou, r4hukou_impute, useNA = "ifany"))
# with(harmonized_addw5, table(r3hukou, r3hukou_impute, useNA = "ifany"))

#---- rural ----
# wave 5
with(charls_w5, table(w5_ba008, useNA = "ifany"))
# special area included mine, farm ....

harmonized_addw5 %<>%
  left_join(charls_w5 %>% mutate(h5rural = case_when(w5_ba008 %in% c(3, 4) ~ 1,
                                                     !is.na(w5_ba008) ~ 0)) %>%
              select(ID, h5rural) , by = "ID")

harmonized_addw5 %>% select(contains("rural")) %>% apply(., 2, table, useNA = "ifany")

#---- Add wave 5 weights ----
harmonized_addw5 %<>%
  left_join(charls_w5 %>% select(ID, w5_INDV_weight_ad2) %>% 
              dplyr::rename(r5wtrespb = w5_INDV_weight_ad2), by = "ID")

#---- imrc and dlrc cleaning ----
##---- Wave 5 imrc and dlrc ----
with(charls_w5, table(w5_dc012_s11, w5_dc012_s12, useNA = "ifany"))
# There are 1244 participants with no responses (can't recall any of the words)
# but there are 427 participants refused to recall

w5rc_mat <- charls_w5 %>% select(
  ID, contains(c("w5_dc012_","w5_dc013_", "w5_dc014_", # Immediate recall score
                 "w5_dc028_"))) # Delayed recall score
colnames(w5rc_mat)

# put every non missing and non zero value to be 1 besides s12
w5rc_mat[w5rc_mat != 0 & !is.na(w5rc_mat)] <- 1

w5rc_mat %<>%
  mutate(across(ends_with("s11"), ~ case_when(.x == 0 ~ NA_real_,
                                              .x == 11 ~ 0)))

charls_w5 %<>%
  mutate(r5imrc_cleaned = case_when(
    w5_dc012_s11 == 11 ~ 0,
    w5_dc012_s12 == 12 ~ NA_real_,
    is.na(w5_dc012_s11) & is.na(w5_dc012_s12) ~ NA_real_,
    TRUE ~ rowSums(w5rc_mat %>% select(
      contains("w5_dc012_s"), -contains(c("s12", "s11"))), na.rm = T)),
    r5imrc_trial2 = case_when(
      w5_dc013_s11 == 11 ~ 0,
      w5_dc013_s12 == 12 ~ NA_real_,
      is.na(w5_dc013_s11) & is.na(w5_dc013_s12) ~ NA_real_,
      TRUE ~ rowSums(w5rc_mat %>% select(
        contains("w5_dc013_s"), -contains(c("s12", "s11"))), na.rm = T)),
    r5imrc_trial3 = case_when(
      w5_dc014_s11 == 11 ~ 0,
      w5_dc014_s12 == 12 ~ NA_real_,
      is.na(w5_dc014_s11) & is.na(w5_dc014_s12) ~ NA_real_,
      TRUE ~ rowSums(w5rc_mat %>% select(
        contains("w5_dc014_s"), -contains(c("s12", "s11"))), na.rm = T)),
    r5dlrc_cleaned = case_when(
      w5_dc028_s11 == 11 ~ 0,
      w5_dc028_s12 == 12 ~ NA_real_,
      is.na(w5_dc028_s11) & is.na(w5_dc028_s12) ~ NA_real_,
      TRUE ~ rowSums(
        w5rc_mat %>% 
          select(contains("dc028_s"), -contains(c("s12", "s11"))), na.rm = T))
  )

with(charls_w5, table(r5imrc_cleaned, useNA = "ifany"))
charls_w5 %>% select(contains(c("imrc", "dlrc"))) %>% summary()
with(charls_w5, table(r5dlrc_cleaned, useNA = "ifany"))

harmonized_addw5 %<>%
  left_join(charls_w5 %>% select(ID, r5imrc_cleaned, r5dlrc_cleaned,
                                 r5imrc_trial2, r5imrc_trial3), by = "ID")

##---- Immediate word recall ----
harmonized_addw5 %<>%
  left_join(charls_w3 %>% select(ID, w3_dc006s11, w3_dc006s12,
                                 w3_dc027s11, w3_dc027s12),
            by = "ID") %>%
  left_join(charls_w4 %>% select(ID, w4_dc028_w4_s11, w4_dc028_w4_s12,
                                 w4_dc047_w4_s11, w4_dc047_w4_s12),
            by = "ID")

# immediate word recall
with(harmonized_addw5 %>% filter(r3imrc == 0),
     table(w3_dc006s11, w3_dc006s12, useNA = "ifany"))
with(harmonized_addw5 %>% filter(r4imrc == 0),
     table(w4_dc028_w4_s11, w4_dc028_w4_s12, useNA = "ifany"))
# There are 1877 participants with non responses (can't recall any of the words)
# But the rest 653 participants refused to recall (needs to be NAed)

harmonized_addw5 %<>% 
  mutate(
    r3imrc_cleaned = case_when(
      r3imrc == 0 & w3_dc006s12 == 12 ~ NA_real_,
      # Note: if using read.dta13, the value will be "12 Refuse to Recall" for dc006s12s
      TRUE ~ as.numeric(r3imrc)),
    r4imrc_cleaned = case_when(
      r4imrc == 0 & w4_dc028_w4_s12 == 12 ~ NA_real_,
      TRUE ~ as.numeric(r4imrc)))
# Sanity check
with(harmonized_addw5, table(r3imrc_cleaned, r3imrc, useNA = "ifany"))
with(harmonized_addw5, table(r4imrc_cleaned, w4_dc028_w4_s12, useNA = "ifany"))

##---- Delayed word recall ----
with(harmonized_addw5 %>% filter(r3dlrc == 0),
     table(w3_dc027s11, w3_dc027s12, useNA = "ifany"))
with(harmonized_addw5 %>% filter(r4dlrc == 0),
     table(w4_dc047_w4_s11, w4_dc047_w4_s12, useNA = "ifany"))
# There are 2334 participants with non responses (can't recall any of the words)
# But the rest 505 participants seem to be NAed

harmonized_addw5 %<>% 
  mutate(r3dlrc_cleaned = case_when(r3dlrc == 0 & w3_dc027s12 == 12 ~ NA_real_,
                                    TRUE ~ r3dlrc),
         r4dlrc_cleaned = case_when(r4dlrc == 0 & w4_dc047_w4_s12 == 12 ~ NA_real_,
                                    TRUE ~ r4dlrc))
# Sanity check
with(harmonized_addw5, table(r3dlrc_cleaned, r3dlrc, useNA = "ifany"))
with(harmonized_addw5, table(r4dlrc_cleaned, r4dlrc, useNA = "ifany"))
with(harmonized_addw5, table(r5dlrc_cleaned, useNA = "ifany"))

# Checking for wave 4, if any participants with age < 60 answered these questions
with(harmonized_addw5 %>% filter(r4age_y < 60),
     table(r4imrc_cleaned, useNA = "ifany"))
with(harmonized_addw5 %>% filter(r4age_y < 60),
     table(r4dlrc_cleaned, useNA = "ifany"))
with(harmonized_addw5 %>% filter(r5age_y < 60),
     table(r5dlrc_cleaned, useNA = "ifany"))
with(harmonized_addw5 %>%
       filter(!is.na(r4imrc_cleaned)),
     proc_sum(r4age_y, more = T))

##---- Wave 4: 3 trials ----
w4rc_mat <- charls_w4 %>% select(
  ID, contains(c("w4_dc028_w4_", "w4_dc029_w4_", "w4_dc030_w4_")))
colnames(w4rc_mat)

# put every non missing and non zero value to be 1 besides s12
w4rc_mat[w4rc_mat != 0 & !is.na(w4rc_mat)] <- 1

w4rc_mat %<>%
  mutate(across(ends_with("s11"), ~ case_when(.x == 0 ~ NA_real_,
                                              .x == 11 ~ 0)))

charls_w4 %<>%
  mutate(
    r4imrc_trial2 = case_when(
      w4_dc029_w4_s11 == 11 ~ 0,
      w4_dc029_w4_s12 == 12 ~ NA_real_,
      is.na(w4_dc029_w4_s11) & is.na(w4_dc029_w4_s12) ~ NA_real_,
      TRUE ~ rowSums(w4rc_mat %>% select(
        contains("dc029_w4_s"), -contains(c("s12", "s11"))), na.rm = T)),
    r4imrc_trial3 = case_when(
      w4_dc030_w4_s11 == 11 ~ 0,
      w4_dc030_w4_s12 == 12 ~ NA_real_,
      is.na(w4_dc030_w4_s11) & is.na(w4_dc030_w4_s12) ~ NA_real_,
      TRUE ~ rowSums(w4rc_mat %>% select(
        contains("dc030_w4_s"), -contains(c("s12", "s11"))), na.rm = T)))

harmonized_addw5 %<>%
  left_join(charls_w4 %>% select(ID, r4imrc_trial2, r4imrc_trial3), by = "ID")

harmonized_addw5 %>%
  select(contains("r4imrc"), contains("r4dlrc")) %>% summary()
with(harmonized_addw5, table(r4imrc_cleaned, useNA = "ifany"))
summary(harmonized_addw5$r4imrc_cleaned)
charls_w4 %>% select(contains("r4imrc")) %>% summary()

#---- Select variables ----
harmonized_addw5_selected <- harmonized_addw5 %>%
  select(# use cleaned or imputed variables only
         -paste0("r", 3:4, "dlrc"),
         -paste0("r", 3:4, "imrc"),
         -paste0("r", 3:4, "hukou"))
harmonized_addw5_selected %>% colnames() %>% sort()

#---- Long dataset ----
harmonized_long_151820 <- harmonized_addw5_selected %>%
  select(ID, female, childhood_edu, 
         -householdID, -communityID, -pnc, -ID_w1, -householdID_w1,
         contains("ra"), contains(c("3", "4", "5")), -contains(c("trial")),
         -contains(paste0("w", 3:5)),
         -contains(as.vector(outer(c("r", "h"), c(1, 2), FUN = "paste0")))) %>%
  dplyr::rename_at(c("h3rural", "h4rural", "h5rural"), 
                   function(x) sub("h", "r", x)) %>%
  pivot_longer(cols = starts_with(paste0("r", 3:5)),
               names_to = c("wave", ".value"),
               names_pattern = ("r(.)(.*)"))

##---- Table 1 ----
table1_151820 <- harmonized_long_151820 %>%
  select(age_y, female, raeduc_c_v2, childhood_edu, rural,
         hukou_impute, imrc_cleaned, dlrc_cleaned, wave) %>%
  filter(!is.na(imrc_cleaned) & 
           age_y >= 45) %>%
  # filter(!is.na(dlrc_cleaned)) %>%
  labelled::set_variable_labels(
    age_y = "Age at waves",
    female = "Female (%)",
    raeduc_c_v2 = "Early life education level (%)",
    childhood_edu = "Early life education level (simplified) (%)",
    rural = "Rural residence (%)",
    hukou_impute = "Hukou status (%)",
    imrc_cleaned = "Immediate word recall", 
    dlrc_cleaned = "Delayed word recall") %>%
  labelled::drop_unused_value_labels() %>%
  labelled::set_value_labels(
    female = c("Yes" = 1, "No" = 0),
    rural = c("Yes" = 1, "No" = 0),
    childhood_edu = c(
      "No formal education" = 0,
      "Elementary school or less" = 1,
      "Vocational school/middle school/high school" = 2,
      "College & above" = 3),
    wave = c("2015" = "3", "2018" = "4", "2020" = "5"),
    hukou_impute = c("Agricultual hukou" = 1, "Non-agricultural hukou" = 2, 
                     "Unified residence hukou" = 3, "Do not have hukou" = 4)) %>%
  modify_if(is.labelled, to_factor) %>%
  # modify_if(is.factor,
  #           function(x) {forcats::fct_na_value_to_level(x, na_level = "Missing")}) %>%
  tbl_summary(
    statistic = list(all_categorical() ~ "{n} ({p}%)",
                     all_continuous() ~ c("{mean} ({sd})",
                                          "({min}, {max})")),
    type = all_continuous() ~ "continuous2",
    by = wave,
    digits = list(all_continuous() ~ 1,
                  all_categorical() ~ c(0, 1)),
    missing = "ifany",
    missing_text = "Missing") %>%
  add_overall() %>%
  # modify_spanning_header(starts_with("stat_") ~ "**Baseline CES-D**") %>%
  bold_labels() %>%
  modify_footnote(
    update = list(
      starts_with("stat_") ~
        "Mean (SD) for continuous variables; n (%) for categorical variables")
  )

table1_151820 %>%  as_flex_table()

#---- Save the dataset ----
save(harmonized_addw5_selected, harmonized_long_151820, file = 
       here::here("data", "analysis_data", "charls_equating_samp_151820.RData"))

#---- OLD ----
# #---- Baseline Table 1 ----
# table1 <- harmonized %>%
#   select(r1age_y, female, raeduc_c, childhood_edu, h1rural,
#          r1hukou, h1rural, r1imrc, r1dlrc) %>%
#   labelled::set_variable_labels(
#     r1age_y = "Baseline age in years",
#     female = "Female (%)",
#     raeduc_c = "Early life education level (%)",
#     childhood_edu = "Early life education level (simplified) (%)",
#     h1rural = "Rural residence (%)",
#     r1hukou = "Hukou status (%)",
#     r1imrc = "Baseline Immediate word recall", 
#     r1dlrc = "Baseline delayed word recall") %>%
#   labelled::drop_unused_value_labels() %>%
#   labelled::set_value_labels(
#     female = c("Yes" = 1, "No" = 0),
#     h1rural = c("Yes" = 1, "No" = 0),
#     childhood_edu = c(
#       "No formal education" = 0,
#       "Elementary school or less" = 1,
#       "Vocational school/middle school/high school" = 2,
#       "College & above" = 3)) %>%
#   modify_if(is.labelled, to_factor) %>%
#   modify_if(is.factor,
#             function(x) {forcats::fct_explicit_na(x, na_level = "Missing")}) %>%
#   tbl_summary(
#     statistic = list(all_categorical() ~ "{n} ({p}%)",
#                      all_continuous() ~ c("{mean} ({sd})",
#                                           "({min}, {max})")),
#     type = all_continuous() ~ "continuous2",
#     # by = childhood_edu20,
#     digits = list(all_continuous() ~ 1,
#                   all_categorical() ~ c(0, 1)),
#     missing = "ifany",
#     missing_text = "Missing") %>%
#   # modify_spanning_header(starts_with("stat_") ~ "**Baseline CES-D**") %>%
#   bold_labels() %>%
#   modify_footnote(
#     update = list(
#       starts_with("stat_") ~
#         "Mean (SD) for continuous variables; n (%) for categorical variables")
#   )
# 
# table1 %>% as_flex_table() %>%
#   flextable::save_as_docx(path = here::here("output", "tables", 
#                                             "table1_equating_sample.docx"))