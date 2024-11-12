# 3 Tables and Figures
# Created by Yingyan Wu
# grid plots code was adapted from Crystal Shaw and Yingyan Wu's 
# exposure trajectory project code for figure (https://github.com/Mayeda-Research-Group/exposure-trajectories/blob/master/RScripts/4b_make_figures.R)
# Dec.28.2023

#---- Package loading + options ----
rm(list = ls())

if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')
}

p_load("here", "readr", "tidyverse", "magrittr", "plyr", "haven", "labelled",
       "gtsummary", "equate", "cowplot")

#---- function ----
source(here::here("scripts", "functions.R"))
source(here::here("scripts", "equating_cog_charls", "functions_weight_dev.R"))

#---- Results descriptives ----
load(here::here("data", "analysis_data", "charls_equating_samp_eqt_151820_05012024.RData"))

harmonized_addw5_selected %>%
  select(ID, contains(c("r4imrc", "r5imrc", "r4dlrc", "r5dlrc"))) %>% 
  pivot_longer(cols = !ID,
               names_to = c("wave", "construct", "trial"),
               names_pattern = "r(\\d)(.*)_(.*)") %>%
  na.omit() %>% select(ID) %>% unique() %>% nrow() # 19364

harmonized_addw5_selected %>% filter(r4age_y >= 60) %>% nrow() # 14440
harmonized_addw5_selected %>% filter(r4age_y >= 60, 
                                     !is.na(r4imrc_cleaned), 
                                     !is.na(r4dlrc_cleaned)) %>% nrow() # 7676
with(harmonized_addw5_selected %>% filter(r4age_y >= 60, 
                                          !is.na(r4imrc_cleaned), 
                                          !is.na(r4dlrc_cleaned)), 
     table(r4dlrc_cleaned > r4imrc_cleaned, useNA = "ifany"))/7676
# FALSE    TRUE 
# 0.47642 0.52358 

harmonized_addw5_selected %>% filter(r5age_y >= 60) %>% nrow() # 15704
harmonized_addw5_selected %>% filter(r5age_y >= 60, 
                                     !is.na(r5imrc_cleaned), 
                                     !is.na(r5dlrc_cleaned)) %>% nrow() # 9137
with(harmonized_addw5_selected %>% filter(r5age_y >= 60, 
                                          !is.na(r5imrc_cleaned), 
                                          !is.na(r5dlrc_cleaned)), 
     table(r5dlrc_cleaned > r5imrc_cleaned, useNA = "ifany"))/9137
# FALSE      TRUE 
# 0.3477071 0.6522929 

#---- Table 2. descriptive table 1 ----
load(here::here("data", "analysis_data", "charls_eqt_selected.RData"))
# Unique participants
charls_selected %>% select(ID) %>% unique() %>% nrow() # 11148
# Table
table1_151820 <- charls_selected %>%
  select(age_y, female, childhood_edu, rural, work_status,
         hukou_impute, imrc_cleaned, dlrc_cleaned, wave) %>%
  mutate(across(c("childhood_edu", "hukou_impute", "work_status"), as.numeric)) %>%
  na.omit() %>%
  labelled::set_variable_labels(
    age_y = "Age at waves",
    female = "Female (%)",
    childhood_edu = "Early life education level (simplified) (%)",
    rural = "Rural residence (%)",
    hukou_impute = "Hukou status (%)",
    imrc_cleaned = "Immediate word recall", 
    dlrc_cleaned = "Delayed word recall",
    work_status = "Work status (%)") %>%
  labelled::drop_unused_value_labels() %>%
  labelled::set_value_labels(
    female = c("Yes" = 1, "No" = 0),
    rural = c("Yes" = 1, "No" = 0),
    childhood_edu = c(
      "Elementary school" = 1,
      "Vocational school/middle school/high school" = 2,
      "College & above" = 3),
    wave = c("2015" = "3", "2018" = "4", "2020" = "5"),
    hukou_impute = c("Agricultural hukou" = 1, "Non-agricultural hukou" = 2, 
                     "Unified residence hukou" = 3, "Do not have hukou" = 4),
    work_status = c("Agriculture employed" = 1,
                    "Non-agriculture employed" = 2,
                    "Unemployed" = 3,
                    "Retired" = 4,
                    "Never work" = 5)) %>%
  modify_if(is.labelled, to_factor) %>%
  # modify_if(is.factor,
  # function(x) {forcats::fct_explicit_na(x, na_level = "Missing")}) %>%
  tbl_summary(
    statistic = list(all_categorical() ~ "{n} ({p})",
                     all_continuous() ~ c("{mean} ({sd})")),
    type = all_continuous() ~ "continuous2",
    by = wave,
    digits = list(all_continuous() ~ 1,
                  all_categorical() ~ c(0, 1)),
    missing = "ifany",
    missing_text = "Missing") %>%
  bold_labels() %>%
  modify_footnote(
    update = list(
      starts_with("stat_") ~
        "Mean (SD) for continuous variables; n (%) for categorical variables")
  )
table1_151820 %>%  as_flex_table() 

# table1_151820 %>%  as_flex_table() %>% flextable::save_as_docx(
#   path = here::here("output", "tables", "table1_equating_calibrate_sample_151820.docx"))

#----  Figure 1. Empirical plot ----
load(here::here("data", "analysis_data", "charls_equating_samp_eqt_151820_05012024.RData"))

harmonized_addw5_selected %<>%
  mutate(imrc_balanced = case_when(!is.na(r1imrc) & !is.na(r2imrc) & 
                                     !is.na(r3imrc_cleaned) & 
                                     !is.na(r4imrc_cleaned) &
                                     !is.na(r5imrc_cleaned) ~ 1,
                                   TRUE ~ 0),
         dlrc_balanced = case_when(!is.na(r1dlrc) & !is.na(r2dlrc) & 
                                     !is.na(r3dlrc_cleaned) & 
                                     !is.na(r4dlrc_cleaned) &
                                     !is.na(r5dlrc_cleaned)~ 1,
                                   TRUE ~ 0))
with(harmonized_addw5_selected, table(imrc_balanced, useNA = "ifany"))
with(harmonized_addw5_selected, table(dlrc_balanced, useNA = "ifany"))

harmonized_long <- harmonized_addw5_selected %>%
  select(ID, contains("imrc"), contains("dlrc"),-contains("trial"),
         imrc_balanced, dlrc_balanced) %>%
  dplyr::rename_with(.cols = c(paste0("r", c(1, 2), "dlrc"),
                               paste0("r", c(1, 2), "imrc")), 
                     function(x){paste0(x, "_cleaned")}) %>%
  pivot_longer(cols = -c(ID, imrc_balanced, dlrc_balanced),
               names_to = c("wave", ".value", "type"),
               names_pattern = "r(\\d+)(.*)_(.*)") %>%
  print()

harmonized_empirical_balanced <- harmonized_long %>%
  filter(imrc_balanced == 1) %>%
  group_by(wave, type) %>%
  summarise_at(vars(imrc), 
               list(n = length, mean = mean, sd = sd)) %>%
  mutate(se = sd/sqrt(n),
         variable = "Immediate word recall") %>%
  # variable = paste0("Immediate word recall\n(n = ", n, ")")) %>%
  rbind(harmonized_long %>%
          filter(dlrc_balanced == 1) %>%
          group_by(wave, type) %>%
          summarise_at(vars(dlrc), 
                       list(n = length, mean = mean, sd = sd)) %>%
          mutate(se = sd/sqrt(n),
                 variable = "Delayed word recall"))  %>%
  # variable = paste0("Delayed word recall\n(n = ", n, ")"))) 
  mutate(year = case_when(wave == 1 ~ 2011,
                          wave == 2 ~ 2013,
                          wave == 3 ~ 2015,
                          wave == 4 ~ 2018,
                          wave == 5 ~ 2020))

harmonized_empirical_balanced %<>%
  rbind(harmonized_empirical_balanced %>% 
          filter(type == "cleaned" & wave <= 3) %>%
          mutate(type = "eqt")) %>%
  mutate(type = factor(type, levels = c("cleaned", "eqt")),
         variable = factor(variable, levels = c("Immediate word recall",
                                                "Delayed word recall"))) %>%
  arrange(type, wave) %>%
  print()

eqted_empirical_plot <-
  harmonized_empirical_balanced %>%
  ggplot(aes(x = year, y = mean,
             group = interaction(variable, type),
             color = interaction(variable, type))) +
  geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se), width=.1) + # size = 1 for poster
  geom_line(aes(linetype = interaction(variable, type))) + # size = 2 for poster
  geom_point(aes(shape = interaction(variable, type))) + # size = 2 for poster
  geom_text(aes(label = round(mean, 2), y = mean + 5*se), show.legend = FALSE) +
  # geom_text(aes(label = ifelse(type == "eqt" & year >= 2018, 
  #                              as.character(round(mean, 2)), '')),hjust=0,vjust=0) +
  scale_x_continuous(breaks = c(2011, 2013, 2015, 2018, 2020)) +
  # scale_y_continuous(limits = c(2.4, 5.2), breaks = seq(3, 5, by = 0.5)) +
  scale_color_manual(name = "Variable",
                     values = c("#67b4b0", "#6c4d86", "#67b4b0", "#6c4d86"),
                     labels = c("Original immediate word recall",
                                "Original delayed word recall",
                                "Equated immediate word recall",
                                "Equated delayed word recall")) +
  scale_linetype_manual(name = "Variable",
                        values = c(1, 1, 2, 2),
                        labels = c("Original immediate word recall",
                                   "Original delayed word recall",
                                   "Equated immediate word recall",
                                   "Equated delayed word recall")) +
  scale_shape_manual(name = "Variable",
                     values = c(19, 19, 17, 17),
                     labels = c("Original immediate word recall",
                                "Original delayed word recall",
                                "Equated immediate word recall",
                                "Equated delayed word recall")) +
  theme_classic() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(size = 10, color = "black"),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        legend.text = element_text(size = 10)) +
  guides(
    shape = guide_legend(
      # title.position = "top",
      # title.vjust = -1.3,
      # title.theme = element_text(size = 10, color = "black"),
      nrow = 2),
    color = guide_legend(
      # title.position = "top",
      # title.vjust = -1.3,
      # title.theme = element_text(size = 10, color = "black"),
      nrow = 2)) +
  # theme_classic(base_size = 20) + # for poster
  # theme(legend.position = "none") + # for poster
  labs(x = "Year", y = "Word recall scores")

eqted_empirical_plot
ggsave(eqted_empirical_plot,
       filename = here::here("output", "figures", "figure1_empirical_plot_eqted_2015_20182020.eps"),
       device = "eps", dpi = 300, width = 7, height = 5, units = "in")
ggsave(eqted_empirical_plot,
       filename = here::here("output", "figures", "figure1_empirical_plot_eqted_2015_20182020.png"),
       device = "png", dpi = 300, width = 7, height = 5, units = "in")

##---- Figure for slides ----
###---- cleaned scores ----
harmonized_empirical_balanced %>%
  filter(type == "cleaned" & str_detect(variable, "Immediate")) %>%
  ggplot(aes(x = year, y = mean, color = variable)) +
  geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se), width=.1, size = 1) + # size = 1 for poster
  geom_line(size = 1) + # size = 2 for poster
  geom_point(size = 2) + # size = 2 for poster
  geom_text(aes(label = round(mean, 2), y = mean + 5*se), show.legend = FALSE) +
  scale_x_continuous(breaks = c(2011, 2013, 2015, 2018, 2020)) +
  scale_y_continuous(limits = c(2.3, 5.2), breaks = seq(3, 5, by = 0.5)) +
  scale_color_manual(values = c("#67b4b0", "#6c4d86")) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  labs(x = "Year", y = "Word recall scores")
ggsave(filename = here::here("output", "figures", "figure1_immediate_cleaned_empirical_plot_slides.png"),
       device = "png", dpi = 300, width = 7, height = 5, units = "in")

harmonized_empirical_balanced %>%
  filter(type == "cleaned") %>%
  ggplot(aes(x = year, y = mean,
             group = variable,
             color = variable)) +
  geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se), width=.1, size = 1) + # size = 1 for poster
  geom_line(size = 1) + # size = 2 for poster
  geom_point(size = 2) + # size = 2 for poster
  geom_text(aes(label = round(mean, 2), y = mean + 5*se), show.legend = FALSE) +
  scale_x_continuous(breaks = c(2011, 2013, 2015, 2018, 2020)) +
  scale_y_continuous(limits = c(2.3, 5.2), breaks = seq(3, 5, by = 0.5)) +
  scale_color_manual(values = c("#67b4b0", "#6c4d86")) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  labs(x = "Year", y = "Word recall scores")

ggsave(filename = here::here("output", "figures", "figure1_cleaned_empirical_plot_slides.png"),
       device = "png", dpi = 300, width = 7, height = 5, units = "in")

###---- Equated scores ----
# Immediate
harmonized_empirical_balanced %>%
  filter(str_detect(variable, "Immediate")) %>%
  ggplot(aes(x = year, y = mean,
             group = interaction(variable, type),
             color = interaction(variable, type))) +
  geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se), width=.1, size = 1) + # size = 1 for poster
  geom_line(aes(linetype = interaction(variable, type)), size = 1) + # size = 2 for poster
  geom_point(aes(shape = interaction(variable, type)), size = 2) + # size = 2 for poster
  geom_text(aes(label = round(mean, 2), y = mean + 5*se), show.legend = FALSE) +
  scale_x_continuous(breaks = c(2011, 2013, 2015, 2018, 2020)) +
  scale_y_continuous(limits = c(2.3, 5.2), breaks = seq(3, 5, by = 0.5)) +
  scale_color_manual(values = c("#67b4b0", "#67b4b0")) +
  scale_linetype_manual(values = c(1, 2)) +
  scale_shape_manual(values = c(19, 17)) +
  theme_classic() +
  theme(legend.position = "none", legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(size = 10, color = "black"),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        legend.text = element_text(size = 10)) +
  guides(shape = guide_legend(nrow = 2),
         color = guide_legend(nrow = 2)) +
  labs(x = "Year", y = "Word recall scores")

ggsave(filename = here::here("output", "figures", "figure1_immediate_empirical_plot_slides.png"),
       device = "png", dpi = 300, width = 7, height = 5, units = "in")

# Delayed
harmonized_empirical_balanced %>%
  filter(str_detect(variable, "Immediate") | 
           str_detect(variable, "Delayed") & type == "cleaned") %>%
  ggplot(aes(x = year, y = mean,
             group = interaction(variable, type),
             color = interaction(variable, type))) +
  geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se), width=.1, size = 1) + # size = 1 for poster
  geom_line(aes(linetype = interaction(variable, type)), size = 1) + # size = 2 for poster
  geom_point(aes(shape = interaction(variable, type)), size = 2) + # size = 2 for poster
  geom_text(aes(label = round(mean, 2), y = mean + 5*se), show.legend = FALSE) +
  scale_x_continuous(breaks = c(2011, 2013, 2015, 2018, 2020)) +
  scale_y_continuous(limits = c(2.3, 5.2), breaks = seq(3, 5, by = 0.5)) +
  scale_color_manual(values = c("#67b4b0", "#6c4d86", "#67b4b0")) +
  scale_linetype_manual(values = c(1, 1, 2)) +
  scale_shape_manual(values = c(19, 19, 17)) +
  theme_classic() +
  theme(legend.position = "none", legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(size = 10, color = "black"),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        legend.text = element_text(size = 10)) +
  guides(shape = guide_legend(nrow = 2),
         color = guide_legend(nrow = 2)) +
  labs(x = "Year", y = "Word recall scores")

ggsave(filename = here::here("output", "figures", "figure1_delayed1_empirical_plot_slides.png"),
       device = "png", dpi = 300, width = 7, height = 5, units = "in")

harmonized_empirical_balanced %>%
  ggplot(aes(x = year, y = mean,
             group = interaction(variable, type),
             color = interaction(variable, type))) +
  geom_errorbar(aes(ymin = mean - 2*se, ymax = mean + 2*se), width=.1, size = 1) + # size = 1 for poster
  geom_line(aes(linetype = interaction(variable, type)), size = 1) + # size = 2 for poster
  geom_point(aes(shape = interaction(variable, type)), size = 2) + # size = 2 for poster
  geom_text(aes(label = round(mean, 2), y = mean + 5*se), show.legend = FALSE) +
  scale_x_continuous(breaks = c(2011, 2013, 2015, 2018, 2020)) +
  scale_y_continuous(limits = c(2.3, 5.2), breaks = seq(3, 5, by = 0.5)) +
  scale_color_manual(values = c("#67b4b0", "#6c4d86", "#67b4b0", "#6c4d86")) +
  scale_linetype_manual(values = c(1, 1, 2, 2)) +
  scale_shape_manual(values = c(19, 19, 17, 17)) +
  theme_classic() +
  theme(legend.position = "none", legend.direction = "horizontal",
        legend.title = element_blank(),
        text = element_text(size = 10, color = "black"),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        legend.text = element_text(size = 10)) +
  guides(shape = guide_legend(nrow = 2),
         color = guide_legend(nrow = 2)) +
  labs(x = "Year", y = "Word recall scores")

ggsave(filename = here::here("output", "figures", "figure1_full_empirical_plot_slides.png"),
       device = "png", dpi = 300, width = 7, height = 5, units = "in")

#---- Figure 2. distribution of different trials of imrc -----
load(here::here("data", "analysis_data", "charls_equating_samp_eqt_151820_05012024.RData"))

rc_long <- harmonized_addw5_selected %>%
  select(ID, contains(c("r4imrc", "r5imrc", "r4dlrc", "r5dlrc"))) %>% 
  pivot_longer(cols = !ID,
               names_to = c("wave", "construct", "trial"),
               names_pattern = "r(\\d)(.*)_(.*)") %>%
  na.omit() %>%
  mutate(year = case_when(wave == 4 ~ "Wave 2018",
                          wave == 5 ~ "Wave 2020"),
         measure = case_when(
           construct == "imrc" & trial == "cleaned" ~ "Original trial 1",
           construct == "dlrc" & trial == "cleaned" ~ "Original",
           trial == "trial2" ~ "Original trial 2",
           trial == "trial3" ~ "Original trial 3",
           trial == "eqt" ~ "Equated"),
         measure = factor(measure, 
                          levels = c(paste0("Original trial ", 1:3),
                                     "Original", "Equated")),
         construct = case_when(construct == "imrc" ~ "Immediate word recall",
                               construct == "dlrc" ~ "Delayed word recall"),
         construct = factor(construct, levels = c("Immediate word recall", 
                                                  "Delayed word recall")))

rc_long %>%
  ggplot() +
  geom_boxplot(aes(x = measure, y = value)) +
  facet_grid(rows = vars(year), cols = vars(construct),
             scale = "free_x", space="free") +
  labs(x = element_blank(), y = "Word recall scores") +
  theme_bw()

ggsave(here::here("output", "figures", "figure2_rc_dist_2015_20182020.eps"),
       device = "eps", dpi = 300, width = 7, height = 5, units = "in")

##---- Figure for slides ----
p_load("ggdist")

rc_allwave_long <- harmonized_addw5_selected %>%
  select(ID, contains(c("imrc", "dlrc"))) %>% 
  pivot_longer(cols = !ID,
               names_to = c("wave", "construct"),
               names_pattern = "r(\\d)(.*)") %>%
  na.omit() %>%
  mutate(
    year = case_when(wave == 1 ~ "Wave 2011",
                     wave == 2 ~ "Wave 2013",
                     wave == 3 ~ "Wave 2015",
                     wave == 4 ~ "Wave 2018",
                     wave == 5 ~ "Wave 2020"),
    year_measure = case_when(
      wave == 1 ~ "2011",
      wave == 2 ~ "2013",
      wave == 3 ~ "2015",
      wave == 4 & construct == "imrc_cleaned" ~ "2018 trial 1",
      wave == 4 & construct == "imrc_trial2" ~ "2018 trial 2",
      wave == 4 & construct == "imrc_trial3" ~ "2018 trial 3",
      wave == 5 & construct == "imrc_cleaned" ~ "2020 trial 1",
      wave == 5 & construct == "imrc_trial2" ~ "2020 trial 2",
      wave == 5 & construct == "imrc_trial3" ~ "2020 trial 3",
      wave == 4 ~ "2018",
      wave == 5 ~ "2020"),
    measure = case_when(
      str_detect(construct, "imrc_cleaned") ~ "Original trial 1",
      str_detect(construct, "dlrc_cleaned") ~ "Original",
      str_detect(construct, "trial2") ~ "Original trial 2",
      str_detect(construct, "trial3") ~ "Original trial 3",
      str_detect(construct, "eqt") ~ "Equated",
      str_detect(construct, "imrc") & wave %in% c(1, 2) ~ "Original trial 1",
      str_detect(construct, "dlrc") & wave %in% c(1, 2) ~ "Original"),
    # measure = factor(measure, 
    #                  levels = c(paste0("Original trial ", 1:3),
    #                             "Original", "Equated")),
    construct = case_when(str_detect(construct, "imrc") ~ "Immediate word recall",
                          str_detect(construct, "dlrc") ~ "Delayed word recall"),
    construct = factor(construct, levels = c("Immediate word recall", 
                                             "Delayed word recall")))

rc_allwave_long %>%
  filter(measure != "Equated" & str_detect(year_measure, "2018|2020")) %>%
  mutate(measure = str_replace(measure, "Original |Original", ""),
         measure = str_replace(measure, "trial", "Trial"),
         year_measure = factor(
           year_measure, 
           levels = c(paste0("2018 trial ", 1:3), "2018",
                      paste0("2020 trial ", 1:3), "2020"))) %>%
  ggplot(aes(x = year_measure, y = value, color = construct)) +
  # ggdist::stat_halfeye(adjust = 17, width = .5, .width = 0, justification = -.3, 
  #                      point_colour = NA) +
  geom_boxplot(width = .2, position = position_dodge((width = 1))) +
  # facet_grid(rows = vars(year), cols = vars(construct),
  #            scale = "free_x", space="free") +
  labs(x = element_blank(), y = "Word recall scores") +
  scale_x_discrete(labels = rep(c(paste0("trial ", 1:3), ""), 2)) +
  scale_color_manual(name = "", values = c("#67b4b0", "#6c4d86")) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 12, color = "black"), 
        strip.text.x = element_text(size = 12, color = "black"),
        strip.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 12, color = "black"), 
        axis.text.y = element_text(size = 12, color = "black"))

ggsave(here::here("output", "figures", "figure2_rc_dist_slides.png"),
       device = "png", dpi = 300, width = 7, height = 5, units = "in")

rc_allwave_long %>%
  filter(measure != "Equated" & year_measure %in% c("2011", "2013", "2015")) %>%
  mutate(measure = str_replace(measure, "Original |Original", ""),
         measure = str_replace(measure, "trial", "Trial")) %>%
  ggplot(aes(x = year_measure, y = value, color = construct)) +
  # ggdist::stat_halfeye(adjust = 15, width = .5, .width = 0, justification = -.3, 
  #                      point_colour = NA) +
  geom_boxplot(width = .2, position = position_dodge((width = 1))) +
  # geom_boxplot(aes(x = year_measure, y = value, color = construct)) + 
  # facet_grid(cols = vars(construct),
  #            scale = "free_x", space="free") +
  labs(x = element_blank(), y = "Word recall scores") +
  scale_color_manual(name = "", values = c("#67b4b0", "#6c4d86")) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 12, color = "black"), 
        strip.text.x = element_text(size = 12, color = "black"),
        strip.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 12, color = "black"), 
        axis.text.y = element_text(size = 12, color = "black"))

ggsave(here::here("output", "figures", "figure_rc_dist_2011_2015_slides.png"),
       device = "png", dpi = 300, width = 7, height = 5, units = "in")

rc_allwave_long %>%
  filter(measure != "Equated") %>%
  filter(construct == "Immediate word recall") %>%
  mutate(measure = str_replace(measure, "Original |Original", ""),
         measure = str_replace(measure, "trial", "Trial")) %>%
  ggplot(aes(x = year_measure, y = value, color = "#67b4b0")) +
  ggdist::stat_halfeye(adjust = 15, width = .5, .width = 0, justification = -.3, 
                       point_colour = NA) +
  geom_boxplot(width = .2, color = "#67b4b0") +
  # geom_boxplot(aes(x = year_measure, y = value, color = construct)) + 
  labs(x = element_blank(), y = "Immediate word recall scores") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size = 10, color = "black"), 
        strip.text.x = element_text(size = 10, color = "black"),
        strip.text.y = element_text(size = 10, color = "black"),
        axis.text.x = element_text(size = 8, color = "black"), 
        axis.text.y = element_text(size = 10, color = "black"))

ggsave(here::here("output", "figures", "figure_rc_dist_2011_2020_imrc_slides.png"),
       device = "png", dpi = 300, width = 9, height = 7, units = "in")

rc_allwave_long %>%
  filter(construct == "Delayed word recall") %>%
  mutate(measure = str_replace(measure, "Original |Original", ""),
         measure = str_replace(measure, "trial", "Trial")) %>%
  ggplot(aes(x = year_measure, y = value, color = "#6c4d86")) +
  ggdist::stat_halfeye(adjust = 15, width = .5, .width = 0, justification = -.3, 
                       point_colour = NA) +
  geom_boxplot(width = .2, color = "#6c4d86") +
  # geom_boxplot(aes(x = year_measure, y = value, color = construct)) + 
  labs(x = element_blank(), y = "Delayed word recall scores") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size = 10, color = "black"), 
        strip.text.x = element_text(size = 10, color = "black"),
        strip.text.y = element_text(size = 10, color = "black"),
        axis.text.x = element_text(size = 8, color = "black"), 
        axis.text.y = element_text(size = 10, color = "black"))

ggsave(here::here("output", "figures", "figure_rc_dist_2011_2020_dlrc_slides.png"),
       device = "png", dpi = 300, width = 7, height = 5, units = "in")

#---- Figure 3. Covariate balance plot ----
load(here::here("data", "analysis_data", "eqt_covbal_data_2015_20182020.RData"))
covbal_comp_plot(w4b_test$covbal_data, unw_test$covbal_data,
                 "2015", "2018/2020")

figure2_covbal <- 
  covbal_comp_plot(w4b_test$covbal_data, unw_test$covbal_data,
                   "2015", "2018/2020")  +
  # xlab("Standardized mean difference") +
  scale_shape_manual(name = element_blank(),
                     values = c(21, 16), labels = c("Unweighted", "Weighted")) +
  scale_x_continuous(breaks = seq(-0.4, 0.4, by = 0.1)) +
  theme(legend.position = "right", legend.direction = "vertical",
        legend.title.align = 0.5,
        legend.text = element_text(size = 12),
        text = element_text(size = 12, color = "black"), 
        axis.text.x = element_text(size = 12, color = "black"), 
        axis.text.y = element_text(size = 12, color = "black"))
figure2_covbal
ggsave(figure2_covbal,
       filename = here::here("output", "figures", "figure3_covbal_plot_2015_20182020.eps"),
       device = "eps", dpi = 300, width = 9, height = 7, units = "in")
ggsave(figure2_covbal,
       filename = here::here("output", "figures", "figure3_covbal_plot_2015_20182020.png"),
       device = "png", dpi = 300, width = 9, height = 7, units = "in")

##---- Figure for slides ----
covbal_comp_plot(w4b_test$covbal_data, unw_test$covbal_data,
                 "2015", "2018/2020")
covbal_comp_plot(w4b_test$covbal_data, unw_test$covbal_data,
                 "2015", "2018/2020")  +
  xlab("Standardized mean difference") +
  scale_shape_manual(name = element_blank(),
                     values = c(21, 16), labels = c("Unweighted", "Weighted")) +
  scale_x_continuous(breaks = seq(-0.4, 0.4, by = 0.1)) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title.align = 0.5,
        legend.text = element_text(size = 12),
        text = element_text(size = 12, color = "black"), 
        axis.text.x = element_text(size = 12, color = "black"), 
        axis.text.y = element_text(size = 12, color = "black"))
ggsave(filename = here::here("output", "figures", "figure3_covbal_plot_pre.png"),
       device = "png", dpi = 300, width = 7, height = 5, units = "in")

#---- Figure 4. equated equating plot ----
# in equating_cog.R script


#---- OLD -----
##---- before equating ----
# empirical_plot <- harmonized_empirical_balanced %>%
#   mutate(year = case_when(wave == 1 ~ 2011,
#                           wave == 2 ~ 2013,
#                           wave == 3 ~ 2015,
#                           wave == 4 ~ 2018,
#                           wave == 5 ~ 2020),
#          variable = factor(variable, levels = c("Delayed word recall",
#                                                 "Immediate word recall"))
#   ) %>%
#   filter(type == "cleaned") %>%
#   ggplot(aes(x = year, y = mean, group = variable, color = variable)) + 
#   geom_errorbar(aes(ymin = mean - 2 * se, ymax = mean + 2 * se), width=.1) +
#   scale_x_continuous(breaks = c(2011, 2013, 2015, 2018, 2020)) +
#   scale_y_continuous(limits = c(2.4, 5.2), breaks = c(3, 4, 5)) +
#   geom_line() +
#   geom_point() +
#   geom_text(aes(label = round(mean, 2), y = mean + 5*se)) +
#   theme_classic() +
#   labs(x = "Wave",
#        y = "Word recall") +
#   theme(legend.position = "none") +
#   labs(x = "Year", y = "Word recall scores") +
#   theme(text = element_text(size = 10, color = "black"),
#         axis.text.x = element_text(size = 10, color = "black"),
#         axis.text.y = element_text(size = 10, color = "black"))
# 
# empirical_plot +
#   scale_linetype_manual(name = "Variable",
#                         values = c(1, 1, 2, 2),
#                         labels = c("Original immediate word recall",
#                                    "Original delayed word recall",
#                                    "Equated immediate word recall",
#                                    "Equated delayed word recall")) +
#   scale_shape_manual(name = "Variable",
#                      values = c(19, 19, 17, 17),
#                      labels = c("Original immediate word recall",
#                                 "Original delayed word recall",
#                                 "Equated immediate word recall",
#                                 "Equated delayed word recall")) +
#   theme_classic() +
#   theme(legend.position = "bottom", legend.direction = "horizontal",
#         text = element_text(size = 10, color = "black"),
#         axis.text.x = element_text(size = 10, color = "black"),
#         axis.text.y = element_text(size = 10, color = "black"),
#         legend.title.align = 0.5,
#         legend.title = element_text(size = 10),
#         legend.text = element_text(size = 10)) +
#   guides(
#     shape = guide_legend(title.position = "top",
#                          title.vjust = -1.3,
#                          title.theme = element_text(size = 10, color = "black"),
#                          nrow = 2),
#     color = guide_legend(title.position = "top",
#                          title.vjust = -1.3,
#                          title.theme = element_text(size = 10, color = "black"),
#                          nrow = 2)) +
#   # theme_classic(base_size = 20) + # for poster
#   # theme(legend.position = "none") + # for poster
#   labs(x = "Year", y = "Word recall scores")
# # ggsave(filename = here::here("output", "figures", "figure0_empirical_plot_uneqted.png"),
# #        device = "png", dpi = 300, width = 7, height = 5, units = "in")
# 
##---- stacked plot ----
# figure1_panel <- 
#   plot_grid(empirical_plot, 
#             eqted_empirical_plot + theme(legend.position = "none"),
#             align = "vh", label_size = 10, labels = "AUTO", ncol = 2, nrow = 1) +
#   theme(plot.margin = unit(c(t = 0, r = 0, b = 8, l = 0), unit = "pt"))
# 
# legend_b <- ggpubr::get_legend(eqted_empirical_plot)
# figure1_panel_final <- plot_grid(figure1_panel, 
#                                  legend_b, 
#                                  ncol = 1, rel_heights = c(1, .09)) +
#   theme(plot.margin = unit(c(t = 0, r = 0, b = 10, l = 0), unit = "pt"))
# 
# figure1_panel_final  
# # theme(plot.margin = unit(c(t = 21, r = 10, b = 10, l = 0), unit = "pt")) +
# ggsave(figure1_panel_final,
#        filename = here::here("output", "figures", "figure1_empirical_plot.png"),
#        device = "png", dpi = 300, width = 7, height = 5, units = "in")
