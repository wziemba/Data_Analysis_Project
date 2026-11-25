library(tidyverse)
library(tidymodels)
library(readxl)
library(dplyr)
library(tibble)
library(DataExplorer)
library(survey)

load("NSDUH_2024.RData")

ls()
df <- NSDUH_2024

dim(df)

head(df[, 1:10])
names(df)

View(df)

var_labels <- sapply(df, function(x) attr(x, "label"))

dictionary <- data.frame(
  Variable = names(df),
  Description = ifelse(sapply(var_labels, is.null), "No label", unlist(var_labels)),
  row.names = NULL
)

View(dictionary)

cohort_mh <- NSDUH_2024 %>%
  filter(AGE3 %in% 2:7) %>%
  mutate(
    age_group = factor(
      AGE3,
      levels = 2:7,
      labels = c("14-15", "16-17", "18-20", "21-23", "24-25", "26-29")
    ),
    dev_stage = if_else(AGE3 %in% c(2, 3), "Adolescent (14-17)", "Young Adult (18-29)"),
    
    # Vaping & marijuana binary flags (0/1)
    nic_vape_pm = as.numeric(NICVAPMON),
    mj_vape_pm  = as.numeric(MJVAPMON),
    flv_vape_pm = as.numeric(FLVVAPMON),
    mj_any_pm   = as.numeric(MRJMON),
    mj_ud_py    = as.numeric(IRPYUD5MRJ),
    
    use_group = case_when(
      is.na(nic_vape_pm) | is.na(mj_any_pm) ~ NA_character_,
      nic_vape_pm == 1 & mj_any_pm == 1 ~ "Both (Nic Vape + MJ)",
      nic_vape_pm == 1 & mj_any_pm == 0 ~ "Nicotine Vape Only",
      nic_vape_pm == 0 & mj_any_pm == 1 ~ "Marijuana Only",
      TRUE ~ "Neither"
    ) %>% factor(levels = c("Neither", "Nicotine Vape Only", "Marijuana Only", "Both (Nic Vape + MJ)")),
    
    mem_diff_bin = case_when(
      LVLDIFMEM2 %in% c(85, 94, 97, 98) ~ NA_real_,
      LVLDIFMEM2 == 1 ~ 0,
      LVLDIFMEM2 %in% c(2, 3) ~ 1,
      TRUE ~ NA_real_
    ),
    
    yth_mde_py = case_when(
      YMDEYR == 1 ~ 1,
      YMDEYR == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    yth_gad_score = as.numeric(YGADTOTSC),
    yth_gad_f     = factor(YGADMODOSEV),
    yth_suic_f    = factor(YUSUITHKYR),
    
    ad_mde_py  = as.numeric(IRAMDEYR),
    ad_spd_py  = as.numeric(SPDPSTYR),
    ad_ami_py  = as.numeric(AMIPY),
    ad_smi_py  = as.numeric(SMIPY),
    ad_suic_py = case_when(
      ADSUITPAYR == 1 ~ 1,
      ADSUITPAYR == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    ad_gad_score = as.numeric(AGADTOTSC),
    ad_gad_f     = factor(AGADMODOSEV),
    
    # Health & demographic factors
    health_f = factor(HEALTH2),
    sex_f    = factor(IRSEX),
    race_f   = factor(NEWRACE2),
    educ_f   = factor(EDUHIGHCAT)
  )

options(survey.lonely.psu = "adjust")

#(14-17) past-year MDE summary & weighted logistic regression
yth_df <- cohort_mh %>%
  filter(dev_stage == "Adolescent (14-17)", !is.na(use_group), !is.na(yth_mde_py))

yth_df %>%
  group_by(use_group) %>%
  summarise(
    n = n(),
    yth_mde_rate = mean(yth_mde_py, na.rm = TRUE),
    mean_gad_score = mean(yth_gad_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

des_yth <- svydesign(
  ids = ~VEREP,
  strata = ~VESTR_C,
  weights = ~ANALWT2_C,
  data = yth_df,
  nest = TRUE
)

fit_yth_mde <- svyglm(
  yth_mde_py ~ use_group + age_group + sex_f + race_f + health_f,
  design = des_yth,
  family = quasibinomial()
)
summary(fit_yth_mde)

#(18-29) past-year MDE summary & weighted logistic regression
ya_df <- cohort_mh %>%
  filter(dev_stage == "Young Adult (18-29)", !is.na(use_group), !is.na(ad_mde_py))

ya_df %>%
  group_by(use_group) %>%
  summarise(
    n = n(),
    ad_mde_rate = mean(ad_mde_py, na.rm = TRUE),
    ad_spd_rate = mean(ad_spd_py, na.rm = TRUE),
    mean_gad_score = mean(ad_gad_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

des_ya <- svydesign(
  ids = ~VEREP,
  strata = ~VESTR_C,
  weights = ~ANALWT2_C,
  data = ya_df,
  nest = TRUE
)

fit_ya_mde <- svyglm(
  ad_mde_py ~ use_group + age_group + sex_f + race_f + health_f + educ_f,
  design = des_ya,
  family = quasibinomial()
)
summary(fit_ya_mde)
