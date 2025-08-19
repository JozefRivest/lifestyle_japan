library(haven)
library(tidyverse)
library(factortable)
library(psych)
library(vincentverse)
library(fontcm)
library(extrafont)
loadfonts()
library(rstatix)
library(lavaan)

# ====== Importing data ========

dat <- read_rds("_SharedFolder_article-lifestyle-japan/Data/df.rds")

# Selecting relevant variables for the issues

selected_dat_issues <- dat |>
  select(
    occupation = ses_occupation_char,
    income = ses_income_char,
    education = ses_education_chr,
    gender = ses_gender_chr,
    age_num = ses_age_num,
    age_group = ses_AgeGroup_char,
    prefecture = ses_prefecture_chr,
    matches("^lifestyle_.*_num$"),
    matches("^lifestyle_.*_char$"),
    matches("^issue_.*_num$"),
    matches("^issue_.*_char$"),
  ) |>
  select(-c(
    "issue_GeneTherapyScaresMe_num",
    "issue_exploreGeneModification_num",
    "issue_benefitsGeneTherapy_num"
  )) |>
  na.omit()
glimpse(selected_dat_issues)

write_rds(selected_dat_issues, "_SharedFolder_article-lifestyle-japan/Data/selected_issues.rds")

# Selecting relevant variables for the vote

selected_dat_vote <- dat |>
  select(
    occupation = ses_occupation_char,
    income = ses_income_char,
    education = ses_education_chr,
    gender = ses_gender_chr,
    age_num = ses_age_num,
    age_group = ses_AgeGroup_char,
    prefecture = ses_prefecture_chr,
    matches("^lifestyle_.*_num$"),
    matches("^lifestyle_.*_char$"),
    matches("^issue_.*_num$"),
    matches("^issue_.*_char$"),
  ) |>
  select(-c(
    "issue_GeneTherapyScaresMe_num",
    "issue_exploreGeneModification_num",
    "issue_benefitsGeneTherapy_num"
  )) |>
  na.omit()
glimpse(selected_dat_vote)

write_rds(selected_dat_vote, "_SharedFolder_article-lifestyle-japan/Data/selected_vote.rds")
