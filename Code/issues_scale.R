library(tidyverse)
library(psych)
library(modelsummary)
library(factortable)
library(tinytable)
library(readr)
library(extrafont)

dat_issues <- read_rds("~/Dropbox/_SharedFolder_article-lifestyle-japan/Data/selected_issues.rds")

# ============ Assessing dimensionality =================
glimpse(dat_issues)

article_9_isuses <- dat_issues |>
  select(
    issue_spendMilitary_num,
    issue_attackEnnemy_num,
    issue_forcesArticle9_num,
    issue_militaryStrengtheded_num
  ) |>
  round(digits = 2) |>
  mutate(
    across(
      everything(), ~ case_when(
        .x == 0 ~ 1,
        .x == 0.33 ~ 2,
        .x == 0.67 ~ 3,
        .x == 1 ~ 4
      )
    )
  ) |>
  na.omit()

datasummary_skim(article_9_isuses) |>
  print("html")
nrow(article_9_isuses) # 1720

datasummary_correlation(article_9_isuses) |>
  format_tt(escape = "latex") |>
  print("latex")

scree_data <- scree(cor(article_9_isuses), pc = FALSE)

scree_df <- tibble(
  Factors = 1:4,
  Eigenvalues = scree_data$fv
)

scree_art_9 <- ggplot(scree_df, aes(x = Factors, y = Eigenvalues)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  theme(text = element_text(family = "CM Roman"))
# ggsave("~/Dropbox/Applications/Overleaf/lifestyle_japan_can_usa/images/scree_article_9.pdf", height = 5, width = 5)

efa_article_9 <- fa(
  article_9_isuses,
  nfactors = 1,
  n.obs = nrow(article_9_isuses),
  fm = "pa",
  cor = "cor"
)

factortable(efa_article_9) |>
  style_tt(i = c(5, 8), bold = TRUE) |>
  style_tt(j = 2:5, align = "c") |>
  print("latex")


psych::alpha(cor(article_9_isuses))


# ================ Women ===================

glimpse(dat_issues)
table(dat_issues$issue_genderMoreWomenInPower_num)

women_issues <- dat_issues |>
  select(
    issue_genderWomenCareHousehold_num,
    issue_genderWomenCareerOverFam_num,
    issue_genderMoreWomenInPower_num,
    issue_genderWomenMoreChallenges_num
  ) |>
  round(digits = 2) |>
  mutate(
    across(
      everything(), ~ case_when(
        .x == 0 ~ 1,
        .x == 0.33 ~ 2,
        .x == 0.67 ~ 3,
        .x == 1 ~ 4
      )
    )
  ) |>
  na.omit()

# verifying the changes
datasummary_skim(women_issues) |>
  print("html")
nrow(women_issues) # 1726


# reversing this item so it becomes logical with the others
women_issues$issue_genderWomenCareHousehold_num <- reverse.code(women_issues$issue_genderWomenCareHousehold_num)


# correlation between all the items
datasummary_correlation(women_issues) |>
  format_tt(escape = "latex") |>
  print("latex")

scree_data <- scree(cor(women_issues), pc = FALSE)

scree_df <- tibble(
  Factors = 1:4,
  Eigenvalues = scree_data$fv
)

scree <- ggplot(scree_df, aes(x = Factors, y = Eigenvalues)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  theme(text = element_text(family = "CM Roman"))
# ggsave("~/Dropbox/Applications/Overleaf/lifestyle_japan/images/scree_gender.pdf", plot = scree, height = 5, width = 5)

efa_gender <- fa(
  women_issues,
  nfactors = 1,
  n.obs = nrow(women_issues),
  fm = "pa",
  cor = "cor"
)

efa_gender

factortable(efa_gender) |>
  style_tt(i = c(5, 8), bold = TRUE) |>
  style_tt(j = 2:5, align = "c") |>
  print("latex")


psych::alpha(cor(women_issues))
