library(tidyverse)
library(psych)
library(modelsummary)
library(factortable)

dat_issues <- read_rds("_SharedFolder_article-lifestyle-japan/Data/selected_issues.rds")

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

scree <- ggplot(scree_df, aes(x = Factors, y = Eigenvalues)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  theme(text = element_text(family = "CM Roman"))
ggsave("~/Dropbox/Applications/Overleaf/lifestyle_japan_can_usa/images/scree_article_9.pdf", height = 5, width = 5)

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
