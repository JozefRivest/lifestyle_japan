library(tidyverse)
library(readr)
library(haven)
library(rsample)
library(ranger)
library(h2o)
library(vip)
library(extrafont)
library(glmnet)
library(caret)
library(pdp)

dat_issues <- read_rds("_SharedFolder_article-lifestyle-japan/Data/selected_issues.rds")

# ========= Sampling the data to create the test set

dat_rd_forest_security <- dat_issues |>
  select(
    issue_spendMilitary_num,
    issue_attackEnnemy_num,
    issue_forcesArticle9_num,
    issue_militaryStrengtheded_num,
    matches("lifestyle_*")
  ) |>
  mutate(
    issue_spendMilitary_num = round(issue_spendMilitary_num, 2),
    issue_attackEnnemy_num = round(issue_attackEnnemy_num, 2),
    issue_forcesArticle9_num = round(issue_forcesArticle9_num, 2),
    issue_militaryStrengtheded_num = round(issue_militaryStrengtheded_num, 2),
    across(matches("*_char"), ~ factor(.x)),
    across(matches("issue_*"), ~ case_when(
      .x == 0 ~ 1,
      .x == 0.33 ~ 2,
      .x == 0.67 ~ 3,
      .x == 1 ~ 4
    )),
    security_scale = rowMeans(dat_issues[, c("issue_spendMilitary_num", "issue_attackEnnemy_num", "issue_forcesArticle9_num", "issue_militaryStrengtheded_num")], na.rm = TRUE)
  ) |>
  na.omit()
nrow(dat_rd_forest)

glimpse(dat_rd_forest)

dat_rd_forest_security <- dat_rd_forest_security |>
  select(
    security_scale,
    matches("lifestyle_*")
  ) |>
  select(-c(lifestyle_movie_char, lifestyle_music_char)) |>
  na.omit()

set.seed(456)
test_1_security <- initial_split(dat_rd_forest_security, prop = 0.7)

# Extract both training and testing data
train_data_security <- training(test_1_security)
test_data_security <- testing(test_1_security)

# Number of features (corrected variable name)
n_features <- length(setdiff(names(dat_rd_forest_security), "security_scale"))

# Train a default random forest model
issues_security <- ranger(
  security_scale ~ .,
  data = train_data_security,
  mtry = floor(n_features / 3),
  respect.unordered.factors = "order",
  seed = 123,
  importance = "permutation"
)

# Print model summary
print(issues_security)

# Check model performance
predictions <- predict(issues_security, test_data)
rmse <- sqrt(mean((test_data$security_scale - predictions$predictions)^2))
print(paste("RMSE:", round(rmse, 4)))

# Variable importance
importance_df <- importance(issues_security)
head(importance_df[order(importance_df, decreasing = TRUE)], 10)


p1 <- vip::vip(issues_security, num_features = 40, bar = FALSE)
rf_security <- p1 +
  theme_classic() +
  theme(text = element_text("CM Roman"))
ggsave("~/Dropbox/Applications/Overleaf/lifestyle_japan/images/rf_security.pdf", plot = rf_security, height = 6, width = 6)


# Compute partial dependence values
pd_object <- partial(issues_security, pred.var = "lifestyle_favSportPlay_char")

# Plot the partial dependence
plotPartial(pd_object) # or autoplot(pd_object) for ggplot2-based plots

# =========== Full model security

dat_rd_forest_security_full <- dat_issues |>
  select(
    gender,
    age_group,
    prefecture,
    income,
    occupation,
    education,
    issue_spendMilitary_num,
    issue_attackEnnemy_num,
    issue_forcesArticle9_num,
    issue_militaryStrengtheded_num,
    matches("lifestyle_*")
  ) |>
  mutate(
    issue_spendMilitary_num = round(issue_spendMilitary_num, 2),
    issue_attackEnnemy_num = round(issue_attackEnnemy_num, 2),
    issue_forcesArticle9_num = round(issue_forcesArticle9_num, 2),
    issue_militaryStrengtheded_num = round(issue_militaryStrengtheded_num, 2),
    across(matches("*_char"), ~ factor(.x)),
    across(matches("issue_*"), ~ case_when(
      .x == 0 ~ 1,
      .x == 0.33 ~ 2,
      .x == 0.67 ~ 3,
      .x == 1 ~ 4
    )),
    security_scale = rowMeans(dat_issues[, c("issue_spendMilitary_num", "issue_attackEnnemy_num", "issue_forcesArticle9_num", "issue_militaryStrengtheded_num")], na.rm = TRUE)
  ) |>
  select(
    -c(matches("issue_*"), lifestyle_movie_char, lifestyle_music_char)
  ) |>
  na.omit()

glimpse(dat_rd_forest_security_full)

set.seed(456)
test_1_security_full <- initial_split(dat_rd_forest_security_full, prop = 0.7)

# Extract the training data
train_data_security_full <- training(test_1_security_full) # Add this line

# number of features
n_features <- length(setdiff(names(dat_rd_forest_security_full), "gender_issues"))

# train a default random forest model
security_issues_full <- ranger(
  security_scale ~ .,
  data = train_data_security_full,
  mtry = floor(n_features / 3),
  respect.unordered.factors = "order",
  seed = 123,
  importance = "permutation"
)

print(security_issues_full)

p1_security_full <- vip::vip(security_issues_full, num_features = 40, bar = FALSE)
rf_security_full <- p1_security_full +
  theme_classic() +
  theme(text = element_text("CM Roman"))
ggsave("~/Dropbox/Applications/Overleaf/lifestyle_japan/images/rf_security_full.pdf", plot = rf_security_full, height = 6, width = 6)

# ==================== Women Random Forest ============================

dat_issues$gender_scale <- rowMeans(
  dat_issues[
    ,
    c(
      "issue_genderWomenCareHousehold_num",
      "issue_genderWomenCareerOverFam_num",
      "issue_genderMoreWomenInPower_num",
      "issue_genderWomenMoreChallenges_num"
    )
  ],
  na.rm = TRUE
)

head(dat_issues)

dat_rd_forest_gender <- dat_issues |>
  select(
    gender_scale,
    matches("lifestyle_*")
  ) |>
  select(-c(lifestyle_movie_char, lifestyle_music_char)) |>
  na.omit()

glimpse(dat_rd_forest_gender)

set.seed(456)
test_1 <- initial_split(dat_rd_forest_gender, prop = 0.7)

# Extract the training data
train_data <- training(test_1) # Add this line

# number of features
n_features <- length(setdiff(names(dat_rd_forest_gender), "gender_issues"))

# train a default random forest model
gender_issues <- ranger(
  gender_scale ~ .,
  data = train_data,
  mtry = floor(n_features / 3),
  respect.unordered.factors = "order",
  seed = 123,
  importance = "permutation"
)

print(gender_issues)

p1 <- vip::vip(gender_issues, num_features = 40, bar = FALSE)
rf_gender <- p1 +
  theme_classic() +
  theme(text = element_text("CM Roman"))
ggsave("~/Dropbox/Applications/Overleaf/lifestyle_japan/images/rf_gender.pdf", plot = rf_gender, height = 6, width = 6)


# Compute partial dependence values
pd_object <- partial(gender_issues, pred.var = "lifestyle_favSportPlay_char")

# Plot the partial dependence
plotPartial(pd_object) # or autoplot(pd_object) for ggplot2-based plots

glimpse(dat_issues)

dat_rd_forest_gender_full <- dat_issues |>
  select(
    gender_scale,
    gender,
    age_group,
    prefecture,
    income,
    occupation,
    education,
    matches("lifestyle_*")
  ) |>
  select(-c(lifestyle_movie_char, lifestyle_music_char)) |>
  na.omit()

glimpse(dat_rd_forest_gender_full)

set.seed(456)
test_1_full <- initial_split(dat_rd_forest_gender_full, prop = 0.7)

# Extract the training data
train_data_full <- training(test_1) # Add this line

# number of features
n_features <- length(setdiff(names(dat_rd_forest_gender_full), "gender_issues"))

# train a default random forest model
gender_issues_full <- ranger(
  gender_scale ~ .,
  data = train_data_full,
  mtry = floor(n_features / 3),
  respect.unordered.factors = "order",
  seed = 123,
  importance = "permutation"
)

print(gender_issues_full)

p1_gender_full <- vip::vip(gender_issues_full, num_features = 40, bar = FALSE)
rf_gender_full <- p1_gender_full +
  theme_classic() +
  theme(text = element_text("CM Roman"))
ggsave("~/Dropbox/Applications/Overleaf/lifestyle_japan/images/rf_gender_full.pdf", plot = rf_gender_full, height = 6, width = 6)
