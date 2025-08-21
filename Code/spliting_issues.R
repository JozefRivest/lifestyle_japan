library(haven)
library(rsample)
library(ranger)
library(h2o)
library(vip)
library(extrafont)


dat_issues <- read_rds("_SharedFolder_article-lifestyle-japan/Data/selected_issues.rds")

# ========= Sampling the data to create the test set

dat_issues$security_scale <- rowMeans(dat_issues[, c("issue_spendMilitary_num", "issue_attackEnnemy_num", "issue_forcesArticle9_num", "issue_militaryStrengtheded_num")], na.rm = TRUE)

dat_rd_forest <- dat_issues |>
  select(
    security_scale,
    matches("lifestyle_*")
  ) |>
  select(-c(lifestyle_movie_char, lifestyle_music_char)) |>
  na.omit()

glimpse(dat_rd_forest)

set.seed(456)
test_1 <- initial_split(dat_rd_forest, prop = 0.7)

# Extract the training data
train_data <- training(test_1) # Add this line

# number of features
n_features <- length(setdiff(names(dat_rd_forest), "Security_Issues"))

# train a default random forest model
issues_security <- ranger(
  security_scale ~ .,
  data = train_data,
  mtry = floor(n_features / 3),
  respect.unordered.factors = "order",
  seed = 123,
  importance = "permutation"
)

print(issues_security)

p1 <- vip::vip(issues_security, num_features = 40, bar = FALSE)
rf_security <- p1 +
  theme_classic() +
  theme(text = element_text("CM Roman"))
ggsave("~/Dropbox/Applications/Overleaf/lifestyle_japan/images/rf_security.pdf", plot = rf_security, height = 6, width = 6)


library(pdp)

# Compute partial dependence values
pd_object <- partial(issues_security, pred.var = "lifestyle_favSportPlay_char")

# Plot the partial dependence
plotPartial(pd_object) # or autoplot(pd_object) for ggplot2-based plots


# Tune hyperparameters
issues_security_tuned <- ranger(
  security_scale ~ .,
  data = train_data,
  mtry = floor(sqrt(n_features)), # Try different mtry
  min.node.size = 10, # Increase min node size
  num.trees = 1000, # More trees
  respect.unordered.factors = "partition",
  seed = 123,
  importance = "permutation"
)

print(issues_security_tuned)

p2 <- vip::vip(issues_security_tuned, num_features = 40, bar = FALSE)
rf_security_2 <- p2 +
  theme_classic() +
  theme(text = element_text("CM Roman"))
ggsave("~/Dropbox/Applications/Overleaf/lifestyle_japan/images/rf_security_2.pdf", plot = rf_security, height = 6, width = 6)
