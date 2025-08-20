library(haven)
library(rsample)
library(ranger)
library(h2o)


dat_issues <- read_rds("_SharedFolder_article-lifestyle-japan/Data/selected_issues.rds")

# ========= Sampling the data to create the test set

set.seed(456)
test_1 <- initial_split(dat_issues, prop = 0.7)


# ======== Creating a quick scale for testing
