# Title: Lab 5 - Using AUC/ ROC and Confusion Matrices #
# Readings ISLR P 147 on ROC curves
#

# Read in the required packages #
require(ISLR)
require(ggplot2)
require(ROCR)
require(magrittr)

# Set up the data that we need #
data(Caravan)

# Look at the summary of the data #
summary(Caravan)

# Start by fitting the Model - as a test we fit the full model here #
# with all the predictors #
purchase.model <- glm(Purchase ~ .,
  data = Caravan,
  family = "binomial"(link = "logit"))

# Look at a ROC Curve #
pred.roc <- ROCR::prediction(
  predictions = predict(purchase.model, type = "response"),
  labels = Caravan$Purchase,
  label.ordering = NULL)

# Create an ROC object #
perf.roc <- ROCR::performance(pred = pred.roc,
  measure = "tpr",
  x.measure = "fpr")

# Assess where the cutoff should be #
cutoff.df <- data.frame(
  cutoffs = slot(pred.roc, "cutoffs") %>% unlist,
  tp = slot(pred.roc, "tp") %>% unlist,
  fp = slot(pred.roc, "fp") %>% unlist,
  tn = slot(pred.roc, "tn") %>% unlist,
  fn = slot(pred.roc, "fn") %>% unlist,
  tpr = slot(perf.roc, "y.values") %>% unlist,
  fpr = slot(perf.roc, "x.values") %>% unlist)

# Create a plot of the ROC curve #
# Note: We are colorzing here to vizualize where the cutoffs are #
plot(perf.roc, lwd = 2, colorize = TRUE)
abline(a = 0, b = 1, lty = 2)

# Assess the area under the curve #
perf.auc <- ROCR::performance(pred = pred.roc,
  measure = "auc")
slot(perf.auc, "y.values") %>% unlist

# Pick the optimal cutoff and extract the elements of the confusion matrix #
cutoff.df %>%
  dplyr::filter(cutoffs > 0.1832 & cutoffs < 0.1840)
