# This problem set examines the impact of fertility on various labor market
# variables:
# 1. female labor supply
# 2. earnings
# 3. etc.
# using a dataset from the US in 1980.
# The analysis is based on Angrist and Evans (henceforth AE), 1998,
# “Children and Their Parents’ Labor Supply: Evidence from Exogneous Variation in Family Size”, American Economic Review, 1998, 88(3):450-477.
# The dataset in aedata.csv consists of 254,654 households in the US in the 1980 with at least two children. You find a data description in the Appendix on page 3. 1.

# Subquestion 1.
# Using OLS, regress various labor market outcomes (
# 1. female labor participation, (workedm)
# 2. female hours of work, (hourswm)
# 3. total family income (faminc1)
# 4. moms labor income (incomem)
# ) on number of children (our measure of fertility)
# while controlling for:
# 1. age of the mother (agem1)
# 2. age of mother at first birth (agefstm)
# 3. education of the mother (educm)
# 4. and racial variables (i.e., blackm, hispm, othracem) (no whitem because it
# is the reference category)
# Explain why these estimates might not reflect the causal effect of fertility
# on labor outcomes.
rm(list = ls())

library(tidyverse)
library(stats)
library(dplyr)
library(lmtest)
library(ivreg)
library(modelsummary)

# Load the dataset
aedata <- read.csv("data/aedata.csv")


# Implement OLS regression
# I don't include the variable "whitem" because it would lead to multiple
# collinearity since the 4 dummy variables ("whitem", "blackm", "hispm",
# "othracem") are mutually exclusive and sum up to 1, and "whitem" has been
# chosen as the reference category. So this would just
# increase the dimensionality and it would reduce the interpretability of the
# coefficients.
formula <- as.formula(
  "workedm ~ kidcount + agem1 + agefstm + educm + blackm + hispm + othracem"
)

# Perform OLS regression for each labor market outcome
model_workedm <- lm(
  workedm ~ kidcount + agem1 + agefstm + educm + blackm + hispm + othracem,
  data = aedata
)
summary(model_workedm)
# The coefficient of kidcount represents the effect of an additional child on
# the probability of working. The coefficient is negative, so the effect is
# negative. This means that an additional child reduces the probability of
# working. This is an intuitive result, but it does not prove a causal relation
# between fertility and labor market outcomes.
print(paste0(
  "The estimate of the coefficient of kidcount is: ",
  model_workedm$coefficients[2]
))
model_hourswm <- lm(
  hourswm ~ kidcount + agem1 + agefstm + educm + blackm + hispm + othracem,
  data = aedata
)
summary(model_hourswm)
print(paste0(
  "The estimate of the coefficient of kidcount is: ",
  model_hourswm$coefficients[2]
))
model_faminc1 <- lm(
  faminc1 ~ kidcount + agem1 + agefstm + educm + blackm + hispm + othracem,
  data = aedata
)
summary(model_faminc1)
print(paste0(
  "The estimate of the coefficient of kidcount is: ",
  model_faminc1$coefficients[2]
))
model_incomem <- lm(
  incomem ~ kidcount + agem1 + agefstm + educm + blackm + hispm + othracem,
  data = aedata
)
summary(model_incomem)
print(paste0(
  "The estimate of the coefficient of kidcount is: ",
  model_incomem$coefficients[2]
))
# Some noticeable features are that:
# 1. The coefficient of kidcount is negative for all labor market outcomes,
# with a small std. deviation, so the effect of an additional child is
# consistently negative across the different outcomes.
# 2. The coefficient of kidcount is statistically significant for all labor
# market outcomes.
# 3. In the last 2 models (faminc1 and incomem), the coefficients of `blackm`
# and `hispm` have opposite sign between the two models. Particularly, the
# coefficients of `blackm` and `hispm` are positive in the model for `incomem`
# and negative for `faminc1`. This may be interpreted as follows: the income of
# the mother is higher when she is black or hispanic, but the income of the
# family is lower when the mother is black or hispanic.
# On the other hand, this causal effect is not proven because this may also be
# due to external factors that are not included in the model, such as the
# racial variables related to the father.
# For example, considering the fact that interracial marriages are usually
# less common, if the mother is black, the father may be more likely to be
# black as well and this is highly related to his income. So the income of the
# family may be lower because the father is black, not because the mother is
# black.

# Subquestion 3.

# The treatment we now consider is the birth of a third child and its effect
# on these labor outcomes. The instrument we are going to use is whether the
# first two children have the same sex. Explain the reasoning for the choice
# of the instrument.
# Assess the relevance of the instrument empirically. Explain what it means
# for an instrument to be relevant and what happens if this is not the case.

# Select data with 2 or 3 children
aedata_2_3 <- aedata[aedata$kidcount == 2 | aedata$kidcount == 3, ]
instr_var_relevance_model <- lm(
  samesex ~ kidcount + agem1 + agefstm + educm + blackm + hispm + othracem,
  data = aedata_2_3
)
summary(instr_var_relevance_model)
# Compute estimate/std. of the coefficient of kidcount
print(paste0(
  "The estimate of the coefficient of kidcount divided by its std. error is: ",
  instr_var_relevance_model$coefficients[2] / sqrt(diag(vcov(instr_var_relevance_model)))[2]
))

# The coefficient of ``kidcount`` is positive and statistically significant,
# so the instrument is relevant.
# Moreover, we notice that the coefficients of the other covariates (that
# we used as controls in 1.) are not statistically significant (p-value > 0.02),
# except for ``agem1`` and ``agefstm``, which have a coefficient that is
# much smaller than the coefficient of ``kidcount``:
print(paste0(
  "The ratio between the coefficient of kidcount and `agefstm` is: ",
  instr_var_relevance_model$coefficients[2] /
    instr_var_relevance_model$coefficients[4]
))
# so the instrument is not related to them.

# Subquestion 4.
# Estimate by 2SLS the effect of the treatment using the controls from 1.
# and compare to the OLS estimates. Briefly justify your choice of standard
# errors.


iv_models <- list()
ols_models <- list()
for (labor_outcome in c("workedm", "hourswm", "faminc1", "incomem")) {
  # labor_outcome <- "hourswm"
  iv_formula <- paste0(
    labor_outcome,
    " ~ agem1 + agefstm + educm + blackm + hispm + othracem |",
    " kidcount |", # endogenous variable
    " samesex" # instrument
  )
  iv_models[[labor_outcome]] <- ivreg(
    iv_formula,
    data = aedata_2_3
  )
  # # Check the 2SLS results
  # print(paste0(
  #   "Results for the second stage regression of ",
  #   labor_outcome,
  #   ":\n",
  #   iv_models[[labor_outcome]]
  # ))
  ols_formula <- paste0(
    labor_outcome,
    " ~ kidcount +
    agem1 + agefstm + educm + blackm + hispm + othracem"
  )
  ols_models[[labor_outcome]] <- lm(ols_formula, data = aedata_2_3)

  # Compare 2SLS and OLS estimates
  m_list <- list(OLS = ols_models[[labor_outcome]], IV = iv_models[[labor_outcome]])
  print(paste0("Results for ", labor_outcome, ":"))
  msummary(m_list)
  modelplot(m_list, coef_omit = "Intercept")

  library(sandwich)
  # Adjusted standard errors using robust/sandwich estimator
  coeftest(ols_models[[labor_outcome]], vcov. = sandwich)

  coeftest(iv_models[[labor_outcome]], vcov. = sandwich)
}

# I used the robust/sandwich estimator because it is more robust to
# heteroskedasticity and autocorrelation than the standard OLS estimator.
# This is important because the standard OLS estimator assumes that the
# residuals are homoskedastic and uncorrelated, which is not always true.


# Plot the residuals of the 2SLS model with ggplot2
preds <- iv_models[["hourswm"]]$fitted.values
resids <- iv_models[["hourswm"]]$residuals

library(ggplot2)
ggplot(mapping = aes(x = preds, y = resids)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals of the 2SLS model",
    x = "Fitted values",
    y = "Residuals"
  )
ggsave("plots/2sls_residuals.png", width = 30, height = 50, units = "cm")


# 2SLS manually
# NOTE: I did not manage to make it work with more than 10K samples because of
# memory constraints when creating the projector P_z.
# Step 1: First Stage Regression
data <- aedata_2_3[1:1000, ]
endogenous_vars <- c(
  "kidcount", "agem1", "agefstm", "educm", "blackm", "hispm", "othracem"
)
exogenous_vars <- c(
  "samesex", "agem1", "agefstm", "educm", "blackm", "hispm", "othracem"
)
target_var <- "hourswm"

regress_2sls_iv <- function(data, endogenous_vars, exogenous_vars, target_var) {
  library(Matrix)
  Z_mat <- as.matrix(data[, exogenous_vars])
  Z_mat <- Matrix(data = cbind(1, Z_mat), sparse = TRUE)

  X_mat <- as.matrix(data[, endogenous_vars])
  X_mat <- Matrix(data = cbind(1, X_mat), sparse = TRUE)
  y_mat <- as.matrix(data[, target_var])

  # P_z = Z(Z'Z)^(-1)Z'
  p_z <- Z_mat %*% solve(crossprod(Z_mat)) %*% t(Z_mat)

  # X_hat = pred_X = P_z * X
  pred_X_mat <- p_z %*% X_mat
  # beta_hat_IV = (X_hat'X_hat)^(-1) * X_hat'Y
  beta_2sls <- solve(crossprod(pred_X_mat)) %*% t(pred_X_mat) %*% y_mat

  residuals <- y_mat - X_mat %*% beta_2sls
  num_samples <- nrow(X_mat)
  sigma_iv <- as.numeric(crossprod(residuals, residuals)) / (num_samples - length(beta_2sls))
  covariance_mat_2sls <- sigma_iv * solve(crossprod(X_mat, p_z) %*% X_mat)
  standard_errors_2sls <- sqrt(diag(covariance_mat_2sls))

  results <- cbind(beta_2sls, standard_errors_2sls, beta_2sls / standard_errors_2sls)
  colnames(results) <- c("beta_2sls", "standard_errors_2sls", "t_stat_2sls")
  return(results)
}

result_manual <- regress_2sls_iv(data, endogenous_vars, exogenous_vars, target_var)
result_manual

# ============= Comparison with ivreg =============
iv_formula <- paste0(
  "hourswm",
  " ~ agem1 + agefstm + educm + blackm + hispm + othracem |",
  " kidcount |", # endogenous variable
  " samesex" # instrument
)
ivreg_model <- ivreg(
  iv_formula,
  data = aedata_2_3_redux
)
ivreg_model
coeftest(ivreg_model, vcov. = sandwich)
