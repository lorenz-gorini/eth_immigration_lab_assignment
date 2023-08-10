# Load the data from PVW\_synth.csv. Set the randomization seed at the beginning of your script to ensure that we can replicate your results. Randomly split the data in (approximately) two halves. The first half is the training sample, the other half is the validation sample.

rm(list = ls())
library(tidyverse)
library(randomForest)
library(glmnet)

# Set seed
set.seed(123)

# Load data
PVW_synth <- read.csv("data/PVW_synth.csv")

# Randomly split the data into training and validation samples
split_index <- sample(nrow(PVW_synth), size = as.integer(nrow(PVW_synth) / 2), )
training_data <- PVW_synth[split_index, ]
test_data <- PVW_synth[-split_index, ]

# Question 3.3
# Fit these learners to the training sample using net financial assets as the outcome and using the predictors listed above. Briefly motivate your choice of (hyper)tuning parameters.

regressors <- c(
    "age", "inc", "educ", "fsize", "marr", "twoearn", "db", "pira", "hown"
)
target_variable <- "net_tfa"

# Fit lasso while tuning lambda
lasso_cv_fit <- cv.glmnet(
    x = as.matrix(training_data[, regressors]),
    y = training_data[, "net_tfa"],
    alpha = 1,
    lambda = 10^seq(10, -2, length = 100)
)
# Plot MSE for multiple lambda values
plot(lasso_cv_fit, xvar = "lambda", label = TRUE)
# Select the best lambda value
best_lambda <- lasso_cv_fit$lambda.min

# Plot the coefficients of regressors for multiple lambda values
lasso_fit <- glmnet(
    x = as.matrix(training_data[, regressors]),
    y = training_data[, "net_tfa"],
    alpha = 1,
    lambda = 10^seq(10, -2, length = 100)
)
# Plot coefficients of regressors for multiple lambda values
plot(
    lasso_fit, xvar = "lambda", label = TRUE,
    # increase font size
    cex.axis = 1.5, cex.main = 1.5, cex.lab = 1.5,
    # add plot title
    main = "Lasso coefficients vs. lambda"
)



# ========= Find the optimal tuning parameters for Random Forest model =========
# 1. Split the training data
split_index <- sample(
    nrow(training_data),
    size = as.integer(nrow(training_data) * 0.20)
)
sub_training_data <- training_data[split_index, ]
sub_validation_data <- training_data[-split_index, ]

# 2. Initialize the mtry and ntree range of values
mtry_values <- seq(1, length(regressors), by = 1)
ntree_values <- seq(50, 400, by = 50)

# 3. Fit Random Forest models with different mtry and ntree values
# Initialize a matrix to store the validation errors
error_matrix <- matrix(NA, nrow = length(mtry_values), ncol = length(ntree_values))

for (mtry_id in seq_along(mtry_values)) {
    for (ntree_id in seq_along(ntree_values)) {
        rf_model <- randomForest(
            x = as.matrix(training_data[, regressors]),
            y = training_data[, "net_tfa"],
            mtry = mtry_values[mtry_id],
            ntree = ntree_values[ntree_id]
        )

        # Calculate the mean squared error (MSE)
        validation_preds <- predict(rf_model, newdata = sub_validation_data)
        error_matrix[mtry_id, ntree_id] <- mean(
            (sub_validation_data[, target_variable] - validation_preds)^2
        )
    }
}
# Apply log to error matrix
log_error_matrix <- log(error_matrix)
image(mtry_values, ntree_values, log_error_matrix,
    xlab = "mtry", ylab = "ntree", main = "Error vs. mtry and ntree",
    col = colorRampPalette(c("#022042", "#ff7676"))(1000),
    # Add a color legend
    # useAbs = TRUE,
    # Increase font size
    axes = FALSE, cex.axis = 1.5, cex.main = 1.5, cex.lab = 1.5
)
axis(1, at = mtry_values, tick = TRUE, lwd = 2,  cex.axis = 1.5)
axis(2, at = ntree_values, tick = TRUE, lwd = 2,  cex.axis = 1.5)
legend("topright",
    legend = "log(MSE)",
    fill = colorRampPalette(c("#022042", "#ff7676"))(20),
    title = "Legend"
)

# Fit random forest on the whole training set with tuned parameters
rf_fit <- randomForest(
    x = as.matrix(training_data[, regressors]),
    y = training_data[, target_variable],
    ntree = 100,
    mtry = 3
)

# Question 3.4
# Assess the relative prediction performance of lasso and random forests using the validation sample. (Short
# statement.)

# Predict lasso
lasso_pred <- predict(lasso_fit, newx = as.matrix(test_data[,regressors]), s=best_lambda)

# Predict random forest
rf_pred <- predict(rf_fit, newdata = as.matrix(test_data[,regressors]))

# Calculate MSE for lasso
lasso_mse <- mean((lasso_pred - test_data[, target_variable])^2)

# Calculate MSE for random forest
rf_mse <- mean((rf_pred - test_data[, target_variable])^2)

# Print MSEs
print(lasso_mse)
print(rf_mse)
