# Generate simulated data
library(tsintermittent)
IDI = c(1.00, 1.32, 2.00, 4.00)
CV2 = c(0.25, 0.49, 1.00, 2.00)
obs = c(84, 108, 132, 156)

dataset_84 = c()
for (i in 1:length(IDI)) {
  for (j in 1:length(CV2)) {
    dataset <- t(simID(n = 1000, obs = obs[1]
                       , idi = IDI[i], cv2 = CV2[j]))
    dataset_84 = rbind(dataset_84, dataset)
  }
}

dataset_108 = c()
for (i in 1:length(IDI)) {
  for (j in 1:length(CV2)) {
    dataset <- t(simID(n = 1000, obs = obs[2]
                       , idi = IDI[i], cv2 = CV2[j]))
    dataset_108 = rbind(dataset_108, dataset)
  }
}

dataset_132 = c()
for (i in 1:length(IDI)) {
  for (j in 1:length(CV2)) {
    dataset <- t(simID(n = 1000, obs = obs[3]
                       , idi = IDI[i], cv2 = CV2[j]))
    dataset_132 = rbind(dataset_132, dataset)
  }
}

dataset_156 = c()
for (i in 1:length(IDI)) {
  for (j in 1:length(CV2)) {
    dataset <- t(simID(n = 1000, obs = obs[4]
                       , idi = IDI[i], cv2 = CV2[j]))
    dataset_156 = rbind(dataset_156, dataset)
  }
}

dataset_simulation = list(dataset_84, dataset_108, dataset_132, dataset_156)
dataset_simulation_test = list()
dataset_simulation_train = list()

for (i in 1:4) {
  for (j in 1:16000) {
    lentry_test = list()
    lentry_train = list()

    lentry_test$tsid = lentry_train$tsid = tsid = (i-1)*16000+j
    lentry_test$h = lentry_train$h = h = 12
    lentry_test$length = len_test = dim(dataset_simulation[[i]])[2]-h
    lentry_train$length = len_train = len_test-h

    x1 = dataset_simulation[[i]][j,][1:len_test]
    x1 <- data.frame(x1, c(1:length(x1)))
    starting_period <- min(x1[x1$x1>0,2])
    x1_new <- x1$x1[starting_period:nrow(x1)]
    lentry_test$x = x = ts(x1_new)
    lentry_test$xx = ts(dataset_simulation[[i]][j,][(len_test+1):(len_test+12)])
    lentry_test$IDI = compute_ADI(x)
    lentry_test$cv2 = compute_CV2(x)

    lentry_train$x = ts(x[1:(length(x)-h)])
    lentry_train$xx = ts(x[(length(x)-h+1):length(x)])
    lentry_train$IDI = compute_ADI(lentry_train$x)
    lentry_train$cv2 = compute_CV2(lentry_train$x)

    dataset_simulation_train[[tsid]] = list()
    dataset_simulation_train[[tsid]] = lentry_train
    dataset_simulation_test[[tsid]] = list()
    dataset_simulation_test[[tsid]] = lentry_test
  }
}

# Plot the simulated text set
idi = c()
cv2 = c()
for (i in 1:length(dataset_simulation_test)) {
  idi = c(idi, dataset_simulation_test[[i]]$IDI)
  cv2 = c(cv2, dataset_simulation_test[[i]]$cv2)
}
simulation_data = data.frame(IDI = idi, CV2 = cv2)

library(ggplot2)
ggplot(simulation_data, aes(x=log(IDI), y=log(CV2))) +
  geom_hex(bins = 50, aes(alpha=log(..count..)),fill="#000000")+
  xlim(-0.5, 2.5) +
  ylim(-3,3)+
  geom_vline(xintercept = log(1.32), color="red",linetype = 2) +
  geom_hline(yintercept = log(0.49), color="red",linetype = 2)+
  labs(x = expression(log(IDI)), y=expression(log(CV^2)))


# An example
# Choose 20 simulated time series and construct the training set and testing set
id_sample = sample(c(1:length(dataset_simulation_train)), 20)
dataset_train_example = dataset_simulation_train[id_sample]
dataset_test_example = dataset_simulation_test[id_sample]

source("D:/time series/intermittent/R/git_R/diversity.R")
source("D:/time series/intermittent/R/git_R/errors_intermittent.R")
source("D:/time series/intermittent/R/git_R/features_intermittent.R")
source("D:/time series/intermittent/R/git_R/forecasts_intermittent.R")

# Forecast with the nine methods in the pool for the training set
for (i in 1:length(dataset_train_example)) {
  dataset_train_example[[i]] = calculate_forec_point(dataset_train_example[[i]])
}

# Calculate errors of different methods (MASE for example)
dataset_train_example = calc_errors_mase(dataset_train_example)

# If use FIDE, calculate the feature vector for each series.
for (i in 1:length(dataset_train_example)) {
  dataset_train_example[[i]]$features = compute_ifeatures(dataset_train_example[[i]]$x)
}

# If use DIVIDE, calculate the diversity vector for each series.
dataset_train_example = compute_diversity(dataset_train_example)

# Train the XGBoost
train_data <- create_feat_classif_problem(dataset_train_example)
meta_model <- train_selection_ensemble(train_data$data,
                                       train_data$errors)

# Forecast with the nine methods in the pool for the testing set
for (i in 1:length(dataset_test_example)) {
  dataset_test_example[[i]] = calculate_forec_point(dataset_test_example[[i]])
}

# If use FIDE, calculate the feature vector for each series.
for (i in 1:length(dataset_test_example)) {
  dataset_test_example[[i]]$features = compute_ifeatures(dataset_test_example[[i]]$x)
}

# If use DIVIDE, calculate the diversity vector for each series.
dataset_test_example = compute_diversity(dataset_test_example)

# Use the pre-trained model to produce combination weights
final_data <- create_feat_classif_problem(dataset_test_example)
preds <- predict_selection_ensemble(meta_model, final_data$data)

# Combine forecasts by the weights
dataset_test_example <- ensemble_forecast(preds, dataset_test_example)

# Output the forecasting values of the fifth time series as an example
# > dataset_test_example[[5]]$y_hat


# Calculate the forecasting errors of the proposed method
predictions_res = summary_performance_error_intermittent(dataset_test_example)

