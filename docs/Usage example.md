# An forecasting example of FIDE

In the proposed forecast combination framework ([Li et al., 2022](https://arxiv.org/abs/2204.08283)), we build an XGBoost model to learn the relationship between features and combination weights. Time series features and the diversity are both valid inputs for the forecast combination model.
We name the approach based on the nine time series features as Feature- based Intermittent DEmand forecasting (FIDE). The diversity-based method is called DIVersity-based Intermittent DEmand forecasting (DIVID).

In the proposed framework, we define a suitable pool of forecasting methods for intermittent demand to
produce forecast combinations. The pool includes traditional forecasting models, which are Naive, seasonal Naive (sNaive), Simple Exponential Smoothing (SES), Moving Averages (MA), AutoRegressive Integrated Moving Average (ARIMA), ExponenTial Smoothing (ETS), and intermittent demand forecasting methods, which are Crostons method (CRO), optimized Crostons method (optCro), Syntetos-Boylan Approximation (SBA), Teunter-Syntetos-Babai (TSB), Aggregate-Disaggregate Intermittent Demand Approach (ADIDA), and Intermittent Multiple Aggregation Prediction Algorithm (IMAPA).

**We choose 20 simulated time series and construct the training set and testing set.** The simulated dataset `dataset_simulation_train` and `dataset_simulation_test` can be obtained by running [simulation_generate.R](https://github.com/lily940703/fide/blob/main/simulation_generate.R).

```r
id_sample = sample(c(1:length(dataset_simulation_train)), 20)
dataset_train_example = dataset_simulation_train[id_sample]
dataset_test_example = dataset_simulation_test[id_sample]
```

## Model training 
 In FIDE, we compute the features selected for intermittent demand and learn the relationship between the features and combination weights. In DIVIDE, the combination model can be obtained based on the diversity of different forecast methods, where the pairwise diversity values of the methods in the pool are used as time series features. Therefore, DIVIDE can be viewed as a special case of FIDE.

In the training phase, we **generate forecasts** based on the methods in the intermittent demand forecasting pool.
```r
for (i in 1:length(dataset_train_example)) {
  dataset_train_example[[i]] = calculate_forec_point(dataset_train_example[[i]])
}
```

Then we **calculate errors** of different methods required in the objective function (MASE for example).
```r
dataset_train_example = calc_errors_mase(dataset_train_example)
```

If use FIDE, we **compute the features** selected for intermittent demand.
```r
for (i in 1:length(dataset_train_example)) {
  dataset_train_example[[i]]$features = compute_ifeatures(dataset_train_example[[i]]$x)
}
```

If use DIVIDE, we **calculate the diversity vector** for each series, where the pairwise diversity values of the methods in the pool are used as time series features. 
```r
dataset_train_example = compute_diversity(dataset_train_example)
```

To estimate the optimal combination weights, we map the features or the diversity to the errors based on the XGBoost model. We **train the XGBoost** by using the functions in R package `M4metalearning`([Montero-Manso et al., 2020](https://github.com/robjhyndman/M4metalearning)). 

```r
library(M4metalearning)
train_data <- create_feat_classif_problem(dataset_train_example)
meta_model <- train_selection_ensemble(train_data$data,
                                       train_data$errors)
```                                       
                                      
## Forecasting
In the forecasting phase, we calculate the features or the diversity for the new time series, and get the combination weights through the pre-trained XGBoost model. Finally, we utilize the optimal weights to average the point forecasts from different methods in the pool and achieve the combined forecast results.

In the forecasting phase, we **forecast** with the nine methods in the pool.
```r
for (i in 1:length(dataset_test_example)) {
  dataset_test_example[[i]] = calculate_forec_point(dataset_test_example[[i]])
}
```

If use FIDE, we **calculate the feature vector** for each series.
```r
for (i in 1:length(dataset_test_example)) {
  dataset_test_example[[i]]$features = compute_ifeatures(dataset_test_example[[i]]$x)
}
```

If use DIVIDE, we **calculate the diversity vector** for each series.
```r
dataset_test_example = compute_diversity(dataset_test_example)
```

Then we **get the combination weights** through the pre-trained XGBoost model.
```r
final_data <- create_feat_classif_problem(dataset_test_example)
preds <- predict_selection_ensemble(meta_model, final_data$data)
```
Finally, we utilize the optimal weights to average the point forecasts from different methods in the pool and **achieve the combined forecast results**.
```r
dataset_test_example <- ensemble_forecast(preds, dataset_test_example)
```


In `FIDE`, we consider a series of forecasting errors to evaluate the performance of the proposed method. For traditional errors, we calculate two absolute errors scaled Mean Absolute Error (sMAE) and  Mean Absolute Scaled Error (MASE), a squared error scaled Mean Squared Error (sMSE), and a cumulated error scaled Mean Absolute Periods In Stock (sMAPIS).
For bias errors, we compute two relative errors, scaled Mean Error (sME) and scaled Mean Periods In Stock (sMPIS), and a shortage-based error Number Of Shortages (NOS). We output the averaged errors of the proposed FIDE based on the 20 simulated time series.

```r
predictions_res = summary_performance_error_intermittent(dataset_test_example)
# Average sME: 0.234193 
# Average sMAE: 1.435195 
# Average sMSE: 9.300547 
# Average MASE: 1.041296 
# Average NOS: 6 
# Average sMAPIS: 66.8499 
# Average sMPIS: -21.69083
```

## References
- Li, Li, [Yanfei Kang](https://yanfei.site), [Fotios Petropoulos](https://researchportal.bath.ac.uk/en/persons/fotios-petropoulos), and [Feng Li](http://feng.li/). 2022. "Feature-based Intermittent Demand Forecast Combinations: Bias, Accuracy and Inventory Implications." [*_Working Paper_*](https://arxiv.org/abs/2204.08283). 
