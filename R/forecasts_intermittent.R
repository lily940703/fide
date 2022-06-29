#' Function to calculate the forecasts of intermittent demand
#'
#' @param data The dataset. A list including some time series.
#' The object \code{x} is the historical value, and \code{xx} is the true value.
#' @param h Forecasting horizon. 12 is the default.
#'
#' @return \code{data} with forecasts
#' @export
calculate_forec_point <- function(data, h=12){
  methods = forec_methods_inter()
  methods_func = lapply(methods, function(method) assign(method, get(method)))
  forec = lapply(methods_func, function(func) func(ts=data$x, h = h))

  # Point forecasts
  ff = t(sapply(forec, function(x) x$point_forec))
  names = unlist(methods)
  rownames(ff) = names
  data$ff = ff

  return(data)
}



#-------------------------- Forecast methods ----------------------------------#
# The pool includes traditional forecasting models, which are Naive,
# seasonal Naive (sNaive), Simple Exponential Smoothing (SES), Moving Averages (MA),
# AutoRegressive Integrated Moving Average (ARIMA), ExponenTial Smoothing (ETS),
# and intermittent demand forecasting methods,
# which are Croston's method (CRO), optimized Croston's method (optCro),
# Syntetos-Boylan Approximation (SBA), Teunter-Syntetos-Babai (TSB),
# Aggregate-Disaggregate Intermittent Demand Approach (ADIDA),
# Intermittent Multiple Aggregation Prediction Algorithm (IMAPA).


#' The methods used to calculate the forecasts of intermittent demand
#'
#' @return A list including the names of forecasting methods.
#' @export
forec_methods_inter <- function() {
  methods_list <- list("CRO_forec_inter")
  methods_list <- append(methods_list, "optCro_forec_inter")
  methods_list <- append(methods_list, "sba_forec_inter")
  methods_list <- append(methods_list, "tsb_forec_inter")
  methods_list <- append(methods_list, "adida_forec_inter")
  methods_list <- append(methods_list, "imapa_forec_inter")
  methods_list <- append(methods_list, "naive_forec_inter")
  methods_list <- append(methods_list, "snaive_forec_inter")
  methods_list <- append(methods_list, "ses_forec_inter")
  methods_list <- append(methods_list, "ma_forec_inter")
  methods_list <- append(methods_list, "arima_forec_inter")
  methods_list <- append(methods_list, "ets_forec_inter")

  methods_list
}



#' Forecast based on Croston??s method
#'
#' @param ts The time series.
#' @param h Forecasting horizon. 12 is the default.
#' @param quantile Logical value. If quantile is TRUE, calculate the quantile forecasts.
#' @param u A vector containing the quantiles to calculate for forecasting.
#'
#' @return A \code{data.frame} with point forecasts and quantile forecasts.
#' @export
CRO_forec_inter <- function(ts, h=12, quantile=F,
                            u = c(0.005,0.025,0.165,0.25,0.5
                                  ,0.75,0.835,0.975,0.995)){
  insample = ts
  cro_forec = tsintermittent::crost(data = insample, h=h, w = c(0.1,0.1)
                                    , type="croston", cost="mar")
  fitted = cro_forec$frc.in
  point_forec = cro_forec$frc.out
  point_forec[point_forec<0] = 0

  if(quantile==T){
    error = tail(insample, length(fitted[!is.na(fitted)])) - fitted[!is.na(fitted)]
    d = density(as.numeric(error), kernel = "epanechnikov", adjust = 1, give.Rkern = F)

    if ( !requireNamespace("BMS")) {
      library(BMS)
    }
    quant = quantile(d, probs = u)
    quant_forec = matrix(nrow = h, ncol = length(u))
    colnames(quant_forec) = u
    for (i_u in 1:length(u)) {
      quant_forec[,i_u] = point_forec + quant[i_u]
    }
    quant_forec[quant_forec<0] = 0

    point_forec = as.numeric(point_forec)
    out = cbind(point_forec, quant_forec)
    out = data.frame(out)
  } else{
    out = data.frame(point_forec = point_forec)
  }

  return(out)
}

#' Forecast based on optimized Croston??s method
#'
#' @param ts The time series.
#' @param h Forecasting horizon. 12 is the default.
#' @param quantile Logical value. If quantile is TRUE, calculate the quantile forecasts.
#' @param u A vector containing the quantiles to calculate for forecasting.
#'
#' @return A \code{data.frame} with point forecasts and quantile forecasts.
#' @export
optCro_forec_inter <- function(ts, h=12, quantile=F,
                               u = c(0.005,0.025,0.165,0.25,0.5,0.75,0.835,0.975,0.995)){
  insample = ts
  cro_forec = tsintermittent::crost(data = insample, h=h, type="croston", cost="mar")
  fitted = cro_forec$frc.in
  point_forec = cro_forec$frc.out
  point_forec[point_forec<0] = 0

  if(quantile==T){
    error = tail(insample, length(fitted[!is.na(fitted)])) - fitted[!is.na(fitted)]
    d = density(as.numeric(error), kernel = "epanechnikov", adjust = 1, give.Rkern = F)

    if ( !requireNamespace("BMS")) {
      library(BMS)
    }
    quant = quantile(d, probs = u)
    quant_forec = matrix(nrow = h, ncol = length(u))
    colnames(quant_forec) = u
    for (i_u in 1:length(u)) {
      quant_forec[,i_u] = point_forec + quant[i_u]
    }
    quant_forec[quant_forec<0] = 0

    point_forec = as.numeric(point_forec)
    out = cbind(point_forec, quant_forec)
    out = data.frame(out)
  } else{
    out = data.frame(point_forec = point_forec)
  }

  return(out)
}

#' Forecast based on SBA method
#'
#' @param ts The time series.
#' @param h Forecasting horizon. 12 is the default.
#' @param quantile Logical value. If quantile is TRUE, calculate the quantile forecasts.
#' @param u A vector containing the quantiles to calculate for forecasting.
#'
#' @return A \code{data.frame} with point forecasts and quantile forecasts.
#' @export
sba_forec_inter <- function(ts, h=12, quantile=F,
                            u = c(0.005,0.025,0.165,0.25,0.5
                                  ,0.75,0.835,0.975,0.995)){
  insample = ts
  cro_forec = tsintermittent::crost(data = insample, h=h, type="sba", cost="mar")
  fitted = cro_forec$frc.in
  point_forec = cro_forec$frc.out
  point_forec[point_forec<0] = 0

  if(quantile==T){
    error = tail(insample, length(fitted[!is.na(fitted)])) - fitted[!is.na(fitted)]
    d = density(as.numeric(error), kernel = "epanechnikov", adjust = 1, give.Rkern = F)

    if ( !requireNamespace("BMS")) {
      library(BMS)
    }
    quant = quantile(d, probs = u)
    quant_forec = matrix(nrow = h, ncol = length(u))
    colnames(quant_forec) = u
    for (i_u in 1:length(u)) {
      quant_forec[,i_u] = point_forec + quant[i_u]
    }
    quant_forec[quant_forec<0] = 0

    point_forec = as.numeric(point_forec)
    out = cbind(point_forec, quant_forec)
    out = data.frame(out)
  } else{
    out = data.frame(point_forec = point_forec)
  }

  return(out)
}

#' Forecast based on TSB method
#'
#' @param ts The time series.
#' @param h Forecasting horizon. 12 is the default.
#' @param quantile Logical value. If quantile is TRUE, calculate the quantile forecasts.
#' @param u A vector containing the quantiles to calculate for forecasting.
#'
#' @return A \code{data.frame} with point forecasts and quantile forecasts.
#' @export
tsb_forec_inter <- function(ts, h=12, quantile=F,
                            u = c(0.005,0.025,0.165,0.25,0.5
                                  ,0.75,0.835,0.975,0.995)){
  insample = ts
  cro_forec = tsintermittent::tsb(insample,h=h, cost="mar", init.opt=TRUE)
  fitted = cro_forec$frc.in
  point_forec = cro_forec$frc.out
  point_forec[point_forec<0] = 0

  if(quantile==T){
    error = tail(insample, length(fitted[!is.na(fitted)])) - fitted[!is.na(fitted)]
    d = density(as.numeric(error), kernel = "epanechnikov", adjust = 1, give.Rkern = F)

    if ( !requireNamespace("BMS")) {
      library(BMS)
    }
    quant = quantile(d, probs = u)
    quant_forec = matrix(nrow = h, ncol = length(u))
    colnames(quant_forec) = u
    for (i_u in 1:length(u)) {
      quant_forec[,i_u] = point_forec + quant[i_u]
    }
    quant_forec[quant_forec<0] = 0

    point_forec = as.numeric(point_forec)
    out = cbind(point_forec, quant_forec)
    out = data.frame(out)
  } else{
    out = data.frame(point_forec = point_forec)
  }

  return(out)
}

#' Forecast based on ADIDA method
#'
#' @param ts The time series.
#' @param h Forecasting horizon. 12 is the default.
#' @param quantile Logical value. If quantile is TRUE, calculate the quantile forecasts.
#' @param u A vector containing the quantiles to calculate for forecasting.
#'
#' @return A \code{data.frame} with point forecasts and quantile forecasts.
#' @export
adida_forec_inter <- function(ts, h=12, quantile=F,
                              u = c(0.005,0.025,0.165,0.25,0.5
                                    ,0.75,0.835,0.975,0.995)){
  if(sum(u == c(0.005,0.025,0.165,0.25,0.5,0.75,0.835,0.975,0.995))==length(u)){
    level = c(50,67,95,99)
  }else{
    cat("Quantiles are not defualt!")
  }
  insample = ts
  al <- round(mean(intervals(insample)),0)
  if(al>12) al = 12
  AS <- as.numeric(na.omit(as.numeric(zoo::rollapply(tail(insample, (length(insample) %/% al)*al), al, FUN=sum, by = al))))
  ses = HoltWinters(AS, beta = F, gamma = F)
  fore = forecast(ses, h = h, level = level)

  point_forec = fore$mean / al
  point_forec[point_forec<0] = 0

  if(quantile==T){
    quant_forec = matrix(nrow = h, ncol = length(u))
    colnames(quant_forec) = u
    quant_forec = data.frame(quant_forec)
    quant_forec$X0.005 = fore$lower[,"99%"]/ al
    quant_forec$X0.025 = fore$lower[,"95%"]/ al
    quant_forec$X0.165 = fore$lower[,"67%"]/ al
    quant_forec$X0.25 = fore$lower[,"50%"]/ al
    quant_forec$X0.5 = point_forec
    quant_forec$X0.75 = fore$upper[,"50%"]/ al
    quant_forec$X0.835 = fore$upper[,"67%"]/ al
    quant_forec$X0.975 = fore$upper[,"95%"]/ al
    quant_forec$X0.995 = fore$upper[,"99%"]/ al
    quant_forec[quant_forec<0] = 0
    point_forec = as.numeric(point_forec)
    out = cbind(point_forec, quant_forec)
  } else{
    out = data.frame(point_forec = point_forec)
  }

  return(out)
}

#' Forecast based on IMAPA method
#'
#' @param ts The time series.
#' @param h Forecasting horizon. 12 is the default.
#' @param quantile Logical value. If quantile is TRUE, calculate the quantile forecasts.
#' @param u A vector containing the quantiles to calculate for forecasting.
#'
#' @return A \code{data.frame} with point forecasts and quantile forecasts.
#' @export
imapa_forec_inter <- function(ts, h=12, quantile=F,
                              u = c(0.005,0.025,0.165,0.25,0.5
                                    ,0.75,0.835,0.975,0.995)){
  insample = ts
  cro_forec = tsintermittent::imapa(data = insample, h=h, maximumAL = NULL )
  fitted = cro_forec$frc.in
  if(!is.vector(fitted)) fitted = fitted["AL1",]
  point_forec = cro_forec$frc.out
  if(!is.vector(point_forec)) point_forec = point_forec["AL1",]
  point_forec[point_forec<0] = 0

  if(quantile==T){
    error = tail(insample, length(fitted[!is.na(fitted)])) - fitted[!is.na(fitted)]
    d = density(as.numeric(error), kernel = "epanechnikov", adjust = 1, give.Rkern = F)

    if ( !requireNamespace("BMS")) {
      library(BMS)
    }
    quant = quantile(d, probs = u)
    quant_forec = matrix(nrow = h, ncol = length(u))
    colnames(quant_forec) = u
    for (i_u in 1:length(u)) {
      quant_forec[,i_u] = point_forec + quant[i_u]
    }
    quant_forec[quant_forec<0] = 0

    point_forec = as.numeric(point_forec)
    out = cbind(point_forec, quant_forec)
    out = data.frame(out)
  } else{
    out = data.frame(point_forec = point_forec)
  }

  return(out)
}

#' Forecast based on Naive model
#'
#' @param ts The time series.
#' @param h Forecasting horizon. 12 is the default.
#' @param quantile Logical value. If quantile is TRUE, calculate the quantile forecasts.
#' @param u A vector containing the quantiles to calculate for forecasting.
#'
#' @return A \code{data.frame} with point forecasts and quantile forecasts.
#' @export
naive_forec_inter <- function(ts, h = 12, quantile=F,
                              u = c(0.005,0.025,0.165,0.25,0.5
                                    ,0.75,0.835,0.975,0.995)) {
  if(sum(u == c(0.005,0.025,0.165,0.25,0.5,0.75,0.835,0.975,0.995))==length(u)){
    level = c(0,50,67,95,99)
  }else{
    cat("Quantiles are not defualt!")
  }
  model = forecast::naive(ts, h=h, level = level)
  fore = forecast::forecast(model, h = h )
  point_forec = fore$mean
  point_forec[point_forec<0] = 0

  if(quantile==T){
    quant_forec = matrix(nrow = h, ncol = length(u))
    colnames(quant_forec) = u
    quant_forec = data.frame(quant_forec)
    quant_forec$X0.005 = fore$lower[,"99%"]
    quant_forec$X0.025 = fore$lower[,"95%"]
    quant_forec$X0.165 = fore$lower[,"67%"]
    quant_forec$X0.25 = fore$lower[,"50%"]
    quant_forec$X0.5 = fore$lower[,"0%"]
    quant_forec$X0.75 = fore$upper[,"50%"]
    quant_forec$X0.835 = fore$upper[,"67%"]
    quant_forec$X0.975 = fore$upper[,"95%"]
    quant_forec$X0.995 = fore$upper[,"99%"]
    quant_forec[quant_forec<0] = 0
    point_forec = as.numeric(point_forec)
    out = cbind(point_forec, quant_forec)
  } else{
    out = data.frame(point_forec = point_forec)
  }

  return(out)
}

#' Forecast based on sNaive model
#'
#' @param ts The time series.
#' @param h Forecasting horizon. 12 is the default.
#' @param quantile Logical value. If quantile is TRUE, calculate the quantile forecasts.
#' @param u A vector containing the quantiles to calculate for forecasting.
#'
#' @return A \code{data.frame} with point forecasts and quantile forecasts.
#' @export
snaive_forec_inter <- function(ts, h = 12, quantile=F,
                               u = c(0.005,0.025,0.165,0.25,0.5
                                     ,0.75,0.835,0.975,0.995)) {
  if(sum(u == c(0.005,0.025,0.165,0.25,0.5,0.75,0.835,0.975,0.995))==length(u)){
    level = c(0,50,67,95,99)
  }else{
    cat("Quantiles are not defualt!")
  }
  model = forecast::snaive(ts, h=h, level = level)
  fore = forecast::forecast(model, h = h )
  point_forec = fore$mean
  point_forec[point_forec<0] = 0

  if(quantile==T){
    quant_forec = matrix(nrow = h, ncol = length(u))
    colnames(quant_forec) = u
    quant_forec = data.frame(quant_forec)
    quant_forec$X0.005 = fore$lower[,"99%"]
    quant_forec$X0.025 = fore$lower[,"95%"]
    quant_forec$X0.165 = fore$lower[,"67%"]
    quant_forec$X0.25 = fore$lower[,"50%"]
    quant_forec$X0.5 = fore$lower[,"0%"]
    quant_forec$X0.75 = fore$upper[,"50%"]
    quant_forec$X0.835 = fore$upper[,"67%"]
    quant_forec$X0.975 = fore$upper[,"95%"]
    quant_forec$X0.995 = fore$upper[,"99%"]
    quant_forec[quant_forec<0] = 0
    point_forec = as.numeric(point_forec)
    out = cbind(point_forec, quant_forec)
  } else{
    out = data.frame(point_forec = point_forec)
  }

  return(out)
}

#' Forecast based on SES method
#'
#' @param ts The time series.
#' @param h Forecasting horizon. 12 is the default.
#' @param quantile Logical value. If quantile is TRUE, calculate the quantile forecasts.
#' @param u A vector containing the quantiles to calculate for forecasting.
#'
#' @return A \code{data.frame} with point forecasts and quantile forecasts.
#' @export
ses_forec_inter <- function(ts, h = 12, quantile=F,
                            u = c(0.005,0.025,0.165,0.25,0.5
                                  ,0.75,0.835,0.975,0.995)) {
  if(sum(u == c(0.005,0.025,0.165,0.25,0.5,0.75,0.835,0.975,0.995))==length(u)){
    level = c(50,67,95,99)
  }else{
    cat("Quantiles are not defualt!")
  }
  ses = HoltWinters(ts, beta = F, gamma = F)
  fore = forecast::forecast(ses, h = h, level = level)
  point_forec = fore$mean
  point_forec[point_forec<0] = 0

  if(quantile==T){
    quant_forec = matrix(nrow = h, ncol = length(u))
    colnames(quant_forec) = u
    quant_forec = data.frame(quant_forec)
    quant_forec$X0.005 = fore$lower[,"99%"]
    quant_forec$X0.025 = fore$lower[,"95%"]
    quant_forec$X0.165 = fore$lower[,"67%"]
    quant_forec$X0.25 = fore$lower[,"50%"]
    quant_forec$X0.5 = point_forec
    quant_forec$X0.75 = fore$upper[,"50%"]
    quant_forec$X0.835 = fore$upper[,"67%"]
    quant_forec$X0.975 = fore$upper[,"95%"]
    quant_forec$X0.995 = fore$upper[,"99%"]
    quant_forec[quant_forec<0] = 0
    point_forec = as.numeric(point_forec)
    out = cbind(point_forec, quant_forec)
  } else{
    out = data.frame(point_forec = point_forec)
  }

  return(out)
}


#' Forecast based on MA method
#'
#' @param ts The time series.
#' @param h Forecasting horizon. 12 is the default.
#' @param quantile Logical value. If quantile is TRUE, calculate the quantile forecasts.
#' @param k_interval The interval for finding the optimal \{k\}.
#' \{k\} is the number of the values to average at the end of the time series.
#' @param u A vector containing the quantiles to calculate for forecasting.
#'
#' @return A \code{data.frame} with point forecasts and quantile forecasts.
#' @export
ma_forec_inter <- function(ts, h = 12, k_interval = c(2:12), quantile=F,
                           u = c(0.005,0.025,0.165,0.25,0.5
                                 ,0.75,0.835,0.975,0.995)) {
  # Find the optimal k
  insample = ts
  mse <- c()
  for (k in k_interval){
    y <- rep(NA, k)
    for (i in (k+1):length(ts)){
      y <- c(y, mean(ts[(i-k):(i-1)]))
    }
    mse <- c(mse, mean((y-ts)^2, na.rm = T))
  }
  k <- which.min(mse)+k_interval[1]-1
  point_forec = rep(mean(as.numeric(tail(ts, k))), h)
  point_forec[point_forec<0] = 0

  if(quantile==T){
    w = rep(1, k)/k
    fitted = filter(insample, w, sides = 1)
    error = tail(insample, length(fitted[!is.na(fitted)])) - fitted[!is.na(fitted)]
    d = density(as.numeric(error), kernel = "epanechnikov", adjust = 1, give.Rkern = F)

    if ( !requireNamespace("BMS")) {
      library(BMS)
    }
    quant = quantile(d, probs = u)
    quant_forec = matrix(nrow = h, ncol = length(u))
    colnames(quant_forec) = u
    for (i_u in 1:length(u)) {
      quant_forec[,i_u] = point_forec + quant[i_u]
    }
    quant_forec[quant_forec<0] = 0

    point_forec = as.numeric(point_forec)
    out = cbind(point_forec, quant_forec)
    out = data.frame(out)
  } else{
    out = data.frame(point_forec = point_forec)
  }

  return(out)
}

#' Forecast based on ARIMA model
#'
#' @param ts The time series.
#' @param h Forecasting horizon. 12 is the default.
#' @param quantile Logical value. If quantile is TRUE, calculate the quantile forecasts.
#' @param u A vector containing the quantiles to calculate for forecasting.
#'
#' @return A \code{data.frame} with point forecasts and quantile forecasts.
#' @export
arima_forec_inter <- function(ts, h = 12, quantile=F,
                              u = c(0.005,0.025,0.165,0.25,0.5
                                    ,0.75,0.835,0.975,0.995)) {
  if(sum(u == c(0.005,0.025,0.165,0.25,0.5,0.75,0.835,0.975,0.995))==length(u)){
    level = c(0,50,67,95,99)
  }else{
    cat("Quantiles are not defualt!")
  }
  model = forecast::auto.arima(ts)
  fore = forecast::forecast(model, h = h, level=level )
  point_forec = fore$mean
  point_forec[point_forec<0] = 0

  if(quantile==T){
    quant_forec = matrix(nrow = h, ncol = length(u))
    colnames(quant_forec) = u
    quant_forec = data.frame(quant_forec)
    quant_forec$X0.005 = fore$lower[,"99%"]
    quant_forec$X0.025 = fore$lower[,"95%"]
    quant_forec$X0.165 = fore$lower[,"67%"]
    quant_forec$X0.25 = fore$lower[,"50%"]
    quant_forec$X0.5 = fore$lower[,"0%"]
    quant_forec$X0.75 = fore$upper[,"50%"]
    quant_forec$X0.835 = fore$upper[,"67%"]
    quant_forec$X0.975 = fore$upper[,"95%"]
    quant_forec$X0.995 = fore$upper[,"99%"]
    quant_forec[quant_forec<0] = 0
    point_forec = as.numeric(point_forec)
    out = cbind(point_forec, quant_forec)
  } else{
    out = data.frame(point_forec = point_forec)
  }

  return(out)
}

#' Forecast based on ETS model
#'
#' @param ts The time series.
#' @param h Forecasting horizon. 12 is the default.
#' @param quantile Logical value. If quantile is TRUE, calculate the quantile forecasts.
#' @param u A vector containing the quantiles to calculate for forecasting.
#'
#' @return A \code{data.frame} with point forecasts and quantile forecasts.
#' @export
ets_forec_inter <- function(ts, h = 12, quantile=F,
                            u = c(0.005,0.025,0.165,0.25,0.5
                                  ,0.75,0.835,0.975,0.995)) {

  if(sum(u == c(0.005,0.025,0.165,0.25,0.5,0.75,0.835,0.975,0.995))==length(u)){
    level = c(0,50,67,95,99)
  }else{
    cat("Quantiles are not defualt!")
  }
  model = forecast::ets(ts)
  fore = forecast::forecast(model, h = h, level=level)
  point_forec = fore$mean
  point_forec[point_forec<0] = 0

  if(quantile==T){
    quant_forec = matrix(nrow = h, ncol = length(u))
    colnames(quant_forec) = u
    quant_forec = data.frame(quant_forec)
    quant_forec$X0.005 = fore$lower[,"99%"]
    quant_forec$X0.025 = fore$lower[,"95%"]
    quant_forec$X0.165 = fore$lower[,"67%"]
    quant_forec$X0.25 = fore$lower[,"50%"]
    quant_forec$X0.5 = fore$lower[,"0%"]
    quant_forec$X0.75 = fore$upper[,"50%"]
    quant_forec$X0.835 = fore$upper[,"67%"]
    quant_forec$X0.975 = fore$upper[,"95%"]
    quant_forec$X0.995 = fore$upper[,"99%"]
    quant_forec[quant_forec<0] = 0
    point_forec = as.numeric(point_forec)
    out = cbind(point_forec, quant_forec)
  } else{
    out = data.frame(point_forec = point_forec)
  }

  return(out)
}
