#' Function to compute the nine features for intermittent demand
#'
#' @param ts Time series.
#' @param scale Logical value. If scale is TRUE, the ts is standardized.
#' @param features Names of selected features for intermittent demand.
#'
#' @return \code{features_out}
#' A matrix with one row and nine columns containing the nine features.
#' @export
compute_ifeatures <- function(ts, scale = TRUE,
                              features = c("IDI", "CV2", "Entropy",
                                           "Percent.zero", "Percent.beyond.sigma",
                                           "Linear.chunk.var", "Change.quantile",
                                           "Ratio.last.chunk", "Percent.zero.end")){
  features_out = matrix(nrow = 1, ncol = length(features))
  colnames(features_out) = features
  f1 = compute_IDI(ts)
  f2 = compute_CV2(ts)
  rle <- rle(as.vector(ts))
  f9 = if(rle$values[length(rle$values)] != 0) 0 else rle$lengths[length(rle$lengths)]/length(ts)
  if(scale){
    ts = scale(ts)
  }
  f4 = percent_zero(ts, t=0)
  f7 = change_quantiles(ts, ql=0.4, qh=1, isabs=T, f_agg="mean")
  f3 = approximate_entropy(ts, m=2, r=0.5)
  f5 = percent_beyond_sigma(ts, r=1)
  f6 = linear_chunk_var(ts, f_agg="var", chunk_len=12)
  f8 = ratio_last_chunk(ts, num_segments = 4)
  features_out[1,]=c(f1,f2,f3,f4,f5,f6,f7,f8,f9)
  return(features_out)
}

#' Computing the averaged Inter-Demand Interval(IDI)
#'
#' @param x Time series.
#'
#' @return IDI.
#' @export
compute_IDI<- function(x){
  IDI <- mean(intervals(x))
  return(IDI)
}

#' Computing the coefficient of variation squared (CV2) of non-zero demand
#'
#' @param x Time series.
#'
#' @return CV2.
#' @export
compute_CV2<- function(x){
  D <- demand(x)
  CV2 <- (sd(D)/mean(D))^2
  return(CV2)
}

# These functions support \code{compute_IDI} and \code{compute_CV2}
intervals <- function(x){
  y<-c()
  k<-1
  counter<-0
  for (tmp in (1:length(x))){

    if(x[tmp]==0){
      counter<-counter+1
    }else{
      k<-k+1
      y[k]<-counter
      counter<-1
    }
  }
  y<-y[y>0]
  y[is.na(y)]<-1
  y
}
demand <- function(x){
  y<-x[x!=0]
  y
}

#' Computing the percentage of observations that are zero values
#'
#' @param x Time series.
#'
#' @return the value of the feature \code{Percent.zero}
#' @export
percent_zero <- function(x, t=0){
  num = sum(x <= t)
  return(num/length(x))
}

#' Compute the feature \code{Change.quantile} of the time series
#'
#' The average, absolute value of consecutive changes of the series
#' inside a fixed range given by the quantiles.
#'
#' @param x The time series.
#' @param ql The lower quantile of the range.
#' @param qh The higher quantile of the range.
#' @param isabs Logical value. Should the absolute differences be taken?
#' @param f_agg The aggregator function (e.g. mean, var, std, median)
#'
#' @return the value of the feature \code{Change.quantile}
#' @export

change_quantiles <- function(x, ql, qh, isabs, f_agg){
  if (ql >= qh){
    return(0)
  }
  div = diff(x)
  if (isabs) {
    div = abs(div)
  }
  bin_cat = quantile(x, probs = c(ql, qh))
  bin_cat_0 = x>=bin_cat[1] & x<=bin_cat[2]
  bin_cat_0_roll_1 = c(bin_cat_0[length(bin_cat_0)], bin_cat_0[-length(bin_cat_0)])
  ind = c(bin_cat_0 & bin_cat_0_roll_1)[-1]
  aggregator = get(f_agg)
  return(aggregator(div[ind]))
}

#' Compute the entropy of the time series
#'
#' The approximate entropy algorithm in package \code{TSEntropies} is used.
#'
#' @param x The time series.
#' @param m Length of compared run of data.
#' @param r Filtering level, must be positive.
#'
#' @return the value of the feature \code{Entropy}
#' @export
approximate_entropy<- function(x, m, r){
  entropy = TSEntropies::ApEn(x, dim = m, r = r*sd(x))
  return (entropy)
}

#' Compute the feature \code{Percent.beyond.sigma} of the time series
#'
#' The percentage of observations that are more than \{\sigma\} away from
#' the mean of the time series (\{\sigma\} is the standard deviation of the time series).
#'
#' @param x The time series.
#' @param r The coefficient of \{\sigma\}, default to 1.
#'
#' @return the value of the feature \code{Percent.beyond.sigma}
#' @export
percent_beyond_sigma<- function(x, r=1){
  num = sum(abs(x - mean(x)) > r * sd(x))
  return(num/length(x))
}


#' Compute the feature \code{Linear.chunk.var} of the time series
#'
#' The coefficient of linear least-squares regression
#' for variances of component chunks in the time series.
#'
#' @param x The time series.
#' @param f_agg The aggregator function (e.g. mean, var, std, median)
#' @param chunk_len The length of the component chunks in the time series.
#'
#' @return the value of the feature \code{Linear.chunk.var}
#' @export
linear_chunk_var <- function(x, f_agg, chunk_len=10){
  num_chunk = ceiling(length(x)/chunk_len)
  aggregator = get(f_agg)
  chunk = c()
  for (i in 1:(num_chunk-1)) {
    chunk = c(chunk, aggregator(x[((i-1)*chunk_len+1):(i*chunk_len)]))
  }
  chunk_last = aggregator(x[((num_chunk-1)*chunk_len+1):length(x)])
  if(is.na(chunk_last)){
    chunk0 = chunk
  }else{
    chunk0 = c(chunk, chunk_last)
  }
  range = c(1:length(chunk0))
  res = lm(range~chunk0)
  coef = res$coefficients[2]
  return(as.numeric(coef))
}

#' Compute the feature \code{Ratio.last.chunk} of the time series
#'
#' The ratio of the sum of squares of the last chunk to the whole series.
#'
#' @param x The time series.
#' @param num_segments The number of chunks in the time series.
#'
#' @return the value of the feature \code{Ratio.last.chunk}
#' @export
ratio_last_chunk<- function(x, num_segments){
  full_series_energy = sum(x^2)
  tail_focus = ceiling(length(x)/num_segments)
  focus_series_energy = (x[(length(x)-tail_focus+1):length(x)])^2
  return(sum(focus_series_energy/full_series_energy))
}
