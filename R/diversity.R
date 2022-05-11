#' Function to compute the diversity
#'
<<<<<<< HEAD
#' @param data The dataset. A list including some time series, each of which has
#' a \code{ff} object with the forecasting values based on the 12 methods
#'
#' @return \code{data} with the diversity of forecasts (shown in \code{features}).
=======
#' @param data The dataset. A list including some time series. For each time series,
#'   \describe{
#'   \item{ff}{The forecasting values based on the 12 methods.}
#'   }
#'
#' @return \code{data} with the diversity of forecasts (\item{features}).
>>>>>>> aed8f17f927cacdcf3670f36a7c61f1d9a850128
compute_diversity <- function(data){

  h = data[[1]]$h
  n = dim(data[[1]]$ff)[1]

  for (i in 1:length(data)) {
    ff = data[[i]]$ff
    sd_x = (mean(abs(data[[i]]$x)))^2
    distance = c()
    distance_name =c()
    for (j in 1:n) {
      for (f in 1:n) {
        if(f>j){
          distance = c(distance, (sum((ff[f,]-ff[j,])^2)/h)/sd_x)
          distance_name = c(distance_name, paste("Div",j,f, sep="_"))
        }
      }
    }
    names(distance) = distance_name
    data[[i]]$features = distance
  }
  return(data)
}
