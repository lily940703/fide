#' Function to compute the diversity
#'
#' @param data The dataset. A list including some time series.
#' \code{$ff} is the forecast values based on the 12 methods.
#'
#' @return \code{data} with the diversity of forecasts (\code{$features}).
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
