#-------------------- Functions to calculate errors --------------------------#
# For traditional errors, we calculate two absolute errors
# scaled Mean Absolute Error (sMAE) and  Mean Absolute Scaled Error (MASE),
# a squared error scaled Mean Squared Error (sMSE),
# and a cumulated error scaled Mean Absolute Periods In Stock (sMAPIS).

# For bias errors, we compute two relative errors
# scaled Mean Error (sME) and scaled Mean Periods In Stock (sMPIS),
# and a shortage-based error Number Of Shortages (NOS).

#' Calculate the sMAPIS error
#'
#' @param dataset The dataset. A list including some time series.
#' In each time series, \code{x} is the historical value, \code{xx} is the true value,
#' and \code{ff} is the forecasting values based on the 12 methods.
#'
#' @return \code{dataset} with sMAPIS errors
#' @export
calc_errors_smapis <- function (dataset)
{
  for (i in 1:length(dataset)) {
    tryCatch({
      lentry <- dataset[[i]]
      insample <- lentry$x
      ff <- lentry$ff
      insample <- as.numeric(insample)
      outsample <- as.numeric(lentry$xx)
      repoutsample <- matrix(rep(outsample, each = nrow(ff)),
                             nrow = nrow(ff))
      scale <- mean(insample)

      h = ncol(ff)
      pis = (ff - repoutsample)[,1]
      for (j in c(2:h)) {
        pis0 = rowSums((ff - repoutsample)[,1:j])
        pis = pis + pis0
      }
      sapis = abs(pis)/scale
      lentry$errors <- sapis

      dataset[[i]] <- lentry
    }, error = function(e) {
      print(paste("Error when processing sMAPIS in series: ",
                  i))
      print(e)
      e
    })
  }
  dataset
}


#' Calculate the sMAE error
#'
#' @param dataset The dataset. A list including some time series.
#' In each time series, \code{x} is the historical value, \code{xx} is the true value,
#' and \code{ff} is the forecasting values based on the 12 methods.
#'
#' @return \code{dataset} with sMAE errors
#' @export
calc_errors_smae <- function (dataset)
{
  for (i in 1:length(dataset)) {
    tryCatch({
      lentry <- dataset[[i]]
      insample <- lentry$x
      ff <- lentry$ff
      insample <- as.numeric(insample)
      outsample <- as.numeric(lentry$xx)
      repoutsample <- matrix(rep(outsample, each = nrow(ff)),
                             nrow = nrow(ff))
      scale <- mean(insample)

      mae_err <- abs(ff - repoutsample)/scale
      lentry$errors <- rowMeans(mae_err)
      dataset[[i]] <- lentry
    }, error = function(e) {
      print(paste("Error when processing sMAE in series: ",
                  i))
      print(e)
      e
    })
  }
  dataset
}

#' Calculate the sME error
#'
#' @param dataset The dataset. A list including some time series.
#' In each time series, \code{x} is the historical value, \code{xx} is the true value,
#' and \code{ff} is the forecasting values based on the 12 methods.
#'
#' @return \code{dataset} with sME errors
#' @export
calc_errors_sme <- function (dataset)
{
  for (i in 1:length(dataset)) {
    tryCatch({
      lentry <- dataset[[i]]
      insample <- lentry$x
      ff <- lentry$ff
      insample <- as.numeric(insample)
      outsample <- as.numeric(lentry$xx)
      repoutsample <- matrix(rep(outsample, each = nrow(ff)),
                             nrow = nrow(ff))
      scale <- mean(insample)

      sme_err <- (repoutsample - ff)/scale
      lentry$errors <- abs(rowMeans(sme_err))
      dataset[[i]] <- lentry
    }, error = function(e) {
      print(paste("Error when processing sME in series: ",
                  i))
      print(e)
      e
    })
  }
  dataset
}

#' Calculate the sMSE error
#'
#' @param dataset The dataset. A list including some time series.
#' In each time series, \code{x} is the historical value, \code{xx} is the true value,
#' and \code{ff} is the forecasting values based on the 12 methods.
#'
#' @return \code{dataset} with sMSE errors
#' @export
calc_errors_smse <- function (dataset)
{
  for (i in 1:length(dataset)) {
    tryCatch({
      lentry <- dataset[[i]]
      insample <- lentry$x
      ff <- lentry$ff
      insample <- as.numeric(insample)
      outsample <- as.numeric(lentry$xx)
      scale <- mean(insample)
      repoutsample <- matrix(rep(outsample, each = nrow(ff)),
                             nrow = nrow(ff))
      smse_err <- ((ff - repoutsample)^2) / (scale^2)
      lentry$errors <- rowMeans(smse_err)
      dataset[[i]] <- lentry
    }, error = function(e) {
      print(paste("Error when processing sMSE in series: ",
                  i))
      print(e)
      e
    })
  }
  dataset
}

#' Calculate the MASE error
#'
#' @param dataset The dataset. A list including some time series.
#' In each time series, \code{x} is the historical value, \code{xx} is the true value,
#' and \code{ff} is the forecasting values based on the 12 methods.
#'
#' @return \code{dataset} with MASE errors
#' @export
calc_errors_mase <- function (dataset)
{
  total_snaive_errors <- c(0, 0)
  for (i in 1:length(dataset)) {
    tryCatch({
      lentry <- dataset[[i]]
      insample <- lentry$x
      ff <- lentry$ff
      # ff <- rbind(ff, snaive_forec(insample, lentry$h))
      frq <- frq <- stats::frequency(insample)
      insample <- as.numeric(insample)
      outsample <- as.numeric(lentry$xx)
      masep <- mean(abs(utils::head(insample, -frq) - utils::tail(insample, -frq)))
      repoutsample <- matrix(rep(outsample, each = nrow(ff)),
                             nrow = nrow(ff))
      mase_err <- abs(ff - repoutsample)/masep
      lentry$mase_err <- mase_err
      dataset[[i]] <- lentry
    }, error = function(e) {
      print(paste("Error when processing MASE in series: ",
                  i))
      print(e)
      e
    })
  }

  for (i in 1:length(dataset)) {
    lentry <- dataset[[i]]
    dataset[[i]]$errors <- rowMeans(lentry$mase_err)
  }
  dataset
}

#' Calculate a compositive error considering MASE and NOS errors
#'
#' @param dataset The dataset. A list including some time series.
#' In each time series, \code{x} is the historical value, \code{xx} is the true value,
#' and \code{ff} is the forecasting values based on the 12 methods.
#' @param a The penalty factor for NOS.
#'
#' @return \code{dataset} with MASE errors
#' @export
calc_errors_mase_nos <- function (dataset, a=0)
{
  for (i in 1:length(dataset)) {
    tryCatch({
      lentry <- dataset[[i]]
      insample <- lentry$x
      ff <- lentry$ff
      frq <- frq <- stats::frequency(insample)
      insample <- as.numeric(insample)
      outsample <- as.numeric(lentry$xx)
      masep <- mean(abs(utils::head(insample, -frq) - utils::tail(insample, -frq)))
      repoutsample <- matrix(rep(outsample, each = nrow(ff)),
                             nrow = nrow(ff))
      mase_err <- abs(ff - repoutsample)/masep
      mase_err <- rowMeans(mase_err)

      h = ncol(ff)
      cfe = matrix(nrow = nrow(ff), ncol = h)
      cfe[,1] = (repoutsample-ff)[,1]
      for (j in c(2:h)) {
        cfe[,j] = rowSums((repoutsample-ff)[,1:j])
      }
      nos = rowSums(cfe>0)

      lentry$errors <- mase_err + a*nos
      dataset[[i]] <- lentry
    }, error = function(e) {
      print(paste("Error when processing sME in series: ",
                  i))
      print(e)
      e
    })
  }
  dataset
}

#' Calculate the forecasting errors of the proposed method
#'
#' @param dataset The dataset. A list including some time series.
#' In each time series, \code{x} is the historical value, \code{xx} is the true value,
#' \code{ff} is the forecasting values based on the 12 methods,
#' and \code{y_hat} is the combined forecast of the proposed method.
#' @param error A vector including the errors to output.
#'
#' @return \code{dataset} with forecasting errors.
#' @export
summary_performance_error_intermittent <- function(dataset
                                                   , error = c("sME"
                                                               , "sMAE"
                                                               , "sMSE"
                                                               , "MASE"
                                                               , "NOS"
                                                               , "sMPIS"
                                                               , "sMAPIS")){
  for (i in 1:length(dataset)) {
    dataset[[i]]$ff <- rbind(dataset[[i]]$ff, dataset[[i]]$y_hat)
  }
  total_errors_sme = 0
  total_errors_smae = 0

  total_errors_smse = 0

  total_errors_mase = 0
  total_errors_nos = 0
  total_errors_smapis = 0
  total_errors_smpis = 0

  for (i in 1:length(dataset)) {
    tryCatch({
      lentry <- dataset[[i]]
      insample <- lentry$x
      ff <- lentry$ff
      frq <- frq <- stats::frequency(insample)
      insample <- as.numeric(insample)
      outsample <- as.numeric(lentry$xx)
      scale <- mean(insample)
      repoutsample <- matrix(rep(outsample, each = nrow(ff)),
                             nrow = nrow(ff))
      # sME
      if("sME" %in% error){
        sme_err <- (repoutsample - ff)/scale
        lentry$sme_yhat <- mean(sme_err[nrow(sme_err), ])
        lentry$sme_ff <- rowMeans(sme_err[-nrow(sme_err), ])
        total_errors_sme = total_errors_sme + lentry$sme_yhat
      }
      #sMAE
      if("sMAE" %in% error){
        mae_err <- abs(ff - repoutsample)
        lentry$smae_yhat <- mean(mae_err[nrow(mae_err), ])/scale
        lentry$smae_ff <- rowMeans(mae_err[-nrow(mae_err), ])/scale
        total_errors_smae = total_errors_smae + lentry$smae_yhat
      }
      #sMSE
      if("sMSE" %in% error){
        smse_err <- ((ff - repoutsample)^2) / (scale^2)
        lentry$smse_yhat <- mean(smse_err[nrow(smse_err), ])
        lentry$smse_ff <- rowMeans(smse_err[-nrow(smse_err), ])
        total_errors_smse = total_errors_smse + lentry$smse_yhat
      }
      #MASE
      if("MASE" %in% error){
        masep <- mean(abs(utils::head(insample, -frq) - utils::tail(insample, -frq)))
        mase_err <- abs(ff - repoutsample)/masep
        lentry$mase_yhat <- mean(mase_err[nrow(mase_err), ])
        lentry$mase_ff <- rowMeans(mase_err[-nrow(mase_err), ])
        total_errors_mase = total_errors_mase + lentry$mase_yhat
      }
      # NOS
      if("NOS" %in% error){
        h = ncol(ff)
        cfe = matrix(nrow = nrow(ff), ncol = h)
        cfe[,1] = (repoutsample-ff)[,1]
        for (j in c(2:h)) {
          cfe[,j] = rowSums((repoutsample-ff)[,1:j])
        }
        nos = rowSums(cfe>0)
        lentry$nos_yhat <- nos[length(nos)]
        lentry$nos_ff <- nos[-length(nos)]
        total_errors_nos = total_errors_nos + lentry$nos_yhat
      }
      #sMPIS
      if("sMPIS" %in% error){
        h = ncol(ff)
        pis = (ff - repoutsample)[,1]
        for (j in c(2:h)) {
          pis0 = rowSums((ff - repoutsample)[,1:j])
          pis = pis + pis0
        }
        sapis = pis/scale
        lentry$smpis_yhat <- sapis[length(sapis)]
        lentry$smpis_ff <- sapis[-length(sapis)]
        total_errors_smpis = total_errors_smpis + lentry$smpis_yhat
      }
      #sMAPIS
      if("sMAPIS" %in% error){
        h = ncol(ff)
        pis = (ff - repoutsample)[,1]
        for (j in c(2:h)) {
          pis0 = rowSums((ff - repoutsample)[,1:j])
          pis = pis + pis0
        }
        sapis = abs(pis)/scale
        lentry$smapis_yhat <- sapis[length(sapis)]
        lentry$smapis_ff <- sapis[-length(sapis)]
        total_errors_smapis = total_errors_smapis + lentry$smapis_yhat
      }
      dataset[[i]] <- lentry
    }, error = function(e) {
      print(paste("Error when processing error in series: ",
                  i))
      print(e)
      e
    })
  }

  n = length(dataset)

  if("sME" %in% error){
    cat("Average sME:", total_errors_sme/n, "\n")
  }
  if("sMAE" %in% error){
    cat("Average sMAE:", total_errors_smae/n, "\n")
  }
  if("sMSE" %in% error){
    cat("Average sMSE:", total_errors_smse/n, "\n")
  }
  if("MASE" %in% error){
    cat("Average MASE:", total_errors_mase/n, "\n")
  }
  if("NOS" %in% error){
    cat("Average NOS:", total_errors_nos/n, "\n")
  }
  if("sMAPIS" %in% error){
    cat("Average sMAPIS:", total_errors_smapis/n, "\n")
  }
  if("sMPIS" %in% error){
    cat("Average sMPIS:", total_errors_smpis/n, "\n")
  }
  return(dataset)
}
