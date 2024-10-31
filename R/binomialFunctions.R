#'
#' An example of recruitment occurrence modelling.
#'
#'
#' @docType data
#'
#' @usage data(exampleRecruitment)
#'
#' @keywords datasets
#'
#' @examples
#' data(exampleRecruitment)
"exampleRecruitment"


.getPredProbAndObsProp <- function(dataSet, cutoff) {
  meanPred <- stats::aggregate(pred ~ roundVar, dataSet, FUN="mean")
  meanObs <- stats::aggregate(obs ~ roundVar, dataSet, FUN="mean")
  n <- stats::aggregate(obs ~ roundVar, dataSet, FUN="length")
  colnames(n)[2] <- "n"
  tmp <- merge(n,merge(meanPred, meanObs, by="roundVar"), by="roundVar")
  tmp <- tmp[which(tmp$n >= cutoff),]
  return(tmp)
}




#'
#' Provides a table of predicted probabilities and observed proportions
#'
#' @param dataSet a data.frame object that was used to fit the model
#' @param fitted a vector of predictions on the original scale
#' @param fieldName a character standing for the field name of the continuous explanatory variable in the dataSet object
#' @param classRange a numeric standing for the width of the classes
#' @param obsFieldName a character standing for the field name of the response variable
#' @param cutoff the minimum number of observations to consider the residual
#' @return a data.frame object
#'
#' @export
getPredictedProbsAndObservedProps <- function(dataSet, fitted, fieldName, classRange, obsFieldName, cutoff = 5) {
  .dataSet <- as.data.frame(dataSet)
  .dataSet$pred <- fitted
  .dataSet$obs <- .dataSet[,obsFieldName]
  .dataSet$roundVar <- round(.dataSet[,fieldName] / classRange) * classRange
  .dataSetTrim <- as.data.frame(.dataSet[,c("pred", "obs", "roundVar")])
  tmp <- .getPredProbAndObsProp(.dataSetTrim, cutoff)
  return(tmp)
}



.binomialResidualsCore <- function(dataSet, fieldName, min, max, cutoff, continuous, title, print=T, textsize = 18, xLabel = NULL) {
  tmp <- .getPredProbAndObsProp(dataSet, cutoff)
  tmp$var <- tmp$pred * (1 - tmp$pred) / tmp$n
  tmp$std <- tmp$var^.5
  tmp$lower95 <- stats::qt(0.025, df=tmp$n - 1)
  tmp$upper95 <- stats::qt(0.975, df=tmp$n - 1)
  tmp$diff <- (tmp$obs - tmp$pred) / tmp$std
  if (print) {
    print(tmp)
  }
  plot <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x=roundVar, y = diff), tmp, size = 2)
  if (continuous) {
    plot <- plot +
      ggplot2::geom_line(ggplot2::aes(x=roundVar, y=upper95), tmp, color = "red", lty=2) +
      ggplot2::geom_line(ggplot2::aes(x=roundVar, y=lower95), tmp, color = "red", lty=2)
  } else {
    plot <- plot +
      ggplot2::geom_point(ggplot2::aes(x=roundVar, y=upper95), tmp, color = "red", shape = 3) +
      ggplot2::geom_point(ggplot2::aes(x=roundVar, y=lower95), tmp, color = "red", shape = 3)
  }
  if (!is.null(title)) {
    titleStr <- paste(title, "; Variable =", fieldName)
    plot <- plot + ggplot2::ggtitle(titleStr)
  } else if (print) {
    titleStr <- paste("Variable =", fieldName)
    plot <- plot + ggplot2::ggtitle(titleStr)
  }
  plot <- plot + ggplot2::geom_hline(yintercept = 0) +
    ggplot2::ylim(min,max) +
    ggplot2::ylab("Average Pearson residuals")

  if (!is.null(xLabel)) {
    plot <- plot + ggplot2::xlab(xLabel)
  }

  plot <- plot +
    ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(size=textsize),
          axis.text = ggplot2::element_text(size=textsize),
          axis.line = ggplot2::element_line(colour = "black"),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          axis.ticks.length = ggplot2::unit(3,"mm"),
          panel.border = ggplot2::element_blank())

  return(plot)
}

#'
#' Provides a graph with the binomial residuals
#'
#' Binomial residuals are computed by classes of a continuous explanatory variable. The function
#' returns a ggplot2 graph.
#'
#' @param dataSet a data.frame object that was used to fit the model
#' @param fitted a vector of predictions on the original scale
#' @param fieldName a character standing for the field name of the continuous explanatory variable in the dataSet object
#' @param classRange a numeric standing for the width of the classes
#' @param obsFieldName a character standing for the field name of the response variable
#' @param min the lower bound of the y-axis
#' @param max the upper bound of the y-axis
#' @param cutoff the minimum number of observations to consider the residual
#' @param title an optional character to be the title of the graph
#' @param print a logical (true prints the dataset before plotting it)
#' @param textsize the font size of the graph
#' @param xLabel a character string for the label of the x axis
#' @return a ggplot2 graph
#'
#' @export
binomialResidualsContinuous <- function(dataSet, fitted, fieldName, classRange, obsFieldName, min = -5, max = +5, cutoff = 5, title = NULL, print = T, textsize = 18, xLabel = NULL) {
  .dataSet <- as.data.frame(dataSet)
  .dataSet$pred <- fitted
  .dataSet$obs <- .dataSet[,obsFieldName]
  .dataSet$roundVar <- round(.dataSet[,fieldName] / classRange) * classRange
  .dataSetTrim <- as.data.frame(.dataSet[,c("pred", "obs", "roundVar")])
  plot <- .binomialResidualsCore(.dataSetTrim, fieldName, min, max, cutoff, continuous = T, title, print, textsize, xLabel)
  return(plot)
}

#'
#' Provides a graph with the binomial residuals
#'
#' Binomial residuals are computed by classes of a categorical explanatory variable. The function
#' returns a ggplot2 graph.
#'
#' @param dataSet a data.frame object that was used to fit the model
#' @param fitted a vector of predictions on the original scale
#' @param fieldName a character standing for the field name of the categorical explanatory variable in the dataSet object
#' @param obsFieldName a character standing for the field name of the response variable
#' @param min the lower bound of the y-axis
#' @param max the upper bound of the y-axis
#' @param cutoff the minimum number of observations to consider the residual
#' @param title an optional character to be the title of the graph
#' @param print a logical (true prints the dataset before plotting it)
#' @return a ggplot2 graph
#'
#' @export
binomialResidualsClass <- function(dataSet, fitted, fieldName, obsFieldName, min = -5, max = +5, cutoff = 5, title = NULL, print = T) {
  .dataSet <- as.data.frame(dataSet)
  .dataSet$pred <- as.numeric(fitted)
  .dataSet$obs <- .dataSet[,obsFieldName]
  .dataSet$roundVar <- .dataSet[,fieldName]
  .dataSetTrim <- as.data.frame(.dataSet[,c("pred", "obs", "roundVar")])
#  d <- .dataSetTrim
  plot <- .binomialResidualsCore(.dataSetTrim, fieldName, min, max, cutoff, continuous = F, title, print)
  return(plot)
}



#'
#' Hosmer-Lemeshow test
#' @param observed a vector of observed values
#' @param predicted a vector of predictions
#' @param nbGroups number of groups for the test (by default is set to 10)
#' @param covar an optional covariate to define the groups
#' @return a list object
#'
#' @export
hosmerLemeshowTest <- function(observed, predicted, nbGroups = 10, covar = NULL){
  n <- length(observed)
  if (n != length(predicted)) {
    stop("The vectors observed and predicted have different lengths!")
  } else if (!is.null(covar)) {
    if (n != length(covar)) {
      stop("The vector covar has a different length!")
    }
  }

  .data <- data.frame(observed, predicted)

  if (is.null(covar)) {
    .data <- .data[order(.data$predicted),]
  } else {
    .data$covar <- covar
    .data <- .data[order(.data$covar),]
  }

  .data$index <- 1:n
  .data$group <- ceiling(.data$index / n * nbGroups)
  meanPred <- stats::aggregate(predicted ~ group, data=.data, FUN = "mean")
  sumObs <- stats::aggregate(observed ~ group, data=.data, FUN = "sum")
  n.group <- stats::aggregate(observed ~ group, data=.data, FUN = "length")
  if (is.null(covar)) {
    minPred <- stats::aggregate(predicted ~ group, data=.data, FUN = "min")
    maxPred <- stats::aggregate(predicted ~ group, data=.data, FUN = "max")
  } else {
    meanCovar <- stats::aggregate(covar ~ group, data=.data, FUN = "mean")
    minPred <- stats::aggregate(covar ~ group, data=.data, FUN = "min")
    maxPred <- stats::aggregate(covar ~ group, data=.data, FUN = "max")
  }

  y1 <- sumObs$observed
  y0 <- n.group$observed - y1

  y1.hat <- meanPred$predicted * n.group$observed
  y0.hat <- n.group$observed - y1.hat

  lower <- minPred$predicted
  upper <- maxPred$predicted

  if (is.null(covar)) {
    mean <- meanPred$predicted
  } else {
    mean <- meanCovar$covar
  }

  chi2.cont <- (y1 - y1.hat)^2 / (n.group$observed * meanPred$predicted * (1-meanPred$predicted))
  expObs <- data.frame(lower, upper, mean, y0, y1, y0.hat, y1.hat, chi2.cont)
  chi2 <- sum(chi2.cont)
  pValue <- 1 - stats::pchisq(chi2, nbGroups-2)
  output <- list()
  output$chi2 <- chi2
  output$expObs <- expObs
  output$pValue <- pValue
  return(output)
}
