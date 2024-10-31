#'
#' Provide a summary of a particular field
#'
#' @param dataset a data.frame object
#' @param variable the field name (charactar)
#' @param onlyWherePresent a logical, if true the summary is calculated only for the
#' @param speciesField the field name that contains the different species
#' observations where the value is greater than 0 (by default is set to false)
#' @param format a logical to format the output. The formatting goes as follows: mean (min,max)
#' @param nbDigits an integer for the number of digits (useless if format = F)
#'
#' @export
summarizeThisVariable <- function(dataset, variable, speciesField, onlyWherePresent=F, format=F, nbDigits=0) {
  if (onlyWherePresent) {
    .dataset <- dataset[which(dataset[,variable] > 0),]
  } else {
    .dataset <- dataset
  }
  formula <- stats::as.formula(paste(variable, speciesField, sep="~"))
  length <- stats::aggregate(formula, .dataset, FUN="length")
  colnames(length)[2] <- paste("n", variable, sep=".")
  mean <- stats::aggregate(formula, .dataset, FUN="mean")
  colnames(mean)[2] <- paste("mean", variable, sep=".")
  min <- stats::aggregate(formula, .dataset, FUN="min")
  colnames(min)[2] <- paste("min", variable, sep=".")
  max <- stats::aggregate(formula, .dataset, FUN="max")
  colnames(max)[2] <- paste("max", variable, sep=".")
  output <- merge(length, mean, by=speciesField)
  output <- merge(output, min, by=speciesField)
  output <- merge(output, max, by=speciesField)
  if (format) {
    output[,3:5] <- format(round(output[,3:5], digits = nbDigits), nsmall = nbDigits)
    output[,paste("formatted", variable, sep=".")] <- paste0(output[,3], " (", output[,4], ",", output[,5], ")")
    output <- output[,c(1,2,6)]
  }
  return(output)
}
