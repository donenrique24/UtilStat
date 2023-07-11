#'
#' Provide a summary of a particular field
#'
#' @param dataset a data.frame object
#' @param variable the field name (charactar)
#' @param onlyWherePresent a logical, if true the summary is calculated only for the
#' observations where the value is greater than 0 (by default is set to false)
#'
#' @export
summarizeThisVariable <- function(dataset, variable, onlyWherePresent=F) {
  if (onlyWherePresent) {
    .dataset <- dataset[which(dataset[,variable] > 0),]
  } else {
    .dataset <- dataset
  }
  formula <- stats::as.formula(paste(variable, "speciesGr", sep="~"))
  length <- stats::aggregate(formula, .dataset, FUN="length")
  colnames(length)[2] <- paste("n", variable, sep=".")
  mean <- stats::aggregate(formula, .dataset, FUN="mean")
  colnames(mean)[2] <- paste("mean", variable, sep=".")
  min <- stats::aggregate(formula, .dataset, FUN="min")
  colnames(min)[2] <- paste("min", variable, sep=".")
  max <- stats::aggregate(formula, .dataset, FUN="max")
  colnames(max)[2] <- paste("max", variable, sep=".")
  output <- merge(length, mean, by="speciesGr")
  output <- merge(output, min, by="speciesGr")
  output <- merge(output, max, by="speciesGr")
  return(output)
}
