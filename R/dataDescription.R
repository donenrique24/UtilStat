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


#'
#' Rename a particular field of a data.frame object
#'
#' @param df a data.frame object
#' @param oldName the current field name
#' @param newName the new field name
#' @return the updated data.frame object
#'
#' @export
renameField <- function(df, oldName, newName) {
  if (!oldName %in% colnames(df)) {
    stop("The field ", oldname, " can't be found in the data.frame object!")
  }
  index <- which(colnames(df) == oldName)
  colnames(df)[index] <- newName
  return(df)
}



#'
#' Drop fields from a data.frame object
#'
#' @param df a data.frame object
#' @param fieldsToBeDropped a vector of field names to be dropped
#' @return the updated data.frame object
#'
#' @export
dropFields <- function(df, fieldsToBeDropped) {
  fieldsToKeep <- colnames(df)[which(!colnames(df) %in% fieldsToBeDropped)]
  return (df[,fieldsToKeep])
}


#'
#' Fill missing values in a dataset.
#'
#' @param df a data.frame object
#' @param missingNumeric a double that stands for missing value
#' @param missingInteger an integer that stands for missing value
#' @param missingCharacter a character string that stands for missing value
#'
#' @return an updated data.frame instance
#'
#' @export
fillMissingValues <- function(df, missingNumeric = -99999999, missingInteger = as.integer(missingNumeric), missingCharacter = "NA") {
  for (field in colnames(df)) {
    fieldValues <- df[,field]
    indexNA <- which(is.na(fieldValues))
    if (length(indexNA) > 0) {
      if ("integer" %in% class(fieldValues)) {
        df[indexNA, field] <- missingInteger
      } else if ("numeric" %in% class(fieldValues)) {
        df[indexNA, field] <- missingNumeric
      } else if ("character" %in% class(fieldValues)) {
        df[indexNA, field] <- missingCharacter
      } else {
        stop("This class cannot be processed for NA: ", class(fieldValues))
      }
      message("There are ", length(indexNA), " NA values in field ", field, ". There were replaced by NA or -99999999!")
    }
  }
  return(df)
}


