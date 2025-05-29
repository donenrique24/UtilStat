#'
#' Linear model assessment tools
#' @author Mathieu Fortin - May 2025
#'


#
# A benchmark dataset for linear modelling.
#
# @docType data
#
# @usage data(dataHeight)
#
# @keywords datasets
#
# @examples
# data(dataHeight)
"dataHeight"

#'
#' Plot average predicted values against classes of a particular
#' continuous covariate.
#'
#' @param model a model object (e.g. an lme, gls, or lm instance)
#' @param data the data.frame object used to fit the model
#' @param fieldname the name of the variables the average residuals are plotted
#' against. By default, it is set to "pred" that is the model predictions
#' @param classWidth a positive numeric defining the width of the classes of
#' the variable on the x axis
#' @param printout a logical (true to print out the average residuals)
#' @param cutoff minimal number of observations for the average residual
#' to be included in the plot
#' @param normalize a logical true to use normalized residuals or false to
#' use raw residuals
#'
#' @export
getResidPlotAgainst <- function(model,
                                data,
                                fieldname,
                                classWidth = NULL,
                                printout = F,
                                cutoff=NULL,
                                normalize=T) {
  if (is.null(model) | (!methods::is(model, "gls") & !methods::is(model, "lm"))) {
    stop("The model argument must be a gls or lm instance!")
  }
  if (is.null(data) | !methods::is(data, "data.frame")) {
    stop("The data argument must be the same data.frame object used to fit the model object!")
  }
  data <- as.data.frame(data) ## to make sure this is not a data.table

  isClassCovariate <- is.null(classWidth)

  if (!isClassCovariate) {
    if (!methods::is(classWidth, "numeric")) {
      stop("If the classWidth is non null, then it must be a numeric!")
    } else if (classWidth <= 0) {
      stop("The classWidth argument must be a positive numeric!")
    }
  }

  if (!fieldname %in% c(colnames(data), "pred")) {
    stop("The fieldname should be either pred or one of the fields in the data argument.")
  }

  if (fieldname == "pred") {
    var <- stats::fitted(model)
    if (length(var) != nrow(data)) {
      stop("The number of rows of the data argument does not match the length of the prediction vector!")
    }
  } else {
    var <- data[,fieldname]
  }

  if (!isClassCovariate) {
    if (!methods::is(var, "numeric") & !methods::is(var, "integer")) {
      stop("The fieldname refers to a field that is not numeric, but the classWidth parameter indicates otherwise.")
    } else {
      data$clVar <- round(var/classWidth + .0001) * classWidth
    }
  } else {
    data$clVar <- var
  }

  if (normalize) {
    if (methods::is(model, "lme") | methods::is(model, "gls")) {
      data$res <- stats::residuals(model, level=0, type = "normalized")
    } else {
      data$res <- stats::rstandard(model)
    }
  } else {
    data$res <- stats::residuals(model)
  }
  tmp <- stats::aggregate(res ~ clVar, data, FUN="mean")
  n <- stats::aggregate(res ~ clVar, data, FUN="length")
  colnames(n) <- c("res", "n")
  tmp <- cbind(tmp, n[,"n"])
  if (printout) {
    print(tmp)
  }
  plot <- ggplot2::ggplot()
  if (!is.null(cutoff)) {
    if (!methods::is(cutoff, "numeric")) {
      warning("The cutoff argument should be a numeric! It will be ignored.")
      plot <- plot + ggplot2::geom_point(ggplot2::aes(x=clVar, y=res), tmp, size=2)
    } else {
      plot <- plot + ggplot2::geom_point(ggplot2::aes(x=clVar, y=res), tmp[which(tmp$n > cutoff),], size=2)
    }
  } else {
    plot <- plot + ggplot2::geom_point(ggplot2::aes(x=clVar, y=res), tmp, size=2)
  }
  plot <- plot +
    ggplot2::geom_hline(yintercept = 0, linewidth = 1) +
    ggplot2::ylim(-5,5) +
    ggplot2::xlab(fieldname) +
    ggplot2::ggtitle(fieldname)
  return(plot)
}
