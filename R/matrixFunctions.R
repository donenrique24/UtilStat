#####################################
# Matrix functions
# Mathieu Fortin - May 2020
#####################################

#'
#' Compute a vector from a symmetric matrix
#'
#' @param matrix a Matrix object
#'
#' @return a vector of values
#'
#' @export
symSquare <- function(matrix) {
  if ("matrix" %in% class(matrix)) {
    vec <- c()
    for (i in 1:length(matrix[,1])) {
      vec <- c(vec,matrix[i,1:i])
    }
    return(vec)
  } else {
    stop("The argument matrix must be of the matrix class!")
  }
}
