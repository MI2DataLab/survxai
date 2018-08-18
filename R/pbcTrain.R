#' @title pbcTrain
#' @description PBC train set
#' Data set based on \code{pbc} from \code{randomForestSRC} package. 
#' The data consists of 138 randomly chosen observations The \code{pbcTrain} contains only complete cases for each observation.
#' It contains 5 variables: `status`, `sex`, `bili`, `stage`, and `years`.
#' 
#' @source randomForestSRC
#' @references Flemming T.R and Harrington D.P., (1991) Counting Processes and Survival Analysis. New York: Wiley.
#' @name pbcTrain
#' @docType data
#' 
#' @examples 
#' data("pbcTrain", package = "survxai")
#' head(pbcTrain)
NULL
