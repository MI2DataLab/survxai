.onAttach <- function(...) {
  packageStartupMessage(paste0("Welcome to survxai (version: ", utils::packageVersion("survxai"), ").", "\n","Information about the package can be found in the GitHub repository: https://github.com/MI2DataLab/survxai"))
}