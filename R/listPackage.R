#' Searching for functions in a package
#' @param package name of package to search
#' @param pattern find all functions with pattern in the name 
#' @return all functions in package
#' @export
lsp <- function (package, pattern) {
  package <- deparse(substitute(package))
  ls(pos = paste("package", package, sep = ":"), 
     pattern = pattern)
}
