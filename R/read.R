##' Read a File
##'
##' These functions read a file into memory. We memory map the file for fast I/O.
##' The file is read in as a character vector (length one for \code{read},
##' length \code{n} for \code{readlines}).
##'
##' @param file Path to a file.
##'
##' @export
read <- function(file) {
  file <- normalizePath( as.character(file) )
  if (!file.exists(file)) {
    stop("No file at file location '", file, "'.")
  }
  .Call(`_loudness_read`, file, FALSE)
}

##' Read lines ofa File
##'
##' @param file Path to a file.
##'
##' @export
##' @examples
##' p <- file.path( R.home(), "NEWS" )
##' if (file.exists(p))
##'   stopifnot( identical( readLines(p), readlines(p) ) )
readlines <- function(file) {
  file <- normalizePath( as.character(file) )
  if (!file.exists(file)) {
    stop("No file at file location '", file, "'.")
  }
  .Call(`_loudness_read`, file, TRUE)
}
