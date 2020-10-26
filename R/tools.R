#' run_cmd
#'
#' Execute a command in a way that is consistent with both Windows and Linux platforms
#'
#' @param cmd Command to execute
#' @param wait Wait command to finish (default TRUE)
#' @param echo Print command in console (default FALSE)
#'
#' @return a value returned from `system` or `shell`
#' @export
#'
#' @examples
#'
#' run_cmd("ls")
#'
run_cmd <- function(cmd,
                    wait = TRUE,
                    echo = FALSE,
                    ignore.stdout = FALSE,
                    ignore.stderr = FALSE) {

  if (.Platform$OS.type == "windows") {
    ret <- shell(cmd, wait=wait, ignore.stdout = ignore.stdout, ignore.stderr = ignore.stderr)
  } else {
    ret <- system(cmd, wait=wait, ignore.stdout = ignore.stdout, ignore.stderr = ignore.stderr)
  }

  if (echo) {
    cat(cmd, "\n")
  }
  ret
}

#' try_require
#'
#' Attempts to load a package in case an error is issued
#'
#' @param package name of the package to load
#' @param fun name of function that needs the package
#'
#' @export
#'
#' @examples
#'
#' try_require("stats", "my_function")
#'
try_require <- function(package, fun) {
  if (requireNamespace(package, quietly = TRUE)) {
    library(package, character.only = TRUE)
    return(invisible())
  }

  stop("Package `", package, "` required for `", fun , "`.\n", # nocov start
       "Please install and try again.", call. = FALSE) # nocov end
}

#' get_datetime_from_file
#'
#' @param filename file name to process
#'
#' @export
#'
#' @examples
#'
#' get_datetime_from_file("R_MIC_091020-123456.mp3")
#'
get_datetime_from_file <- function(filename) {

  # filename <- basename(filepath)
  pattern <- "R_MIC_(\\d{2})(\\d{2})(\\d{2})-(\\d{2})(\\d{2})(\\d{2})[.].*"
  capture <- gsub(pattern, "\\1-\\2-\\3 \\4:\\5:\\6", regmatches(filename, gregexpr(pattern,filename)))
  as.POSIXct(capture, format="%y-%m-%d %H:%M:%S")
}


#' assert
#'
#' Verify a condition and stop the execution if it is not met
#'
#' @param expr expression to check
#' @param error error message to display
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' assert(1 == 2, "1 is not 2!!")
#'}
assert <- function (expr, error) {
  if (!expr) stop(error, call. = FALSE)
}


#' dhms
#'
#' Convert a time expressed in seconds into hour:minute:second format
#'
#' @param t Time in seconds
#'
#' @return
#' @export
#'
#' @examples
#' dhms(86400)
dhms <- function(t){
  paste(paste(formatC(t %/% (60*60), width = 2, format = "d", flag = "0")
               ,formatC(t %/% 60 %% 60, width = 2, format = "d", flag = "0")
               ,formatC(t %% 60, width = 2, format = "d", flag = "0")
               ,sep = ":"
        )
  )
}
