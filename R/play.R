play <- function(x, ...) {
  UseMethod("play")
}


#' play.lutl
#'
#' Play a lutl object
#'
#' @param x A `lutl` object
#' @param from From "hour:minute:second" (Default `NA`, from begining of timeline)
#' @param duration Length in "Hour:minute:second" (Default `NA`, all timeline)
#' @param verbose be verbose (default FALSE)
#' @param ... Anything
#'
#' @export
#' @examples
#' \dontrun{
#' data <- lutl_from_file('~/Descargas/ejemplo.wav')
#' play(data)
#' }
play.lutl <- function(x,
                      from = NA,
                      duration = NA,
                      verbose = FALSE,
                      ...) {

  paste('-ss',
        ifelse(is.na(from), '00:00:00', from),
        ifelse(is.na(duration), '', paste('-t', duration))
  ) -> cut_param

  ff_cmd <- paste(Sys.which("ffplay"), x$file, cut_param, "-autoexit")
  if (verbose) cat(paste("ffplay call :", ff_cmd), collapse = "\n")
  run_cmd(ff_cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
}

play_secs <- function(x,
                      secs = 1,
                      from = NA,
                      verbose = FALSE,
                      ...) {
  play.lutl(x, from, secs, verbose)
}

