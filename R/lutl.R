#' lutl_from_file
#'
#' Loudness Units Time Line from audo file. A lutl object is a timeline of sound levels
#' with a resolution of tenths of a second. The data is obtained by processing an audio
#' file with ffmpeg and the EBUR128 plug-in.
#'
#' EBU R 128 is a recommendation for loudness normalisation and maximum level of audio
#' signals. It is primarily followed during audio mixing of television and radio
#' programmes and adopted by broadcasters to measure and control programme loudness.
#' It was first issued by the European Broadcasting Union in August 2010 and most
#' recently revised in June 2014. R 128 employs an international standard for measuring
#' audio loudness, stated in the ITU-R BS.1770 recommendation and using the loudness
#' measures LU (loudness units) and LUFS (loudness units referenced to full scale),
#' specifically created with this purpose.[3] The EBU Tech 3341 document further clarified
#' loudness metering implementation and practices in 2016.
#'
#' @param file Any file with an audio track that is consistent with ffmpeg
#' @param skip_first_n Skip the first n observations. On at least one Sansa Fuze device, the first observations
#'                     of the recorded audio are basically silent, so they can be ignored.
#' @param start_date Date and time of the start of the audio recording. In case you do not set this parameter,
#'                   it tries to retrieve the file name information, for now only valid for the devices with
#'                   the rockbox firmware.
#' @param from From "hour:minute:second"
#' @param duration Length in "Hour:minute:second"
#' @param group_by_n Each observation corresponds to one tenth of a second, with this parameter we can group
#'                   them to reduce the final size of the data. Example `group_by_n = 10` corresponds to one
#'                   observation every second.
#' @param adjfun Summation or adjustment function when grouping observations By default, `mean` is used
#' @param compact_fmt Use compact format? (default = TRUE)
#' @param verbose default FALSE
#'
#' @return a lutl object
#' @importFrom stats as.formula aggregate
#' @importFrom utils tail head
#' @export
#' @examples
#' \dontrun{
#' data <- lutl_from_file('~/Descargas/ejemplo.wav')
#' data <- lutl_from_file('~/Descargas/ejemplo.wav', start_date = "2020-10-02 16:45:34")
#' }
lutl_from_file <- function(file,
                           skip_first_n = 3,
                           start_date = NA,
                           from = NA,
                           duration = NA,
                           group_by_n = 1,
                           adjfun = mean,
                           compact_fmt = TRUE,
                           verbose = FALSE) {

  assert(file.exists(file), paste(file, 'does wnot exist!'))

  tmpfile <-tempfile("get_loudness_data_from_file_out")

  paste('-ss',
        ifelse(is.na(from), '00:00:00', from),
        ifelse(is.na(duration), '', paste('-t', duration))
  ) -> cut_param

  ff_cmd <- paste(Sys.which("ffmpeg"), '-i', file, cut_param, "-af ebur128 -f null - 2>", tmpfile)

  cat(paste0("Get loudness from ", file, " using ffmpeg..."), collapse = "\n")
  if (verbose) cat(paste("stats file  :", tmpfile),collapse = "\n")
  if (verbose) cat(paste("ffmpeg call :", ff_cmd), collapse = "\n")
  # cat(ff_cmd, collapse = "\n")

  start.time <- Sys.time()

  run_cmd(ff_cmd)
  assert(file.exists(tmpfile), paste('working file:', tmpfile, 'does not exist!'))
  cat("Parsinng data...", collapse = "\n")

  df <- parse_ffmpeg_out(tmpfile, nlines = 10000, compact_fmt=compact_fmt)

  if (compact_fmt) {df <- df[,c("t","M")]}
  # For sansa recordings, skip first records, have outliers
  df <- tail(df, -skip_first_n)

  M <- sum(range(df$M)*c(1,-1))
  S <- sum(range(df$S)*c(1,-1))
  I <- sum(range(df$I)*c(1,-1))
  LRA <- sum(range(df$LRA)*c(1,-1))

  # Group by group_by_n observations
  if (group_by_n > 1) {
    cat("Grouping observations...", collapse = "\n")
    n <- nrow(df)
    df$group <- head(rep(1:n, each=group_by_n), n)
    group_formula = as.formula(ifelse(compact_fmt, "cbind(t,M) ~ group", "cbind(t,M,S,I,LRA) ~ group"))
    df <- aggregate(group_formula, df, adjfun)
    df$group <- NULL
  } else {
    if (compact_fmt) df <- df[, 1:2]
  }

  cat("Process datetime...", collapse = "\n")
  if (is.na(start_date)){
    start_date <- get_datetime_from_file(basename(file))
  } else {
    start_date <- as.POSIXct(start_date, format="%Y-%m-%d %H:%M:%S")
  }
  if (!is.na(start_date)) {
    order_cols <- colnames(df)
    df$dt <- as.POSIXct(start_date, ) + df$t
    df <- df[, c("dt", order_cols)]
  }

  cat(paste("Elapsed time:", round(Sys.time() - start.time,2)), collapse = "\n")
  structure(
    list(tldata = df,
         file = file,
         start_date = start_date,
         from = from,
         duration = duration,
         group_by_n = group_by_n,
         adjfun = adjfun,
         compact_fmt = compact_fmt,
         LRA = LRA,
         M = M,
         S = S,
         I = I,
         Lengh = difftime(head(df$dt,1), tail(df$dt,1))
         ),
    class = "lutl"
  )
}


#' play.lutl
#'
#' Play a lutl object
#'
#' @param x A `lutl` object
#' @param ... Anything
#' @param from From "hour:minute:second" (Default `NA`, from begining of timeline)
#' @param duration Length in "Hour:minute:second" (Default `NA`, all timeline)
#' @param verbose be verbose (default FALSE)
#'
#' @return
#' @export
#'
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
  play.lutl(x, from, duration, verbose)
}

play <- function(x, ...) {
  UseMethod("play")
}

summary.lutl <- function(x, ...) {summary(x$tldata)}

plot_density <- function(x,
                         from = NA,
                         duration = NA,
                         ...) {

  d <- density(x$tldata$M)
  q <- quantile(x$tldata$M,  prob=seq(0, 1, length = 101))
  m <- mean(x$tldata$M)
  plot(d, main="Kernel Density of LUFS",
       ylab="LUFS density",
       xlab="LUFS Values")
  polygon(d, col=c("#6699FF"), border="black")
  abline(v = m, col="red", lwd=3, lty=2)
  abline(v = q["99%"], col="red", lwd=3, lty=2)
  c
}
