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

  ff_cmd <- paste(Sys.which("ffmpeg"), cut_param, '-i', file, "-af ebur128 -f null - 2>", tmpfile)

  cat(paste0("Get loudness from ", file, " using ffmpeg..."), collapse = "\n")
  if (verbose) cat(paste("stats file  :", tmpfile),collapse = "\n")
  if (verbose) cat(paste("ffmpeg call :", ff_cmd), collapse = "\n")
  # cat(ff_cmd, collapse = "\n")

  start.time <- Sys.time()

  run_cmd(ff_cmd)
  assert(file.exists(tmpfile), paste('working file:', tmpfile, 'does not exist!'))
  cat("Parsinng data...", collapse = "\n")

  # df <- parse_ffmpeg_out(tmpfile, nlines = 10000, compact_fmt=FALSE)
  df <- parse_ffmpeg_out_fast(tmpfile, compact_fmt=FALSE)

  # For sansa recordings, skip first records, have outliers
  df <- tail(df, -skip_first_n)
  # Filter From
  # df <- df[df$t < as.difftime(from, format = "%H:%M:%S", units = "secs"), ]

  M <- sum(range(df$M, na.rm = FALSE)*c(1,-1))
  S <- sum(range(df$S, na.rm = FALSE)*c(1,-1))
  I <- sum(range(df$I), na.rm = FALSE*c(1,-1))
  LRA <- sum(range(df$LRA, na.rm = FALSE)*c(1,-1))

  if (compact_fmt) {df <- df[,c("t","M")]}

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

  d <- difftime(tail(df$dt,1), head(df$dt,1), units = "secs")

  cat(paste("Elapsed time:", round(Sys.time() - start.time,2)), collapse = "\n")
  cat(paste("Lenght of tl:", duration(d)), collapse = "\n")
  structure(
    list(data = df,
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
         Lenght = d
         ),
    class = c("lutl", "list")
  )
}

resample <- function(x, group_by_n = 1, adjfun = max) {
  # Group by group_by_n observations
  if (group_by_n > 1) {
    cat("Grouping observations...", collapse = "\n")
    n <- nrow(x$data)
    x$data$group <- head(rep(1:n, each=group_by_n), n)
    group_formula = as.formula(ifelse(x$compact_fmt, "cbind(t,M) ~ group", "cbind(t,M,S,I,LRA) ~ group"))
    x$data <- aggregate(group_formula, x$data, adjfun)
    x$data$group <- NULL
  } else {
    if (x$compact_fmt) x$data <- x$data[, 1:2]
  }
  x
}


summary.lutl <- function(x, ...) {summary(x$data)}
head.lutl <- function(x, ...) {head(x$data)}
tail.lutl <- function(x, ...) {tail(x$data)}
nrow.lutl <- function(x, ...) {nrow(x$data)}

#' save_lutl
#'
#' Save a `lutl` object in native R format RDS
#'
#' @param x a `lutl` object
#' @param ... Anything
#'
#' @export
#'
#' @examples
#' \dontrun{
#' save_lutl(data)
#' }
save_lutl <- function(x, ...) {
  cat(paste("Save lutl into", x$file), collapse='\n')
  rdsfile <- paste0(tools::file_path_sans_ext(x$file), '.Rds')
  saveRDS(x, file = rdsfile)
}

#' load_lutl
#'
#' Loads a previously saved 'lutl' object
#'
#' @param x `lutl` object filename, like `*.Rds`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_lutl("data.Rds")
#' }
load_lutl <- function(x) {
  readRDS(x)
}

duration <- function(length) {
  t <- abs(as.numeric(length))
  sprintf("%02d:%02d:%02d:%02d",
          t %/% 86400,
          t %% 86400 %/% 3600,
          t %% 3600 %/% 60,
          t %% 60 %/% 1)
}
