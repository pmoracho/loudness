#' plot_density
#'
#' A Kernel Density of LUFS timeline
#'
#' @param x A `lutl` object
#' @param from From "hour:minute:second" (Default `NA`, from begining of timeline)
#' @param duration Length in "Hour:minute:second" (Default `NA`, all timeline)
#' @param ... Anything
#' @importFrom graphics polygon abline text
#' @importFrom stats density quantile
#' @export
#' @examples
#' \dontrun{
#' plot_density(data)
#' }
plot_density <- function(x,
                         from = NA,
                         duration = NA,
                         ...) {

  duration <- duration(x$Lenght)

  d <- density(x$data$M)
  q <- quantile(x$data$M,  prob=seq(0, 1, length = 101))
  m <- as.integer(mean(x$data$M))
  q99 <- as.integer(q["99%"])
  q95 <- as.integer(q["95%"])

  plot(d,
       main = "Kernel Density of LUFS timeline",
       sub = paste("Time Line from", format(x$start_date, "%a %d-%m-%Y"), "(", duration, ")"),
       ylab = "LUFS density",
       xlab = paste("LUFS Values")
  )
  polygon(d, col=c("#33CC99"), border="black")
  abline(v = m, col="red", lwd=3, lty=3)
  abline(v = q95, col="red", lwd=3, lty=3)
  abline(v = q99, col="red", lwd=3, lty=3)

  y = max(d$y)
  text(m + .5, y, paste("LUFS:", m, "(mean)"), srt = 90, adj = 1)
  text(q95 + .5, y, paste("LUFS:", q95, "(95%)"), srt = 90, adj = 1)
  text(q99 + .5, y, paste("LUFS:", q99, "(99%)"), srt = 90, adj = 1)
}

#' plot_timeline
#'
#' It is a scatter plot of sound levels on the timeline
#'
#' @param x a `lutl` object
#' @param percentil Limit the plot to sounds above this percentile
#'
#' @export
#' @importFrom graphics axis
#' @examples
#' \dontrun{
#' plot_timeline(data, percentil=99)
#' }
plot_timeline <- function(x, percentil = NA, nbreaks = 10){

  duration <- duration(x$Lenght)
  q <- quantile(x$data$M,  prob=seq(0, 1, length = 101))
  if (!is.na(percentil)) M <- ifelse(x$data$M < q[percentil+1], NA, x$data$M)

  pbreaks <- breaks(x$data$dt, nbreaks)
  xbreaks <- x$data$dt[pbreaks]
  plot(x$data$dt, M,
       main = paste("LUFS grater than", q[percentil], "over timeline"),
       sub = paste("Lenght:", duration),
       ylab = "LUFS Values",
       xlab = paste("Time Line from", format(x$start_date, "%a %d-%m-%Y")),
       col=c("#33CC99"),
       xaxt = "n"
  )
  axis(1,
       at = xbreaks,
       cex.axis = .75,
       las=2,
       labels = format(strptime(xbreaks,'%Y-%m-%d %H:%M:%S'),'%H:%M'))
}
