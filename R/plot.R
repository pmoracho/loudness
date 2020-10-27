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

  t <- abs(as.numeric(x$Lenght))
  duration <- sprintf("%02d:%02d:%02d:%02d",
                      t %/% 86400,
                      t %% 86400 %/% 3600,
                      t %% 3600 %/% 60,
                      t %% 60 %/% 1)

  d <- density(x$data$M)
  q <- quantile(x$data$M,  prob=seq(0, 1, length = 101))
  m <- as.integer(mean(x$data$M))
  q99 <- as.integer(q["99%"])
  q95 <- as.integer(q["95%"])

  plot(d,
       main = "Kernel Density of LUFS timeline",
       sub = paste("Time Line:", duration),
       ylab = "LUFS density",
       xlab = "LUFS Values")
  polygon(d, col=c("#66FFCC"), border="black")
  abline(v = m, col="red", lwd=3, lty=3)
  abline(v = q95, col="red", lwd=3, lty=3)
  abline(v = q99, col="red", lwd=3, lty=3)

  y = max(d$y)
  text(m + .5, y, paste("LUFS:", m, "(mean)"), srt = 90, adj = 1)
  text(q95 + .5, y, paste("LUFS:", q95, "(95%)"), srt = 90, adj = 1)
  text(q99 + .5, y, paste("LUFS:", q99, "(99%)"), srt = 90, adj = 1)
}
