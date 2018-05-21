# Plot_Hist.R
#
#' Plot histogram
#'
#' @param Distribution The distribution of values
#' @param alpha The alpha threshold
#' @param Score The score(s) to compare to the distribution
#' @param Score_colors The score(s) to compare to the distribution
#' @param pad A fraction (of the range), to pad xlim
#' @param main Title
#' @param dev_new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @return Histogram
#' @export

Plot_Hist <- function(Distribution, alpha = .05, Score, Score_colors = "blue", breaks = 40, pad = 1/5, main = NULL,  dev_new = TRUE){

  The_range <- range(Distribution, Score)
  The_gap <- The_range[2] - The_range[1]
  xlim <- c(The_range[1] - (The_gap * pad),
            The_range[2] + (The_gap * pad))

  if(dev_new) dev.new()
  hist(Distribution, breaks=breaks, xlim = xlim, main=main)
  abline(v = quantile(Distribution, 1-alpha), col="black", lwd=6, lty = 2)
  abline(v = Score, col= Score_colors, lwd=4)

}
