# Plot_Hist.R
#
#' Plot histogram
#'
#' @param Distribution The distribution of values
#' @param alpha The alpha threshold
#' @param Stat The statistic(s) to compare to the distribution
#' @param Stat_colors The color(s) for each statistic
#' @param pad A fraction (of the range), to pad xlim
#' @param main Title
#' @param dev_new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @return Histogram
#' @export

Plot_Hist <- function(Distribution, alpha = .05, Stat, Stat_colors = "blue", breaks = 40, pad = 1/5, main = NULL,  dev_new = TRUE){

  The_range <- range(c(Distribution, Stat))
  The_gap <- The_range[2] - The_range[1]
  xlim <- c(The_range[1] - (The_gap * pad),
            The_range[2] + (The_gap * pad))

  if(dev_new) dev.new()
  Thehist <- hist(Distribution, breaks=breaks, xlim = xlim, main=main)
  abline(v = quantile(Distribution, 1-alpha), col="black", lwd=6, lty = 2)
  # abline(v = Score, col= Score_colors, lwd=3)

  if(length(Stat_colors)==1) { Stat_colors <- rep(Stat_colors, length(Stat))}
  for(i in 1:length(Stat)){
    arrows(x0 = Stat[i], y0 = max(Thehist$counts)-1, y1 = 0, col = Stat_colors[i], lwd = 4)
    p_val <- Happy_p(Distribution, Stat[i])
    text(x = Stat[i], y = max(Thehist$counts), paste0("stat = ", round(Stat[i], 2), "\n p = ", p_val))
  }


}
