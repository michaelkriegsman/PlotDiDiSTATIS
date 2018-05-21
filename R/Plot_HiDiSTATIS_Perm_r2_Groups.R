# Plot_HiDiSTATIS_Perm_r2_Groups.R
#
#' Plot histogram of permtuted r^2_Groups
#'
#' @param res_HiDiSTATIS The output of HiDiSTATIS
#' @param dev_new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @return Histograms of the r^2s
#' @export

Plot_HiDiSTATIS_Perm_r2_Groups <- function(res_HiDiSTATIS, dev_new = TRUE){

  #Plot the histogram of the total squared distances from each group compromise to the compromise,
  #aggregated over all stimuli
  if(dev_new) dev.new()
  hist(res_HiDiSTATIS$Perm_Tables$r2_Groups_perm, breaks=50, main="HiDiSTATIS: r2_Groups")
  abline(v = quantile(res_HiDiSTATIS$Perm_Tables$r2_Groups_perm, .95), col="black", lwd=6, lty = 2)
  abline(v = res_HiDiSTATIS$res_GrandComp$EffectSize$r2_Groups, col= "blue", lwd=4)

}
