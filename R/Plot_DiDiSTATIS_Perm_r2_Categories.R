# Plot_DiDiSTATIS_Perm_r2_Categories.R
#
#' Plot histogram of permtuted r^2_Categories
#'
#' @param res_DiDiSTATIS The output of DiDiSTATIS
#' @param dev_new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @return Histograms of the r^2s
#' @export

Plot_DiDiSTATIS_Perm_r2_Categories <- function(res_DiDiSTATIS, dev_new = TRUE){

  #Plot the histogram of the total squared distances from each group compromise to the compromise,
  #aggregated over all stimuli
  if(dev_new) dev.new()
  Plot_Hist(Distribution = res_DiDiSTATIS$Perm_Omnibus$r2_Categories,
            Score = res_DiDiSTATIS$res_BaryGrand$EffectSize$r2_Categories,
            main = "DiDiSTATIS Omni Perm: r2_Categories",
            dev_new = dev_new)

}
