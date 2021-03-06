# Plot_DiDiSTATIS_Perm_r2_Plain_B_...R
#
#' Plot histogram of permtuted r^2_Categories
#'
#' @param res_DiDiSTATIS The output of DiDiSTATIS
#' @param dev_new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @return Histograms of the r^2s
#' @export

Plot_DiDiSTATIS_Perm_r2_Plain_b_.. <- function(res_DiDiSTATIS, dev_new = TRUE){

  #Plot the histogram of the total squared distances from each group compromise to the compromise,
  #aggregated over all stimuli
  Plot_Hist(Distribution = res_DiDiSTATIS$Perm_Omnibus$r2_Plain_b_..,
            Stat = res_DiDiSTATIS$res_BaryGrand$EffectSize$r2_Plain_b_..,
            main = "DiDiSTATIS Omni Perm: r2_Plain_b_..",
            dev_new = dev_new)

}


