# Plot_DiDiSTATIS_Perm_r2_Plain_Disc_.d.R
#
#' Plot histogram of permtuted r^2_Plain_Disc_.d
#'
#' @param res_DiDiSTATIS The output of DiDiSTATIS
#' @param dev_new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @return Histogram
#' @export

Plot_DiDiSTATIS_Perm_r2_Plain_Disc_.d <- function(res_DiDiSTATIS, dev_new = TRUE){

  #Plot the histogram of the total squared distances from each group compromise to the compromise,
  #aggregated over all stimuli
  Plot_Hist(Distribution = res_DiDiSTATIS$Perm_Omnibus$r2_Plain_Disc_.D,
            Stat = res_DiDiSTATIS$res_BaryGrand$EffectSize$r2_Plain_Disc_.D,
            main = "DiDiSTATIS Omni Perm: r2_Plain_Disc_.d",
            dev_new = dev_new)

}


