# Plot_HiDiSTATIS_Perm_d2
#
#' Plot histogram of permtuted squared distance from Groups to Grand Compromise
#'
#' @param res_HiDiSTATIS The output of HiDiSTATIS
#' @param dev_new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @return Histograms of the Dev2's
#' @export

Plot_HiDiSTATIS_Perm_d2 <- function(res_HiDiSTATIS, dev_new = TRUE){

  #Plot the histogram of the total squared distances from each group compromise to the compromise,
  #aggregated over all stimuli
  if(dev_new) dev.new()
  hist(res_HiDiSTATIS$Perm_Tables$Dev2_F.d.perm_2_F.perm, breaks=50, main="HiDiSTATIS: d^2(F.d, F..)")
  abline(v = quantile(res_HiDiSTATIS$Perm_Tables$Dev2_F.d.perm_2_F.perm, .95), col="black", lwd=6, lty = 2)
  abline(v = res_HiDiSTATIS$Perm_Tables$Dev2_F.d_2_F, col=res_HiDiSTATIS$input$DESIGN_tables$colors_D, lwd=4)


  # #one histogram for each stimulus
  # for(ab in 1:nrow(res_HiDiSTATIS$Perm_Tables$Dev2_Fab.d_2_Fab)){
  #
  #   #Plot the histogram of the total squared distances from each group to the average,
  #   hist(res_HiDiSTATIS$Perm_Tables$Dev2_Fab.d.perm_2_Fab.perm[ab,,], breaks=50, main=paste0("HiDiSTATIS: Dev2 Fab.d to Fab.. (",rownames(res_HiDiSTATIS$Perm_Tables$Dev2_Fab.d_2_Fab)[ab],")"))
  #   abline(v = quantile(res_HiDiSTATIS$Perm_Tables$Dev2_Fab.d.perm_2_Fab.perm[ab,,], .95), col="black", lwd=3)
  #   abline(v = res_HiDiSTATIS$Perm_Tables$Dev2_Fab.d_2_Fab[ab,], col=res_HiDiSTATIS$input$DESIGN_tables$colors_D, lwd=3)
  #
  # }

}
