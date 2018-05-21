# Plot_HiDiSTATIS_Boot_centered_example
#
#' Plot histogram of permtuted squared distance from Groups to Grand Compromise
#'
#' @param res_HiDiSTATIS The output of HiDiSTATIS
#' @param i The stimulus to plot
#' @param dev.new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @return Histograms of the Dev2's
#' @export

Plot_HiDiSTATIS_Boot_centered_example <- function(res_HiDiSTATIS, i = 1, dev.new = TRUE){

  #Show that I need the correction in order to isolate the error of interest
  #In other words, there are 2 sources of "error"
  # 1) The difference between each bootstrapped grand compromise and the fixed grand compromise
  # 2) The difference between each group's factor scores and the grand compromise factor scores

  # I am not interested in the 1st source of error, so I subtract it, to give the corrected:
  # Fab_boot_d_corrected

  #Select a given stimulus
  ab <- i
  #Plot the fixed grand compromise factor scores
  prettyPlot(res_HiDiSTATIS$res_GrandComp$eig$F[c(ab,ab),],
             col = res_HiDiSTATIS$input$DESIGN_rows$colors_AB[ab],
             pch = 15, cex = 1.5,
             dev.new = dev.new,
             constraints = minmaxHelper(rbind_array_2_matrix(res_HiDiSTATIS$Boot_Tables$Fab..boot)))
  #Plot the bootstrap group compromise (whose barycenter is the booted grand compromise)
  prettyPlot(t(res_HiDiSTATIS$Boot_Tables$Fab_boot_d[ab,1:2,,1]),
             new.plot = F, dev.new = F,
             col=res_HiDiSTATIS$input$DESIGN_tables$colors_D,
             display_names = F,
             pch = 16, cex = 1.5)
  #Plot the bootstrapped grand compromise (to visualize the 1st error)
  segments(x0 = t(res_HiDiSTATIS$Boot_Tables$Fab..boot[ab,1,1]),
           y0 = t(res_HiDiSTATIS$Boot_Tables$Fab..boot[ab,2,1]),
           x1 = t(t(res_HiDiSTATIS$Boot_Tables$Fab_boot_d[ab,1,,1])),
           y1 = t(t(res_HiDiSTATIS$Boot_Tables$Fab_boot_d[ab,2,,1])),
           col = res_HiDiSTATIS$input$DESIGN_tables$colors_D)

  prettyPlot(t(res_HiDiSTATIS$Boot_Tables$Fab..boot[ab,1:2,1]),
             new.plot = F, dev.new = F, col="grey",
             pch = 15, cex = 1.5)
  text(t(res_HiDiSTATIS$Boot_Tables$Fab..boot[ab,1:2,1]), col = "grey",
       labels = "Booted", pos=3)


  #And plot the corrected bootstrap group compromise (whose barycenter is the fixed grand compromise)
  prettyPlot(t(res_HiDiSTATIS$Boot_Tables$Fab_boot_d_corrected[ab,1:2,,1]),
             new.plot = F, dev.new = F,
             col=res_HiDiSTATIS$input$DESIGN_tables$colors_D,
             display_names = F,
             pch = 16, cex = 1.5)
  segments(x0 = res_HiDiSTATIS$res_GrandComp$eig$F[ab,1],
           y0 = res_HiDiSTATIS$res_GrandComp$eig$F[ab,2],
           x1 = t(t(res_HiDiSTATIS$Boot_Tables$Fab_boot_d_corrected[ab,1,,1])),
           y1 = t(t(res_HiDiSTATIS$Boot_Tables$Fab_boot_d_corrected[ab,2,,1])),
           col = res_HiDiSTATIS$input$DESIGN_tables$colors_D)
  #This shows that I want to use the corrected group compromise factor scores to estimate the group effect
  #because the deviation between between each booted grand compromise and the fixed grand compromise
  #needs to be mentioned, but is not of interest.

}
