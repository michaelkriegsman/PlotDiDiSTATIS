# Plot_HiDiSTATIS_Boot_centered_CIs
#
#' Plot histogram of permtuted squared distance from Groups to Grand Compromise
#'
#' @param res_HiDiSTATIS The output of HiDiSTATIS
#' @param axes Axes to plot
#' @param i The stimulus to plot
#' @param ellipse Boolean for ellipse or convex hull
#' @param percentage 1 - alpha threshold
#' @param dev.new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @return Histograms of the Dev2's
#' @export

Plot_HiDiSTATIS_Boot_centered_CIs <- function(res_HiDiSTATIS, axes = c(1,2), i = 1, ellipse = TRUE, percentage = 0.95, dev.new = TRUE){

  #Also, plot the fixed grand compromise, and the fixed group compromises
  # And then put a 95% CI around the corrected group compromise factor scores.
  prettyPlot(res_HiDiSTATIS$res_GrandComp$eig$F[c(i,i), axes],
             constraints=minmaxHelper(rbind_array_2_matrix(res_HiDiSTATIS$Boot_Tables$Fab_boot_d_corrected[,axes,,])),
             display_names = F, dev.new = dev.new,
             col = res_HiDiSTATIS$input$DESIGN_rows$colors_AB[i],
             xlab = paste0("Component ", axes[1]),
             ylab = paste0("Component ", axes[2]),
             pch = 15, cex = 1.5,
             main=paste0("HiDiSTATIS: 95% Bootstrap CIs, for ", rownames(res_HiDiSTATIS$input$DESIGN_rows$mat)[i]))

  prettyPlot(t(res_HiDiSTATIS$res_GrandComp$ProjGroup$F[i,axes,]),
             col=res_HiDiSTATIS$input$DESIGN_tables$colors_D,
             dev.new = F, new.plot = F)


  if(ellipse){
    for(d in 1:res_HiDiSTATIS$input$DESIGN_tables$D){
      dataEllipse(x = res_HiDiSTATIS$Boot_Tables$Fab_boot_d_corrected[i, axes[1], d, ],
                  y = res_HiDiSTATIS$Boot_Tables$Fab_boot_d_corrected[i, axes[2], d, ],
                  add = TRUE, levels = percentage,
                  plot.points = FALSE, center.pch = FALSE,
                  col = res_HiDiSTATIS$input$DESIGN_tables$colors_D[d], lwd = 3.5,
                  fill = TRUE, fill.alpha = 0.4)
    }
  }else{
    for(d in 1:res_HiDiSTATIS$input$DESIGN_tables$D){
      hull.matrix <- t(res_HiDiSTATIS$Boot_Tables$Fab_boot_d_corrected[i,,d,])
      peeledHull(hull.matrix[,axes],percentage=percentage,lwd=4)
      peeledHull(hull.matrix[,axes],percentage=percentage,lwd=2,col=res_HiDiSTATIS$input$DESIGN_tables$colors_D[d])
    }
  }

}
