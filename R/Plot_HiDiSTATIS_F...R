# Plot_HiDiSTATIS_F..
#
#' Plot F.. (HiDiSTATIS Grand Compromise Factor Scores)
#'
#' @param res_HiDiSTATIS The output of HiDiSTATIS
#' @param axes Axes to plot
#' @param Flip_axis1 Flag to flip Component 1
#' @param Flip_axis2 Flag to flip Component 2
#' @param dev.new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @return Grand Compromise Factor Scores
#' @export

Plot_HiDiSTATIS_F.. <- function(res_HiDiSTATIS, axes = c(1,2), Flip_axis1 = FALSE, Flip_axis2 = FALSE, dev.new = TRUE){

  Flip_mat <- makeFlip_mat(Flip_axis1, Flip_axis2)

  alpha <- .8
  prettyPlot(res_HiDiSTATIS$res_GrandComp$eig$F[,axes] %*% Flip_mat,
             col = add.alpha(res_HiDiSTATIS$input$DESIGN_rows$colors_AB, alpha),
             display_names = F,
             xlab = paste0("Component ", axes[1]," variance = ", round(res_HiDiSTATIS$res_GrandComp$eig$t[axes][1],1), "%"),
             ylab = paste0("Component ", axes[2]," variance = ", round(res_HiDiSTATIS$res_GrandComp$eig$t[axes][2],1), "%"),
             cex = 1.25, pch=15, dev.new = dev.new,
             main = "HiDiSTATIS Grand Compromise Factor Scores \nF.(.)")


  prettyPlot(res_HiDiSTATIS$input$DESIGN_rows$Pb_Cond %*% res_HiDiSTATIS$res_GrandComp$eig$F[,axes] %*% Flip_mat,
             display_points = F,
             dev.new = F, new.plot = F,
             text.cex = 1.5,
             col = add.alpha(res_HiDiSTATIS$input$DESIGN_rows$colors_B, alpha))

}
