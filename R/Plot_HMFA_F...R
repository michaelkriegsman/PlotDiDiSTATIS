# Plot_HMFA_F..
#
#' Plot F.. (Grand Factor Scores)
#'
#' @param res_HMFA The output of HMFA
#' @param axes Axes to plot
#' @param Flip_axis1 Flag to flip Component 1
#' @param Flip_axis2 Flag to flip Component 2
#' @param dev.new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @export

Plot_HMFA_F.. <- function(res_HMFA, axes = c(1,2), Flip_axis1 = FALSE, Flip_axis2 = FALSE, dev.new = TRUE){

  Flip_mat <- makeFlip_mat(Flip_axis1, Flip_axis2)

  alpha <- .8
  prettyPlot(res_HMFA$res_HMFA$eig$F[,axes] %*% Flip_mat,
             col = add.alpha(res_HMFA$input$DESIGN_rows$colors_AB, alpha),
             display_names = F,
             xlab = paste0("Component ", axes[1]," variance = ", round(res_HMFA$res_HMFA$eig$t[axes][1],1), "%"),
             ylab = paste0("Component ", axes[2]," variance = ", round(res_HMFA$res_HMFA$eig$t[axes][2],1), "%"),
             cex = 2, pch=15, dev.new = dev.new,
             main = "Grand Factor Scores \nF.(.)")


  prettyPlot(res_HMFA$input$DESIGN_rows$Pb_Cond %*% res_HMFA$res_HMFA$eig$F[,axes] %*% Flip_mat,
             display_points = F,
             dev.new = F, new.plot = F,
             text.cex = 1.75,
             col = add.alpha(res_HMFA$input$DESIGN_rows$colors_B, alpha))

}
