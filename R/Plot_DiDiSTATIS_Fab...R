# Plot_DiDiSTATIS_FAB..
#
#' Plot FAB.. (Discriminant Grand Factor Scores)
#'
#' @param res_DiDiSTATIS The output of DiDiSTATIS
#' @param axes Axes to plot
#' @param Flip_axis1 Flag to flip Component 1
#' @param Flip_axis2 Flag to flip Component 2
#' @param dev.new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @export


Plot_DiDiSTATIS_FAB.. <- function(res_DiDiSTATIS, axes = c(1,2), Flip_axis1 = FALSE, Flip_axis2 = FALSE, dev.new = TRUE){

  Flip_mat <- makeFlip_mat(Flip_axis1, Flip_axis2)

  alpha = .8
  prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond[,axes] %*% Flip_mat,
             col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B, .8),
             cex = 3, pch=15,
             dev.new = dev.new,
             xlab = paste0("Component ", axes[1]),
             ylab = paste0("Component ", axes[2]),
             constraints = minmaxHelper(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[,axes]  %*% Flip_mat),
             main = paste0("DiDiSTATIS Disc Grand Compromise, Fab.., \n MFA2 = ",as.character(res_DiDiSTATIS$input$MFA2_Flag)))

  Segments_from_to(From = res_DiDiSTATIS$res_BaryGrand$eig$Fb..[,axes] %*% Flip_mat,
                   To   = res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[,axes] %*% Flip_mat,
                   col  = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_AB, alpha))
  prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[,axes] %*% Flip_mat,
             col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_AB, .8),
             dev.new = F, new.plot = F, pch=15, cex = 1.5)

}

