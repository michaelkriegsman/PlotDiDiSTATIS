# Plot_HMFA_F.d
#
#' Plot F.d (Group Factor Scores)
#'
#' @param res_HMFA The output of HMFA
#' @param axes Axes to plot
#' @param d Which group to plot
#' @param Flip_axis1 Flag to flip Component 1
#' @param Flip_axis2 Flag to flip Component 2
#' @param dev.new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @export

Plot_HMFA_F.d <- function(res_HMFA, axes = c(1,2), d = 1, Flip_axis1 = FALSE, Flip_axis2 = FALSE, dev.new = TRUE){

  Flip_mat <- makeFlip_mat(Flip_axis1, Flip_axis2)

  constraints <- minmaxHelper(mat1 = res_HMFA$res_HMFA$eig$F[,axes] %*% Flip_mat,
                             mat2 = rbind_array_2_matrix(res_HMFA$res_HMFA$ProjGroup$F[,axes,]) %*% Flip_mat)

  alpha <- .8
  prettyPlot(res_HMFA$res_HMFA$eig$F[,axes] %*% Flip_mat,
             col = add.alpha(res_HMFA$input$DESIGN_rows$colors_AB, alpha),
             display_names = F,
             constraints = constraints,
             cex = 1.25, pch=15, dev.new = dev.new,
             main = paste0("Group Factor Scores \nF.(d) for ", res_HMFA$input$DESIGN_tables$labels[d]))

  Segments_from_to(From = res_HMFA$res_HMFA$eig$F[,axes] %*% Flip_mat,
                   To = res_HMFA$res_HMFA$ProjGroup$F[,axes,d] %*% Flip_mat,
                   XandY = axes,
                   col = add.alpha(res_HMFA$input$DESIGN_rows$colors_AB, alpha),
                   lwd = 2)

  prettyPlot(res_HMFA$res_HMFA$ProjGroup$F[,axes,d] %*% Flip_mat,
             display_names = F,
             dev.new = F, new.plot = F,
             cex = 2, pch = 16,
             col = add.alpha(res_HMFA$input$DESIGN_tables$colors_D[d], alpha))

}
