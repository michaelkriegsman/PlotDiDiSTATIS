# Plot_HiDiSTATIS_F.d
#
#' Plot F.d (Group Factor Scores)
#'
#' @param res_HiDiSTATIS The output of HiDiSTATIS
#' @param axes Axes to plot
#' @param d Which group to plot
#' @param Flip_axis1 Flag to flip Component 1
#' @param Flip_axis2 Flag to flip Component 2
#' @param dev.new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @export

Plot_HiDiSTATIS_F.d <- function(res_HiDiSTATIS, axes = c(1,2), d = 1, Flip_axis1 = FALSE, Flip_axis2 = FALSE, dev.new = TRUE){

  Flip_mat <- makeFlip_mat(Flip_axis1, Flip_axis2)

  constraints <- minmaxHelper(mat1 = res_HiDiSTATIS$res_GrandComp$eig$F[,axes] %*% Flip_mat,
                              mat2 = rbind_array_2_matrix(res_HiDiSTATIS$res_GrandComp$ProjGroup$F[,axes,]) %*% Flip_mat)

  alpha <- .8
  prettyPlot(res_HiDiSTATIS$res_GrandComp$eig$F[,axes] %*% Flip_mat,
             col = add.alpha(res_HiDiSTATIS$input$DESIGN_rows$colors_AB, alpha),
             display_names = F,
             constraints = constraints,
             xlab = paste0("Component ", axes[1]),
             ylab = paste0("Component ", axes[2]),
             cex = 1.25, pch=15, dev.new = dev.new,
             main = paste0("HiDiSTATIS Group Factor Scores \nF.(d) for ", res_HiDiSTATIS$input$DESIGN_tables$labels[d]))

  Segments_from_to(From = res_HiDiSTATIS$res_GrandComp$eig$F[,axes] %*% Flip_mat,
                   To = res_HiDiSTATIS$res_GrandComp$ProjGroup$F[,axes,d] %*% Flip_mat,
                   XandY = axes,
                   col = add.alpha(res_HiDiSTATIS$input$DESIGN_rows$colors_AB, alpha),
                   lwd = 2)

  prettyPlot(res_HiDiSTATIS$res_GrandComp$ProjGroup$F[,axes,d] %*% Flip_mat,
             display_names = F,
             dev.new = F, new.plot = F,
             cex = 2, pch = 16,
             col = add.alpha(res_HiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha))

}
