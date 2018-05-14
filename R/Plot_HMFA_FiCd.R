# Plot_HMFA_FiCd
#
#' Plot FiCd (Grand Factor Scores)
#'
#' @param res_HMFA The output of HMFA
#' @param axes Axes to plot
#' @param i Which row to plot
#' @param d Which group to plot
#' @param Flip_axis1 Flag to flip Component 1
#' @param Flip_axis2 Flag to flip Component 2
#' @param dev.new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @export

Plot_HMFA_FiCd <- function(res_HMFA, axes = c(1,2), i = 1, d = 1, Flip_axis1 = FALSE, Flip_axis2 = FALSE, dev.new = TRUE){


  Flip_mat <- makeFlip_mat(Flip_axis1, Flip_axis2)

  # #format the row index, i
  # if(length(i)==1) {
  #   if(i == 0) {
  #     i <- 1:res_MFA$input$DESIGN_rows$AB
  #   }else{ #protect against prettyPlot doesn't like to plot a single point
  #     i <- rep(i,2)
  #   }
  # }

  which_tables_Cd <- which(res_HMFA$input$DESIGN_tables$mat[,d]==1)

  constraints <- minmaxHelper(mat1 = res_HMFA$res_HMFA$eig$F[,axes] %*% Flip_mat,
                              mat2 = t(res_HMFA$res_HMFA$ProjCP$F[i, axes, which_tables_Cd]) %*% Flip_mat)

  #Plot Grand Factor Scores F.(.)
  alpha <- .8
  prettyPlot(res_HMFA$res_HMFA$eig$F[,axes] %*% Flip_mat,
             col = add.alpha(res_HMFA$input$DESIGN_rows$colors_AB, alpha),
             display_names = F,
             constraints = constraints,
             cex = 1.25, pch=15, dev.new = dev.new,
             main = paste0("HMFA: Stimuli are Hierarchically Barycentric \n i = ", i, ", d = ", d))

  #For a single stimulus, Plot F.(D), and connect the lines
  Segments_from_to(From = res_HMFA$res_HMFA$eig$F[i,axes] %*% Flip_mat,
                   To = t(res_HMFA$res_HMFA$ProjGroup$F[i,axes,]) %*% Flip_mat,
                   XandY = axes,
                   col = add.alpha(res_HMFA$input$DESIGN_tables$colors_D, alpha),
                   lwd = 2)


  prettyPlot(t(res_HMFA$res_HMFA$ProjGroup$F[i,axes,]) %*% Flip_mat,
             col = add.alpha(res_HMFA$input$DESIGN_tables$colors_D, alpha),
             display_names = F,
             cex = 1.75, pch=16,
             dev.new = F, new.plot = F)

  #For that stimulus, and for a given d, plot C(d), and connect the lines
  Segments_from_to(From = res_HMFA$res_HMFA$ProjGroup$F[i,axes,d] %*% Flip_mat,
                   To = t(res_HMFA$res_HMFA$ProjCP$F[i, axes, which_tables_Cd]) %*% Flip_mat,
                   XandY = axes,
                   col = add.alpha(res_HMFA$input$DESIGN_tables$colors_D[d], alpha),
                   lwd = 3)

  prettyPlot(t(res_HMFA$res_HMFA$ProjCP$F[i, axes, which_tables_Cd]) %*% Flip_mat,
             col = add.alpha(res_HMFA$input$DESIGN_tables$colors_D[d], alpha),
             display_names = F,
             cex = 2.25, pch=16,
             dev.new = F, new.plot = F)

}
