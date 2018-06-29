# Plot_DiDiSTATIS_FaB.d_v2
#
#' Plot FaB.d_v2 (Discriminant Group Partial Factor Scores)
#'
#' @param res_DiDiSTATIS The output of DiDiSTATIS
#' @param axes Axes to plot
#' @param ab Which stimulus to plot
#' @param d Which group to plot
#' @param Flip_axis1 Flag to flip Component 1
#' @param Flip_axis2 Flag to flip Component 2
#' @param dev.new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @export

Plot_DiDiSTATIS_FaB.d_v2 <- function(res_DiDiSTATIS, axes = c(1,2), ab = 1, d = 1,
                                     Flip_axis1 = FALSE, Flip_axis2 = FALSE, dev.new = TRUE){

  Flip_mat <- makeFlip_mat(Flip_axis1, Flip_axis2)

  TheTitle <- paste0("DiDiSTATIS Disc Group Compromises, Fab.d \nMFA2 = ",as.character(res_DiDiSTATIS$input$MFA2_Flag)," \nab = ", ab, "\nd = ", d)

  alpha_foreground <- .8
  alpha_background <- .4

  # #which category is that stimulus in?
  b <- which(res_DiDiSTATIS$input$DESIGN_rows$mat[ab,]==1)
  # #which stimuli are also in that category?
  Ab <- which(res_DiDiSTATIS$input$DESIGN_rows$mat[,b]==1)


  prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond[,axes] %*% Flip_mat,
               col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B, alpha_background),
               cex = 3, pch=15,
               display_points = T,
               dev.new = dev.new,
               xlab = paste0("Component ", axes[1]),
               ylab = paste0("Component ", axes[2]),
               constraints = minmaxHelper(mat1 = res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[Ab,axes,d] %*% Flip_mat,
                                          mat2 = res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond[,axes] %*% Flip_mat),
               main = TheTitle)

  prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond[b,axes] %*% Flip_mat,
               col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B[b], alpha_foreground),
               dev.new = F, new.plot = F,
             cex = 3, pch=15)


    Group_cols <- res_DiDiSTATIS$input$DESIGN_tables$colors_D
      Group_cols[d] <- add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha_foreground)
      Group_cols[-d] <- add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[-d], alpha_background)


    legend(x = "right",
           legend = c(res_DiDiSTATIS$input$DESIGN_rows$labels, " ", res_DiDiSTATIS$input$DESIGN_tables$labels),
           col = c(add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B, alpha_foreground), 'white', Group_cols),
           pch = c(rep(15, res_DiDiSTATIS$input$DESIGN_rows$B), 15, rep(19, res_DiDiSTATIS$input$DESIGN_tables$D)),
           pt.cex = 2)

    Segments_from_to(From = res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond[b,axes] %*% Flip_mat,
                       To   = t(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[b,axes,]) %*% Flip_mat,
                       col  = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha_background),
                       lwd = 1.5)

    prettyPlot(t(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[b,axes,]) %*% Flip_mat,
                 col = Group_cols,
                 cex=2, pch=19,
                 new.plot = F, dev.new = F)


    Segments_from_to(From = res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[b,axes,d] %*% Flip_mat,
                     To   = res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[Ab,axes,d] %*% Flip_mat,
                     col  = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha_foreground),
                     lwd = 1.5)


    prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[Ab,axes,d] %*% Flip_mat,
                 # col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], .8),
                 col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B[b], alpha_foreground),
                 cex=1, pch=15,
                 new.plot = F, dev.new = F)







}


