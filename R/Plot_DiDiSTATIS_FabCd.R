# Plot_DiDiSTATIS_FaB.d
#
#' Plot FaB.d (Discriminant Group Partial Factor Scores)
#'
#' @param res_DiDiSTATIS The output of DiDiSTATIS
#' @param axes Axes to plot
#' @param ab The stimulus to plot
#' @param d The group to plot
#' @param quiet_B.. Flag to suppress B..
#' @param Flip_axis1 Flag to flip Component 1
#' @param Flip_axis2 Flag to flip Component 2
#' @param dev.new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @export

Plot_DiDiSTATIS_FabCd <- function(res_DiDiSTATIS, axes = c(1,2), ab = 5, d = 2, quiet_B.. = FALSE,
                                  Flip_axis1 = FALSE, Flip_axis2 = FALSE, dev.new = TRUE){

  Flip_mat <- makeFlip_mat(Flip_axis1, Flip_axis2)

  TheTitle <- paste0("DiDiSTATIS Disc Individual Partial Factor Scores, FabCd")

  #which category is that stimulus in?
  b <- which(res_DiDiSTATIS$input$DESIGN_rows$mat[ab,]==1)
  #which stimuli are also in that category?
  Ab <- which(res_DiDiSTATIS$input$DESIGN_rows$mat[,b]==1)
  #which participants are in that group?
  Cd <- which(res_DiDiSTATIS$input$DESIGN_tables$mat[,d]==1)

  alpha_foreground <- .7
  alpha_background <- .3
  alpha_B.. <- alpha_background
  if(isTRUE(quiet_B..)){ alpha_B.. <- 0 }

  # F.b.. ####
  prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond[,axes] %*% Flip_mat,
             col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B, alpha_B..),
             cex = 3, pch=15,
             display_names = F,
             dev.new = dev.new,
             xlab = paste0("Component ", axes[1]),
             ylab = paste0("Component ", axes[2]),
             constraints = minmaxHelper(mat1 = t(res_DiDiSTATIS$res_BaryGrand$Proj_disc.cd$F_disc.cd[ab,axes,Cd]) %*% Flip_mat,
                                        mat2 = res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[,axes] %*% Flip_mat),
             main = TheTitle)

  # prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond[c(b,b),axes] %*% Flip_mat,
  #            col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B[b], alpha_foreground),
  #            cex = 2, pch=15,
  #            display_names = F,
  #            dev.new = F, new.plot = F)

  # Fab.. ####

  prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[Ab,axes] %*% Flip_mat,
             col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B[b], alpha_background),
             cex = 1.5, pch=15,
             display_names = F,
             dev.new = F, new.plot = F)

  prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[ab,axes] %*% Flip_mat,
             col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B[b], alpha_foreground),
             cex = 1.5, pch=15,
             display_names = F,
             dev.new = F, new.plot = F)

  Segments_from_to(From = res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond[b,axes] %*% Flip_mat,
                   To = res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[ab,axes] %*% Flip_mat,
                   XandY = axes,
                   col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B[b], alpha_background),
                   lwd = 1)

  # Fab.d ####

  prettyPlot(t(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[ab,axes,]) %*% Flip_mat,
             col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D, alpha_background),
             cex = 1.5, pch=19,
             display_names = F,
             dev.new = F, new.plot = F)

  Segments_from_to(From = res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[ab,axes] %*% Flip_mat,
                   To = res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[ab,axes,d] %*% Flip_mat,
                   XandY = axes,
                   col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha_background),
                   lwd = 2)

  prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[ab,axes,d] %*% Flip_mat,
             col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha_foreground),
             cex = 1.5, pch=19,
             display_names = F,
             dev.new = F, new.plot = F)


  #FabCd
  prettyPlot(t(res_DiDiSTATIS$res_BaryGrand$Proj_disc.cd$F_disc.cd[ab,axes,Cd])%*% Flip_mat,
             col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha_foreground),
             cex = 1, pch=19,
             display_names = F,
             dev.new = F, new.plot = F)

  Segments_from_to(From = res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[ab,axes,d] %*% Flip_mat,
                   To   = t(res_DiDiSTATIS$res_BaryGrand$Proj_disc.cd$F_disc.cd[ab,axes,Cd])%*% Flip_mat,
                   col  = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha_background),
                   lwd = 1)


}


