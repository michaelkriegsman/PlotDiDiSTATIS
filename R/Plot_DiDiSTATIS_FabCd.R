# Plot_DiDiSTATIS_FabCd
#
#' Plot FabCd (Discriminant Group Partial Factor Scores)
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

Plot_DiDiSTATIS_FabCd <- function(res_DiDiSTATIS, axes = c(1,2), ab = 1, d = 1, quiet_B.. = FALSE,
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
             col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B, alpha_background),
             cex = 3, pch=15,
             display_names = F,
             dev.new = dev.new,
             xlab = paste0("Component ", axes[1]),
             ylab = paste0("Component ", axes[2]),
             constraints = minmaxHelper(mat1 = t(res_DiDiSTATIS$res_BaryGrand$Proj_disc.cd$F_disc.cd[ab,axes,Cd]) %*% Flip_mat,
                                        mat2 = res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[,axes] %*% Flip_mat),
             main = TheTitle)

  prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond[c(b,b),axes] %*% Flip_mat,
             col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B[b], alpha_foreground),
             cex = 3, pch=15,
             display_names = F,
             dev.new = F, new.plot = F)

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
                   To = res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[Ab,axes] %*% Flip_mat,
                   XandY = c(1,2),
                   col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B[b], alpha_background),
                   lwd = 1)

  Segments_from_to(From = res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond[b,axes] %*% Flip_mat,
                   To = res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[ab,axes] %*% Flip_mat,
                   XandY = c(1,2),
                   col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B[b], alpha_background),
                   lwd = 3)

  # Fab.d ####

  prettyPlot(t(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[ab,axes,]) %*% Flip_mat,
             col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D, alpha_background),
             cex = 2, pch=19,
             display_names = F,
             dev.new = F, new.plot = F)

  Segments_from_to(From = res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[ab,axes] %*% Flip_mat,
                   To = t(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[ab,axes,]) %*% Flip_mat,
                   XandY = c(1,2),
                   col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D, alpha_background),
                   lwd = 1)

  prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[ab,axes,d] %*% Flip_mat,
             col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha_foreground),
             cex = 2, pch=19,
             display_names = F,
             dev.new = F, new.plot = F)

  Segments_from_to(From = res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..[ab,axes] %*% Flip_mat,
                   To = t(res_DiDiSTATIS$res_BaryGrand$Proj_disc.D$F_disc.D[ab,axes,d]) %*% Flip_mat,
                   XandY = c(1,2),
                   col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D, alpha_background),
                   lwd = 3)


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

  Bary_cols <- res_DiDiSTATIS$input$DESIGN_rows$colors_B
  Bary_cols[b] <- add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B[b], alpha_foreground + alpha_background)

  Bary_cols[-b] <- add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B[-b], alpha_background)

  Group_cols <- res_DiDiSTATIS$input$DESIGN_tables$colors_D
  Group_cols[d] <- add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha_foreground + alpha_background)
  Group_cols[-d] <- add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[-d], alpha_background)

  legend(x = "right",
         legend = c(res_DiDiSTATIS$input$DESIGN_rows$labels, " ", res_DiDiSTATIS$input$DESIGN_tables$labels),
         col = c(Bary_cols, 'white', Group_cols),
         pch = c(rep(15, res_DiDiSTATIS$input$DESIGN_rows$B), 15, rep(19, res_DiDiSTATIS$input$DESIGN_tables$D)),
         pt.cex = 2)

}


