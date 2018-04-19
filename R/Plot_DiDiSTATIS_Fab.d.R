# Plot_DiDiSTATIS_Fab.d
#
#' Plot Fab.d (Discriminant Group Partial Factor Scores)
#'
#' @param res_DiDiSTATIS The output of DiDiSTATIS
#' @param axes Axes to plot


Plot_DiDiSTATIS_Fab.d <- function(res_DiDiSTATIS, axes = c(1,2)){

  prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond,
             col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B, .8),
             cex = 3, pch=15,
             constraints = minmaxHelper(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..),
             main = "Disc Grand Compromise")
  Segments_from_to(res_DiDiSTATIS$res_BaryGrand$eig$Fb.., res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..,
                   col = res_DiDiSTATIS$input$DESIGN_rows$colors_AB)
  prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_disc..$F_disc..,
             col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_AB, .8),
             dev.new = F, new.plot = F, pch=15, cex = 1.5)

}

