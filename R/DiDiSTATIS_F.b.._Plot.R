# DiDiSTATIS_F.b.._Plot
#
#' Plot F.b.. (Barycentric Grand Factor Scores)
#'
#' @param res_DiDiSTATIS The output of DiDiSTATIS
#' @param axes Axes to plot
#'

DiDiSTATIS_F.b.._Plot <- function(res_DiDiSTATIS, axes = c(1,2)){

  prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond,
             col = res_DiDiSTATIS$input$DESIGN_rows$colors_B,
             cex = 3, pch=15,
             constraints = minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond)),
             main = "Barycentric Grand Compromise")

}

