# Plot_DiDiSTATIS_F.b.d
#
#' Plot F.b.d (Barycentric Group Partial Factor Scores)
#'
#' @param res_DiDiSTATIS The output of DiDiSTATIS
#' @param axes Axes to plot
#' @export


Plot_DiDiSTATIS_F.b.d <- function(res_DiDiSTATIS, axes = c(1,2)){

  prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond, col = res_DiDiSTATIS$input$DESIGN_rows$colors_B,
             cex = 3, pch=15,
             constraints = minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond)),
             main = "Barycentric Group Compromises, F.b.d")

  for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
    segments(x0 = res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond[,axes[1]],
             y0 = res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond[,axes[2]],
             x1 = res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[,axes[1],d],
             y1 = res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[,axes[2],d],
             col = res_DiDiSTATIS$input$DESIGN_rows$colors_B,
             lwd = 2)
    prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[,,d], col = res_DiDiSTATIS$input$DESIGN_tables$colors_D[d],
               cex=2.5, pch = 19,
               new.plot = F, dev.new = F)
  }

}

