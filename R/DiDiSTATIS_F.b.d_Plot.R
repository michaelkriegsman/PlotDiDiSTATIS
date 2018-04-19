# DiDiSTATIS_F.b.d_Plot
#
#' Plot F.b.d (Barycentric Group Partial Factor Scores)
#'
#' @param res_DiDiSTATIS The output of DiDiSTATIS
#' @param axes Axes to plot


DiDiSTATIS_F.b.d_Plot <- function(res_DiDiSTATIS, axes = c(1,2)){

  prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond, col = res_DiDiSTATIS$input$DESIGN_rows$colors_B,
             cex = 3, pch=15,
             constraints = minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond)),
             main = "Barycentric Group Compromises")

  for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
    Segments_from_to(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[,,d], res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond,
                     col = res_DiDiSTATIS$input$DESIGN_rows$colors_B, lwd=2)
    prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[,,d], col = res_DiDiSTATIS$input$DESIGN_tables$colors_D[d],
               cex=2.5, pch = 19,
               new.plot = F, dev.new = F)
  }

}

