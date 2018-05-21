# Plot_DiDiSTATIS_F.b.d
#
#' Plot F.B.D (Barycentric Group Partial Factor Scores)
#'
#' @param res_DiDiSTATIS The output of DiDiSTATIS
#' @param axes Axes to plot
#' @param Flip_axis1 Flag to flip Component 1
#' @param Flip_axis2 Flag to flip Component 2
#' @param dev.new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @export


Plot_DiDiSTATIS_F.B.D <- function(res_DiDiSTATIS, axes = c(1,2), Flip_axis1 = FALSE, Flip_axis2 = FALSE, dev.new = TRUE){

  Flip_mat <- makeFlip_mat(Flip_axis1, Flip_axis2)

  alpha = .8
  prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond[,axes] %*% Flip_mat,
             col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B, alpha),
             cex = 3, pch=15,
             dev.new = dev.new,
             xlab = paste0("Component ", axes[1]),
             ylab = paste0("Component ", axes[2]),
             constraints = minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[,axes,]) %*% Flip_mat),
             main = paste0("DiDiSTATIS Barycentric Group Compromises, F.b.d, \n MFA2 = ",as.character(res_DiDiSTATIS$input$MFA2_Flag)))

  for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
    Segments_from_to(From = res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond[,axes] %*% Flip_mat,
                     To   = res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[,axes,d] %*% Flip_mat,
                     col  = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B, alpha),
                     lwd = 2)
    prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[,axes,d] %*% Flip_mat,
               col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d],alpha),
               cex=2.5, pch = 19,
               new.plot = F, dev.new = F)
  }

}

