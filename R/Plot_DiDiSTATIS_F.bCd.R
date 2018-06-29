# Plot_DiDiSTATIS_F.bCd
#
#' Plot F.bCd (Discriminant Group Partial Factor Scores)
#'
#' @param res_DiDiSTATIS The output of DiDiSTATIS
#' @param axes Axes to plot
#' @param b The Category to plot
#' @param d The Group to plot
#' @param Flip_axis1 Flag to flip Component 1
#' @param Flip_axis2 Flag to flip Component 2
#' @param dev.new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @export

Plot_DiDiSTATIS_F.bCd <- function(res_DiDiSTATIS, axes = c(1,2), b = 1, d = 1,
                                  Flip_axis1 = FALSE, Flip_axis2 = FALSE, dev.new = TRUE){

  Flip_mat <- makeFlip_mat(Flip_axis1, Flip_axis2)

  TheTitle <- paste0("DiDiSTATIS F.bCd")

  # #which category is that stimulus in?
  # b <- which(res_DiDiSTATIS$input$DESIGN_rows$mat[ab,]==1)
  # #which stimuli are also in that category?
  # Ab <- which(res_DiDiSTATIS$input$DESIGN_rows$mat[,b]==1)
  #which participants are in that group?
  Cd <- which(res_DiDiSTATIS$input$DESIGN_tables$mat[,d]==1)

  alpha = .8
  prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond[,axes] %*% Flip_mat,
             col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B, alpha),
             cex = 3, pch=15,
             dev.new = dev.new,
             xlab = paste0("Component ", axes[1]),
             ylab = paste0("Component ", axes[2]),
             constraints = minmaxHelper(mat1 = rbind_array_2_matrix(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[,axes,]) %*% Flip_mat,
                                        mat2 = t(res_DiDiSTATIS$res_BaryGrand$Proj_B.cd$F_B.cd_Cond[b,axes,Cd]) %*% Flip_mat),
             main = TheTitle)

  for(dee in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
    Segments_from_to(From = res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond[,axes] %*% Flip_mat,
                     To   = res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[,axes,dee] %*% Flip_mat,
                     col  = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B, alpha),
                     lwd = 2)
    prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[,axes,dee] %*% Flip_mat,
               col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[dee],alpha),
               cex=2.5, pch = 19,
               new.plot = F, dev.new = F)
  }

  legend(x = "right",
         legend = c(res_DiDiSTATIS$input$DESIGN_rows$labels, " ", res_DiDiSTATIS$input$DESIGN_tables$labels),
         col = add.alpha(c(res_DiDiSTATIS$input$DESIGN_rows$colors_B, 'white', res_DiDiSTATIS$input$DESIGN_tables$colors_D), alpha),
         pch = c(rep(15, res_DiDiSTATIS$input$DESIGN_rows$B), 15, rep(19, res_DiDiSTATIS$input$DESIGN_tables$D)),
         pt.cex = 2)

  prettyPlot(t(res_DiDiSTATIS$res_BaryGrand$Proj_B.cd$F_B.cd_Cond[b,axes,Cd]),
             dev.new = F, new.plot = F,
             pch = 19, cex = 1.7,
             col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], .5))

  Segments_from_to(From = res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[b,axes,d] %*% Flip_mat,
                   To = t(res_DiDiSTATIS$res_BaryGrand$Proj_B.cd$F_B.cd_Cond[b,axes,Cd]) %*% Flip_mat,
                   XandY = c(1,2),
                   col = add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], .5),
                   lwd = 1)



}


