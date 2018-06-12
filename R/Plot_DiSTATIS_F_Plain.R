#Plot_DiSTATIS_F_Plain.R
#'
#' Plot DiSTATIS Compromise Factor Scores
#'
#' @param res_DiSTATIS The output of EigenDiSTATIS
#' @param axes Axes to plot, by default c(1,2)
#' @param Flip_axis1 Flag to flip Component 1
#' @param Flip_axis2 Flag to flip Component 2
#' @param dev.new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @export

Plot_DiSTATIS_F_Plain <- function(res_DiSTATIS, axes = c(1,2), Flip_axis1 = FALSE, Flip_axis2=FALSE, dev.new = TRUE){

  Flip_mat <- makeFlip_mat(Flip_axis1, Flip_axis2)

  prettyPlot(res_DiSTATIS$res_DiSTATIS$eig$F[,axes] %*% Flip_mat,
             dev.new = dev.new,
             display_names = FALSE,
             cex=1.5,
             xlab = paste0("Component ", axes[1]," variance = ", round(res_DiSTATIS$res_DiSTATIS$eig$t[axes][1],1), "%"),
             ylab = paste0("Component ", axes[2]," variance = ", round(res_DiSTATIS$res_DiSTATIS$eig$t[axes][2],1), "%"),
             # col = res_DiSTATIS$input$DESIGN_rows$colors_AB,
             col = add.alpha("black", .7),
             pch=15,
             main = paste0('DiSTATIS F_Plain'))

  # prettyPlot(res_DiSTATIS$res_DiSTATIS$eig$F_Cond[,axes] %*% Flip_mat,
  #            display_names = F,
  #            dev.new = F, new.plot = F,
  #            # text.cex = 1.5,
  #            cex = 3, pch = 15,
  #            col = res_DiSTATIS$input$DESIGN_rows$colors_B)

}

