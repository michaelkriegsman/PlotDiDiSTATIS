#Plot_MFA_F.R
#'
#' Plot MFA Consensus Factor Scores
#'
#' @param res_MFA The output of EigenMFA
#' @param axes Axes to plot, by default c(1,2)
#' @param Flip_axis1 Flag to flip Component 1
#' @param Flip_axis2 Flag to flip Component 2
#' @param dev.new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @export

Plot_MFA_F <- function(res_MFA, axes = c(1,2), Flip_axis1 = FALSE, Flip_axis2=FALSE, dev.new = TRUE){

  Flip_mat <- makeFlip_mat(Flip_axis1, Flip_axis2)

  prettyPlot(res_MFA$res_MFA$eig$F[,axes] %*% Flip_mat,
             dev.new = dev.new,
             display_names = FALSE, cex=1.5,
             xlab = paste0("Component ", axes[1]," variance = ", round(res_MFA$res_MFA$eig$t[axes][1],1), "%"),
             ylab = paste0("Component ", axes[2]," variance = ", round(res_MFA$res_MFA$eig$t[axes][2],1), "%"),
             col = res_MFA$input$DESIGN_rows$colors_AB,
             pch=15,
             main = paste0('MFA Compromise Factor Scores'))

  prettyPlot(res_MFA$res_MFA$eig$F_Cond[,axes] %*% Flip_mat,
             display_points = F,
             dev.new = F, new.plot = F,
             text.cex = 2,
             col = res_MFA$input$DESIGN_rows$colors_B)

}

