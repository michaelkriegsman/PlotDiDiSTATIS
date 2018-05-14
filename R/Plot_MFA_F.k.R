#Plot_MFA_F.k.R
#'
#' Plot MFA Partial Factor Scores
#'
#' @param res_MFA The output of EigenMFA
#' @param axes Axes to plot, by default c(1,2)
#' @param k Which table to plot
#' @param Flip_axis1 Flag to flip Component 1
#' @param Flip_axis2 Flag to flip Component 2
#' @param dev.new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @export

Plot_MFA_F.k <- function(res_MFA, axes = c(1,2), k = 1, Flip_axis1 = FALSE, Flip_axis2=FALSE, dev.new = TRUE){

  Flip_mat <- makeFlip_mat(Flip_axis1, Flip_axis2)

  # # Map 2: PFS of 1 table (participant)
  alpha1 <- .4
  prettyPlot(res_MFA$res_MFA$ProjCP$F[,axes,k] %*% Flip_mat,
             display_names = TRUE, cex=1.5,
             dev.new = dev.new,
             # constraints = constraints_PFS,
             xlab = paste0('Component ', axes[1]),
             ylab = paste0('Component ', axes[2]),
             pch=15,
             main = paste0('MFA Individual Partial Factor Scores
                           \n Participant ',rownames(res_MFA$input$DESIGN_tables$mat)[k],
                           ' (',
                           res_MFA$input$DESIGN_tables$labels[which(res_MFA$input$DESIGN_tables$mat[k,]==1)],
                           ')'),
             col = add.alpha(res_MFA$input$DESIGN_rows$colors_AB, alpha1)
             )

  alpha2 <- .8
  prettyPlot(res_MFA$input$DESIGN_rows$Pb_Cond %*% res_MFA$res_MFA$ProjCP$F[,axes,k] %*% Flip_mat,
             display_points = F,
             dev.new = F, new.plot = F,
             text.cex = 1.5,
             col = add.alpha(res_MFA$input$DESIGN_rows$colors_B, alpha2))

}


