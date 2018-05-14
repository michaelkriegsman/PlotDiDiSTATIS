#Plot_MFA_FiK.R
#'
#' Plot MFA Partial Factor Scores
#'
#' @param res_MFA The output of EigenMFA
#' @param axes Axes to plot, by default c(1,2)
#' @param i Which rows to plot (i = 0 gives all)
#' @param Flip_axis1 Flag to flip Component 1
#' @param Flip_axis2 Flag to flip Component 2
#' @param dev.new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @export

Plot_MFA_FiK <- function(res_MFA, axes = c(1,2), i = 0, Flip_axis1 = FALSE, Flip_axis2=FALSE, dev.new = TRUE){


  Flip_mat <- makeFlip_mat(Flip_axis1, Flip_axis2)

  #format the row index, i
  if(length(i)==1) {
    if(i == 0) {
      i <- 1:res_MFA$input$DESIGN_rows$AB
    }else{ #protect against prettyPlot doesn't like to plot a single point
      i <- rep(i,2)
    }
  }


  constraints_PFS <- minmaxHelper(rbind_array_2_matrix(res_MFA$res_MFA$ProjCP$F[i,axes,]) %*% Flip_mat)

  alpha1 <- .5
  prettyPlot(res_MFA$res_MFA$eig$F[,axes] %*% Flip_mat,
             display_names = F, cex=1.5,
             dev.new = dev.new,
             constraints = constraints_PFS,
             xlab = paste0("Component ", axes[1]),
             ylab = paste0("Component ", axes[2]),
             col = add.alpha(res_MFA$input$DESIGN_rows$colors_AB,alpha1),
             pch=15,
             main = paste0('PFS (i = 20, 33)'))

  prettyPlot(res_MFA$res_MFA$eig$F[i,axes] %*% Flip_mat,
             dev.new = F, new.plot = F,
             display_names = TRUE, cex=1.5,
             col = res_MFA$input$DESIGN_rows$colors_AB[i],
             pch=15)


  for(k in 1:res_MFA$input$DESIGN_tables$CD){
    prettyPlot(res_MFA$res_MFA$ProjCP$F[i, axes, k] %*% Flip_mat,
               dev.new=FALSE, new.plot=FALSE,
               display_names = FALSE,
               col = res_MFA$input$DESIGN_rows$colors_AB[i],
               pch=15)
    segments(res_MFA$res_MFA$eig$F[i,axes[1]] * Flip_mat[1,1],
             res_MFA$res_MFA$eig$F[i,axes[2]] * Flip_mat[2,2],
             res_MFA$res_MFA$ProjCP$F[i, axes[1], k] * Flip_mat[1,1],
             res_MFA$res_MFA$ProjCP$F[i, axes[2], k] * Flip_mat[2,2],
             col = res_MFA$input$DESIGN_rows$colors_AB[i])

  }
}



# SS_from_F(res_MFA$res_MFA$eig$F)
# apply(res_MFA$res_MFA$ProjCP$F, 3, SS_from_F)
## somehow, the consensus is 3x larger than the average PFS
