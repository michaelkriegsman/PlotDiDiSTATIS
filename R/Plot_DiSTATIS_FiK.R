#Plot_DiSTATIS_FiK.R
#'
#' Plot DiSTATIS Partial Factor Scores
#'
#' @param res_DiSTATIS The output of EigenDiSTATIS
#' @param axes Axes to plot, by default c(1,2)
#' @param i Which rows to plot (i = 0 gives all)
#' @param Flip_axis1 Flag to flip Component 1
#' @param Flip_axis2 Flag to flip Component 2
#' @param dev.new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @export

Plot_DiSTATIS_FiK <- function(res_DiSTATIS, axes = c(1,2), i = 0, Flip_axis1 = FALSE, Flip_axis2=FALSE, dev.new = TRUE){

  TheMain <- paste0('PFS (i = ', i, ')')

  Flip_mat <- makeFlip_mat(Flip_axis1, Flip_axis2)

  #format the row index, i
  if(length(i)==1) {
    if(i == 0) {
      i <- 1:res_DiSTATIS$input$DESIGN_rows$AB
    }else{ #protect against prettyPlot doesn't like to plot a single point
      i <- rep(i,2)
    }
  }


  constraints_PFS <- minmaxHelper(rbind_array_2_matrix(res_DiSTATIS$res_DiSTATIS$ProjCP$F[i,axes,]) %*% Flip_mat)

  alpha1 <- .5
  prettyPlot(res_DiSTATIS$res_DiSTATIS$eig$F[,axes] %*% Flip_mat,
             display_names = F, cex=1,
             dev.new = dev.new,
             constraints = constraints_PFS,
             xlab = paste0("Component ", axes[1]),
             ylab = paste0("Component ", axes[2]),
             col = add.alpha(res_DiSTATIS$input$DESIGN_rows$colors_AB,alpha1),
             pch=15,
             main = TheMain)

  prettyPlot(res_DiSTATIS$res_DiSTATIS$eig$F[i,axes] %*% Flip_mat,
             dev.new = F, new.plot = F,
             display_names = TRUE, cex=1.5,
             col = res_DiSTATIS$input$DESIGN_rows$colors_AB[i],
             pch=15)


  for(k in 1:res_DiSTATIS$input$DESIGN_tables$CD){
    prettyPlot(res_DiSTATIS$res_DiSTATIS$ProjCP$F[i, axes, k] %*% Flip_mat,
               dev.new=FALSE, new.plot=FALSE,
               display_names = FALSE,
               col = res_DiSTATIS$input$DESIGN_rows$colors_AB[i],
               pch=15)
    segments(res_DiSTATIS$res_DiSTATIS$eig$F[i,axes[1]] * Flip_mat[1,1],
             res_DiSTATIS$res_DiSTATIS$eig$F[i,axes[2]] * Flip_mat[2,2],
             res_DiSTATIS$res_DiSTATIS$ProjCP$F[i, axes[1], k] * Flip_mat[1,1],
             res_DiSTATIS$res_DiSTATIS$ProjCP$F[i, axes[2], k] * Flip_mat[2,2],
             col = res_DiSTATIS$input$DESIGN_rows$colors_AB[i])

  }
}


