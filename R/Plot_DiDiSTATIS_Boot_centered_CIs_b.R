# Plot_DiDiSTATIS_Boot_centered_CIs_b.R
#
#' Plot histogram of permtuted squared distance from Groups to Grand Compromise
#'
#' @param res_DiDiSTATIS The output of DiDiSTATIS
#' @param axes Axes to plot
#' @param b The barycenter to plot
#' @param ellipse Boolean for ellipse or convex hull
#' @param percentage
#' @param Flip_axis1 Flag to flip Component 1
#' @param Flip_axis2 Flag to flip Component 2
#' @param dev.new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @return Histograms of the Dev2's
#' @export

Plot_DiDiSTATIS_Boot_centered_CIs_b <- function(res_DiDiSTATIS, axes = c(1,2), b = 1, ellipse = TRUE, percentage = 0.95,
                                                Flip_axis1 = FALSE, Flip_axis2 = FALSE, dev.new = TRUE){

  Flip_mat <- makeFlip_mat(Flip_axis1, Flip_axis2)

  #Also, plot the fixed grand compromise, and the fixed group compromises
  # And then put a 95% CI around the corrected group compromise factor scores.
  alpha <- .8
  prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond[c(b,b), axes] %*% Flip_mat,
             constraints=minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$Boot_Tables$F.b_boot_d_corrected[,axes,,]) %*% Flip_mat),
             display_names = F,
             dev.new = dev.new,
             col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B[b],alpha),
             xlab = paste0("Component ", axes[1]),
             ylab = paste0("Component ", axes[2]),
             pch = 15, cex = 2.5,
             main=paste0("DiDiSTATIS: 95% Bootstrap CIs, for ", colnames(res_DiDiSTATIS$input$DESIGN_rows$mat)[b]))

  prettyPlot(t(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[b,axes,]) %*% Flip_mat,
             col=add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D,alpha),
             pch = 19, cex = 1.5,
             dev.new = F, new.plot = F)


  if(ellipse){
    for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
      dataEllipse(x = res_DiDiSTATIS$Boot_Tables$F.b_boot_d_corrected[b, axes[1], d, ] * Flip_mat[1,1],
                  y = res_DiDiSTATIS$Boot_Tables$F.b_boot_d_corrected[b, axes[2], d, ] * Flip_mat[2,2],
                  add = TRUE, levels = percentage,
                  plot.points = FALSE, center.pch = FALSE,
                  col = res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], lwd = 3.5,
                  fill = TRUE, fill.alpha = 0.4)
    }
  }else{
    for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
      hull.matrix <- t(res_DiDiSTATIS$Boot_Tables$F.b_boot_d_corrected[b,axes,d,]) %*% Flip_mat
      peeledHull(hull.matrix[,axes],percentage=percentage,lwd=4)
      peeledHull(hull.matrix[,axes],percentage=percentage,lwd=2,col=res_DiDiSTATIS$input$DESIGN_tables$colors_D[d])
    }
  }

}




#
# #PLot corrected
# prettyPlot(rbind_array_2_matrix(res_DiDiSTATIS$Boot_Tables$F.b..boot_Cond), col=rep(res_DiDiSTATIS$input$DESIGN_rows$colors_B, res_DiDiSTATIS$Boot_Tables$Boot_tables_niter),
#            pch=22,
#            constraints = minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$Boot_Tables$F.b_boot_d_corrected)),
#            main = "Bootstrap F_B_.(.) and F_B_.(D)_corrected")
#
# for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
#   prettyPlot(rbind_array_2_matrix(res_DiDiSTATIS$Boot_Tables$F.b_boot_d_corrected[,,d,]),
#              pch = 16, col=add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], alpha = .3),
#              new.plot = F, dev.new = F)
#   for(b in 1:res_DiDiSTATIS$input$DESIGN_rows$B){
#     hull.matrix <- t(res_DiDiSTATIS$Boot_Tables$F.b_boot_d_corrected[b,,d,])
#     peeledHull(hull.matrix[,XandY],percentage=.95,lwd=4)
#     peeledHull(hull.matrix[,XandY],percentage=.95,lwd=2,col=res_DiDiSTATIS$input$DESIGN_tables$colors_D[d])
#   }
# }
#
# prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond, col = res_DiDiSTATIS$input$DESIGN_rows$colors_B,
#            dev.new = F, new.plot = F,
#            pch = 22, cex = 2.5, display_names = F)

