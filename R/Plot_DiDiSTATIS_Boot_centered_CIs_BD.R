# Plot_DiDiSTATIS_Boot_centered_CIs_BD.R
#
#' Plot histogram of permtuted squared distance from Groups to Grand Compromise
#'
#' @param res_DiDiSTATIS The output of DiDiSTATIS
#' @param axes Axes to plot
#' @param ellipse Boolean for ellipse or convex hull
#' @param percentage 1 - alpha cutoff
#' @param Flip_axis1 Flag to flip Component 1
#' @param Flip_axis2 Flag to flip Component 2
#' @param dev.new Flag to appease ReporteRs (set FALSE to print results to pptx)
#' @return Histograms of the Dev2's
#' @export

Plot_DiDiSTATIS_Boot_centered_CIs_BD <- function(res_DiDiSTATIS, axes = c(1,2), ellipse = TRUE, percentage = 0.95,
                                                Flip_axis1 = FALSE, Flip_axis2 = FALSE, dev.new = TRUE){

  Flip_mat <- makeFlip_mat(Flip_axis1, Flip_axis2)

  #Also, plot the fixed grand compromise, and the fixed group compromises
  # And then put a 95% CI around the corrected group compromise factor scores.
  alpha <- .8
  prettyPlot(res_DiDiSTATIS$res_BaryGrand$eig$Fb..Cond[, axes] %*% Flip_mat,
             constraints=minmaxHelper(rbind_array_2_matrix(res_DiDiSTATIS$Boot_Tables$F.b_boot_d_corrected[,axes,,]) %*% Flip_mat),
             display_names = T, text.cex = 1.4,
             dev.new = dev.new,
             col = add.alpha(res_DiDiSTATIS$input$DESIGN_rows$colors_B,alpha),
             xlab = paste0("Component ", axes[1]),
             ylab = paste0("Component ", axes[2]),
             pch = 15, cex = 2.5,
             main=paste0("DiDiSTATIS: 95% Bootstrap CIs, for F.B.D"))

  for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
    prettyPlot(res_DiDiSTATIS$res_BaryGrand$Proj_B.D$F_B.D_Cond[,axes,d] %*% Flip_mat,
               col=add.alpha(res_DiDiSTATIS$input$DESIGN_tables$colors_D[d],alpha),
               pch = 19, cex = 1.5,
               dev.new = F, new.plot = F)
  }


  if(ellipse){
    for(b in 1:res_DiDiSTATIS$input$DESIGN_rows$B){
      for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
        dataEllipse(x = res_DiDiSTATIS$Boot_Tables$F.b_boot_d_corrected[b, axes[1], d, ] * Flip_mat[1,1],
                    y = res_DiDiSTATIS$Boot_Tables$F.b_boot_d_corrected[b, axes[2], d, ] * Flip_mat[2,2],
                    add = TRUE, levels = percentage,
                    plot.points = FALSE, center.pch = FALSE,
                    col = res_DiDiSTATIS$input$DESIGN_tables$colors_D[d], lwd = 3.5,
                    fill = TRUE, fill.alpha = 0.4)
      }
    }

  }else{
    for(b in 1:res_DiDiSTATIS$input$DESIGN_rows$B){
      for(d in 1:res_DiDiSTATIS$input$DESIGN_tables$D){
        hull.matrix <- t(res_DiDiSTATIS$Boot_Tables$F.b_boot_d_corrected[b,axes,d,]) %*% Flip_mat
        peeledHull(hull.matrix[,axes],percentage=percentage,lwd=4)
        peeledHull(hull.matrix[,axes],percentage=percentage,lwd=2,col=res_DiDiSTATIS$input$DESIGN_tables$colors_D[d])
      }
    }

  }

  legend(x = "right",
         legend = c(res_DiDiSTATIS$input$DESIGN_rows$labels, " ", res_DiDiSTATIS$input$DESIGN_tables$labels),
         col = add.alpha(c(res_DiDiSTATIS$input$DESIGN_rows$colors_B, 'white', res_DiDiSTATIS$input$DESIGN_tables$colors_D), alpha),
         pch = c(rep(15, res_DiDiSTATIS$input$DESIGN_rows$B), 15, rep(19, res_DiDiSTATIS$input$DESIGN_tables$D)),
         pt.cex = 2)

}


